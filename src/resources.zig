const std = @import("std");

const vk = @import("vulkan");

const utils_mod = @import("utils.zig");

const math = @import("math.zig");
const assets_mod = @import("assets.zig");

const Engine = @import("engine.zig");
const Device = Engine.VulkanContext.Api.Device;

const render_utils = @import("render_utils.zig");
const Buffer = render_utils.Buffer;
const GraphicsPipeline = render_utils.GraphicsPipeline;
const CmdBuffer = render_utils.CmdBuffer;

const main = @import("main.zig");
const allocator = main.allocator;

pub const CameraUniform = extern struct {
    camera: utils_mod.ShaderUtils.Camera,
    mouse: utils_mod.ShaderUtils.Mouse,
    world_to_screen: math.Mat4x4,
    frame: utils_mod.ShaderUtils.Frame,
};

pub const PushConstants = extern struct {
    first_draw_ctx: u32,
};

pub const Vertex = extern struct {
    pos: math.Vec3,
    normal: math.Vec3,
    uv: math.Vec4, // vec2
    bone_ids: [4]u32,
    bone_weights: [4]f32,
};

pub const Instance = extern struct {
    bone_offset: u32,
};

pub const UniformBinds = enum(u32) {
    camera,
    vertices,
    indices,
    instances,
    bones,
    call_ctxts,
    texture,
    line_vertex_buffer,

    pub fn bind(self: @This()) u32 {
        return @intFromEnum(self);
    }
};

pub const ResourceManager = struct {
    // static if we load all assets at startup :P
    assets: Assets,
    asset_buffers: AssetBuffers,

    // uniforms
    camera_uniform: CameraUniform = std.mem.zeroes(CameraUniform),
    camera_uniform_buf: Buffer,

    // dynamic as instances vary at runtime
    instances: InstanceResources,

    // uploads every frame :(
    jolt_debug_resources: JoltDebugResources,

    pub fn init(assets: Assets, engine: *Engine, pool: vk.CommandPool) !@This() {
        var camera_uniform_buf = try Buffer.new_initialized(&engine.graphics, .{
            .size = @sizeOf(CameraUniform),
            .usage = .{
                .uniform_buffer_bit = true,
            },
            .memory_type = .{
                // https://community.khronos.org/t/memory-type-practice-for-an-mvp-uniform-buffer/109458/7
                // we ideally want device local for cpu to gpu, but instance transforms are not a bottleneck (generally)
                // so we save this memory (device_local + host_visible) for more useful things
                // .device_local_bit = true,

                .host_visible_bit = true,
                .host_coherent_bit = true,
            },
            .desc_type = .uniform_buffer,
        }, std.mem.zeroes(CameraUniform), pool);
        errdefer camera_uniform_buf.deinit(&engine.graphics.device);

        return .{
            .assets = assets,
            .asset_buffers = try assets.upload(engine, pool),
            .camera_uniform_buf = camera_uniform_buf,
            .instances = try .init(engine, pool, .{}),
            .jolt_debug_resources = try .init(engine, pool, .{}),
        };
    }

    pub fn deinit(self: *@This(), device: *Device) void {
        self.assets.deinit();
        self.asset_buffers.deinit(device);
        self.camera_uniform_buf.deinit(device);
        self.instances.deinit(device);
        self.jolt_debug_resources.deinit(device);
    }

    pub fn add_binds(self: *@This(), builder: *render_utils.DescriptorSet.Builder) !void {
        const add_to_set = struct {
            fn func(set_builder: @TypeOf(builder), buf: *Buffer, bind: UniformBinds) !void {
                try set_builder.add(buf, bind.bind());
            }
        }.func;

        try add_to_set(builder, &self.camera_uniform_buf, .camera);
        try add_to_set(builder, &self.asset_buffers.vertex_buffer, .vertices);
        try add_to_set(builder, &self.asset_buffers.index_buffer, .indices);
        try add_to_set(builder, &self.instances.instance_buffer.current.buffer, .instances);
        try add_to_set(builder, &self.instances.bone_buffer.current.buffer, .bones);
        try add_to_set(builder, &self.instances.draw_ctx_buffer.current.buffer, .call_ctxts);

        try add_to_set(builder, &self.jolt_debug_resources.line_buffer.current.buffer, .line_vertex_buffer);
    }

    pub fn update_uniforms(self: *@This(), device: *Device) !void {
        const maybe_mapped = try device.mapMemory(self.camera_uniform_buf.memory, 0, vk.WHOLE_SIZE, .{});
        const mapped = maybe_mapped orelse return error.MappingMemoryFailed;
        // keeping memory mapped is not bad in vulkan
        defer device.unmapMemory(self.camera_uniform_buf.memory);

        const mem: *CameraUniform = @ptrCast(@alignCast(mapped));
        mem.* = self.camera_uniform;

        // flush manually + host_coherent_bit = false
        // device.flushMappedMemoryRanges(memory_range_count: u32, p_memory_ranges: [*]const MappedMemoryRange)
    }

    const BufferWithCapacity = struct {
        buffer: Buffer,
        capacity: u32,
        mapped_buffer_mem: ?*anyopaque = null,

        pub fn map(self: *@This(), device: *Device) !void {
            if (self.mapped_buffer_mem) |_| return;
            self.mapped_buffer_mem = try device.mapMemory(self.buffer.memory, 0, vk.WHOLE_SIZE, .{});
        }

        pub fn unmap(self: *@This(), device: *Device) void {
            if (self.mapped_buffer_mem) |_| device.unmapMemory(self.buffer.memory);
            self.mapped_buffer_mem = null;
        }

        pub fn deinit(self: *@This(), device: *Device) void {
            self.unmap(device);
            self.buffer.deinit(device);
        }
    };
    const ElementDoubleBuffer = struct {
        current: BufferWithCapacity,
        count: u32 = 0,
        back: ?BufferWithCapacity = null,
        usage: vk.BufferUsageFlags,
        memory_type: vk.MemoryPropertyFlags,
        desc_type: vk.DescriptorType,
        state: enum {
            enough,
            out_of_space,
            back_allocated,
        } = .enough,

        pub fn init(ctx: *Engine.VulkanContext, pool: vk.CommandPool, v: Buffer.Args, val: anytype) !@This() {
            const device = &ctx.device;

            var buf = try Buffer.new_initialized(ctx, v, val, pool);
            errdefer buf.deinit(device);

            var buf_with_cap = BufferWithCapacity{ .buffer = buf, .capacity = @intCast(v.size) };
            errdefer buf_with_cap.unmap(device);

            if (v.memory_type.host_visible_bit and v.memory_type.host_coherent_bit) {
                try buf_with_cap.map(device);
            }

            return .{
                .current = buf_with_cap,
                .usage = v.usage,
                .memory_type = v.memory_type,
                .desc_type = v.desc_type,
            };
        }

        pub fn deinit(self: *@This(), device: *Device) void {
            self.current.deinit(device);
            if (self.back) |*back| back.deinit(device);
        }

        pub fn alloc(self: *@This(), buf: anytype) u32 {
            const E = std.meta.Elem(@TypeOf(buf));

            var gpu_items: [*c]E = @ptrCast(@alignCast(self.current.mapped_buffer_mem.?));

            const can_alloc = @min(self.current.capacity - self.count, buf.len);
            @memcpy(gpu_items[self.count..][0..can_alloc], buf[0..can_alloc]);
            self.count += can_alloc;

            if (can_alloc < buf.len and self.state == .enough) {
                self.state = .out_of_space;
            }

            return can_alloc;
        }

        pub fn reset(self: *@This()) void {
            self.count = 0;
        }

        pub fn alloc_tick(self: *@This(), ctx: *Engine.VulkanContext, pool: vk.CommandPool, val: anytype) !void {
            const device = &ctx.device;

            switch (self.state) {
                .enough, .back_allocated => {},
                .out_of_space => {
                    const cap = self.current.capacity;
                    const new_cap = cap + cap / 2;
                    var buf = try Buffer.new_initialized(ctx, .{
                        .size = new_cap,
                        .usage = self.usage,
                        .memory_type = self.memory_type,
                        .desc_type = self.desc_type,
                    }, val, pool);
                    errdefer buf.deinit(device);

                    self.back = .{
                        .buffer = buf,
                        .capacity = new_cap,
                    };
                    errdefer self.back.?.unmap(device);
                    try self.back.?.map(device);

                    self.state = .back_allocated;
                },
            }
        }

        pub fn swap_tick(self: *@This(), device: *Device) bool {
            switch (self.state) {
                .enough, .out_of_space => return false,
                .back_allocated => {
                    // this is okay cuz we have an empty queue every frame
                    self.current.buffer.deinit(device);
                    self.current = self.back.?;
                    self.back = null;
                    self.state = .enough;
                    return true;
                },
            }
        }
    };

    pub const JoltDebugResources = struct {
        line_buffer: ElementDoubleBuffer,

        pub const LineVertex = extern struct {
            pos: math.Vec3,
            color: math.Vec4,
        };

        pub fn init(engine: *Engine, pool: vk.CommandPool, v: struct {
            line_vertex_cap: u32 = 40000,
        }) !@This() {
            const ctx = &engine.graphics;
            const device = &ctx.device;

            var line_buffer = try ElementDoubleBuffer.init(ctx, pool, .{
                .size = v.line_vertex_cap,
                .usage = .{ .storage_buffer_bit = true },
                .memory_type = .{
                    .device_local_bit = true,
                    .host_visible_bit = true,
                    .host_coherent_bit = true,
                },
            }, std.mem.zeroes(LineVertex));
            errdefer line_buffer.deinit(device);

            return .{ .line_buffer = line_buffer };
        }

        pub fn deinit(self: *@This(), device: *Device) void {
            self.line_buffer.deinit(device);
        }

        pub fn reset(self: *@This()) void {
            self.line_buffer.reset();
        }

        pub fn update(
            self: *@This(),
            ctx: *Engine.VulkanContext,
            command_pool: vk.CommandPool,
            v: struct {
                line_buffer: []LineVertex,
            },
        ) !struct {
            buffer_invalid: bool,
            cmdbuf_invalid: bool,
        } {
            // try to swap the buffer created in the last frame
            const swapped = self.swap_tick(&ctx.device);

            try self.update_vertices(v.line_buffer);

            // start allocation of new buffer asap after we know current buffer is not enough
            try self.alloc_tick(ctx, command_pool);

            return .{ .buffer_invalid = swapped, .cmdbuf_invalid = true };
        }

        fn update_vertices(self: *@This(), vertices: []LineVertex) !void {
            _ = self.line_buffer.alloc(vertices);
        }

        fn alloc_tick(self: *@This(), ctx: *Engine.VulkanContext, pool: vk.CommandPool) !void {
            try self.line_buffer.alloc_tick(ctx, pool, std.mem.zeroes(LineVertex));
        }

        fn swap_tick(self: *@This(), device: *Device) bool {
            return self.line_buffer.swap_tick(device);
        }

        pub fn draw(
            self: *@This(),
            device: *Device,
            cmdbufs: *CmdBuffer,
            desc_sets: []const vk.DescriptorSet,
            dbg_pipeline: *const GraphicsPipeline,
        ) void {
            const buf = &self.line_buffer;
            for (cmdbufs.bufs) |cmdbuf| {
                device.cmdBindPipeline(cmdbuf, .graphics, dbg_pipeline.pipeline);
                device.cmdBindDescriptorSets(cmdbuf, .graphics, dbg_pipeline.layout, 0, @intCast(desc_sets.len), desc_sets.ptr, 0, null);
                device.cmdDraw(cmdbuf, buf.count, 1, 0, 0);
            }
        }
    };

    pub const BatchHandle = struct {
        index: u32,
    };

    pub const InstanceResources = struct {
        // we can only instance a contiguous chunk of instances (in gpu memory)
        // but we might want to delete entities randomly
        // so we have this abstraction that bump allocates instance buffer memory each frame
        // for each type of instance we might want to render.

        // the cpu side buffers that are copied to the gpu each frame
        bones: Bones,
        batches: Batches,
        // different pipelines have to have different draw calls even when we have drawIndirect
        // so this is needed to make sure that all batches in the same draw call are contiguous in memory
        material_batches: MaterialBatches,

        // gpu side buffers
        // (double buffered for reallocation)
        instance_buffer: ElementDoubleBuffer,
        bone_buffer: ElementDoubleBuffer,
        draw_call_buffer: ElementDoubleBuffer,
        draw_ctx_buffer: ElementDoubleBuffer,

        hash: u64 = 0,

        pub const DrawCall = vk.DrawIndexedIndirectCommand;
        pub const DrawCtx = extern struct {
            first_vertex: u32,
            first_index: u32,
            first_instance: u32,
            _pad: u32 = 0,
        };
        const Instances = std.ArrayList(Instance);
        const Bones = std.ArrayList(math.Mat4x4);
        const Batch = struct {
            mesh: MeshHandle,
            material: MaterialHandle,
            instances: Instances,
            first_draw: u32 = 0,
            can_draw: u32 = 0,
        };
        const Batches = std.ArrayList(Batch);
        const MaterialBatches = std.AutoHashMap(MaterialHandle, struct {
            batch: std.ArrayList(BatchHandle),

            // as far as i an tell - push contents don't need to live after the call to cmdPushConstants
            // but for some reason i can't get a bugfree result unless i make it live longer
            push: PushConstants,
        });

        pub fn init(engine: *Engine, pool: vk.CommandPool, v: struct {
            instance_cap: u32 = 500,
            bone_cap: u32 = 1500,
            call_cap: u32 = 50,
        }) !@This() {
            const ctx = &engine.graphics;
            const device = &ctx.device;

            var instance_buffer = try ElementDoubleBuffer.init(ctx, pool, .{
                .size = v.instance_cap,
                .usage = .{ .storage_buffer_bit = true },
                .memory_type = .{
                    .device_local_bit = true,
                    .host_visible_bit = true,
                    .host_coherent_bit = true,
                },
            }, std.mem.zeroes(Instance));
            errdefer instance_buffer.deinit(device);

            var bone_buffer = try ElementDoubleBuffer.init(ctx, pool, .{
                .size = v.bone_cap,
                .usage = .{ .storage_buffer_bit = true },
                .memory_type = .{
                    .device_local_bit = true,
                    .host_visible_bit = true,
                    .host_coherent_bit = true,
                },
            }, std.mem.zeroes(math.Mat4x4));
            errdefer bone_buffer.deinit(device);

            var draw_call_buffer = try ElementDoubleBuffer.init(ctx, pool, .{
                .size = v.call_cap,
                .usage = .{ .indirect_buffer_bit = true },
                .memory_type = .{
                    .device_local_bit = true,
                    .host_visible_bit = true,
                    .host_coherent_bit = true,
                },
            }, std.mem.zeroes(DrawCall));
            errdefer draw_call_buffer.deinit(device);

            var draw_ctx_buffer = try ElementDoubleBuffer.init(ctx, pool, .{
                .size = v.bone_cap,
                .usage = .{ .storage_buffer_bit = true },
                .memory_type = .{
                    .device_local_bit = true,
                    .host_visible_bit = true,
                    .host_coherent_bit = true,
                },
            }, std.mem.zeroes(DrawCtx));
            errdefer draw_ctx_buffer.deinit(device);

            return @This(){
                .bones = .init(allocator.*),
                .batches = .init(allocator.*),
                .material_batches = .init(allocator.*),
                .instance_buffer = instance_buffer,
                .bone_buffer = bone_buffer,
                .draw_call_buffer = draw_call_buffer,
                .draw_ctx_buffer = draw_ctx_buffer,
            };
        }

        pub fn deinit(self: *@This(), device: *Device) void {
            self.bones.deinit();

            {
                var it = self.material_batches.iterator();
                while (it.next()) |batch| {
                    batch.value_ptr.batch.deinit();
                }
                self.material_batches.deinit();
            }

            {
                for (self.batches.items) |*batch| {
                    batch.instances.deinit();
                }
                self.batches.deinit();
            }

            self.instance_buffer.deinit(device);
            self.bone_buffer.deinit(device);
            self.draw_call_buffer.deinit(device);
            self.draw_ctx_buffer.deinit(device);
        }

        pub fn reset(self: *@This()) void {
            for (self.batches.items) |*batch| {
                batch.instances.clearRetainingCapacity();
                batch.first_draw = 0;
                batch.can_draw = 0;
            }
            self.bones.clearRetainingCapacity();

            self.instance_buffer.reset();
            self.bone_buffer.reset();
            self.draw_call_buffer.reset();
            self.draw_ctx_buffer.reset();
        }

        fn did_change(self: *@This()) bool {
            var hasher = std.hash.Wyhash.init(0);

            for (self.batches.items) |batch| {
                hasher.update(std.mem.asBytes(&batch.mesh));
                hasher.update(std.mem.asBytes(&batch.material));
            }

            hasher.update(std.mem.asBytes(&self.draw_call_buffer.count));
            hasher.update(std.mem.asBytes(&self.draw_ctx_buffer.count));

            var it = self.material_batches.iterator();
            while (it.next()) |batch| {
                hasher.update(std.mem.asBytes(&batch.value_ptr.batch.items.len));
            }

            const hash = hasher.final();
            defer self.hash = hash;
            return self.hash != hash;
        }

        pub fn get_batch(self: *@This(), mesh: MeshHandle, material: MaterialHandle) !BatchHandle {
            for (self.batches.items, 0..) |batch, i| {
                if (std.meta.eql(batch.mesh, mesh) and std.meta.eql(batch.material, material)) {
                    return .{ .index = @intCast(i) };
                }
            }

            const handle: BatchHandle = .{ .index = @intCast(self.batches.items.len) };
            try self.batches.append(.{
                .material = material,
                .mesh = mesh,
                .instances = .init(allocator.*),
            });

            const batch = try self.material_batches.getOrPut(material);
            if (!batch.found_existing) {
                batch.value_ptr.batch = .init(allocator.*);
                batch.value_ptr.push = std.mem.zeroes(@TypeOf(batch.value_ptr.push));
            }
            try batch.value_ptr.batch.append(handle);

            return handle;
        }

        // null means we don't have the resources currently available to render this.
        // but we will try to allocate necessary resources on next frame
        pub fn reserve_instance(self: *@This(), handle: BatchHandle) !?*Instance {
            const batch = &self.batches.items[handle.index];
            return try batch.instances.addOne();
        }

        pub fn reserve_bones(self: *@This(), num: usize) !struct { first: u32, buf: []math.Mat4x4 } {
            const first = self.bones.items.len;
            return .{ .first = @intCast(first), .buf = try self.bones.addManyAsSlice(num) };
        }

        // call asap after reserving all instances of a frame
        fn alloc_tick(self: *@This(), ctx: *Engine.VulkanContext, pool: vk.CommandPool) !void {
            try self.instance_buffer.alloc_tick(ctx, pool, std.mem.zeroes(Instance));
            try self.bone_buffer.alloc_tick(ctx, pool, std.mem.zeroes(math.Mat4x4));
            try self.draw_call_buffer.alloc_tick(ctx, pool, std.mem.zeroes(DrawCall));
            try self.draw_ctx_buffer.alloc_tick(ctx, pool, std.mem.zeroes(DrawCtx));
        }

        // call when the buffers are not in use
        fn swap_tick(self: *@This(), device: *Device) bool {
            var did_swap = false;
            // 'or' short circuits :/
            did_swap = self.instance_buffer.swap_tick(device) or did_swap;
            did_swap = self.bone_buffer.swap_tick(device) or did_swap;
            did_swap = self.draw_call_buffer.swap_tick(device) or did_swap;
            did_swap = self.draw_ctx_buffer.swap_tick(device) or did_swap;
            return did_swap;
        }

        pub fn update(self: *@This(), ctx: *Engine.VulkanContext, command_pool: vk.CommandPool) !struct {
            buffer_invalid: bool,
            cmdbuf_invalid: bool,
        } {
            // try to swap the buffer created in the last frame
            const swapped = self.swap_tick(&ctx.device);

            self.update_instances();
            self.update_bones();
            self.update_draw_calls();
            self.update_draw_ctxts();

            const changed = self.did_change();

            // start allocation of new buffer asap after we know current buffer is not enough
            try self.alloc_tick(ctx, command_pool);

            return .{ .buffer_invalid = swapped, .cmdbuf_invalid = changed };
        }

        fn update_instances(self: *@This()) void {
            const buf = &self.instance_buffer;

            for (self.batches.items) |*batch| {
                const instances = batch.instances.items;
                batch.first_draw = buf.count;
                batch.can_draw = buf.alloc(instances);
            }
        }

        fn update_bones(self: *@This()) void {
            const buf = &self.bone_buffer;

            const bones = self.bones.items;
            _ = buf.alloc(bones);
        }

        fn update_draw_calls(self: *@This()) void {
            const buf = &self.draw_call_buffer;

            var it = self.material_batches.iterator();
            while (it.next()) |mbatch| for (mbatch.value_ptr.batch.items) |hbatch| {
                const batch = &self.batches.items[hbatch.index];
                const call_buf: [1]DrawCall = .{.{
                    .index_count = batch.mesh.regions.index.count,
                    .instance_count = @intCast(batch.instances.items.len),
                    .first_index = 0,
                    .vertex_offset = 0,
                    .first_instance = 0,
                }};
                const count = buf.alloc(&call_buf);
                if (count == 0) {
                    break;
                }
            };
        }

        fn update_draw_ctxts(self: *@This()) void {
            const buf = &self.draw_ctx_buffer;

            var it = self.material_batches.iterator();
            while (it.next()) |mbatch| for (mbatch.value_ptr.batch.items) |hbatch| {
                const batch = &self.batches.items[hbatch.index];
                const ctx_buf: [1]DrawCtx = .{.{
                    .first_vertex = batch.mesh.regions.vertex.first,
                    .first_index = batch.mesh.regions.index.first,
                    .first_instance = batch.first_draw,
                }};
                const count = buf.alloc(&ctx_buf);
                if (count == 0) {
                    break;
                }
            };
        }

        pub fn draw(
            self: *@This(),
            device: *Device,
            cmdbuf: *CmdBuffer,
            desc_sets: []const vk.DescriptorSet,
            pipelines: *std.AutoHashMap(MaterialHandle, GraphicsPipeline),
        ) void {
            var count: usize = 0;
            var it = self.material_batches.iterator();
            while (it.next()) |batch| {
                const push = &batch.value_ptr.push;
                push.* = .{ .first_draw_ctx = @intCast(count) };

                cmdbuf.draw_indirect(device, .{
                    .pipeline = &pipelines.get(batch.key_ptr.*).?,
                    .desc_sets = desc_sets,
                    .offsets = &[_]u32{},
                    .calls = .{
                        .buffer = self.draw_call_buffer.current.buffer.buffer,
                        .count = @intCast(batch.value_ptr.batch.items.len),
                        .stride = @sizeOf(DrawCall),
                        .offset = @intCast(@sizeOf(DrawCall) * count),
                    },
                    .push_constants = std.mem.asBytes(push),
                });

                count += batch.value_ptr.batch.items.len;
            }
        }
    };

    pub const MeshHandle = struct {
        index: u32,
        regions: Regions,

        const Regions = struct {
            index: Region,
            vertex: Region,
        };
        const Region = struct {
            first: u32,
            count: u32,
        };
    };
    pub const ArmatureHandle = struct {
        index: u32,
    };
    pub const AudioHandle = struct {
        index: u32,
    };
    pub const ImageHandle = struct {
        index: u32,
    };
    pub const GltfHandle = struct {
        index: u32,
    };
    pub const MaterialHandle = struct {
        index: u32,
    };

    pub const Assets = struct {
        vertices: Vertices,
        triangles: Triangles,
        mesh_info: MeshInfo,
        armatures: Armatures,
        audio: AudioSamples,
        meshes: Meshes,
        images: Images,
        gltfs: Gltfs,
        materials: Materials,

        const Vertices = std.ArrayList(Vertex);
        const Triangles = std.ArrayList([3]u32);
        const MeshInfo = std.ArrayList(MeshHandle);
        const Armatures = std.ArrayList(assets_mod.Armature);
        const AudioSamples = std.ArrayList(assets_mod.Wav);
        const Meshes = std.ArrayList(assets_mod.Mesh);
        const Images = std.ArrayList(utils_mod.StbImage.UnormImage);
        const Gltfs = std.ArrayList(GltfData);
        const Materials = std.ArrayList(Material);

        pub const Material = struct {
            name: []const u8,
            frag: []const u8,
            vert: []const u8,
            src: []const u8,
            cull_mode: vk.CullModeFlags = .{ .back_bit = true },
            render_mode: render_utils.GraphicsPipeline.RenderMode = .solid_triangles,
        };

        pub const GltfData = struct {
            gltf: assets_mod.Gltf,
            handles: struct {
                meshes: []MeshHandle,
                armatures: []ArmatureHandle,
            },

            fn deinit(self: *@This()) void {
                defer self.gltf.deinit();
                defer self.gltf.alloc.free(self.handles.meshes);
                defer self.gltf.alloc.free(self.handles.armatures);
            }
        };

        pub fn init() @This() {
            return .{
                .vertices = .init(allocator.*),
                .triangles = .init(allocator.*),
                .mesh_info = .init(allocator.*),
                .armatures = .init(allocator.*),
                .audio = .init(allocator.*),
                .meshes = .init(allocator.*),
                .images = .init(allocator.*),
                .gltfs = .init(allocator.*),
                .materials = .init(allocator.*),
            };
        }

        pub fn deinit(self: *@This()) void {
            self.vertices.deinit();
            self.triangles.deinit();
            self.mesh_info.deinit();
            self.armatures.deinit();
            self.materials.deinit();

            for (self.audio.items) |*t| {
                t.deinit();
            }
            self.audio.deinit();

            for (self.meshes.items) |*t| {
                t.deinit();
            }
            self.meshes.deinit();

            for (self.images.items) |*t| {
                t.deinit();
            }
            self.images.deinit();

            for (self.gltfs.items) |*t| {
                t.deinit();
            }
            self.gltfs.deinit();
        }

        fn to_handle(typ: type) type {
            return switch (typ) {
                assets_mod.Wav => AudioHandle,
                assets_mod.Armature => ArmatureHandle,
                assets_mod.Mesh => MeshHandle,
                assets_mod.Gltf => GltfHandle,
                utils_mod.StbImage.UnormImage => ImageHandle,
                Material => MaterialHandle,
                else => @compileError("can't handle type: '" ++ @typeName(typ) ++ "' here"),
            };
        }

        fn to_asset(typ: type) type {
            return switch (typ) {
                AudioHandle => *assets_mod.Wav,
                ArmatureHandle => *assets_mod.Armature,
                MeshHandle => *assets_mod.Mesh,
                GltfHandle => *GltfData,
                ImageHandle => *utils_mod.StbImage.UnormImage,
                MaterialHandle => *Material,
                else => @compileError("can't handle type: '" ++ @typeName(typ) ++ "' here"),
            };
        }

        pub fn add(self: *@This(), asset: anytype) !to_handle(@TypeOf(asset)) {
            switch (@TypeOf(asset)) {
                assets_mod.Wav => {
                    const handle = self.audio.items.len;
                    try self.audio.append(asset);
                    return .{ .index = @intCast(handle) };
                },
                assets_mod.Armature => {
                    const handle = self.armatures.items.len;
                    try self.armatures.append(asset);
                    return .{ .index = @intCast(handle) };
                },
                assets_mod.Mesh => {
                    const regions = try self.add_mesh(&asset);
                    const handle = self.meshes.items.len;
                    try self.meshes.append(asset);
                    const mesh_handle = MeshHandle{ .index = @intCast(handle), .regions = regions };
                    try self.mesh_info.append(mesh_handle);
                    return mesh_handle;
                },
                assets_mod.Gltf => {
                    const data = try self.add_gltf(asset);
                    const handle = self.gltfs.items.len;
                    try self.gltfs.append(data);
                    return .{ .index = @intCast(handle) };
                },
                utils_mod.StbImage.UnormImage => {
                    const handle = self.images.items.len;
                    try self.images.append(asset);
                    return .{ .index = @intCast(handle) };
                },
                Material => {
                    const handle = self.materials.items.len;
                    try self.materials.append(asset);
                    return .{ .index = @intCast(handle) };
                },
                else => @compileError("can't handle type: '" ++ @typeName(asset) ++ "' here"),
            }
        }

        pub fn ref(self: *@This(), handle: anytype) to_asset(@TypeOf(handle)) {
            const typ = @TypeOf(handle);
            return switch (typ) {
                AudioHandle => &self.audio.items[handle.index],
                ArmatureHandle => &self.armatures.items[handle.index],
                MeshHandle => &self.meshes.items[handle.index],
                ImageHandle => &self.images.items[handle.index],
                GltfHandle => &self.gltfs.items[handle.index],
                MaterialHandle => &self.materials.items[handle.index],
                else => @compileError("can't handle type: '" ++ @typeName(typ) ++ "' here"),
            };
        }

        fn add_gltf(self: *@This(), _gltf: assets_mod.Gltf) !GltfData {
            var gltf = _gltf;
            const info = &gltf.info.value;

            var meshes = std.ArrayList(MeshHandle).init(gltf.alloc);
            for (info.meshes) |*m| {
                const mesh = try gltf.parse_mesh(m);
                try meshes.append(try self.add(mesh));
            }

            var armatures = std.ArrayList(ArmatureHandle).init(gltf.alloc);
            for (info.skins) |*skin| {
                const armature = try gltf.parse_armature(skin);
                try armatures.append(try self.add(armature));
            }

            return .{
                .handles = .{
                    .meshes = try meshes.toOwnedSlice(),
                    .armatures = try armatures.toOwnedSlice(),
                },
                .gltf = gltf,
            };
        }

        fn add_mesh(self: *@This(), m: *const assets_mod.Mesh) !MeshHandle.Regions {
            const handle = MeshHandle.Regions{
                .index = .{
                    .first = @intCast(self.triangles.items.len * 3),
                    .count = @intCast(m.faces.len * 3),
                },
                .vertex = .{
                    .first = @intCast(self.vertices.items.len),
                    .count = @intCast(m.vertices.len),
                },
            };

            for (m.vertices, m.normals, m.uvs, 0..) |v, n, uv, j| {
                var vertex = std.mem.zeroes(Vertex);
                vertex.pos = math.Vec3.from_buf(v);
                vertex.normal = math.Vec3.from_buf(n);
                vertex.uv = .{ .x = uv[0], .y = uv[1] };

                const zero_bones = [1]assets_mod.VertexBone{
                    .{ .bone = 0, .weight = 1 },
                };
                const bone: []const assets_mod.VertexBone = if (m.bones.len > j) m.bones[j] else zero_bones[0..];

                for (bone[0..@min(4, bone.len)], 0..) |b, i| {
                    vertex.bone_ids[i] = b.bone;
                    vertex.bone_weights[i] = b.weight;
                }

                try self.vertices.append(vertex);
            }
            try self.triangles.appendSlice(m.faces);

            return handle;
        }

        pub fn upload(self: *const @This(), engine: *Engine, pool: vk.CommandPool) !AssetBuffers {
            return try AssetBuffers.init(self, engine, pool);
        }
    };

    pub const AssetBuffers = struct {
        vertex_buffer: Buffer,
        index_buffer: Buffer,

        pub fn init(cpu: *const Assets, engine: *Engine, pool: vk.CommandPool) !@This() {
            const ctx = &engine.graphics;
            const device = &ctx.device;

            var vertex_buffer = try Buffer.new_from_slice(ctx, .{ .usage = .{
                .storage_buffer_bit = true,
            } }, cpu.vertices.items, pool);
            errdefer vertex_buffer.deinit(device);

            var index_buffer = try Buffer.new_from_slice(ctx, .{ .usage = .{
                .storage_buffer_bit = true,
            } }, cpu.triangles.items, pool);
            errdefer index_buffer.deinit(device);

            return .{
                .vertex_buffer = vertex_buffer,
                .index_buffer = index_buffer,
            };
        }

        pub fn deinit(self: *@This(), device: *Device) void {
            self.vertex_buffer.deinit(device);
            self.index_buffer.deinit(device);
        }
    };
};
