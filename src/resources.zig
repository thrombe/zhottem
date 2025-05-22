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

    pub fn bind(self: @This()) u32 {
        return @intFromEnum(self);
    }
};

pub const ResourceManager = struct {
    // static if we load all assets at startup :P
    assets: Assets,
    asset_buffers: AssetBuffers,

    // dynamic as instances vary at runtime
    instances: InstanceResources,

    pub fn init(assets: Assets, engine: *Engine, pool: vk.CommandPool) !@This() {
        return .{
            .assets = assets,
            .asset_buffers = try assets.upload(engine, pool),
            .instances = try .init(engine, pool, .{}),
        };
    }

    pub fn deinit(self: *@This(), device: *Device) void {
        self.assets.deinit();
        self.asset_buffers.deinit(device);
        self.instances.deinit(device);
    }

    pub fn add_binds(self: *@This(), builder: *render_utils.DescriptorSet.Builder) !void {
        const add_to_set = struct {
            fn func(set_builder: @TypeOf(builder), buf: *Buffer, bind: UniformBinds) !void {
                try set_builder.add(buf, bind.bind());
            }
        }.func;

        try add_to_set(builder, &self.asset_buffers.vertex_buffer, .vertices);
        try add_to_set(builder, &self.asset_buffers.index_buffer, .indices);
        try add_to_set(builder, &self.instances.instance_buffer.current.buffer, .instances);
        try add_to_set(builder, &self.instances.bone_buffer.current.buffer, .bones);
        try add_to_set(builder, &self.instances.draw_ctx_buffer.current.buffer, .call_ctxts);
    }

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
        instance_buffer: DoubleBuffer,
        bone_buffer: DoubleBuffer,
        draw_call_buffer: DoubleBuffer,
        draw_ctx_buffer: DoubleBuffer,

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
        const MaterialBatches = std.AutoArrayHashMap(MaterialHandle, struct {
            batch: std.ArrayList(BatchHandle),

            // as far as i an tell - push contents don't need to live after the call to cmdPushConstants
            // but for some reason i can't get a bugfree result unless i make it live longer
            push: PushConstants,
        });
        const BufferWithCapacity = struct {
            buffer: Buffer,
            capacity: u32,
        };
        const DoubleBuffer = struct {
            current: BufferWithCapacity,
            count: u32 = 0,
            back: ?BufferWithCapacity = null,
            state: enum {
                enough,
                out_of_objects,
                back_allocated,
            } = .enough,
        };

        pub fn init(engine: *Engine, pool: vk.CommandPool, v: struct {
            instance_cap: u32 = 500,
            bone_cap: u32 = 1500,
            call_cap: u32 = 50,
        }) !@This() {
            const ctx = &engine.graphics;
            const device = &ctx.device;

            var instance_buffer = try Buffer.new_initialized(ctx, .{
                .size = v.instance_cap,
                .usage = .{ .storage_buffer_bit = true },
                .memory_type = .{
                    .device_local_bit = true,
                    .host_visible_bit = true,
                    .host_coherent_bit = true,
                },
            }, std.mem.zeroes(Instance), pool);
            errdefer instance_buffer.deinit(device);

            var bone_buffer = try Buffer.new_initialized(ctx, .{
                .size = v.bone_cap,
                .usage = .{ .storage_buffer_bit = true },
                .memory_type = .{
                    .device_local_bit = true,
                    .host_visible_bit = true,
                    .host_coherent_bit = true,
                },
            }, std.mem.zeroes(math.Mat4x4), pool);
            errdefer bone_buffer.deinit(device);

            var draw_call_buffer = try Buffer.new_initialized(ctx, .{
                .size = v.call_cap,
                .usage = .{ .indirect_buffer_bit = true },
                .memory_type = .{
                    .device_local_bit = true,
                    .host_visible_bit = true,
                    .host_coherent_bit = true,
                },
            }, std.mem.zeroes(DrawCall), pool);
            errdefer draw_call_buffer.deinit(device);

            var draw_ctx_buffer = try Buffer.new_initialized(ctx, .{
                .size = v.bone_cap,
                .usage = .{ .storage_buffer_bit = true },
                .memory_type = .{
                    .device_local_bit = true,
                    .host_visible_bit = true,
                    .host_coherent_bit = true,
                },
            }, std.mem.zeroes(DrawCtx), pool);
            errdefer draw_ctx_buffer.deinit(device);

            return @This(){
                .bones = .init(allocator.*),
                .batches = .init(allocator.*),
                .material_batches = .init(allocator.*),
                .instance_buffer = .{ .current = .{
                    .buffer = instance_buffer,
                    .capacity = v.instance_cap,
                } },
                .bone_buffer = .{ .current = .{
                    .buffer = bone_buffer,
                    .capacity = v.bone_cap,
                } },
                .draw_call_buffer = .{ .current = .{
                    .buffer = draw_call_buffer,
                    .capacity = v.call_cap,
                } },
                .draw_ctx_buffer = .{ .current = .{
                    .buffer = draw_ctx_buffer,
                    .capacity = v.call_cap,
                } },
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

            {
                const buf = &self.instance_buffer;
                buf.current.buffer.deinit(device);
                if (buf.back) |*back| back.buffer.deinit(device);
            }

            {
                const buf = &self.bone_buffer;
                buf.current.buffer.deinit(device);
                if (buf.back) |*back| back.buffer.deinit(device);
            }

            {
                const buf = &self.draw_call_buffer;
                buf.current.buffer.deinit(device);
                if (buf.back) |*back| back.buffer.deinit(device);
            }

            {
                const buf = &self.draw_ctx_buffer;
                buf.current.buffer.deinit(device);
                if (buf.back) |*back| back.buffer.deinit(device);
            }
        }

        pub fn reset(self: *@This()) void {
            for (self.batches.items) |*batch| {
                batch.instances.clearRetainingCapacity();
                batch.first_draw = 0;
                batch.can_draw = 0;
            }
            self.bones.clearRetainingCapacity();

            self.instance_buffer.count = 0;
            self.bone_buffer.count = 0;
            self.draw_call_buffer.count = 0;
            self.draw_ctx_buffer.count = 0;
        }

        fn did_change(self: *@This()) bool {
            var hasher = std.hash.Wyhash.init(0);
            for (self.batches.items) |batch| {
                hasher.update(std.mem.asBytes(&batch.mesh));
                hasher.update(std.mem.asBytes(&batch.material));
            }
            hasher.update(std.mem.asBytes(&self.draw_call_buffer.count));
            hasher.update(std.mem.asBytes(&self.draw_ctx_buffer.count));
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
            const device = &ctx.device;

            {
                const buf = &self.instance_buffer;
                switch (buf.state) {
                    .enough, .back_allocated => {},
                    .out_of_objects => {
                        const instance_cap = buf.current.capacity;
                        const new_instance_cap = instance_cap + instance_cap / 2;
                        var instance_buffer = try Buffer.new_initialized(ctx, .{
                            .size = new_instance_cap,
                            .usage = .{ .storage_buffer_bit = true },
                            .memory_type = .{
                                .device_local_bit = true,
                                .host_visible_bit = true,
                                .host_coherent_bit = true,
                            },
                        }, std.mem.zeroes(Instance), pool);
                        errdefer instance_buffer.deinit(device);

                        buf.back = .{
                            .buffer = instance_buffer,
                            .capacity = new_instance_cap,
                        };
                        buf.state = .back_allocated;
                    },
                }
            }

            {
                const buf = &self.bone_buffer;

                switch (buf.state) {
                    .enough, .back_allocated => {},
                    .out_of_objects => {
                        const bone_cap = buf.current.capacity;
                        const new_bone_cap = bone_cap + bone_cap / 2;
                        var bone_buffer = try Buffer.new_initialized(ctx, .{
                            .size = new_bone_cap,
                            .usage = .{ .storage_buffer_bit = true },
                            .memory_type = .{
                                .device_local_bit = true,
                                .host_visible_bit = true,
                                .host_coherent_bit = true,
                            },
                        }, std.mem.zeroes(math.Mat4x4), pool);
                        errdefer bone_buffer.deinit(device);

                        buf.back = .{
                            .buffer = bone_buffer,
                            .capacity = new_bone_cap,
                        };
                        buf.state = .back_allocated;
                    },
                }
            }

            {
                const buf = &self.draw_call_buffer;

                switch (buf.state) {
                    .enough, .back_allocated => {},
                    .out_of_objects => {
                        const bone_cap = buf.current.capacity;
                        const new_bone_cap = bone_cap + bone_cap / 2;
                        var bone_buffer = try Buffer.new_initialized(ctx, .{
                            .size = new_bone_cap,
                            .usage = .{ .indirect_buffer_bit = true },
                            .memory_type = .{
                                .device_local_bit = true,
                                .host_visible_bit = true,
                                .host_coherent_bit = true,
                            },
                        }, std.mem.zeroes(DrawCall), pool);
                        errdefer bone_buffer.deinit(device);

                        buf.back = .{
                            .buffer = bone_buffer,
                            .capacity = new_bone_cap,
                        };
                        buf.state = .back_allocated;
                    },
                }
            }

            {
                const buf = &self.draw_ctx_buffer;

                switch (buf.state) {
                    .enough, .back_allocated => {},
                    .out_of_objects => {
                        const bone_cap = buf.current.capacity;
                        const new_bone_cap = bone_cap + bone_cap / 2;
                        var bone_buffer = try Buffer.new_initialized(ctx, .{
                            .size = new_bone_cap,
                            .usage = .{ .storage_buffer_bit = true },
                            .memory_type = .{
                                .device_local_bit = true,
                                .host_visible_bit = true,
                                .host_coherent_bit = true,
                            },
                        }, std.mem.zeroes(DrawCtx), pool);
                        errdefer bone_buffer.deinit(device);

                        buf.back = .{
                            .buffer = bone_buffer,
                            .capacity = new_bone_cap,
                        };
                        buf.state = .back_allocated;
                    },
                }
            }
        }

        // call when the buffers are not in use
        fn swap_tick(self: *@This(), device: *Device) bool {
            var did_swap = false;
            {
                const buf = &self.instance_buffer;

                switch (buf.state) {
                    .enough, .out_of_objects => {},
                    .back_allocated => {
                        // this is okay cuz we have an empty queue every frame
                        buf.current.buffer.deinit(device);
                        buf.current = buf.back.?;
                        buf.back = null;
                        buf.state = .enough;
                        did_swap = true;
                    },
                }
            }
            {
                const buf = &self.bone_buffer;

                switch (buf.state) {
                    .enough, .out_of_objects => {},
                    .back_allocated => {
                        // this is okay cuz we have an empty queue every frame
                        buf.current.buffer.deinit(device);
                        buf.current = buf.back.?;
                        buf.back = null;
                        buf.state = .enough;
                        did_swap = true;
                    },
                }
            }
            {
                const buf = &self.draw_call_buffer;

                switch (buf.state) {
                    .enough, .out_of_objects => {},
                    .back_allocated => {
                        // this is okay cuz we have an empty queue every frame
                        buf.current.buffer.deinit(device);
                        buf.current = buf.back.?;
                        buf.back = null;
                        buf.state = .enough;
                        did_swap = true;
                    },
                }
            }
            {
                const buf = &self.draw_ctx_buffer;

                switch (buf.state) {
                    .enough, .out_of_objects => {},
                    .back_allocated => {
                        // this is okay cuz we have an empty queue every frame
                        buf.current.buffer.deinit(device);
                        buf.current = buf.back.?;
                        buf.back = null;
                        buf.state = .enough;
                        did_swap = true;
                    },
                }
            }
            return did_swap;
        }

        pub fn update(self: *@This(), ctx: *Engine.VulkanContext, command_pool: vk.CommandPool) !struct {
            buffer_invalid: bool,
            cmdbuf_invalid: bool,
        } {
            // try to swap the buffer created in the last frame
            const swapped = self.swap_tick(&ctx.device);

            try self.update_instances(&ctx.device);
            try self.update_bones(&ctx.device);
            try self.update_draw_calls(&ctx.device);
            try self.update_draw_ctxts(&ctx.device);

            const changed = self.did_change();

            // start allocation of new buffer asap after we know current buffer is not enough
            try self.alloc_tick(ctx, command_pool);

            return .{ .buffer_invalid = swapped, .cmdbuf_invalid = changed };
        }

        fn update_instances(self: *@This(), device: *Device) !void {
            const buf = &self.instance_buffer;
            const data = try device.mapMemory(buf.current.buffer.memory, 0, vk.WHOLE_SIZE, .{});
            defer device.unmapMemory(buf.current.buffer.memory);

            const gpu_items: [*]Instance = @ptrCast(@alignCast(data));

            for (self.batches.items) |*batch| {
                const instances = batch.instances.items;
                const can_alloc = @min(buf.current.capacity - buf.count, instances.len);
                batch.first_draw = buf.count;
                batch.can_draw = can_alloc;
                @memcpy(gpu_items[buf.count..][0..can_alloc], instances[0..can_alloc]);
                buf.count += can_alloc;

                if (can_alloc < instances.len and buf.state == .enough) {
                    buf.state = .out_of_objects;
                }
            }
        }

        fn update_bones(self: *@This(), device: *Device) !void {
            const buf = &self.bone_buffer;
            const data = try device.mapMemory(buf.current.buffer.memory, 0, vk.WHOLE_SIZE, .{});
            defer device.unmapMemory(buf.current.buffer.memory);

            const gpu_items: [*]math.Mat4x4 = @ptrCast(@alignCast(data));

            const bones = self.bones.items;
            const can_alloc = @min(buf.current.capacity - buf.count, bones.len);
            @memcpy(gpu_items[buf.count..][0..can_alloc], bones[0..can_alloc]);
            buf.count = can_alloc;

            if (can_alloc < bones.len and buf.state == .enough) {
                buf.state = .out_of_objects;
            }
        }

        fn update_draw_calls(self: *@This(), device: *Device) !void {
            const buf = &self.draw_call_buffer;
            const data = try device.mapMemory(buf.current.buffer.memory, 0, vk.WHOLE_SIZE, .{});
            defer device.unmapMemory(buf.current.buffer.memory);

            const gpu_items: [*]DrawCall = @ptrCast(@alignCast(data));

            for (self.batches.items) |batch| {
                if (buf.count >= buf.current.capacity) {
                    if (buf.state == .enough) {
                        buf.state = .out_of_objects;
                    }
                    break;
                }

                gpu_items[buf.count] = .{
                    .index_count = batch.mesh.regions.index.count,
                    .instance_count = @intCast(batch.instances.items.len),
                    .first_index = 0,
                    .vertex_offset = 0,
                    .first_instance = 0,
                };
                buf.count += 1;
            }
        }

        fn update_draw_ctxts(self: *@This(), device: *Device) !void {
            const buf = &self.draw_ctx_buffer;
            const data = try device.mapMemory(buf.current.buffer.memory, 0, vk.WHOLE_SIZE, .{});
            defer device.unmapMemory(buf.current.buffer.memory);

            const gpu_items: [*]DrawCtx = @ptrCast(@alignCast(data));

            for (self.batches.items) |*batch| {
                if (buf.count >= buf.current.capacity) {
                    if (buf.state == .enough) {
                        buf.state = .out_of_objects;
                    }
                    break;
                }

                gpu_items[buf.count] = .{
                    .first_vertex = batch.mesh.regions.vertex.first,
                    .first_index = batch.mesh.regions.index.first,
                    .first_instance = batch.first_draw,
                };
                buf.count += 1;
            }
        }

        pub fn draw(
            self: *@This(),
            device: *Device,
            cmdbuf: *CmdBuffer,
            desc_sets: []const vk.DescriptorSet,
            pipelines: *std.AutoArrayHashMap(MaterialHandle, GraphicsPipeline),
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
                        .count = self.draw_call_buffer.count,
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
