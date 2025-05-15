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

pub const Vertex = extern struct {
    pub const binding_description = [_]vk.VertexInputBindingDescription{.{
        .binding = VertexBinds.vertex.bind(),
        .stride = @sizeOf(Vertex),

        // new data per vertex
        .input_rate = .vertex,
    }};

    pub const attribute_description = [_]vk.VertexInputAttributeDescription{
        .{
            .binding = VertexBinds.vertex.bind(),
            .location = VertexInputLocations.vertex_position.bind(),
            .format = .r32g32b32_sfloat,
            .offset = @offsetOf(Vertex, "pos"),
        },
        .{
            .binding = VertexBinds.vertex.bind(),
            .location = VertexInputLocations.normal.bind(),
            .format = .r32g32b32_sfloat,
            .offset = @offsetOf(Vertex, "normal"),
        },
        .{
            .binding = VertexBinds.vertex.bind(),
            .location = VertexInputLocations.uv.bind(),
            .format = .r32g32_sfloat,
            .offset = @offsetOf(Vertex, "uv"),
        },
        .{
            .binding = VertexBinds.vertex.bind(),
            .location = VertexInputLocations.bone_ids.bind(),
            .format = .r32g32b32a32_uint,
            .offset = @offsetOf(Vertex, "bone_ids"),
        },
        .{
            .binding = VertexBinds.vertex.bind(),
            .location = VertexInputLocations.bone_weights.bind(),
            .format = .r32g32b32a32_sfloat,
            .offset = @offsetOf(Vertex, "bone_weights"),
        },
    };

    pos: [4]f32,
    normal: [4]f32,
    uv: [4]f32,
    bone_ids: [4]u32,
    bone_weights: [4]f32,

    pub fn from_slices(vertices: [][3]f32, normals: [][3]f32, uvs: [][2]f32) ![]@This() {
        const buf = try allocator.alloc(@This(), vertices.len);
        errdefer allocator.free(buf);

        for (vertices, normals, uvs, 0..) |v, n, uv, i| {
            buf[i].pos = [4]f32{ v[0], v[1], v[2], 0 };
            buf[i].normal = [4]f32{ n[0], n[1], n[2], 0 };
            buf[i].uv = [4]f32{ uv[0], uv[1], 0, 0 };
        }

        return buf;
    }
};

pub const Instance = extern struct {
    pub const binding_desc = [_]vk.VertexInputBindingDescription{.{
        .binding = VertexBinds.instance.bind(),
        .stride = @sizeOf(Instance),

        // new data per instance
        .input_rate = .instance,
    }};
    pub const attribute_desc = [_]vk.VertexInputAttributeDescription{
        .{
            .binding = VertexBinds.instance.bind(),
            .location = VertexInputLocations.instance_bone_offset.bind(),
            .format = .r32_uint,
            .offset = @offsetOf(Instance, "bone_offset"),
        },
    };

    bone_offset: u32,
};

pub const VertexBinds = enum(u32) {
    vertex,
    instance,

    pub fn bind(self: @This()) u32 {
        return @intFromEnum(self);
    }
};

// so input locations are just numbers but you still have to reserve them if you want to store more than 4 floats worth of data? tf?
pub const VertexInputLocations = enum(u32) {
    instance_bone_offset,
    vertex_position,
    normal,
    uv,
    bone_ids,
    bone_weights,

    pub fn bind(self: @This()) u32 {
        return @intFromEnum(self);
    }
};

pub const UniformBinds = enum(u32) {
    camera,
    instanced,

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

    pub const InstanceResources = struct {
        // we can only instance a contiguous chunk of instances (in gpu memory)
        // but we might want to delete entities randomly
        // so we have this abstraction that bump allocates instance buffer memory each frame
        // for each type of instance we might want to render.

        // the cpu side buffers that are copied to the gpu each frame
        bones: Bones,
        batches: Batches,

        // gpu side buffers
        // (double buffered for reallocation)
        instance_buffer: DoubleBuffer,
        bone_buffer: DoubleBuffer,

        hash: u64 = 0,

        const Instances = std.ArrayList(Instance);
        const Bones = std.ArrayList(math.Mat4x4);
        const Batch = struct {
            instances: Instances,
            first_draw: u32 = 0,
            can_draw: u32 = 0,
        };
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

        // currently each batch is unique only by it's mesh,
        // but batches will need to be unique also by it's material somehow.
        const Batches = std.AutoArrayHashMap(MeshHandle, Batch);

        pub fn init(engine: *Engine, pool: vk.CommandPool, v: struct {
            instance_cap: u32 = 500,
            bone_cap: u32 = 1500,
        }) !@This() {
            const ctx = &engine.graphics;
            const device = &ctx.device;

            var instance_buffer = try Buffer.new_initialized(ctx, .{
                .size = v.instance_cap,
                .usage = .{ .vertex_buffer_bit = true },
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

            return .{
                .bones = .init(allocator.*),
                .batches = .init(allocator.*),
                .instance_buffer = .{ .current = .{
                    .buffer = instance_buffer,
                    .capacity = v.instance_cap,
                } },
                .bone_buffer = .{ .current = .{
                    .buffer = bone_buffer,
                    .capacity = v.bone_cap,
                } },
            };
        }

        pub fn deinit(self: *@This(), device: *Device) void {
            self.bones.deinit();

            {
                var batches = self.batches.iterator();
                while (batches.next()) |batch| {
                    batch.value_ptr.instances.deinit();
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
        }

        pub fn reset(self: *@This()) void {
            var batches = self.batches.iterator();
            while (batches.next()) |batch| {
                batch.value_ptr.instances.clearRetainingCapacity();
                batch.value_ptr.first_draw = 0;
                batch.value_ptr.can_draw = 0;
            }
            self.bones.clearRetainingCapacity();

            self.instance_buffer.count = 0;
            self.bone_buffer.count = 0;
        }

        pub fn did_change(self: *@This()) bool {
            var hasher = std.hash.Wyhash.init(0);
            var batches = self.batches.iterator();
            while (batches.next()) |batch| {
                hasher.update(std.mem.asBytes(batch.key_ptr));
                hasher.update(std.mem.asBytes(&batch.value_ptr.first_draw));
                hasher.update(std.mem.asBytes(&batch.value_ptr.can_draw));
            }
            const hash = hasher.final();
            defer self.hash = hash;
            return self.hash != hash;
        }

        // null means we don't have the resources currently available to render this.
        // but we will try to allocate necessary resources on next frame
        pub fn reserve_instance(self: *@This(), mesh: MeshHandle) !?*Instance {
            const entry = try self.batches.getOrPut(mesh);
            if (!entry.found_existing) {
                entry.value_ptr.* = .{
                    .instances = .init(allocator.*),
                };
            }
            return try entry.value_ptr.instances.addOne();
        }

        pub fn reserve_bones(self: *@This(), num: usize) !struct { first: u32, buf: []math.Mat4x4 } {
            const first = self.bones.items.len;
            return .{ .first = @intCast(first), .buf = try self.bones.addManyAsSlice(num) };
        }

        // call at the end of the frame
        pub fn alloc_tick(self: *@This(), engine: *Engine, pool: vk.CommandPool) !void {
            const ctx = &engine.graphics;
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
                            .usage = .{ .vertex_buffer_bit = true },
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
        }

        // call at the start of the frame
        pub fn swap_tick(self: *@This(), device: *Device) void {
            {
                const buf = &self.instance_buffer;

                switch (buf.state) {
                    .enough, .out_of_objects => {},
                    .back_allocated => {
                        // this is okay cuz we have an empty queue every frame
                        buf.current.buffer.deinit(device);
                        buf.current = buf.back;
                        buf.back = null;
                        buf.state = .enough;
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
                        buf.current = buf.back;
                        buf.back = null;
                        buf.state = .enough;
                    },
                }
            }
        }

        pub fn update(self: *@This(), device: *Device) !void {
            try self.update_instances(device);
            try self.update_bones(device);
        }

        fn update_instances(self: *@This(), device: *Device) !void {
            const buf = &self.instance_buffer;
            const data = try device.mapMemory(buf.current.buffer.memory, 0, vk.WHOLE_SIZE, .{});
            defer device.unmapMemory(buf.current.buffer.memory);

            const gpu_items: [*]Instance = @ptrCast(@alignCast(data));

            var batches = self.batches.iterator();
            while (batches.next()) |batch| {
                const instances = batch.value_ptr.instances.items;
                const can_alloc = @min(buf.current.capacity - buf.count, instances.len);
                batch.value_ptr.first_draw = buf.count;
                batch.value_ptr.can_draw = can_alloc;
                @memcpy(gpu_items[buf.count..][0..can_alloc], instances[0..can_alloc]);
                buf.count += can_alloc;

                if (can_alloc < instances.len) {
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

            if (can_alloc < bones.len) {
                buf.state = .out_of_objects;
            }
        }

        pub fn draw(
            self: *const @This(),
            pipeline: *GraphicsPipeline,
            desc_sets: []const vk.DescriptorSet,
            offsets: []const u32,
            cmdbuf: *CmdBuffer,
            device: *Device,
            resources: *ResourceManager.AssetBuffers,
        ) void {
            var batches = self.batches.iterator();
            while (batches.next()) |batch| {
                const regions = batch.key_ptr.regions;

                if (batch.value_ptr.can_draw == 0) {
                    continue;
                }

                // these `offsets` are for each dynamic descriptor.
                // basically - you bind a buffer of objects in the desc set, and just tell
                // it what offset you want for that data here.
                cmdbuf.draw(device, .{
                    .pipeline = pipeline,
                    .desc_sets = desc_sets,
                    .dynamic_offsets = offsets,
                    .vertices = .{
                        .buffer = resources.vertex_buffer.buffer,
                        .count = regions.vertex.count,
                        .first = regions.vertex.first,
                    },
                    .indices = .{
                        .buffer = resources.index_buffer.buffer,
                        .count = regions.index.count,
                        .first = regions.index.first,
                    },
                    .instances = .{
                        .buffer = self.instance_buffer.current.buffer.buffer,
                        .count = batch.value_ptr.can_draw,
                        .first = batch.value_ptr.first_draw,
                    },
                });
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
    pub const BatchedInstanceResourceHandle = struct {
        first: u32,
        count: u32,
    };
    pub const BonesResourceHandle = struct {
        first: u32,
        count: u32,
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

    pub const Assets = struct {
        vertices: Vertices,
        triangles: Triangles,
        armatures: Armatures,
        audio: AudioSamples,
        meshes: Meshes,
        images: Images,
        gltf: Gltf,

        const Vertices = std.ArrayList(Vertex);
        const Triangles = std.ArrayList([3]u32);
        const Armatures = std.ArrayList(assets_mod.Armature);
        const AudioSamples = std.ArrayList(assets_mod.Wav);
        const Meshes = std.ArrayList(assets_mod.Mesh);
        const Images = std.ArrayList(utils_mod.StbImage.UnormImage);
        const Gltf = std.ArrayList(GltfData);

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
                .armatures = .init(allocator.*),
                .audio = .init(allocator.*),
                .meshes = .init(allocator.*),
                .images = .init(allocator.*),
                .gltf = .init(allocator.*),
            };
        }

        pub fn deinit(self: *@This()) void {
            self.vertices.deinit();
            self.triangles.deinit();
            self.armatures.deinit();

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

            for (self.gltf.items) |*t| {
                t.deinit();
            }
            self.gltf.deinit();
        }

        fn to_handle(typ: type) type {
            return switch (typ) {
                assets_mod.Wav => AudioHandle,
                assets_mod.Armature => ArmatureHandle,
                assets_mod.Mesh => MeshHandle,
                assets_mod.Gltf => GltfHandle,
                utils_mod.StbImage.UnormImage => ImageHandle,
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
                    return .{ .index = @intCast(handle), .regions = regions };
                },
                assets_mod.Gltf => {
                    const data = try self.add_gltf(asset);
                    const handle = self.gltf.items.len;
                    try self.gltf.append(data);
                    return .{ .index = @intCast(handle) };
                },
                utils_mod.StbImage.UnormImage => {
                    const handle = self.images.items.len;
                    try self.images.append(asset);
                    return .{ .index = @intCast(handle) };
                },
                else => @compileError("can't handle type: '" ++ @typeName(asset) ++ "' here"),
            }
        }

        pub fn ref(self: *@This(), handle: anytype) to_asset(@TypeOf(handle)) {
            const typ = @TypeOf(handle);
            switch (typ) {
                AudioHandle => {
                    return &self.audio.items[handle.index];
                },
                ArmatureHandle => {
                    return &self.armatures.items[handle.index];
                },
                MeshHandle => {
                    return &self.meshes.items[handle.index];
                },
                ImageHandle => {
                    return &self.images.items[handle.index];
                },
                GltfHandle => {
                    return &self.gltf.items[handle.index];
                },
                else => @compileError("can't handle type: '" ++ @typeName(typ) ++ "' here"),
            }
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
                vertex.pos = [4]f32{ v[0], v[1], v[2], 0 };
                vertex.normal = [4]f32{ n[0], n[1], n[2], 0 };
                vertex.uv = [4]f32{ uv[0], uv[1], 0, 0 };

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
                .vertex_buffer_bit = true,
            } }, cpu.vertices.items, pool);
            errdefer vertex_buffer.deinit(device);

            var index_buffer = try Buffer.new_from_slice(ctx, .{ .usage = .{
                .index_buffer_bit = true,
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
