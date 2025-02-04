const std = @import("std");

const vk = @import("vulkan");

const math = @import("math.zig");
const mesh_mod = @import("mesh.zig");

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
    };

    pos: [4]f32,
    normal: [4]f32,
    uv: [4]f32,

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
            .location = VertexInputLocations.instance_transform.bind(),
            .format = .r32g32b32a32_sfloat, // there's no matrix type in here
            .offset = @offsetOf(Instance, "transform"),
        },
        .{
            .binding = VertexBinds.instance.bind(),
            .location = VertexInputLocations.instance_transform.bind() + 1,
            .format = .r32g32b32a32_sfloat, // there's no matrix type in here
            .offset = @offsetOf(Instance, "transform") + 4 * 4,
        },
        .{
            .binding = VertexBinds.instance.bind(),
            .location = VertexInputLocations.instance_transform.bind() + 2,
            .format = .r32g32b32a32_sfloat, // there's no matrix type in here
            .offset = @offsetOf(Instance, "transform") + 8 * 4,
        },
        .{
            .binding = VertexBinds.instance.bind(),
            .location = VertexInputLocations.instance_transform.bind() + 3,
            .format = .r32g32b32a32_sfloat, // there's no matrix type in here
            .offset = @offsetOf(Instance, "transform") + 12 * 4,
        },
    };

    transform: math.Mat4x4,
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
    instance_transform = 0,
    vertex_position = 4,
    normal = 5,
    uv = 6,

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

pub const DrawCallReserve = struct {
    mesh: GpuResourceManager.MeshResourceHandle,
    instances: GpuResourceManager.BatchedInstanceResourceHandle,
    count: u32 = 0,

    pub fn reset(self: *@This()) void {
        self.count = 0;
    }

    pub fn reserve(self: *@This()) u32 {
        self.count = @min(self.count + 1, self.instances.count);
        return self.count + self.instances.first - 1;
    }

    pub fn maybe_reserve(self: *@This(), mesh: GpuResourceManager.MeshResourceHandle) u32 {
        if (std.meta.eql(self.mesh, mesh)) {
            _ = self.reserve();
            return self.count + self.instances.first - 1;
        }
        return 0;
    }

    pub fn draw(
        self: *const @This(),
        pipeline: *GraphicsPipeline,
        desc_sets: []const vk.DescriptorSet,
        offsets: []const u32,
        cmdbuf: *CmdBuffer,
        device: *Device,
        resources: *GpuResourceManager.GpuResources,
    ) void {
        cmdbuf.draw(device, .{
            .pipeline = pipeline,
            .desc_sets = desc_sets,
            .dynamic_offsets = offsets,
            .vertices = .{
                .buffer = resources.vertex_buffer.buffer,
                .count = self.mesh.vertex.count,
                .first = self.mesh.vertex.first,
            },
            .indices = .{
                .buffer = resources.index_buffer.buffer,
                .count = self.mesh.index.count,
                .first = self.mesh.index.first,
            },
            .instances = .{
                .buffer = resources.instance_buffer.buffer,
                .count = self.count,
                .first = self.instances.first,
            },
        });
    }
};

pub const GpuResourceManager = struct {
    pub const MeshResourceHandle = struct {
        index: Region,
        vertex: Region,

        const Region = struct {
            first: u32,
            count: u32,
        };
    };
    pub const BatchedInstanceResourceHandle = struct {
        first: u32,
        count: u32,
    };
    pub const CpuResources = struct {
        vertices: Vertices,
        triangles: Triangles,
        instances: Instances,

        const Vertices = std.ArrayList(Vertex);
        const Triangles = std.ArrayList([3]u32);
        const Instances = std.ArrayList(Instance);

        pub fn init() @This() {
            return .{
                .vertices = Vertices.init(allocator),
                .triangles = Triangles.init(allocator),
                .instances = Instances.init(allocator),
            };
        }

        pub fn deinit(self: *@This()) void {
            self.vertices.deinit();
            self.triangles.deinit();
            self.instances.deinit();
        }

        pub fn add_mesh(self: *@This(), m: *mesh_mod.Mesh) !MeshResourceHandle {
            const handle = MeshResourceHandle{
                .index = .{
                    .first = @intCast(self.triangles.items.len * 3),
                    .count = @intCast(m.faces.len * 3),
                },
                .vertex = .{
                    .first = @intCast(self.vertices.items.len),
                    .count = @intCast(m.vertices.len),
                },
            };

            for (m.vertices, m.normals, m.uvs) |v, n, uv| {
                var vertex = std.mem.zeroes(Vertex);
                vertex.pos = [4]f32{ v[0], v[1], v[2], 0 };
                vertex.normal = [4]f32{ n[0], n[1], n[2], 0 };
                vertex.uv = [4]f32{ uv[0], uv[1], 0, 0 };

                try self.vertices.append(vertex);
            }
            try self.triangles.appendSlice(m.faces);

            return handle;
        }

        pub fn batch_instances(self: *@This(), instances: []const Instance) !BatchedInstanceResourceHandle {
            const first = self.instances.items.len;
            try self.instances.appendSlice(instances);

            return .{
                .first = @intCast(first),
                .count = @intCast(instances.len),
            };
        }

        pub fn batch_instances_cloned(self: *@This(), instance: Instance, num: usize) !BatchedInstanceResourceHandle {
            const first = self.instances.items.len;
            try self.instances.appendNTimes(instance, num);

            return .{
                .first = @intCast(first),
                .count = @intCast(num),
            };
        }

        pub fn batch_reserve(self: *@This(), num: usize) !BatchedInstanceResourceHandle {
            const first = self.instances.items.len;
            try self.instances.appendNTimes(std.mem.zeroes(Instance), num);

            return .{
                .first = @intCast(first),
                .count = @intCast(num),
            };
        }

        pub fn upload(self: *@This(), engine: *Engine, pool: vk.CommandPool) !GpuResources {
            return try GpuResources.init(self, engine, pool);
        }
    };

    // TODO: overallocate and suddenly we have a dynamic version of this.
    // maybe pin some memory and have the handles point to it.
    pub const GpuResources = struct {
        vertex_buffer: Buffer,
        index_buffer: Buffer,
        instance_buffer: Buffer,

        pub fn init(cpu: *CpuResources, engine: *Engine, pool: vk.CommandPool) !@This() {
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

            var instance_buffer = try Buffer.new_from_slice(ctx, .{
                .usage = .{ .vertex_buffer_bit = true },
                .memory_type = .{
                    .device_local_bit = true,
                    .host_visible_bit = true,
                    .host_coherent_bit = true,
                },
            }, cpu.instances.items, pool);
            errdefer instance_buffer.deinit(device);

            return .{
                .vertex_buffer = vertex_buffer,
                .index_buffer = index_buffer,
                .instance_buffer = instance_buffer,
            };
        }

        pub fn update_instances(self: *@This(), device: *Device, instances: []Instance) !void {
            const data = try device.mapMemory(self.instance_buffer.memory, 0, vk.WHOLE_SIZE, .{});
            defer device.unmapMemory(self.instance_buffer.memory);

            const gpu_vertices: [*]Instance = @ptrCast(@alignCast(data));
            @memcpy(gpu_vertices[0..instances.len], instances);
        }

        pub fn deinit(self: *@This(), device: *Device) void {
            self.vertex_buffer.deinit(device);
            self.index_buffer.deinit(device);
            self.instance_buffer.deinit(device);
        }
    };
};
