const std = @import("std");

const vk = @import("vulkan");

const mesh = @import("mesh.zig");

const Engine = @import("engine.zig");

const render_utils = @import("render_utils.zig");
const Buffer = render_utils.Buffer;
const GraphicsPipeline = render_utils.GraphicsPipeline;
const CmdBuffer = render_utils.CmdBuffer;

const app_mod = @import("app.zig");
const Vertex = app_mod.Vertex;
const Instance = app_mod.Instance;

const main = @import("main.zig");
const allocator = main.allocator;

const Device = Engine.VulkanContext.Api.Device;

pub const DrawCall = struct {
    mesh: GpuResourceManager.MeshResourceHandle,
    instances: GpuResourceManager.BatchedInstanceResourceHandle,

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
                .count = self.instances.count,
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

        pub fn add_mesh(self: *@This(), m: *mesh.Mesh) !MeshResourceHandle {
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
