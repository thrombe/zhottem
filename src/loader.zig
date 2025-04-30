const std = @import("std");

const math = @import("math.zig");
const Vec4 = math.Vec4;
const Vec3 = math.Vec3;

const assets_mod = @import("assets.zig");
const GltfInfo = assets_mod.Gltf.Info;

const world_mod = @import("world.zig");
const C = world_mod.C;

const ecs_mod = @import("ecs.zig");
const EntityComponentStore = ecs_mod.EntityComponentStore;
const Entity = ecs_mod.Entity;

const resources_mod = @import("resources.zig");
const ResourceManager = resources_mod.ResourceManager;
const InstanceManager = resources_mod.InstanceManager;
const InstanceAllocator = resources_mod.InstanceAllocator;

const main = @import("main.zig");
const allocator = main.allocator;

const types = .{
    .{ .type = Entity },
    .{ .type = C.Name },
    .{ .type = C.Transform, .default = C.Transform{} },
};

pub fn generate_type_registry() !void {
    var importer = try assets_mod.TypeSchemaGenerator.init(.{
        .name = C.Name,
        .entity = Entity,
        .transform = C.Transform,
    });
    defer importer.deinit();

    inline for (&types) |typ| {
        const default = if (@hasField(@TypeOf(typ), "default")) @field(typ, "default") else null;
        try importer.write_schema(typ.type, default);
    }

    try importer.write_to_file("blender/components.json");
}

const Meshes = std.ArrayList(struct {
    mesh: assets_mod.Mesh,
    handle: ResourceManager.MeshResourceHandle,
    count: u32 = 0,
});

pub fn load_gltf(
    world: *world_mod.World,
    cpu_resources: *ResourceManager.CpuResources,
    instance_manager: *InstanceManager,
    cmdbuf: *EntityComponentStore.CmdBuf,
    gltf: *assets_mod.Gltf,
) !void {
    const info = &gltf.info.value;
    var meshes = Meshes.init(allocator.*);
    defer meshes.deinit();

    for (info.meshes) |*m| {
        const mesh = try gltf.parse_mesh(m);
        const mesh_handle = try cpu_resources.add_mesh(&mesh);
        try meshes.append(.{
            .mesh = mesh,
            .handle = mesh_handle,
        });
    }

    const scene = &info.scenes[info.scene];
    std.debug.print("spawning {s} scene\n", .{scene.name});
    for (scene.nodes) |ni| {
        try spawn_node(world, cmdbuf, ni, info.nodes, &meshes, .{});
    }

    for (meshes.items) |mesh| {
        try instance_manager.instances.append(.{
            .mesh = mesh.handle,
            .instances = try cpu_resources.batch_reserve(mesh.count),
        });
    }
}

fn spawn_node(
    world: *world_mod.World,
    cmdbuf: *EntityComponentStore.CmdBuf,
    node_index: GltfInfo.NodeIndex,
    nodes: []GltfInfo.Node,
    meshes: *Meshes,
    transform: C.Transform,
) !void {
    const node = &nodes[node_index];
    const local = C.Transform.from_asset_transform(node.transform());
    for (node.children) |ci| {
        try spawn_node(world, cmdbuf, ci, nodes, meshes, transform.apply_local(local));
    }

    const entity = if (node.extras) |*e| try maybe_get_entity(e) orelse cmdbuf.reserve() else cmdbuf.reserve();
    try cmdbuf.insert_reserved(entity, .{});

    const mesh = if (node.mesh) |mi| &meshes.items[mi] else null;
    if (mesh) |m| {
        m.count += 1;
        try cmdbuf.add_component(entity, C.StaticRender{ .mesh = m.handle });
        try cmdbuf.add_component(entity, try world.phy.add_body(.{
            .shape = .{ .mesh = .{
                .index_buffer = std.mem.bytesAsSlice(u32, std.mem.sliceAsBytes(m.mesh.faces)),
                .vertex_buffer = std.mem.bytesAsSlice(f32, std.mem.sliceAsBytes(m.mesh.vertices)),
            } },
            .pos = local.pos.xyz(),
            .rotation = local.rotation,
            .scale = local.scale.xyz(),
            .motion_type = .static,
            .friction = 0.4,
        }));
    }

    if (node.extras) |extras| {
        for (extras.zhott_components) |comp| {
            inline for (&types) |typ| {
                if (std.mem.eql(u8, @typeName(typ.type), comp.component_name)) {
                    const component = try std.json.parseFromValue(
                        typ.type,
                        allocator.*,
                        comp.value,
                        .{ .allocate = .alloc_always },
                    );
                    defer component.deinit();

                    switch (typ.type) {
                        Entity => {},
                        C.Name => {
                            const value = try C.Name.from(component.value.name);
                            try cmdbuf.add_component(entity, value);
                        },
                        else => {
                            try cmdbuf.add_component(entity, component.value);
                        },
                    }
                }
            }
        }
    }
}

fn maybe_get_entity(extras: *GltfInfo.ZhottExtras) !?Entity {
    for (extras.zhott_components) |comp| {
        if (std.mem.eql(u8, @typeName(Entity), comp.component_name)) {
            const component = try std.json.parseFromValue(
                Entity,
                allocator.*,
                comp.value,
                .{ .allocate = .alloc_if_needed },
            );
            defer component.deinit();

            return component.value;
        }
    }
    return null;
}
