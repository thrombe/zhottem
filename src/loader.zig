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

    const entity = try cmdbuf.insert(.{
        try C.Name.from(node.name),
        local,
    });

    if (node.mesh) |mi| {
        const mesh = &meshes.items[mi];
        mesh.count += 1;
        try cmdbuf.add_component(entity, C.StaticRender{ .mesh = mesh.handle });
        try cmdbuf.add_component(entity, try world.phy.add_body(.{
            .shape = .{ .mesh = .{
                .index_buffer = std.mem.bytesAsSlice(u32, std.mem.sliceAsBytes(mesh.mesh.faces)),
                .vertex_buffer = std.mem.bytesAsSlice(f32, std.mem.sliceAsBytes(mesh.mesh.vertices)),
            } },
            .pos = local.pos.xyz(),
            .rotation = local.rotation,
            .scale = local.scale.xyz(),
            .motion_type = .static,
            .friction = 0.4,
        }));
    }

    // if (node.extras) |extras| {
    //     try add_components(cmdbuf, entity, extras.zhott_components);
    // }
}

fn add_components(
    cmdbuf: *EntityComponentStore.CmdBuf,
    entity: Entity,
    components: []GltfInfo.ZhottExtras.Component,
) !void {
    for (components) |comp| {
        const t = C.Transform;
        // inline for (&.{C.Transform}) |t| {
        if (std.mem.eql(u8, @typeName(t), comp.component_name)) {
            const component = try std.json.parseFromValue(t, allocator.*, comp.value, .{ .allocate = .alloc_always });
            try cmdbuf.add_component(entity, component);

            switch (t) {
                C.Transform => {
                    try cmdbuf.add_component(entity, C.LastTransform{ .t = component });
                },
                else => {},
            }
        }
        // }
    }
}
