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
    .{ .type = Rigidbody, .default = Rigidbody{} },
};

pub const Rigidbody = struct {
    collider_shape: ColliderShape = .mesh,
    motion_type: world_mod.Jphysics.jolt.MotionType = .static,
    motion_quality: world_mod.Jphysics.jolt.MotionQuality = .discrete,
    friction: f32 = 0.0,

    pub const ColliderShape = union(enum) {
        mesh: struct {},
        sphere: Sphere,
        plane: Plane,
        box: Box,

        pub const Sphere = struct {
            center: Vec4 = .{},
            radius: f32,
        };
        pub const Plane = struct {
            normal: Vec4 = .{ .y = 1 },
            offset: f32 = 0,
        };
        pub const Box = struct {
            half_extent: Vec3,
        };
    };
    pub const ColliderType = enum {
        dynamic,
        static,
    };
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
    handle: ResourceManager.MeshHandle,
    count: u32 = 0,
});

const Armatures = std.ArrayList(struct { handle: ResourceManager.ArmatureHandle, len: usize });

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
        try meshes.append(.{
            .handle = try cpu_resources.add(mesh),
        });
    }

    var armatures = Armatures.init(allocator.*);
    defer armatures.deinit();

    for (info.skins) |*skin| {
        const armature = try gltf.parse_armature(skin);
        try armatures.append(.{
            .handle = try cpu_resources.add(armature),
            .len = skin.joints.len,
        });
    }

    const scene = &info.scenes[info.scene];
    std.debug.print("spawning {s} scene\n", .{scene.name});
    for (scene.nodes) |ni| {
        _ = try spawn_node(
            world,
            cpu_resources,
            cmdbuf,
            null,
            ni,
            info.nodes,
            &meshes,
            &armatures,
            .{},
        );
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
    cpu_resources: *ResourceManager.CpuResources,
    cmdbuf: *EntityComponentStore.CmdBuf,
    parent: ?Entity,
    node_index: GltfInfo.NodeIndex,
    nodes: []GltfInfo.Node,
    meshes: *Meshes,
    armatures: *Armatures,
    transform: C.Transform,
) !Entity {
    const node = &nodes[node_index];
    const local = C.Transform.from_asset_transform(node.transform());
    const global = transform.apply_local(local);

    const entity = if (node.extras) |*e| try maybe_get_entity(e) orelse cmdbuf.reserve() else cmdbuf.reserve();

    {
        var children = std.ArrayList(Entity).init(allocator.*);
        errdefer children.deinit();
        for (node.children) |ci| {
            const child = try spawn_node(
                world,
                cpu_resources,
                cmdbuf,
                entity,
                ci,
                nodes,
                meshes,
                armatures,
                global,
            );
            try children.append(child);
        }

        try cmdbuf.insert_reserved(entity, .{
            C.Node{ .parent = parent, .children = children },
            C.LocalTransform{ .transform = local },
            C.GlobalTransform{ .transform = global },
            C.LastTransform{ .transform = global },
        });
    }

    const meshi = if (node.mesh) |mi| &meshes.items[mi] else null;
    const armature = if (node.skin) |si| &armatures.items[si] else null;
    if (meshi) |m| {
        m.count += 1;

        if (armature) |a| {
            const bones = try allocator.alloc(math.Mat4x4, a.len);
            const indices = try allocator.alloc(C.AnimatedRender.AnimationIndices, a.len);
            @memset(bones, .{});
            @memset(indices, std.mem.zeroes(C.AnimatedRender.AnimationIndices));
            try cmdbuf.add_component(entity, C.AnimatedRender{
                .mesh = m.handle,
                .armature = a.handle,
                .bones = bones,
                .indices = indices,
            });
        } else {
            try cmdbuf.add_component(entity, C.StaticRender{ .mesh = m.handle });
        }
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
                        Rigidbody => {
                            const value: Rigidbody = component.value;
                            const shape: world_mod.Jphysics.ShapeSettings = blk: switch (value.collider_shape) {
                                .mesh => {
                                    const m = meshi orelse return error.MissingMesh;
                                    const mesh = cpu_resources.ref(m.handle);
                                    break :blk .{ .mesh = .{
                                        .index_buffer = std.mem.bytesAsSlice(u32, std.mem.sliceAsBytes(mesh.faces)),
                                        .vertex_buffer = std.mem.bytesAsSlice(f32, std.mem.sliceAsBytes(mesh.vertices)),
                                    } };
                                },
                                .sphere => |s| .{ .sphere = .{ .radius = s.radius } },
                                .box => |s| .{ .box = .{ .size = s.half_extent } },
                                else => return error.ShapeNotSupportedYet,
                            };
                            try cmdbuf.add_component(entity, try world.phy.add_body(.{
                                .shape = shape,
                                .pos = global.pos,
                                .rotation = global.rotation,
                                .scale = global.scale,
                                .motion_quality = value.motion_quality,
                                .motion_type = value.motion_type,
                                .friction = value.friction,
                            }));
                        },
                        C.Transform => {
                            // try cmdbuf.add_component(entity, global);
                            // try cmdbuf.add_component(entity, transform.apply_local(component.value));
                        },
                        else => {
                            try cmdbuf.add_component(entity, component.value);
                        },
                    }
                }
            }
        }
    }

    return entity;
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
