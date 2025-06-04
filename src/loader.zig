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

const resources_mod = @import("resources.zig");
const ResourceManager = resources_mod.ResourceManager;
const InstanceManager = resources_mod.InstanceManager;
const InstanceAllocator = resources_mod.InstanceAllocator;

const main = @import("main.zig");
const allocator = main.allocator;

const types = .{
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
        capsule: Capsule,
        sphere: Sphere,
        plane: Plane,
        box: Box,

        pub const Capsule = struct {
            half_height: f32 = 0.5,
            radius: f32 = 0.2,
        };
        pub const Sphere = struct {
            center: Vec4 = .{},
            radius: f32 = 1,
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

pub fn spawn_default_scene(
    world: *world_mod.World,
    cpu: *ResourceManager.Assets,
    cmdbuf: *EntityComponentStore.CmdBuf,
    gltf_handle: ResourceManager.GltfHandle,
) !void {
    const gltf = cpu.ref(gltf_handle);
    const info = &gltf.gltf.info.value;

    const scene = &info.scenes[info.scene];
    std.debug.print("spawning {s} scene\n", .{scene.name});
    for (scene.nodes) |ni| {
        _ = try _spawn_node(
            world,
            cpu,
            cmdbuf,
            gltf_handle,
            null,
            ni,
            .{},
        );
    }
}

pub fn spawn_node(
    world: *world_mod.World,
    cpu: *ResourceManager.Assets,
    cmdbuf: *EntityComponentStore.CmdBuf,
    gltf_handle: ResourceManager.GltfHandle,
    name: []const u8,
    transform: C.Transform,
    material: ResourceManager.MaterialHandle,
) !C.Entity {
    const gltf = cpu.ref(gltf_handle);
    const info = &gltf.gltf.info.value;

    const nodes = info.nodes;
    var ni: ?usize = null;
    for (nodes, 0..) |*node, i| {
        if (std.mem.eql(u8, node.name, name)) {
            ni = i;
            break;
        }
    }

    const node = if (ni) |i| &nodes[i] else return error.NodeNotFound;
    std.debug.print("spawning {s} node\n", .{node.name});

    const entity = try _spawn_node(
        world,
        cpu,
        cmdbuf,
        gltf_handle,
        null,
        ni.?,
        transform,
        material,
    );

    return entity;
}

fn _spawn_node(
    world: *world_mod.World,
    cpu: *ResourceManager.Assets,
    cmdbuf: *EntityComponentStore.CmdBuf,
    gltf_handle: ResourceManager.GltfHandle,
    parent: ?C.Entity,
    node_index: GltfInfo.NodeIndex,
    transform: C.Transform,
    material: ResourceManager.MaterialHandle,
) !C.Entity {
    const gltf = cpu.ref(gltf_handle);
    const info: *assets_mod.Gltf.Info = &gltf.gltf.info.value;
    const nodes = info.nodes;
    const node = &nodes[node_index];
    const local = C.Transform.from_asset_transform(node.transform());
    const global = transform.apply_local(local);

    const entity = try cmdbuf.reserve();

    {
        var children = std.ArrayList(C.Entity).init(allocator.*);
        errdefer children.deinit();
        for (node.children) |ci| {
            const child = try _spawn_node(
                world,
                cpu,
                cmdbuf,
                gltf_handle,
                entity,
                ci,
                global,
                material,
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

    if (node.mesh) |m| {
        const hmesh = gltf.handles.meshes[m];

        if (node.skin) |si| {
            const harmature = gltf.handles.armatures[si];
            const armature = cpu.ref(harmature);
            const bones = try allocator.alloc(math.Mat4x4, armature.bones.len);
            const indices = try allocator.alloc(C.AnimatedMesh.AnimationIndices, armature.bones.len);
            @memset(bones, .{});
            @memset(indices, std.mem.zeroes(C.AnimatedMesh.AnimationIndices));
            try cmdbuf.add_component(entity, C.AnimatedMesh{
                .mesh = hmesh,
                .material = material,
                .armature = harmature,
                .animation_index = @intCast(entity.index % armature.animations.len),
                .bones = bones,
                .indices = indices,
            });
        } else {
            try cmdbuf.add_component(entity, C.StaticMesh{ .mesh = hmesh, .material = material });
        }

        try cmdbuf.add_component(entity, C.BatchedRender{});
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
                        C.Name => {
                            const value = try C.Name.from(component.value.name);
                            try cmdbuf.add_component(entity, value);
                        },
                        Rigidbody => {
                            const value: Rigidbody = component.value;
                            const shape: world_mod.Jphysics.ShapeSettings = blk: switch (value.collider_shape) {
                                .mesh => {
                                    const m = node.mesh orelse return error.MissingMesh;
                                    const hmesh = gltf.handles.meshes[m];
                                    const mesh = cpu.ref(hmesh);
                                    break :blk .{ .mesh = .{
                                        .index_buffer = std.mem.bytesAsSlice(u32, std.mem.sliceAsBytes(mesh.faces)),
                                        .vertex_buffer = std.mem.bytesAsSlice(f32, std.mem.sliceAsBytes(mesh.vertices)),
                                    } };
                                },
                                .sphere => |s| .{ .sphere = .{ .radius = s.radius } },
                                .box => |s| .{ .box = .{ .size = s.half_extent } },
                                .capsule => |s| .{ .capsule = .{ .half_height = s.half_height, .radius = s.radius } },
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

// fn maybe_get_entity(extras: *GltfInfo.ZhottExtras) !?Entity {
//     for (extras.zhott_components) |comp| {
//         if (std.mem.eql(u8, @typeName(Entity), comp.component_name)) {
//             const component = try std.json.parseFromValue(
//                 Entity,
//                 allocator.*,
//                 comp.value,
//                 .{ .allocate = .alloc_if_needed },
//             );
//             defer component.deinit();

//             return component.value;
//         }
//     }
//     return null;
// }
