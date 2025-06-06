const std = @import("std");

const math = @import("math.zig");
const Vec4 = math.Vec4;
const Vec3 = math.Vec3;

const utils_mod = @import("utils.zig");
const cast = utils_mod.cast;

const Engine = @import("engine.zig");

const resources_mod = @import("resources.zig");
const ResourceManager = resources_mod.ResourceManager;

const assets_mod = @import("assets.zig");

const ecs_mod = @import("ecs.zig");

const main = @import("main.zig");
const allocator = main.allocator;

pub const C = EntityComponentStore.component_decls;
pub const EntityComponentStore = ecs_mod.EntityComponentStore(struct {
    pub usingnamespace Components;
    pub const BodyId = Jphysics.BodyId;
    pub const CharacterBody = Jphysics.CharacterBody;
    pub const Camera = math.Camera;
}, *World);

pub const Jphysics = struct {
    pub const jolt = @import("jolt/jolt.zig");

    phy: *jolt.PhysicsSystem,
    global_state: ?jolt.GlobalState = null,
    state: *State,

    const State = struct {
        broadphase_layer_interface: Impl.MyBroadphaseLayerInterface = .init(),
        obj_vs_broadphase_layer_interface: Impl.MyObjectVsBroadPhaseLayerFilter = .{},
        obj_layer_pair_filter: Impl.MyObjectLayerPairFilter = .{},
        character_contact_listener: Impl.MyCharacterContactListener = .{},
        object_layer_filter: Impl.MyObjectLayerFilter = .{},
        broadphase_layer_filter: Impl.MyBroadphaseLayerFilter = .{},
        debug_renderer: ?Impl.MyDebugRenderer = null,
    };

    pub const BodyId = struct {
        id: jolt.BodyId,

        pub fn deinit_with_context(self: *@This(), ctx: *World) void {
            ctx.phy.phy.getBodyInterfaceMut().removeAndDestroyBody(self.id);
        }
    };
    pub const CharacterBody = struct {
        character: *jolt.CharacterVirtual,
        force: Vec3 = .{},
        impulse: Vec3 = .{},

        pub fn deinit(self: *@This()) void {
            self.character.destroy();
        }
    };

    pub fn init(up: Vec3) !@This() {
        try jolt.init(allocator.*, .{});
        errdefer jolt.deinit();

        var state = try allocator.create(State);
        errdefer allocator.destroy(state);
        state.* = .{};

        state.debug_renderer = try .init();
        try jolt.DebugRenderer.init(@ptrCast(&state.debug_renderer));
        errdefer {
            jolt.DebugRenderer.deinit();
            state.debug_renderer = null;
        }

        var ps = try jolt.PhysicsSystem.create(
            state.broadphase_layer_interface.interface(),
            state.obj_vs_broadphase_layer_interface.interface(),
            state.obj_layer_pair_filter.interface(),
            .{
                .max_bodies = 65536,
                .num_body_mutexes = 0,
                .max_body_pairs = 65536,
                .max_contact_constraints = 10240,
            },
        );
        errdefer ps.destroy();

        ps.setGravity(up.scale(-16).to_buf());

        return .{ .phy = ps, .state = state };
    }

    pub fn deinit(self: *@This()) void {
        self.phy.destroy();

        jolt.DebugRenderer.deinit(@ptrCast(&self.state.debug_renderer));
        self.state.debug_renderer.?.deinit();
        self.state.debug_renderer = null;

        jolt.deinit();
        allocator.destroy(self.state);
    }

    pub fn pre_reload(self: *@This()) void {
        self.global_state = jolt.preReload();
    }

    pub fn post_reload(self: *@This()) void {
        jolt.postReload(allocator.*, self.global_state.?);
        self.global_state = null;
    }

    pub fn optimize(self: *@This()) void {
        self.phy.optimizeBroadPhase();
    }

    pub fn update(self: *@This(), sim_time: f32, steps: u32) !void {
        try self.phy.update(sim_time, .{
            .collision_steps = @intCast(steps),
        });
    }

    pub fn add_body(self: *@This(), settings: BodySettings) !BodyId {
        const bodyi = self.phy.getBodyInterfaceMut();
        var body_settings = jolt.BodyCreationSettings{
            .position = settings.pos.withw(0).to_buf(),
            .rotation = settings.rotation.to_buf(),
            .linear_velocity = settings.velocity.withw(0).to_buf(),
            .angular_velocity = settings.angular_velocity.withw(0).to_buf(),
            .motion_type = settings.motion_type,
            .motion_quality = settings.motion_quality,
            .friction = settings.friction,
            .object_layer = Impl.object_layers.moving,
            .override_mass_properties = .calc_inertia,
            .mass_properties_override = .{
                .mass = 1.0,
            },
        };

        switch (settings.shape) {
            .sphere => |s| {
                const shape = try jolt.SphereShapeSettings.create(s.radius);
                defer shape.release();
                const rt_shape = try jolt.DecoratedShapeSettings.createRotatedTranslated(
                    shape.asShapeSettings(),
                    Vec4.quat_identity_rot().to_buf(),
                    settings.offset.to_buf(),
                );
                defer rt_shape.release();
                body_settings.shape = try rt_shape.createShape();
            },
            .box => |s| {
                const shape = try jolt.BoxShapeSettings.create(s.size.to_buf());
                defer shape.release();
                const rt_shape = try jolt.DecoratedShapeSettings.createRotatedTranslated(
                    shape.asShapeSettings(),
                    Vec4.quat_identity_rot().to_buf(),
                    settings.offset.to_buf(),
                );
                defer rt_shape.release();
                body_settings.shape = try rt_shape.createShape();
            },
            .capsule => |s| {
                const shape = try jolt.CapsuleShapeSettings.create(s.half_height, s.radius);
                defer shape.release();
                const rt_shape = try jolt.DecoratedShapeSettings.createRotatedTranslated(
                    shape.asShapeSettings(),
                    Vec4.quat_identity_rot().to_buf(),
                    settings.offset.to_buf(),
                );
                defer rt_shape.release();
                body_settings.shape = try rt_shape.createShape();
            },
            .mesh => |s| {
                const shape = try jolt.MeshShapeSettings.create(
                    @ptrCast(s.vertex_buffer.ptr),
                    @intCast(s.vertex_buffer.len / 3),
                    @sizeOf([3]f32),
                    s.index_buffer,
                );
                defer shape.release();
                const scaled_shape = try jolt.DecoratedShapeSettings.createScaled(
                    shape.asShapeSettings(),
                    settings.scale.to_buf(),
                );
                defer scaled_shape.release();
                const rt_shape = try jolt.DecoratedShapeSettings.createRotatedTranslated(
                    scaled_shape.asShapeSettings(),
                    Vec4.quat_identity_rot().to_buf(),
                    settings.offset.to_buf(),
                );
                defer rt_shape.release();
                body_settings.shape = try rt_shape.createShape();
            },
        }
        // it is refcounted so we release this one
        defer body_settings.shape.?.release();

        const bid = try bodyi.createAndAddBody(body_settings, .activate);
        return .{ .id = bid };
    }

    pub fn add_character(self: *@This(), v: struct {
        pos: Vec3 = .{},
        rot: Vec4 = .quat_identity_rot(),
        half_height: f32 = 1.0,
        radius: f32 = 0.4,
    }) !CharacterBody {
        const settings = try jolt.CharacterVirtualSettings.create();
        defer settings.release();

        const shape = try jolt.CapsuleShapeSettings.create(v.half_height, v.radius);
        // const shape = try jolt.BoxShapeSettings.create(Vec3.splat(v.radius).to_buf());
        // const shape = try jolt.SphereShapeSettings.create(v.radius);
        defer shape.release();

        const rotated = try jolt.DecoratedShapeSettings.createRotatedTranslated(
            shape.asShapeSettings(),
            Vec4.quat_identity_rot().to_buf(),
            (Vec3{ .y = -v.half_height - v.radius }).to_buf(),
        );
        defer rotated.release();
        settings.base.shape = try rotated.createShape();
        errdefer settings.base.shape.release();
        // settings.inner_body_shape = try shape.createShape();
        // errdefer settings.inner_body_shape.?.release();
        settings.inner_body_layer = Impl.object_layers.moving;
        settings.back_face_mode = .collide_with_back_faces;
        settings.max_strength = 10000000000;
        settings.mass = 100000000;

        const character = try jolt.CharacterVirtual.create(
            settings,
            v.pos.to_buf(),
            v.rot.to_buf(),
            self.phy,
        );
        errdefer character.destroy();
        character.setListener(@ptrCast(@constCast(self.state.character_contact_listener.interface())));

        return .{ .character = character };
    }

    pub fn get_transform(self: *@This(), bid: BodyId) struct { active: bool, position: Vec3, rotation: Vec4 } {
        const pos = self.phy.getBodyInterface().getPosition(bid.id);
        const rot = self.phy.getBodyInterface().getRotation(bid.id);
        const active = self.phy.getBodyInterface().isActive(bid.id);
        return .{
            .active = active,
            .position = Vec3.from_buf(pos),
            .rotation = Vec4.from_buf(rot),
        };
    }

    pub fn apply_force(self: *@This(), bid: BodyId, force: Vec3) void {
        self.phy.getBodyInterfaceMut().addForce(bid.id, force.to_buf());
    }

    pub fn set_rotation(self: *@This(), bid: BodyId, rot: Vec4) void {
        self.phy.getBodyInterfaceMut().setRotation(bid.id, rot.to_buf());
    }

    pub fn render_clear(self: *@This()) void {
        _ = self.state.debug_renderer orelse return;

        if (self.state.debug_renderer) |*dbg| {
            dbg.clear();
        }
    }

    pub fn render_tick(self: *@This()) void {
        _ = self.state.debug_renderer orelse return;

        self.phy.drawBodies(&jolt.DebugRenderer.BodyDrawSettings{
            .shape = true,
            .shape_wireframe = true,
            // .center_of_mass_transform = true,
            // .world_transform = true,
            // .velocity = true,
            // .mass_and_inertia = true,
            // .sleep_stats = true,
        }, null);
    }

    pub const BodySettings = struct {
        shape: ShapeSettings,
        motion_type: jolt.MotionType = .dynamic,
        motion_quality: jolt.MotionQuality = .discrete,
        pos: Vec3 = .{},
        rotation: Vec4 = Vec4.quat_identity_rot(),
        scale: Vec3 = Vec3.splat(1.0),
        velocity: Vec3 = .{},
        angular_velocity: Vec3 = .{},
        friction: f32 = 0,
        offset: Vec3 = .{},
    };
    pub const ShapeSettings = union(enum) {
        sphere: struct {
            radius: f32,
        },
        box: struct {
            size: Vec3,
        },
        capsule: struct {
            half_height: f32,
            radius: f32,
        },
        mesh: struct {
            vertex_buffer: []f32,
            index_buffer: []u32,
        },
    };

    pub const Impl = struct {
        pub const object_layers = struct {
            pub const non_moving: jolt.ObjectLayer = 0;
            pub const moving: jolt.ObjectLayer = 1;
            pub const len: u32 = 2;
        };

        pub const broad_phase_layers = struct {
            pub const non_moving: jolt.BroadPhaseLayer = 0;
            pub const moving: jolt.BroadPhaseLayer = 1;
            pub const len: u32 = 2;
        };

        const MyCharacterContactListener = extern struct {
            usingnamespace jolt.CharacterContactListener.Methods(@This());
            __v: *const jolt.CharacterContactListener.VTable = &vtable,
            const vtable = jolt.vtableFrom(jolt.CharacterContactListener.VTable, @This());

            pub fn OnContactSolve(
                iself: *jolt.CharacterContactListener,
                character: *const jolt.CharacterVirtual,
                body: *const jolt.BodyId,
                sub_shape_id: *const jolt.SubShapeId,
                contact_position: jolt.c.rvec3,
                contact_normal: jolt.c.vec3,
                contact_velocity: jolt.c.vec3,
                contact_material: *const jolt.Material,
                character_velocity: jolt.c.vec3,
                character_velocity_out: *jolt.c.vec3,
            ) callconv(.C) void {
                _ = character;
                _ = body;
                _ = sub_shape_id;
                _ = contact_position;
                _ = contact_normal;
                _ = contact_velocity;
                _ = contact_material;
                const self: *@This() = @ptrCast(iself);
                _ = self;

                _ = character_velocity;
                _ = character_velocity_out;
            }

            pub fn OnAdjustBodyVelocity(
                iself: *jolt.CharacterContactListener,
                character: *const jolt.CharacterVirtual,
                body: *const jolt.BodyId,
                io_linear_velocity: *jolt.c.vec3,
                io_angular_velocity: *jolt.c.vec3,
            ) callconv(.C) void {
                _ = iself;
                _ = character;
                _ = body;
                _ = io_linear_velocity;
                _ = io_angular_velocity;
            }
            pub fn OnContactValidate(
                iself: *jolt.CharacterContactListener,
                character: *const jolt.CharacterVirtual,
                body: *const jolt.BodyId,
                sub_shape_id: *const jolt.SubShapeId,
            ) callconv(.C) bool {
                _ = iself;
                _ = character;
                _ = body;
                _ = sub_shape_id;
                return true;
            }
            pub fn OnCharacterContactValidate(
                iself: *jolt.CharacterContactListener,
                character: *const jolt.CharacterVirtual,
                other_character: *const jolt.CharacterVirtual,
                sub_shape_id: *const jolt.SubShapeId,
            ) callconv(.C) bool {
                _ = iself;
                _ = character;
                _ = other_character;
                _ = sub_shape_id;
                return true;
            }
            pub fn OnContactAdded(
                iself: *jolt.CharacterContactListener,
                character: *const jolt.CharacterVirtual,
                body: *const jolt.BodyId,
                sub_shape_id: *const jolt.SubShapeId,
                contact_position: jolt.c.rvec3,
                contact_normal: jolt.c.vec3,
                io_settings: *jolt.CharacterContactSettings,
            ) callconv(.C) void {
                _ = iself;
                _ = character;
                _ = body;
                _ = sub_shape_id;
                _ = contact_position;
                _ = contact_normal;
                // _ = io_settings;

                io_settings.can_push_character = false;
            }
            pub fn OnCharacterContactAdded(
                iself: *jolt.CharacterContactListener,
                character: *const jolt.CharacterVirtual,
                other_character: *const jolt.CharacterVirtual,
                sub_shape_id: *const jolt.SubShapeId,
                contact_position: jolt.c.rvec3,
                contact_normal: jolt.c.vec3,
                io_settings: *jolt.CharacterContactSettings,
            ) callconv(.C) void {
                _ = iself;
                _ = character;
                _ = other_character;
                _ = sub_shape_id;
                _ = contact_position;
                _ = contact_normal;
                _ = io_settings;
            }
            pub fn OnCharacterContactSolve(
                iself: *jolt.CharacterContactListener,
                character: *const jolt.CharacterVirtual,
                other_character: *const jolt.CharacterVirtual,
                sub_shape_id: *const jolt.SubShapeId,
                contact_position: jolt.c.rvec3,
                contact_normal: jolt.c.vec3,
                contact_velocity: jolt.c.vec3,
                contact_material: *const jolt.Material,
                character_velocity: jolt.c.vec3,
                character_velocity_out: *jolt.c.vec3,
            ) callconv(.C) void {
                _ = iself;
                _ = character;
                _ = other_character;
                _ = sub_shape_id;
                _ = contact_position;
                _ = contact_normal;
                _ = contact_velocity;
                _ = contact_material;
                _ = character_velocity;
                _ = character_velocity_out;
            }
            pub fn OnContactPersisted(
                iself: *jolt.CharacterContactListener,
                character: *const jolt.CharacterVirtual,
                body: *const jolt.BodyId,
                sub_shape_id: *const jolt.SubShapeId,
                contact_position: jolt.c.rvec3,
                contact_normal: jolt.c.vec3,
                io_settings: *jolt.CharacterContactSettings,
            ) callconv(.C) void {
                _ = iself;
                _ = character;
                _ = body;
                _ = sub_shape_id;
                _ = contact_position;
                _ = contact_normal;
                _ = io_settings;
            }
            pub fn OnContactRemoved(
                iself: *jolt.CharacterContactListener,
                character: *const jolt.CharacterVirtual,
                body: *const jolt.BodyId,
                sub_shape_id2: *const jolt.SubShapeId,
            ) callconv(.C) void {
                _ = iself;
                _ = character;
                _ = body;
                _ = sub_shape_id2;
            }
            pub fn OnCharacterContactPersisted(
                iself: *jolt.CharacterContactListener,
                character: *const jolt.CharacterVirtual,
                other_character: *const jolt.CharacterVirtual,
                sub_shape_id2: *const jolt.SubShapeId,
                contact_position: jolt.c.rvec3,
                contact_normal: jolt.c.vec3,
                io_settings: *jolt.CharacterContactSettings,
            ) callconv(.C) void {
                _ = iself;
                _ = character;
                _ = other_character;
                _ = sub_shape_id2;
                _ = contact_position;
                _ = contact_normal;
                _ = io_settings;
            }
            pub fn OnCharacterContactRemoved(
                iself: *jolt.CharacterContactListener,
                character: *const jolt.CharacterVirtual,
                other_character_id: *const jolt.CharacterId,
                sub_shape_id2: *const jolt.SubShapeId,
            ) callconv(.C) void {
                _ = iself;
                _ = character;
                _ = other_character_id;
                _ = sub_shape_id2;
            }
        };

        // TODO: fix
        const MyObjectLayerFilter = extern struct {
            usingnamespace jolt.ObjectLayerFilter.Methods(@This());
            __v: *const jolt.ObjectLayerFilter.VTable = &vtable,
            const vtable = jolt.vtableFrom(jolt.ObjectLayerFilter.VTable, @This());

            pub fn shouldCollide(self: *const jolt.ObjectLayerFilter, layer: jolt.ObjectLayer) callconv(.C) bool {
                _ = self;
                _ = layer;
                return true;
            }
        };

        // TODO: fix
        const MyBroadphaseLayerFilter = extern struct {
            usingnamespace jolt.BroadPhaseLayerFilter.Methods(@This());
            __v: *const jolt.BroadPhaseLayerFilter.VTable = &vtable,
            const vtable = jolt.vtableFrom(jolt.BroadPhaseLayerFilter.VTable, struct {
                pub fn shouldCollide(self: *const jolt.BroadPhaseLayerFilter, layer: jolt.BroadPhaseLayer) callconv(.C) bool {
                    _ = self;
                    _ = layer;
                    return true;
                }
            });
        };

        const MyBroadphaseLayerInterface = extern struct {
            usingnamespace jolt.BroadPhaseLayerInterface.Methods(@This());
            __v: *const jolt.BroadPhaseLayerInterface.VTable = &vtable,

            object_to_broad_phase: [object_layers.len]jolt.BroadPhaseLayer = undefined,

            const vtable = jolt.BroadPhaseLayerInterface.VTable{
                .getNumBroadPhaseLayers = _getNumBroadPhaseLayers,
                .getBroadPhaseLayer = if (@import("builtin").abi == .msvc)
                    _getBroadPhaseLayerMsvc
                else
                    _getBroadPhaseLayer,
            };

            fn init() MyBroadphaseLayerInterface {
                var layer_interface: MyBroadphaseLayerInterface = .{};
                layer_interface.object_to_broad_phase[object_layers.non_moving] = broad_phase_layers.non_moving;
                layer_interface.object_to_broad_phase[object_layers.moving] = broad_phase_layers.moving;
                return layer_interface;
            }

            fn _getNumBroadPhaseLayers(iself: *const jolt.BroadPhaseLayerInterface) callconv(.C) u32 {
                const self = @as(*const MyBroadphaseLayerInterface, @ptrCast(iself));
                return @as(u32, @intCast(self.object_to_broad_phase.len));
            }

            fn _getBroadPhaseLayer(
                iself: *const jolt.BroadPhaseLayerInterface,
                layer: jolt.ObjectLayer,
            ) callconv(.C) jolt.BroadPhaseLayer {
                const self = @as(*const MyBroadphaseLayerInterface, @ptrCast(iself));
                return self.object_to_broad_phase[@as(usize, @intCast(layer))];
            }

            fn _getBroadPhaseLayerMsvc(
                iself: *const jolt.BroadPhaseLayerInterface,
                out_layer: *jolt.BroadPhaseLayer,
                layer: jolt.ObjectLayer,
            ) callconv(.C) *const jolt.BroadPhaseLayer {
                const self = @as(*const MyBroadphaseLayerInterface, @ptrCast(iself));
                out_layer.* = self.object_to_broad_phase[@as(usize, @intCast(layer))];
                return out_layer;
            }
        };

        const MyObjectVsBroadPhaseLayerFilter = extern struct {
            usingnamespace jolt.ObjectVsBroadPhaseLayerFilter.Methods(@This());
            __v: *const jolt.ObjectVsBroadPhaseLayerFilter.VTable = &vtable,

            const vtable = jolt.ObjectVsBroadPhaseLayerFilter.VTable{ .shouldCollide = _shouldCollide };

            fn _shouldCollide(
                _: *const jolt.ObjectVsBroadPhaseLayerFilter,
                layer1: jolt.ObjectLayer,
                layer2: jolt.BroadPhaseLayer,
            ) callconv(.C) bool {
                return switch (layer1) {
                    object_layers.non_moving => layer2 == broad_phase_layers.moving,
                    object_layers.moving => true,
                    else => unreachable,
                };
            }
        };

        const MyObjectLayerPairFilter = extern struct {
            usingnamespace jolt.ObjectLayerPairFilter.Methods(@This());
            __v: *const jolt.ObjectLayerPairFilter.VTable = &vtable,

            const vtable = jolt.ObjectLayerPairFilter.VTable{ .shouldCollide = _shouldCollide };

            fn _shouldCollide(
                _: *const jolt.ObjectLayerPairFilter,
                object1: jolt.ObjectLayer,
                object2: jolt.ObjectLayer,
            ) callconv(.C) bool {
                return switch (object1) {
                    object_layers.non_moving => object2 == object_layers.moving,
                    object_layers.moving => true,
                    else => unreachable,
                };
            }
        };

        const MyPhysicsStepListener = extern struct {
            usingnamespace jolt.PhysicsStepListener.Methods(@This());
            __v: *const jolt.PhysicsStepListener.VTable = &vtable,
            steps_heard: u32 = 0,

            const vtable = jolt.PhysicsStepListener.VTable{ .onStep = _onStep };

            fn _onStep(psl: *jolt.PhysicsStepListener, delta_time: f32, physics_system: *jolt.PhysicsSystem) callconv(.C) void {
                _ = delta_time;
                _ = physics_system;
                const self = @as(*MyPhysicsStepListener, @ptrCast(psl));
                self.steps_heard += 1;
            }
        };

        const MyDebugRenderer = extern struct {
            usingnamespace jolt.DebugRenderer.Methods(@This());
            __v: *const jolt.DebugRenderer.VTable(@This()) = &vtable,

            state: *RenderState,

            // TODO: support the more efficient methods too
            //   - don't forget to modify DebugRendererImpl in src/jolt/c.cpp
            //     to enable the other methods
            const vtable = jolt.DebugRenderer.VTable(@This()){
                .drawLine = _drawLine,
                // .drawTriangle = _drawTriangle,
                // .createTriangleBatch = _createTriangleBatch,
                // .createTriangleBatchIndexed = _createTriangleBatchIndexed,
                // .drawGeometry = _drawGeometry,
                .drawText3D = _drawText3D,
            };

            const Real = jolt.Real;
            const RMatrix = jolt.RMatrix;
            const AABox = jolt.AABox;
            const Color = jolt.DebugRenderer.Color;
            const Triangle = jolt.DebugRenderer.Triangle;
            const Vertex = jolt.DebugRenderer.Vertex;
            const Geometry = jolt.DebugRenderer.Geometry;
            const CullMode = jolt.DebugRenderer.CullMode;
            const CastShadow = jolt.DebugRenderer.CastShadow;
            const DrawMode = jolt.DebugRenderer.DrawMode;

            const JoltDebugResources = resources_mod.ResourceManager.JoltDebugResources;
            const RenderState = struct {
                lock: std.Thread.Mutex = .{},
                lines: std.ArrayList(JoltDebugResources.LineVertex),
            };

            pub fn init() !@This() {
                const state = try allocator.create(RenderState);
                errdefer allocator.destroy(state);
                state.* = .{
                    .lines = .init(allocator.*),
                };
                return .{ .state = state };
            }

            pub fn deinit(self: *@This()) void {
                defer allocator.destroy(self.state);
                self.state.lock.lock();
                defer self.state.lock.unlock();

                self.state.lines.deinit();
            }

            // clear buffers every frame before physics tick
            pub fn clear(self: *@This()) void {
                self.state.lock.lock();
                defer self.state.lock.unlock();

                self.state.lines.clearRetainingCapacity();
            }

            fn _drawLine(
                self: *@This(),
                from: *const [3]Real,
                to: *const [3]Real,
                color: Color,
            ) callconv(.C) void {
                self.state.lock.lock();
                defer self.state.lock.unlock();

                self.state.lines.append(.{
                    .pos = .from_buf(from),
                    .color = .{
                        .x = @as(f32, @floatFromInt(color.comp.r)) / 255.0,
                        .y = @as(f32, @floatFromInt(color.comp.g)) / 255.0,
                        .z = @as(f32, @floatFromInt(color.comp.b)) / 255.0,
                        .w = @as(f32, @floatFromInt(color.comp.a)) / 255.0,
                    },
                }) catch @panic("oom");
                self.state.lines.append(.{
                    .pos = .from_buf(to),
                    .color = .{
                        .x = @as(f32, @floatFromInt(color.comp.r)) / 255.0,
                        .y = @as(f32, @floatFromInt(color.comp.g)) / 255.0,
                        .z = @as(f32, @floatFromInt(color.comp.b)) / 255.0,
                        .w = @as(f32, @floatFromInt(color.comp.a)) / 255.0,
                    },
                }) catch @panic("oom");
            }
            fn _drawTriangle(
                self: *@This(),
                v1: *const [3]Real,
                v2: *const [3]Real,
                v3: *const [3]Real,
                color: Color,
            ) callconv(.C) void {
                _ = self;
                _ = v1;
                _ = v2;
                _ = v3;
                _ = color;
            }
            fn _createTriangleBatch(
                self: *@This(),
                triangles: [*]Triangle,
                triangle_count: u32,
            ) callconv(.C) *anyopaque {
                _ = self;
                _ = triangles;
                _ = triangle_count;
                @panic("oof");
            }
            fn _createTriangleBatchIndexed(
                self: *@This(),
                vertices: [*]Vertex,
                vertex_count: u32,
                indices: [*]u32,
                index_count: u32,
            ) callconv(.C) *anyopaque {
                _ = self;
                _ = vertices;
                _ = vertex_count;
                _ = indices;
                _ = index_count;
                @panic("oof");
            }
            fn _drawGeometry(
                self: *@This(),
                model_matrix: *const RMatrix,
                world_space_bound: *const AABox,
                lod_scale_sq: f32,
                color: Color,
                geometry: *const Geometry,
                cull_mode: CullMode,
                cast_shadow: CastShadow,
                draw_mode: DrawMode,
            ) callconv(.C) void {
                _ = self;
                _ = model_matrix;
                _ = world_space_bound;
                _ = lod_scale_sq;
                _ = color;
                _ = geometry;
                _ = cull_mode;
                _ = cast_shadow;
                _ = draw_mode;
            }
            fn _drawText3D(
                self: *@This(),
                positions: *const [3]Real,
                string: [*:0]const u8,
                color: Color,
                height: f32,
            ) callconv(.C) void {
                _ = self;
                _ = positions;
                _ = string;
                _ = color;
                _ = height;
            }
        };
    };
};

pub const AudioPlayer = Engine.Audio.Stream(.output, struct {
    const c = Engine.c;

    // owned by ResourceManager.CpuResources
    samples: []assets_mod.Wav,
    frame_count: u64 = 0,
    offsets: []Offset = &.{},

    playing: struct {
        // only swap in callback.
        // self.samples accessed only in callback.
        // other thread can fill only when not fused.
        // callback can only swap when fused

        swap_fuse: utils_mod.Fuse = .{},

        // NOTE: only audio thread may lock
        lock: utils_mod.Fuse = .{},

        buf1: Buf,
        buf2: Buf,

        const Buf = struct {
            samples: Samples,
            volume: f32 = 1.0,
            speed: f32 = 1.0,
        };

        fn fused_swap(self: *@This()) void {
            _ = self.lock.fuse();
            defer _ = self.lock.unfuse();

            if (self.swap_fuse.unfuse()) {
                std.mem.swap(Buf, &self.buf1, &self.buf2);
            }
        }
    },

    const Offset = struct {
        // offset in sample buf starting from 0
        offset: u32,

        // for interpolation
        t: f32,
    };

    pub fn init(samples: []assets_mod.Wav) !@This() {
        return @This(){
            .samples = samples,
            .playing = .{
                .buf1 = .{
                    .samples = try Samples.initCapacity(allocator.*, 200),
                },
                .buf2 = .{
                    .samples = try Samples.initCapacity(allocator.*, 200),
                },
            },
        };
    }

    pub fn deinit(self: *@This()) void {
        _ = self.playing.lock.fuse();
        defer _ = self.playing.lock.unfuse();

        allocator.free(self.offsets);
        self.playing.buf1.samples.deinit();
        self.playing.buf2.samples.deinit();
    }

    pub fn callback(
        self: *AudioPlayer.CallbackContext,
        output: [][2]f32,
        timeinfo: *c.PaStreamCallbackTimeInfo,
        flags: c.PaStreamCallbackFlags,
    ) !void {
        _ = flags;
        _ = timeinfo;

        if (output.len > self.ctx.offsets.len) {
            allocator.free(self.ctx.offsets);
            self.ctx.offsets = try allocator.alloc(Offset, output.len);
        }

        // stretch audio: mix(buf[floor(index * speed)], buf[ceil(index * speed)], fract(index * speed))
        const offsets = self.ctx.offsets[0..output.len];
        for (0..output.len) |i| {
            const t = cast(f32, i) * self.ctx.playing.buf1.speed;
            offsets[i] = .{ .offset = cast(u32, t), .t = t - @floor(t) };
        }
        defer self.ctx.frame_count += offsets[offsets.len - 1].offset;

        @memset(output, [2]f32{ 0, 0 });

        self.ctx.playing.fused_swap();
        for (self.ctx.playing.buf1.samples.items) |*ps| {
            _ = ps.fill(
                self.ctx.playing.buf1.volume,
                self.ctx.playing.buf1.speed,
                self.ctx.frame_count,
                offsets,
                self.ctx.samples,
                output,
            );
        }
    }

    pub const Samples = std.ArrayList(PlayingSample);
    pub const PlayingSample = struct {
        handle: ResourceManager.AudioHandle,
        start_frame: u64,

        // relative position of the sound. (listener +z fwd, +x right, -y up)
        pos: Vec3,
        volume: f32,

        // the float values supplied to audio apis is the instantaneous amplitude of the wave
        // power is proportional to the square of amplitude
        // intensity is power carried by wave per unit area (perp to the area)
        // percieved loudness is logarithmic in power
        // delta dB = 10*log10(p2/p1)
        // "twice as loud" is a diff of 10dB
        //
        // with distance - sound's intensity decreases (as area increases)
        // so twice as far means a quarter the intensity
        // which means quarter the power (per unit area (which is what we end up hearing ig. cuz the ear drums are constant in size))
        // which means we just divide the amplitude by 2 to account for the distance

        pub fn fill(
            self: *@This(),
            volume: f32,
            speed: f32,
            frame_count: u64,
            offsets: []Offset,
            samples: []assets_mod.Wav,
            output: [][2]f32,
        ) bool {
            const min = 1.0;
            const max = 100.0;
            const original_dist = self.pos.length();
            const dist = @min(max, @max(min, original_dist));

            const att = volume * self.volume / dist;

            const right_dot = 0.5 * if (original_dist > 0.01) self.pos.x / original_dist else 0.0;
            var left = 0.5 - right_dot;
            var right = 0.5 + right_dot;

            // rescale in [0, 1]
            left *= 2;
            right *= 2;

            // 20% audio always leaks into the other ear
            left = left * 0.8 + 0.2;
            right = right * 0.8 + 0.2;

            // if close, we leak more
            left = @min(1.0, left + 1.0 / @max(0.01, original_dist));
            right = @min(1.0, right + 1.0 / @max(0.01, original_dist));

            std.debug.assert(output.len == offsets.len);

            const sample = samples[self.handle.index].data;
            const base = frame_count - self.start_frame;
            for (output, 0..) |*oframe, i| {
                const index = base + offsets[i].offset;
                const t = offsets[i].t;

                if (sample.len <= index) {
                    return true;
                }

                const curr_frame = sample[index];
                if (speed >= 1.0 or index + 1 >= sample.len) {
                    oframe[0] = std.math.clamp(oframe[0] + curr_frame[0] * left * att, -1, 1);
                    oframe[1] = std.math.clamp(oframe[1] + curr_frame[1] * right * att, -1, 1);
                    continue;
                }

                const next_frame = sample[index + 1];
                const frame_l = std.math.lerp(curr_frame[0], next_frame[0], t);
                const frame_r = std.math.lerp(curr_frame[1], next_frame[1], t);

                oframe[0] = std.math.clamp(oframe[0] + frame_l * left * att, -1, 1);
                oframe[1] = std.math.clamp(oframe[1] + frame_r * right * att, -1, 1);
            }

            return false;
        }
    };
});

pub const AudioRecorder = Engine.Audio.Stream(.input, struct {
    const c = Engine.c;

    recorded: utils_mod.Channel([256]f32),

    pub fn callback(
        self: *AudioRecorder.CallbackContext,
        input: []const f32,
        timeinfo: *c.PaStreamCallbackTimeInfo,
        flags: c.PaStreamCallbackFlags,
    ) !void {
        _ = flags;
        _ = timeinfo;
        _ = self;

        var buf: [256]f32 = undefined;
        @memcpy(&buf, input);
        // try self.ctx.recorded.send(buf);
    }

    pub fn deinit(self: *@This()) void {
        self.recorded.deinit();
    }
});

pub const World = struct {
    ecs: EntityComponentStore,
    phy: Jphysics,

    pub fn init(up: Vec3) !@This() {
        var self = @This(){
            .phy = undefined,
            .ecs = undefined,
        };
        self.phy = try Jphysics.init(up);
        self.ecs = try EntityComponentStore.init(@ptrCast(&self));
        errdefer self.deinit();

        return self;
    }

    pub fn deinit(self: *@This()) void {
        self.ecs.deinit(@ptrCast(self));
        self.phy.deinit();
    }
};

pub const Components = struct {
    pub const Name = struct {
        name: []const u8,

        pub fn from(name: []const u8) !@This() {
            return .{
                .name = try allocator.dupe(u8, name),
            };
        }

        pub fn deinit(self: *@This()) void {
            std.debug.print("deleting entity: {s}\n", .{self.name});

            allocator.free(self.name);
        }
    };

    pub const Transform = struct {
        pos: Vec3 = .{},
        scale: Vec3 = Vec3.splat(1.0),
        rotation: Vec4 = Vec4.quat_identity_rot(),

        pub fn mat4(self: *const @This()) math.Mat4x4 {
            const translate = math.Mat4x4.translation_mat(self.pos);
            const rot = math.Mat4x4.rot_mat_from_quat(self.rotation);
            const scale = math.Mat4x4.scaling_mat(self.scale);
            return translate.mul_mat(rot).mul_mat(scale);
        }

        pub inline fn from_asset_transform(t: anytype) @This() {
            return .{
                .pos = t.translation,
                .rotation = t.rotation,
                .scale = t.scale,
            };
        }

        pub inline fn transform_pos(self: *const @This(), pos: Vec3) Vec3 {
            return self.pos.add(self.rotation.rotate_vector(pos.mul(self.scale)));
        }

        pub inline fn inverse_transform_pos(self: *const @This(), pos: Vec3) Vec3 {
            return self.rotation.inverse_rotate_vector(pos.sub(self.pos)).mul(.{
                .x = 1.0 / self.scale.x,
                .y = 1.0 / self.scale.y,
                .z = 1.0 / self.scale.z,
            });
        }

        pub inline fn transform_direction(self: *const @This(), dir: Vec3) Vec3 {
            return self.rotation.rotate_vector(dir.scale(self.scale));
        }

        pub inline fn apply_global(self: *const @This(), transform: @This()) @This() {
            return .{
                .pos = transform.pos.add(transform.rotation.rotate_vector(self.pos)),
                .rotation = transform.rotation.quat_local_rot(self.rotation),
                .scale = transform.scale.mul(self.scale),
            };
        }

        pub inline fn apply_local(self: *const @This(), transform: @This()) @This() {
            return transform.apply_global(self.*);
        }

        pub fn lerp(self: *const @This(), new: *const @This(), t: f32) Transform {
            return .{
                .pos = self.pos.mix(new.pos, t),
                .rotation = self.rotation.mix(new.rotation, t),
                .scale = self.scale.mix(new.scale, t),
            };
        }
    };

    pub const LocalTransform = struct {
        transform: Transform = .{},

        pub fn set(self: *@This(), t: Transform) void {
            self.transform = t;
        }
    };

    // maybe just put 2 transforms (last and current) inside rigidbody and store the interpolated transform in the
    // Transform component. that way it's easy to just use the interpolated transform for everything else.
    // but maybe it's better to keep it framerate independent?
    pub const LastTransform = struct {
        transform: Transform = .{},
    };

    pub const GlobalTransform = struct {
        transform: Transform = .{},

        pub fn set(self: *@This(), t: Transform) void {
            self.transform = t;
        }
    };

    pub const Node = struct {
        parent: ?C.Entity,
        children: std.ArrayList(C.Entity),

        pub fn deinit(self: *@This()) void {
            self.children.deinit();
        }
    };

    pub const Controller = struct {
        pitch: f32 = 0,
        yaw: f32 = 0,
        speed: f32 = 1.0,
        sensitivity: f32 = 1.0,
        sensitivity_scale: f32 = 0.001,
        did_rotate: bool = false,
        did_move: bool = false,
    };

    pub const Shooter = struct {
        audio: ResourceManager.AudioHandle,
        ticker: utils_mod.Ticker,
        hold: bool,

        pub fn try_shoot(self: *@This(), action: Engine.Window.Action, time: u64) bool {
            return ((action.pressed() and self.hold) or (action.just_pressed() and !self.hold)) and self.ticker.tick(time);
        }
    };

    pub const Sound = struct {
        start_frame: u64,
        audio: ResourceManager.AudioHandle,
        volume: f32 = 1.0,
    };

    pub const StaticSound = struct {
        audio: ResourceManager.AudioHandle,
        pos: Vec3,
        start_frame: u64,
        volume: f32 = 1.0,
    };

    // TODO: better despawn strategy.
    // - if despawn_time is reached - we switch state and start checking for any components like dying animation.
    // - if all dying components are clear - we despawn

    pub const TimeDespawn = struct {
        despawn_time: f32,
        state: EntityState,

        pub const EntityState = enum {
            alive,
            dying,
            dead,
        };
    };

    pub const PlayerId = struct {
        id: u8,
        conn: u32,
    };

    pub const StaticMesh = struct {
        mesh: ResourceManager.MeshHandle,
        material: ResourceManager.MaterialHandle,
    };

    pub const AnimatedMesh = struct {
        mesh: ResourceManager.MeshHandle,
        material: ResourceManager.MaterialHandle,
        armature: ResourceManager.ArmatureHandle,

        animation_index: u32,

        // relative to the start of this animation
        // += dt each frame
        time: f32 = 0,
        bones: []math.Mat4x4 = &[_]math.Mat4x4{},

        // animation indices for
        indices: []AnimationIndices = &[_]AnimationIndices{},

        pub const AnimationIndices = struct {
            translation: u32,
            rotation: u32,
            scale: u32,
        };

        pub fn deinit(self: *@This()) void {
            allocator.free(self.bones);
            allocator.free(self.indices);
        }
    };

    pub const BatchedRender = struct {
        batch: ?ResourceManager.BatchHandle = null,
    };
};
