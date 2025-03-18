const std = @import("std");

const math = @import("math.zig");
const Vec4 = math.Vec4;
const Vec3 = math.Vec3;

const utils_mod = @import("utils.zig");

const Engine = @import("engine.zig");

const resources_mod = @import("resources.zig");
const ResourceManager = resources_mod.ResourceManager;

const ecs_mod = @import("ecs.zig");
const Entity = ecs_mod.Entity;
const EntityComponentStore = ecs_mod.EntityComponentStore;

const main = @import("main.zig");
const allocator = main.allocator;

pub const Zphysics = struct {
    pub const jolt = @import("jolt/jolt.zig");

    phy: *jolt.PhysicsSystem,
    global_state: ?jolt.GlobalState = null,
    state: *State,

    const State = struct {
        broadphase_layer_interface: Impl.MyBroadphaseLayerInterface = .init(),
        obj_vs_broadphase_layer_interface: Impl.MyObjectVsBroadPhaseLayerFilter = .{},
        obj_layer_pair_filter: Impl.MyObjectLayerPairFilter = .{},
    };

    pub const BodyId = struct {
        id: jolt.BodyId,
    };
    pub const CharacterBody = struct {
        character: *jolt.CharacterVirtual,
        force: Vec3 = .{},
    };

    pub fn init() !@This() {
        try jolt.init(allocator.*, .{});
        errdefer jolt.deinit();

        var state = try allocator.create(State);
        errdefer allocator.destroy(state);
        state.* = .{};

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

        return .{ .phy = ps, .state = state };
    }

    pub fn deinit(self: *@This()) void {
        self.phy.destroy();
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
            .linear_velocity = settings.velocity.withw(0).to_buf(),
            .angular_velocity = settings.angular_velocity.withw(0).to_buf(),
            .rotation = settings.rotation.to_buf(),
            .motion_type = settings.motion_type,
            .motion_quality = settings.motion_quality,
            .friction = settings.friction,
            .object_layer = Impl.object_layers.moving,
        };

        switch (settings.shape) {
            .sphere => |s| {
                const shape = try jolt.SphereShapeSettings.create(s.radius);
                body_settings.shape = try shape.createShape();
            },
            .box => |s| {
                const shape = try jolt.BoxShapeSettings.create(s.size.to_buf());
                body_settings.shape = try shape.createShape();
            },
            .capsule => |s| {
                const shape = try jolt.CapsuleShapeSettings.create(s.half_height, s.radius);
                body_settings.shape = try shape.createShape();
            },
        }

        return .{ .id = try bodyi.createAndAddBody(body_settings, .activate) };
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
        defer shape.release();

        const rotated = try jolt.DecoratedShapeSettings.createRotatedTranslated(shape.asShapeSettings(), Vec4.quat_identity_rot().to_buf(), (Vec3{ .y = -v.half_height - v.radius }).to_buf());
        settings.base.shape = try rotated.createShape();
        settings.inner_body_shape = try shape.createShape();
        settings.inner_body_layer = Impl.object_layers.moving;

        const character = try jolt.CharacterVirtual.create(settings, v.pos.to_buf(), v.rot.to_buf(), self.phy);

        return .{ .character = character };
    }

    pub fn get_transform(self: *@This(), bid: BodyId) Components.Transform {
        const pos = self.phy.getBodyInterface().getPosition(bid.id);
        const rot = self.phy.getBodyInterface().getRotation(bid.id);
        return .{
            .pos = .{
                .x = pos[0],
                .y = pos[1],
                .z = pos[2],
            },
            .rotation = .{
                .x = rot[0],
                .y = rot[1],
                .z = rot[2],
                .w = rot[3],
            },
        };
    }

    pub fn apply_force(self: *@This(), bid: BodyId, force: Vec3) void {
        self.phy.getBodyInterfaceMut().addForce(bid.id, force.to_buf());
    }

    pub fn set_rotation(self: *@This(), bid: BodyId, rot: Vec4) void {
        self.phy.getBodyInterfaceMut().setRotation(bid.id, rot.to_buf());
    }

    pub const BodySettings = struct {
        shape: ShapeSettings,
        motion_type: jolt.MotionType = .dynamic,
        motion_quality: jolt.MotionQuality = .discrete,
        pos: Vec3 = .{},
        rotation: Vec4 = Vec4.quat_identity_rot(),
        velocity: Vec3 = .{},
        angular_velocity: Vec3 = .{},
        friction: f32 = 0,
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
    };
};

pub const World = struct {
    ecs: EntityComponentStore,
    phy: Zphysics,

    pub fn init() !@This() {
        var self = @This(){
            .ecs = try EntityComponentStore.init(),
            .phy = try Zphysics.init(),
        };
        errdefer self.deinit();

        _ = try self.ecs.register([]const u8);
        _ = try self.ecs.register(math.Camera);
        _ = try self.ecs.register(Components.Controller);
        _ = try self.ecs.register(Components.Shooter);
        _ = try self.ecs.register(Components.Sound);
        _ = try self.ecs.register(Components.StaticSound);
        _ = try self.ecs.register(Components.Transform);
        _ = try self.ecs.register(Components.LastTransform);
        _ = try self.ecs.register(Components.TimeDespawn);
        _ = try self.ecs.register(Components.PlayerId);
        _ = try self.ecs.register(Components.StaticRender);
        _ = try self.ecs.register(Components.AnimatedRender);
        _ = try self.ecs.register(Zphysics.BodyId);
        _ = try self.ecs.register(Zphysics.CharacterBody);

        return self;
    }

    pub fn deinit(self: *@This()) void {
        self.ecs.deinit();
        self.phy.deinit();
    }

    pub fn step(self: *@This(), sim_time: f32, steps: u32) !void {
        try self.phy.update(sim_time, steps);

        var it = try self.ecs.iterator(struct { t: Components.Transform, bid: Zphysics.BodyId });
        while (it.next()) |e| {
            const t = self.phy.get_transform(e.bid.*);
            e.t.pos = t.pos;
            e.t.rotation = t.rotation;
        }

        var player_it = try self.ecs.iterator(struct { t: Components.Transform, char: Zphysics.CharacterBody });
        while (player_it.next()) |e| {
            const char: *Zphysics.CharacterBody = e.char;
            char.force = .{};
            const pos = Vec3.from_buf(char.character.getPosition());
            const rot = Vec4.from_buf(char.character.getRotation());
            e.t.pos = pos.withw(0);
            e.t.rotation = rot;
        }
    }
};

pub const Components = struct {
    pub const Transform = struct {
        pos: Vec4 = .{},
        scale: Vec4 = Vec4.splat3(1.0),
        rotation: Vec4 = Vec4.quat_identity_rot(),

        pub fn mat4(self: *const @This()) math.Mat4x4 {
            const translate = math.Mat4x4.translation_mat(self.pos);
            const rot = math.Mat4x4.rot_mat_from_quat(self.rotation);
            const scale = math.Mat4x4.scaling_mat(self.scale);
            return translate.mul_mat(rot).mul_mat(scale);
        }

        pub fn transform_pos(self: *const @This(), pos: Vec4) Vec4 {
            return self.pos.add(self.rotation.rotate_vector(pos.mul(self.scale)));
        }

        pub fn inverse_transform_pos(self: *const @This(), pos: Vec4) Vec4 {
            return self.rotation.inverse_rotate_vector(pos.sub(self.pos)).mul(.{
                .x = 1.0 / self.scale.x,
                .y = 1.0 / self.scale.y,
                .z = 1.0 / self.scale.z,
            });
        }

        pub fn transform_direction(self: *const @This(), dir: Vec4) Vec4 {
            return self.rotation.rotate_vector(dir.scale(self.scale));
        }
    };

    // maybe just put 2 transforms (last and current) inside rigidbody and store the interpolated transform in the
    // Transform component. that way it's easy to just use the interpolated transform for everything else.
    // but maybe it's better to keep it framerate independent?
    pub const LastTransform = struct {
        t: Transform = .{},

        pub fn lerp(self: *const @This(), new: *const Transform, t: f32) Transform {
            return .{
                .pos = self.t.pos.mix(new.pos, t),
                .rotation = self.t.rotation.mix(new.rotation, t),
                .scale = self.t.scale.mix(new.scale, t),
            };
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

        pub fn try_shoot(self: *@This(), action: Engine.Window.Action) bool {
            return ((action.pressed() and self.hold) or (action.just_pressed() and !self.hold)) and self.ticker.tick();
        }
    };

    pub const Sound = struct {
        start_frame: u64,
        audio: ResourceManager.AudioHandle,
        volume: f32 = 1.0,
    };

    pub const StaticSound = struct {
        audio: ResourceManager.AudioHandle,
        pos: Vec4,
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
        addr: ?std.net.Address = null,
    };

    pub const StaticRender = struct {
        mesh: ResourceManager.MeshResourceHandle,
    };

    pub const AnimatedRender = struct {
        model: ResourceManager.ModelHandle,

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
};
