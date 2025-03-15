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
    pub const c = @cImport({
        @cInclude("jolt.h");
    });

    phy: *anyopaque,
    alloc: *Alloc,

    pub const BodyId = struct { bid: u32 };

    const Alloc = struct {
        var state: *@This() = undefined;

        mutex: std.Thread.Mutex = .{},
        allocations: Allocations,

        const Allocations = std.AutoHashMap(usize, struct { size: u32, alignment: u32 });

        const alloc_alignment = 16;

        fn init() !*@This() {
            const self = try allocator.create(@This());
            errdefer allocator.destroy(self);
            state = self;

            self.mutex = .{};
            self.allocations = Allocations.init(allocator.*);
            return self;
        }

        fn deinit(self: *@This()) void {
            {
                self.mutex.lock();
                defer self.mutex.unlock();
                self.allocations.deinit();
            }
            allocator.destroy(self);
        }

        fn reload(self: *@This()) void {
            state = self;
        }

        fn allocate(size: usize) callconv(.C) ?*anyopaque {
            state.mutex.lock();
            defer state.mutex.unlock();

            const mem = allocator.allocWithOptions(u8, size, alloc_alignment, null) catch @panic("OOM");
            state.allocations.put(@intFromPtr(mem.ptr), .{ .size = @intCast(mem.len), .alignment = alloc_alignment }) catch @panic("OOM");
            return mem.ptr;
        }

        fn realloc(maybe_ptr: ?*anyopaque, reported_old_size: usize, new_size: usize) callconv(.C) ?*anyopaque {
            state.mutex.lock();
            defer state.mutex.unlock();

            const old_size = if (maybe_ptr != null) reported_old_size else 0;

            const old_mem = if (old_size > 0)
                @as([*]align(alloc_alignment) u8, @ptrCast(@alignCast(maybe_ptr)))[0..old_size]
            else
                @as([*]align(alloc_alignment) u8, undefined)[0..0];

            const mem = allocator.realloc(old_mem, new_size) catch @panic("OOM");
            _ = state.allocations.remove(@intFromPtr(mem.ptr));
            state.allocations.put(@intFromPtr(mem.ptr), .{ .size = @intCast(mem.len), .alignment = alloc_alignment }) catch @panic("OOM");
            return mem.ptr;
        }

        fn aligned_alloc(size: usize, alignment: usize) callconv(.C) ?*anyopaque {
            state.mutex.lock();
            defer state.mutex.unlock();

            const mem = allocator.rawAlloc(
                size,
                std.mem.Alignment.fromByteUnits(alignment),
                @returnAddress(),
            ) orelse @panic("OOM");
            state.allocations.put(@intFromPtr(mem), .{ .size = @intCast(size), .alignment = @intCast(alignment) }) catch @panic("OOM");
            return mem;
        }

        fn free(maybe_ptr: ?*anyopaque) callconv(.C) void {
            state.mutex.lock();
            defer state.mutex.unlock();

            const ptr = maybe_ptr orelse return;
            const allocation = state.allocations.fetchRemove(@intFromPtr(ptr)) orelse return;
            const size = allocation.value.size;
            const mem = @as([*]u8, @ptrCast(@alignCast(ptr)))[0..size];
            allocator.rawFree(mem, std.mem.Alignment.fromByteUnits(allocation.value.alignment), @returnAddress());
        }
    };

    pub fn init() !@This() {
        return .{
            .alloc = try Alloc.init(),
            .phy = c.physics_create(.{
                .allocfn = &Alloc.allocate,
                .reallocfn = &Alloc.realloc,
                .freefn = &Alloc.free,
                .aligned_allocfn = &Alloc.aligned_alloc,
                .aligned_freefn = &Alloc.free,
            }) orelse return error.CouldNotInitializeJolt,
        };
    }

    pub fn deinit(self: *@This()) void {
        c.physics_delete(self.phy);
        self.alloc.deinit();
    }

    pub fn post_reload(self: *@This()) void {
        self.alloc.reload();
        c.physics_post_reload(self.phy, .{
            .allocfn = &Alloc.allocate,
            .reallocfn = &Alloc.realloc,
            .freefn = &Alloc.free,
            .aligned_allocfn = &Alloc.aligned_alloc,
            .aligned_freefn = &Alloc.free,
        });
    }

    pub fn start(self: *@This()) !void {
        c.physics_start(self.phy);
    }

    pub fn optimize(self: *@This()) void {
        c.physics_optimize(self.phy);
    }

    pub fn update(self: *@This(), sim_time: f32, steps: u32) void {
        c.physics_update(self.phy, sim_time, steps);
    }

    pub fn add_body(self: *@This(), settings: BodySettings) BodyId {
        const bid = c.physics_add_body(self.phy, settings.type_struct());
        return .{ .bid = bid };
    }

    pub fn get_transform(self: *@This(), bid: BodyId) Components.Transform {
        const transform = c.physics_get_transform(self.phy, bid.bid);
        return .{
            .pos = .{
                .x = transform.pos.x,
                .y = transform.pos.y,
                .z = transform.pos.z,
            },
            .rotation = .{
                .x = transform.rot.x,
                .y = transform.rot.y,
                .z = transform.rot.z,
                .w = transform.rot.w,
            },
        };
    }

    pub fn apply_force(self: *@This(), bid: BodyId, force: Vec3) void {
        c.physics_add_force(self.phy, bid.bid, helpers.to.vec3(force));
    }

    pub fn set_rotation(self: *@This(), bid: BodyId, rot: Vec4) void {
        c.physics_set_rotation(self.phy, bid.bid, helpers.to.vec4(rot));
    }

    const helpers = struct {
        const to = struct {
            fn vec3(t: anytype) c.vec3 {
                return .{ .x = t.x, .y = t.y, .z = t.z };
            }
            fn vec4(t: anytype) c.vec4 {
                return .{ .x = t.x, .y = t.y, .z = t.z, .w = t.w };
            }
        };
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

        fn type_enum(self: *const @This()) c.ShapeType {
            return switch (self.*) {
                .sphere => c.SHAPE_SPHERE,
                .box => c.SHAPE_BOX,
                .capsule => c.SHAPE_CAPSULE,
            };
        }
        fn type_union(self: @This()) c.ZShapeSettings {
            return switch (self) {
                .sphere => |s| .{ .sphere = .{ .radius = s.radius } },
                .box => |s| .{ .box = .{ .size = helpers.to.vec3(s.size) } },
                .capsule => |s| .{ .capsule = .{ .half_height = s.half_height, .radius = s.radius } },
            };
        }
    };
    pub const MotionType = enum {
        static,
        kinematic,
        dynamic,

        fn type_enum(self: @This()) c.MotionType {
            return switch (self) {
                .static => c.MOTION_STATIC,
                .kinematic => c.MOTION_KINEMATIC,
                .dynamic => c.MOTION_DYNAMIC,
            };
        }
    };

    pub const MotionQuality = enum {
        discrete,
        linear_cast,

        fn type_enum(self: @This()) c.MotionType {
            return switch (self) {
                .discrete => c.MOTION_DISCRETE,
                .linear_cast => c.MOTION_LINEAR_CAST,
            };
        }
    };
    pub const BodySettings = struct {
        shape: ShapeSettings,
        motion_type: MotionType = .dynamic,
        motion_quality: MotionQuality = .discrete,
        pos: Vec3 = .{},
        rotation: Vec4 = Vec4.quat_identity_rot(),
        velocity: Vec3 = .{},
        angular_velocity: Vec3 = .{},
        friction: f32 = 0,

        fn type_struct(self: *const @This()) c.ZBodySettings {
            return .{
                .shape_type = self.shape.type_enum(),
                .shape = self.shape.type_union(),
                .motion_type = self.motion_type.type_enum(),
                .motion_quality = self.motion_quality.type_enum(),
                .pos = helpers.to.vec3(self.pos),
                .rotation = helpers.to.vec4(self.rotation),
                .velocity = helpers.to.vec3(self.velocity),
                .angular_velocity = helpers.to.vec3(self.angular_velocity),
                .friction = self.friction,
            };
        }
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

        try self.phy.start();

        _ = try self.ecs.register([]const u8);
        _ = try self.ecs.register(math.Camera);
        _ = try self.ecs.register(Components.Controller);
        _ = try self.ecs.register(Components.Shooter);
        _ = try self.ecs.register(Components.Sound);
        _ = try self.ecs.register(Components.StaticSound);
        _ = try self.ecs.register(Components.Transform);
        _ = try self.ecs.register(Components.LastTransform);
        _ = try self.ecs.register(Components.Collider);
        _ = try self.ecs.register(Components.CableAttached);
        _ = try self.ecs.register(Components.RodAttached);
        _ = try self.ecs.register(Components.TimeDespawn);
        _ = try self.ecs.register(Components.PlayerId);
        _ = try self.ecs.register(Components.StaticRender);
        _ = try self.ecs.register(Components.AnimatedRender);
        _ = try self.ecs.register(Zphysics.BodyId);

        return self;
    }

    pub fn deinit(self: *@This()) void {
        self.ecs.deinit();
        self.phy.deinit();
    }

    pub fn step(self: *@This(), sim_time: f32, steps: u32) !void {
        self.phy.update(sim_time, steps);

        var it = try self.ecs.iterator(struct { t: Components.Transform, bid: Zphysics.BodyId });
        while (it.next()) |e| {
            const t = self.phy.get_transform(e.bid.*);
            e.t.pos = t.pos;
            e.t.rotation = t.rotation;
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

    pub const Collider = union(enum) {
        sphere: Sphere,
        plane: Plane,
        cuboid: Cuboid,

        pub const Sphere = struct {
            center: Vec4 = .{},
            radius: f32,
        };
        pub const Plane = struct {
            normal: Vec4 = .{ .y = 1 },
            offset: f32 = 0,
        };
        pub const Cuboid = struct {
            center: Vec4 = .{},
            half_extent: Vec4,
        };
    };

    pub const CableAttached = struct {
        length: f32,
        restitution: f32,
        a: Entity,
        b: Entity,
    };

    pub const RodAttached = struct {
        length: f32,
        a: Entity,
        b: Entity,
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
