const std = @import("std");

const math = @import("math.zig");
const Vec4 = math.Vec4;

const utils_mod = @import("utils.zig");
const TypeId = utils_mod.TypeId;

const Engine = @import("engine.zig");

const app = @import("app.zig");
const AppState = app.AppState;

const resources_mod = @import("resources.zig");
const ResourceManager = resources_mod.ResourceManager;

const ecs_mod = @import("ecs.zig");
const Type = ecs_mod.Type;
const Entity = ecs_mod.Entity;
const EntityComponentStore = ecs_mod.EntityComponentStore;

const main = @import("main.zig");
const allocator = main.allocator;

pub const World = struct {
    ecs: EntityComponentStore,
    collider: EntityCollider,

    pub fn init() !@This() {
        var self = @This(){
            .ecs = try EntityComponentStore.init(),
            .collider = EntityCollider.init(),
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
        _ = try self.ecs.register(Components.Rigidbody);
        _ = try self.ecs.register(Components.Collider);
        _ = try self.ecs.register(Components.CableAttached);
        _ = try self.ecs.register(Components.RodAttached);
        _ = try self.ecs.register(Components.TimeDespawn);
        _ = try self.ecs.register(Components.PlayerId);
        _ = try self.ecs.register(Components.StaticRender);
        _ = try self.ecs.register(Components.AnimatedRender);

        return self;
    }

    pub fn deinit(self: *@This()) void {
        self.ecs.deinit();
        self.collider.deinit();
    }

    pub fn step(self: *@This(), delta: f32) !void {
        {
            var it = try self.ecs.iterator(struct { r: Components.Rigidbody });
            while (it.next()) |e| {
                if (e.r.flags.pinned) continue;

                // damping
                const s = e.r.vel.length();
                if (s > 0) {
                    e.r.force = e.r.force.add(e.r.vel.normalize3D().scale(-1).scale(s * 0.0001 + s * s * 0.001));
                }

                e.r.vel = e.r.vel.add(e.r.force.scale(e.r.invmass).scale(delta));

                const r = e.r.angular_vel.length();
                if (r > 0) {
                    e.r.torque = e.r.torque.add(e.r.angular_vel.normalize3D().scale(-1).scale(0.0001));
                }

                e.r.angular_vel = e.r.angular_vel.add(e.r.invinertia.mul_vec4(e.r.torque).scale(delta));
            }
        }

        defer self.collider.reset();
        {
            var it1 = try self.ecs.iterator(EntityCollider.RigidEntity);
            var it2 = it1;
            while (it1.next()) |a| {
                it2.reset();
                while (it2.next()) |b| {
                    if (std.meta.eql(a.id, b.id)) {
                        break;
                    }
                    const collision = try self.collider.collide(a, b, .{}) orelse continue;

                    try self.collider.collisions.append(.{ .a = a, .b = b, .collision = collision });
                }
            }
        }
        // {
        //     var it = try self.ecs.iterator(struct { cable: Components.CableAttached });
        //     while (it.next()) |e| {
        //         const a = try self.ecs.get(e.cable.a, EntityCollider.RigidEntity);
        //         const b = try self.ecs.get(e.cable.b, EntityCollider.RigidEntity);

        //         const ba = a.transform.pos.sub(b.transform.pos);
        //         if (ba.length() > e.cable.length) {
        //             try self.collider.collisions.append(.{
        //                 .a = a,
        //                 .b = b,
        //                 .collision = .{
        //                     .restitution = e.cable.restitution,
        //                     .depth = 0,
        //                     .normal = ba.normalize3D().scale(1),
        //                 },
        //             });
        //         }
        //     }
        // }
        // {
        //     var it = try self.ecs.iterator(struct { rod: Components.RodAttached });
        //     while (it.next()) |e| {
        //         const a = try self.ecs.get(e.rod.a, EntityCollider.RigidEntity);
        //         const b = try self.ecs.get(e.rod.b, EntityCollider.RigidEntity);

        //         const ba = a.transform.pos.sub(b.transform.pos);
        //         const bal = ba.length();
        //         if (@abs(bal - e.rod.length) > 0.001) {
        //             try self.collider.collisions.append(.{
        //                 .a = a,
        //                 .b = b,
        //                 .collision = .{
        //                     .restitution = 0,
        //                     .depth = 0,
        //                     .normal = ba.normalize3D().scale(if (bal > e.rod.length) 1 else -1),
        //                 },
        //             });
        //         }
        //     }
        // }

        var positions = std.ArrayList(PositionSolver.PositionEntity).init(allocator.*);
        defer positions.deinit();

        for (self.collider.collisions.items) |*e| {
            ImpulseSolver.solve(self.collider.contacts.items, e, delta);

            try positions.append(.{ .entity = e.* });
        }

        for (positions.items) |*e| {
            PositionSolver.solve(self.collider.contacts.items, e);
        }

        for (positions.items) |*e| {
            PositionSolver.apply(e, delta);
        }

        {
            var it = try self.ecs.iterator(struct { r: Components.Rigidbody, t: Components.Transform });
            while (it.next()) |e| {
                if (e.r.flags.pinned) continue;
                e.t.pos = e.t.pos.add(e.r.vel.scale(delta));
            }
        }
    }
};

pub const EntityCollider = struct {
    contacts: Contacts,
    collisions: Collisions,
    last: struct {
        contacts: Contacts,
        collisions: Collisions,
    },

    const Contacts = std.ArrayList(Contact);
    const Collisions = std.ArrayList(CollisionEntity);

    pub const Contact = struct {
        pos: Vec4 = .{},

        // norm(a - b)
        normal: Vec4 = .{},

        // len(a - b)
        depth: f32 = 0,
    };

    const ContactsHandle = packed struct {
        len: u8,
        index: u24,

        fn reserve(self: *@This(), ec: *EntityCollider) !*Contact {
            defer self.len += 1;
            try ec.contacts.append(.{});
            return &ec.contacts.items[self.index + self.len];
        }
    };

    pub const Collision = struct {
        contacts: ContactsHandle,
        restitution: f32,
    };

    pub const CollisionEntity = struct {
        a: RigidEntity.p,
        b: RigidEntity.p,
        collision: Collision,
    };

    const RigidEntity = struct {
        id: Entity,
        transform: Components.Transform,
        rigidbody: Components.Rigidbody,
        collider: Components.Collider,

        const p = Type.pointer(@This());
    };

    pub fn init() @This() {
        return .{
            .contacts = Contacts.init(allocator.*),
            .collisions = Collisions.init(allocator.*),
            .last = .{
                .contacts = Contacts.init(allocator.*),
                .collisions = Collisions.init(allocator.*),
            },
        };
    }

    pub fn deinit(self: *@This()) void {
        self.contacts.deinit();
        self.collisions.deinit();
        self.last.contacts.deinit();
        self.last.collisions.deinit();
    }

    pub fn reset(self: *@This()) void {
        std.mem.swap(Contacts, &self.contacts, &self.last.contacts);
        std.mem.swap(Collisions, &self.collisions, &self.last.collisions);

        self.contacts.clearRetainingCapacity();
        self.collisions.clearRetainingCapacity();
    }

    fn contact_handle(self: *@This()) ContactsHandle {
        return .{ .index = @intCast(self.contacts.items.len), .len = 0 };
    }

    pub fn collide(self: *@This(), a: RigidEntity.p, b: RigidEntity.p, args: struct { flip: bool = false }) !?Collision {
        var handle = self.contact_handle();

        var ac = a.collider.*;
        var bc = b.collider.*;
        switch (ac) {
            .sphere => |*sa| blk: {
                // negative radius only supported for b
                if (sa.radius < 0) break :blk;

                switch (bc) {
                    .sphere => |*sb| {
                        sa.center = sa.center.add(a.transform.pos);
                        sb.center = sb.center.add(b.transform.pos);

                        sa.radius *= a.transform.scale.max3();
                        sb.radius *= b.transform.scale.max3();

                        var ba = sb.center.sub(sa.center);
                        if (sb.radius > 0) {
                            const bal = ba.length();
                            if (bal < 0.001) {
                                ba = .{ .y = 1 };
                            }
                            const dist = bal - sa.radius - sb.radius;

                            if (dist > 0) {
                                return null;
                            }

                            const normal = ba.normalize3D();

                            const contact = try handle.reserve(self);
                            contact.* = .{
                                .pos = sa.center.add(normal.scale(@abs(dist) * 0.5)),
                                .normal = normal,
                                .depth = @abs(dist),
                            };
                            return .{
                                .contacts = handle,
                                .restitution = a.rigidbody.restitution * b.rigidbody.restitution,
                            };
                        } else {
                            const bal = ba.length();
                            if (bal < 0.001) {
                                ba = .{ .y = 1 };
                            }
                            const dist = -sb.radius - bal - sa.radius;

                            if (dist > 0) {
                                return null;
                            }

                            const normal = ba.normalize3D().scale(-1);
                            const contact = try handle.reserve(self);
                            contact.* = .{
                                .pos = sa.center.add(normal.scale(-(sa.radius - @abs(dist)))),
                                .normal = normal,
                                .depth = @abs(dist),
                            };

                            return .{
                                .contacts = handle,
                                .restitution = a.rigidbody.restitution * b.rigidbody.restitution,
                            };
                        }
                    },
                    .plane => |*pb| {
                        sa.center = sa.center.add(a.transform.pos);
                        sa.radius *= a.transform.scale.max3();

                        pb.normal.w = 0;
                        pb.normal = b.transform.rotation.rotate_vector(pb.normal).normalize3D();

                        const ba = sa.center.sub(b.transform.pos.add(pb.normal.scale(pb.offset)));
                        const dist = ba.dot(pb.normal) - sa.radius;

                        if (dist > 0) {
                            return null;
                        }

                        const normal = pb.normal.scale(-1);
                        const contact = try handle.reserve(self);
                        contact.* = .{
                            .pos = sa.center.add(normal.scale(-(sa.radius - @abs(dist)))),
                            .normal = normal,
                            .depth = @abs(dist),
                        };

                        return .{
                            .contacts = handle,
                            .restitution = a.rigidbody.restitution * b.rigidbody.restitution,
                        };
                    },
                }
            },
            else => {},
        }

        if (args.flip) {
            return null;
        }

        const collision = (try self.collide(b, a, .{ .flip = true })) orelse return null;
        for (self.contacts.items[collision.contacts.index..][0..collision.contacts.len]) |*contact| {
            contact.normal = contact.normal.scale(-1);
        }
        return collision;
    }
};

pub const ImpulseSolver = struct {
    pub fn solve(contacts: []EntityCollider.Contact, entity: *EntityCollider.CollisionEntity, delta: f32) void {
        // - [Inelastic collision - Wikipedia](https://en.wikipedia.org/wiki/Inelastic_collision)

        const rba = entity.a.rigidbody;
        const rbb = entity.b.rigidbody;
        const contact = &contacts[entity.collision.contacts.index];
        const restitution = entity.collision.restitution;

        const vba = rbb.vel.sub(rba.vel);

        // normal velocity
        const nvel = contact.normal.dot(vba);

        if (nvel >= -0.0) return;

        // impulse
        const e = restitution;
        const j = -(1 + e) * nvel / (rba.invmass + rbb.invmass);

        const fvel = (rbb.force.scale(rbb.invmass).sub(rba.force.scale(rba.invmass))).scale(delta).dot(contact.normal);
        if (-fvel + 0.001 >= -nvel) {
            rba.vel = rba.vel.sub(contact.normal.scale(-rba.vel.dot(contact.normal)));
            rbb.vel = rbb.vel.add(contact.normal.scale(-rbb.vel.dot(contact.normal)));
        } else {
            const impulse = contact.normal.scale(j);
            rba.vel = rba.vel.sub(impulse.scale(rba.invmass));
            rbb.vel = rbb.vel.add(impulse.scale(rbb.invmass));
        }

        // friction
        var tangent = vba.sub(contact.normal.scale(nvel));
        if (tangent.length() > 0.0001) {
            tangent = tangent.normalize3D();
        }

        const dmu = (Vec4{ .x = rba.friction, .y = rbb.friction }).length();

        // tangential velocity
        const tvel = vba.dot(tangent);
        const f = -tvel / (rba.invmass + rbb.invmass);
        var friction = Vec4{};
        if (@abs(f) < j * dmu) {
            friction = tangent.scale(f);
        } else {
            friction = tangent.scale(-j * dmu);
        }

        rba.vel = rba.vel.sub(friction.scale(rba.invmass));
        rbb.vel = rbb.vel.add(friction.scale(rbb.invmass));

        // const torque = entity.collision.normal.cross(friction);
        // rba.angular_vel = rba.angular_vel.sub(torque.scale(amassinv));
        // rbb.angular_vel = rbb.angular_vel.add(torque.scale(bmassinv));
    }
};

pub const PositionSolver = struct {
    const PositionEntity = struct {
        entity: EntityCollider.CollisionEntity,
        dela: Vec4 = .{},
        delb: Vec4 = .{},
    };

    const constants = struct {
        // - [fix objects jittering in physics engine](https://gamedev.stackexchange.com/questions/53991/how-do-i-fix-objects-popping-or-jittering-in-physics-engine)
        const slop = 0.01;
    };

    pub fn solve(contacts: []EntityCollider.Contact, entity: *PositionEntity) void {
        const rba = entity.entity.a.rigidbody;
        const rbb = entity.entity.b.rigidbody;
        const contact = &contacts[entity.entity.collision.contacts.index];

        const del = contact.normal.scale(@max(contact.depth - constants.slop, 0) / (rba.invmass + rbb.invmass));

        entity.dela = del.scale(rba.invmass);
        entity.delb = del.scale(rbb.invmass);
    }

    pub fn apply(entity: *PositionEntity, delta: f32) void {
        _ = delta;
        const a = entity.entity.a;
        const b = entity.entity.b;

        a.transform.pos = a.transform.pos.sub(entity.dela);
        b.transform.pos = b.transform.pos.add(entity.delb);
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
        // cuboid: Cuboid,

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

        pub fn raycast(self: @This(), transform: *const Transform, ro: Vec4, rd: Vec4) ?f32 {
            var this = self;
            switch (this) {
                .sphere => |*s| {
                    s.center = s.center.add(transform.pos);
                    s.radius *= transform.scale.max3();

                    const oc = s.center.sub(ro);
                    const b = oc.dot(rd);
                    const c = oc.dot(oc) - b * b;

                    if (c > s.radius * s.radius) return null;

                    const d = @sqrt(s.radius * s.radius - c);
                    const t0 = @min(b - d, b + d);
                    const t1 = @max(b - d, b + d);

                    if (t0 < 0 and t1 < 0) return null;
                    if (t0 < 0) return t1;
                    return t0;
                },
                .plane => |p| {
                    _ = p;
                    return null;
                },
            }
        }
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

    pub const Rigidbody = struct {
        flags: packed struct {
            pinned: bool = false,
            player: bool = false,
        } = .{},

        invmass: f32 = 1.0,
        restitution: f32 = 0.3,

        vel: Vec4 = .{},
        force: Vec4 = .{},

        invinertia: math.Mat4x4 = inertia.cuboid(1.0, Vec4.splat3(1.0)),

        angular_vel: Vec4 = .{},
        torque: Vec4 = .{},

        friction: f32 = 0.5,

        const inertia = struct {
            pub fn cuboid(mass: f32, half_sides: Vec4) math.Mat4x4 {
                return math.Mat4x4.scaling_mat(.{
                    .x = 1.0 / 3.0 * mass * (half_sides.y * half_sides.y + half_sides.z * half_sides.z),
                    .y = 1.0 / 3.0 * mass * (half_sides.x * half_sides.x + half_sides.z * half_sides.z),
                    .z = 1.0 / 3.0 * mass * (half_sides.x * half_sides.x + half_sides.y * half_sides.y),
                });
            }

            pub fn sphere(mass: f32, radius: f32) math.Mat4x4 {
                _ = mass;
                _ = radius;
                return .{};
            }
        };
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
