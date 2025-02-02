const std = @import("std");

const math = @import("math.zig");
const Vec4 = math.Vec4;

const app = @import("app.zig");
const AppState = app.AppState;

const main = @import("main.zig");
const allocator = main.allocator;

pub const World = struct {
    entities: std.ArrayList(Entity),

    pub fn init() @This() {
        return .{ .entities = std.ArrayList(Entity).init(allocator) };
    }

    pub fn deinit(self: *@This()) void {
        self.entities.deinit();
    }

    pub fn tick(self: *@This(), state: *AppState, delta: f32) !void {
        for (self.entities.items) |*e| {
            if (e.typ.player) {
                e.transform.pos = state.camera.pos;
            } else if (!e.rigidbody.flags.pinned) {
                e.rigidbody.force = state.camera.world_basis.up.scale(-e.rigidbody.mass);
            }
        }

        try self.step(delta);

        for (self.entities.items) |*e| {
            e.rigidbody.force = .{};
        }
    }

    pub fn step(self: *@This(), delta: f32) !void {
        for (self.entities.items) |*e| {
            const rb = &e.rigidbody;
            if (rb.flags.pinned) continue;
            rb.vel = rb.vel.add(rb.force.scale(1 / rb.mass));
        }

        var collisions = std.ArrayList(EntityCollider.CollisionEntity).init(allocator);
        defer collisions.deinit();

        for (self.entities.items) |*a| {
            for (self.entities.items) |*b| {
                if (a == b) {
                    break;
                }
                const collision = EntityCollider.collide(a, b, .{}) orelse continue;

                try collisions.append(.{ .a = a, .b = b, .collision = collision });
            }
        }

        var positions = std.ArrayList(PositionSolver.PositionEntity).init(allocator);
        defer positions.deinit();

        for (collisions.items) |*e| {
            ImpulseSolver.solve(e);

            try positions.append(.{ .entity = e.* });
        }

        for (positions.items) |*e| {
            PositionSolver.solve(e);
        }

        for (positions.items) |*e| {
            PositionSolver.apply(e, delta);
        }

        for (self.entities.items) |*e| {
            const rb = &e.rigidbody;
            if (rb.flags.pinned) continue;
            e.transform.pos = e.transform.pos.add(rb.vel.scale(delta));
        }
    }
};

pub const EntityCollider = struct {
    pub const Collision = struct {
        // norm(a - b)
        normal: Vec4,

        // len(a - b)
        depth: f32,
    };

    pub const CollisionEntity = struct {
        a: *Entity,
        b: *Entity,
        collision: Collision,
    };

    pub fn collide(a: *Entity, b: *Entity, v: struct { flip: bool = false }) ?Collision {
        var ac = a.collider;
        var bc = b.collider;
        switch (ac) {
            .sphere => |*sa| blk: {
                // negative radius only supported for b
                if (sa.radius < 0) break :blk;

                switch (bc) {
                    .sphere => |*sb| {
                        sa.center = sa.center.add(a.transform.pos);
                        sb.center = sb.center.add(b.transform.pos);

                        sa.radius *= a.transform.scale.max_v3();
                        sb.radius *= b.transform.scale.max_v3();

                        const ba = sb.center.sub(sa.center);
                        if (sb.radius > 0) {
                            const dist = ba.length() - sa.radius - sb.radius;

                            if (dist > 0) {
                                return null;
                            }

                            return .{
                                .normal = ba.normalize3D(),
                                .depth = @abs(dist),
                            };
                        } else {
                            const dist = -sb.radius - ba.length() - sa.radius;

                            if (dist > 0) {
                                return null;
                            }

                            return .{
                                .normal = ba.normalize3D().scale(-1),
                                .depth = @abs(dist),
                            };
                        }
                    },
                    .plane => |*pb| {
                        sa.center = sa.center.add(a.transform.pos);
                        sa.radius *= a.transform.scale.max_v3();

                        const ba = b.transform.pos.sub(sa.center);

                        pb.normal.w = 0;
                        pb.normal = b.transform.rotation.rotate_vector(pb.normal);
                        pb.normal = pb.normal.abs().mul(ba.sign()).normalize3D();

                        const dist = ba.dot(pb.normal) - sa.radius;

                        if (dist > 0) {
                            return null;
                        }

                        return .{
                            .normal = pb.normal,
                            .depth = @abs(dist),
                        };
                    },
                }
            },
            else => {},
        }

        if (v.flip) {
            return null;
        }

        var collision = collide(b, a, .{ .flip = true }) orelse return null;
        collision.normal = collision.normal.scale(-1);
        return collision;
    }
};

pub const ImpulseSolver = struct {
    pub fn solve(entity: *EntityCollider.CollisionEntity) void {
        // - [Inelastic collision - Wikipedia](https://en.wikipedia.org/wiki/Inelastic_collision)

        const rba = &entity.a.rigidbody;
        const rbb = &entity.b.rigidbody;

        var vba = rbb.vel.sub(rba.vel);
        // normal velocity
        var nvel = entity.collision.normal.dot(vba);

        if (nvel >= 0) return;

        // impulse
        const e = rba.restitution * rbb.restitution;
        const j = -(1 + e) * nvel / (1 / rba.mass + 1 / rbb.mass);
        const impulse = entity.collision.normal.scale(j);
        if (!rba.flags.pinned) {
            rba.vel = rba.vel.sub(impulse.scale(1 / rba.mass));
        }
        if (!rbb.flags.pinned) {
            rbb.vel = rbb.vel.add(impulse.scale(1 / rbb.mass));
        }

        // friction
        vba = rbb.vel.sub(rba.vel);
        nvel = entity.collision.normal.dot(vba);
        var tangent = vba.sub(entity.collision.normal.scale(nvel));
        if (tangent.length() > 0.0001) {
            tangent = tangent.normalize3D();
        }

        // tangential velocity
        const tvel = vba.dot(tangent);
        const smu = (Vec4{ .x = rba.static_friction, .y = rbb.static_friction }).length();

        const f = -tvel / (1 / rba.mass + 1 / rbb.mass);

        var friction = Vec4{};
        if (@abs(f) < j * smu) {
            friction = tangent.scale(f);
        } else {
            const dmu = (Vec4{ .x = rba.dynamic_friction, .y = rbb.dynamic_friction }).length();
            friction = tangent.scale(-j * dmu);
        }

        if (!rba.flags.pinned) {
            rba.vel = rba.vel.sub(friction.scale(1 / rba.mass));
        }
        if (!rbb.flags.pinned) {
            rbb.vel = rbb.vel.add(friction.scale(1 / rbb.mass));
        }
    }
};

pub const PositionSolver = struct {
    const PositionEntity = struct {
        entity: EntityCollider.CollisionEntity,
        dela: Vec4 = .{},
        delb: Vec4 = .{},
    };

    const constants = struct {
        const p = 0.8;
        const s = 0.01;
    };

    pub fn solve(entity: *PositionEntity) void {
        const rba = &entity.entity.a.rigidbody;
        const rbb = &entity.entity.b.rigidbody;
        const col = &entity.entity.collision;

        const del = col.normal.scale(col.depth);

        if (!rba.flags.pinned) {
            entity.dela = del;
        }
        if (!rbb.flags.pinned) {
            entity.delb = del;
        }
    }

    pub fn apply(entity: *PositionEntity, delta: f32) void {
        const a = entity.entity.a;
        const b = entity.entity.b;
        if (!a.rigidbody.flags.pinned) {
            a.transform.pos = a.transform.pos.sub(entity.dela.scale(delta));
        }
        if (!b.rigidbody.flags.pinned) {
            b.transform.pos = b.transform.pos.add(entity.delb.scale(delta));
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
        };
        pub const Cuboid = struct {
            center: Vec4 = .{},
            half_extent: Vec4,
        };
    };

    pub const Rigidbody = struct {
        flags: packed struct {
            pinned: bool = false,
        },
        vel: Vec4 = .{},
        force: Vec4 = .{},
        restitution: f32 = 1.0,
        mass: f32 = 1.0,
        static_friction: f32 = 0.5,
        dynamic_friction: f32 = 0.5,
    };
};

pub const Entity = struct {
    typ: packed struct {
        player: bool = false,
        cube: bool = false,
        object: bool = false,
    },

    transform: Components.Transform = .{},
    collider: Components.Collider = .{ .sphere = .{ .radius = 1 } },
    rigidbody: Components.Rigidbody = .{ .flags = .{} },

    instance_attr_index: u32,
};
