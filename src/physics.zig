const std = @import("std");

const math = @import("math.zig");
const Vec4 = math.Vec4;

const utils_mod = @import("utils.zig");
const TypeId = utils_mod.TypeId;

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

const Components = struct {
    usingnamespace @import("world.zig").Components;

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

        pub fn raycast(self: @This(), transform: *const Components.Transform, ro: Vec4, rd: Vec4) ?f32 {
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
                .cuboid => {
                    return null;
                },
            }
        }
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
        // impulse: Vec4 = .{},

        invinertia: math.Mat4x4 = inertia.cuboid(1.0, Vec4.splat3(1.0)),

        angular_vel: Vec4 = .{},
        torque: Vec4 = .{},
        // angular_impulse: Vec4 = .{},

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

    pub fn step(self: *@This(), ecs: *EntityComponentStore, delta: f32) !void {
        {
            var it = try ecs.iterator(struct { r: Components.Rigidbody });
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

        defer self.reset();
        {
            var it1 = try ecs.iterator(EntityCollider.RigidEntity);
            var it2 = it1;
            while (it1.next()) |a| {
                it2.reset();
                while (it2.next()) |b| {
                    if (std.meta.eql(a.id, b.id)) {
                        break;
                    }
                    const collision = try self.collide(a, b, .{}) orelse continue;

                    try self.collisions.append(.{ .a = a, .b = b, .collision = collision });
                }
            }
        }
        // {
        //     var it = try ecs.iterator(struct { cable: Components.CableAttached });
        //     while (it.next()) |e| {
        //         const a = try ecs.get(e.cable.a, EntityCollider.RigidEntity);
        //         const b = try ecs.get(e.cable.b, EntityCollider.RigidEntity);

        //         const ba = a.transform.pos.sub(b.transform.pos);
        //         if (ba.length() > e.cable.length) {
        //             try self.collisions.append(.{
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
        //     var it = try ecs.iterator(struct { rod: Components.RodAttached });
        //     while (it.next()) |e| {
        //         const a = try ecs.get(e.rod.a, EntityCollider.RigidEntity);
        //         const b = try ecs.get(e.rod.b, EntityCollider.RigidEntity);

        //         const ba = a.transform.pos.sub(b.transform.pos);
        //         const bal = ba.length();
        //         if (@abs(bal - e.rod.length) > 0.001) {
        //             try self.collisions.append(.{
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

        for (self.collisions.items) |*e| {
            ImpulseSolver.solve(self.contacts.items, e, delta);

            try positions.append(.{ .entity = e.* });
        }

        for (positions.items) |*e| {
            PositionSolver.solve(self.contacts.items, e);
        }

        for (positions.items) |*e| {
            PositionSolver.apply(e, delta);
        }

        {
            var it = try ecs.iterator(struct { r: Components.Rigidbody, t: Components.Transform });
            while (it.next()) |e| {
                if (e.r.flags.pinned) continue;
                e.t.pos = e.t.pos.add(e.r.vel.scale(delta));

                // - [physically based modelling](https://graphics.stanford.edu/courses/cs448b-00-winter/papers/phys_model.pdf)
                // q' = 0.5 * {omega, .w = 0} * q
                // q ~= q + q' * dt
                e.r.angular_vel.w = 0;
                e.t.rotation = e.t.rotation.add(e.r.angular_vel.quat_mul(e.t.rotation).scale(0.5 * delta)).normalize();
            }
        }
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
                    .cuboid => {},
                }
            },
            .cuboid => |*ca| {
                switch (bc) {
                    .plane => |*pb| {
                        ca.center = a.transform.transform_pos(ca.center);
                        ca.half_extent.w = 0;
                        ca.half_extent = ca.half_extent.mul(a.transform.scale);

                        pb.normal.w = 0;
                        pb.normal = b.transform.rotation.rotate_vector(pb.normal.mul(b.transform.scale)).normalize3D();

                        const vertices = [8]Vec4{
                            ca.center.add(a.transform.rotation.rotate_vector(.{ .x = ca.half_extent.x, .y = ca.half_extent.y, .z = ca.half_extent.z })),
                            ca.center.add(a.transform.rotation.rotate_vector(.{ .x = ca.half_extent.x, .y = ca.half_extent.y, .z = -ca.half_extent.z })),
                            ca.center.add(a.transform.rotation.rotate_vector(.{ .x = ca.half_extent.x, .y = -ca.half_extent.y, .z = ca.half_extent.z })),
                            ca.center.add(a.transform.rotation.rotate_vector(.{ .x = ca.half_extent.x, .y = -ca.half_extent.y, .z = -ca.half_extent.z })),
                            ca.center.add(a.transform.rotation.rotate_vector(.{ .x = -ca.half_extent.x, .y = ca.half_extent.y, .z = ca.half_extent.z })),
                            ca.center.add(a.transform.rotation.rotate_vector(.{ .x = -ca.half_extent.x, .y = ca.half_extent.y, .z = -ca.half_extent.z })),
                            ca.center.add(a.transform.rotation.rotate_vector(.{ .x = -ca.half_extent.x, .y = -ca.half_extent.y, .z = ca.half_extent.z })),
                            ca.center.add(a.transform.rotation.rotate_vector(.{ .x = -ca.half_extent.x, .y = -ca.half_extent.y, .z = -ca.half_extent.z })),
                        };

                        for (vertices) |v| {
                            const ba = v.sub(b.transform.pos.add(pb.normal.scale(pb.offset)));
                            const dist = ba.dot(pb.normal);

                            if (dist > 0) {
                                continue;
                            }

                            const normal = pb.normal.scale(-1);
                            const contact = try handle.reserve(self);
                            contact.* = .{
                                .pos = v.add(normal.scale(dist * 0.5)),
                                .normal = normal,
                                .depth = @abs(dist),
                            };
                        }

                        if (handle.len == 0) {
                            return null;
                        }

                        return .{
                            .contacts = handle,
                            .restitution = a.rigidbody.restitution * b.rigidbody.restitution,
                        };
                    },
                    .sphere => |*sb| {
                        // sb.center.w = 1;
                        // sb.center = b.transform.transform_pos(sb.center);
                        sb.radius = sb.radius * b.transform.scale.max3();
                        // ca.half_extent = ca.half_extent.mul(a.transform.scale);

                        const relcenter = a.transform.inverse_transform_pos(sb.center);

                        var closest = Vec4{};
                        closest.x = relcenter.x;
                        if (closest.x > ca.half_extent.x) closest.x = ca.half_extent.x;
                        if (closest.x < -ca.half_extent.x) closest.x = -ca.half_extent.x;

                        closest.y = relcenter.y;
                        if (closest.y > ca.half_extent.y) closest.y = ca.half_extent.y;
                        if (closest.y < -ca.half_extent.y) closest.y = -ca.half_extent.y;

                        closest.z = relcenter.z;
                        if (closest.z > ca.half_extent.z) closest.z = ca.half_extent.z;
                        if (closest.z < -ca.half_extent.z) closest.z = -ca.half_extent.z;

                        const ab = closest.sub(sb.center);
                        if (ab.length() > sb.radius) return null;
                        if (ab.length() < 0.0001) return null;

                        const normal = ab.normalize3D();
                        const contact = try handle.reserve(self);
                        closest.w = 1;
                        contact.* = .{
                            .pos = a.transform.transform_pos(closest),
                            .normal = normal,
                            .depth = @abs(sb.radius - ab.length()),
                        };

                        return .{
                            .contacts = handle,
                            .restitution = a.rigidbody.restitution * b.rigidbody.restitution,
                        };
                    },
                    .cuboid => |*cb| {
                        const mata = a.transform.mat4();
                        const matb = b.transform.mat4();

                        ca.center = a.transform.transform_pos(ca.center);
                        cb.center = b.transform.transform_pos(cb.center);

                        const axis_overlap = struct {
                            fn project_on_axis(mat: *const math.Mat4x4, axis: Vec4, dir: Vec4) f32 {
                                return (Vec4{ .x = dir.x, .y = dir.y, .z = dir.z }).dot((Vec4{
                                    .x = mat.axis(.x).dot(axis),
                                    .y = mat.axis(.y).dot(axis),
                                    .z = mat.axis(.z).dot(axis),
                                }).abs());
                            }

                            fn axis_overlap(
                                cua: *const Components.Collider.Cuboid,
                                mat_a: *const math.Mat4x4,
                                cub: *const Components.Collider.Cuboid,
                                mat_b: *const math.Mat4x4,
                                axis: Vec4,
                            ) f32 {
                                const p1 = project_on_axis(mat_a, axis, cua.half_extent);
                                const p2 = project_on_axis(mat_b, axis, cub.half_extent);
                                const dist = @abs(cub.center.sub(cua.center).dot(axis));
                                return p1 + p2 - dist;
                            }
                        }.axis_overlap;

                        var axes = [15]Vec4{
                            mata.axis(.x),
                            mata.axis(.y),
                            mata.axis(.z),
                            matb.axis(.x),
                            matb.axis(.y),
                            matb.axis(.z),
                            mata.axis(.x).cross(matb.axis(.x)),
                            mata.axis(.x).cross(matb.axis(.y)),
                            mata.axis(.x).cross(matb.axis(.z)),
                            mata.axis(.y).cross(matb.axis(.x)),
                            mata.axis(.y).cross(matb.axis(.y)),
                            mata.axis(.y).cross(matb.axis(.z)),
                            mata.axis(.z).cross(matb.axis(.x)),
                            mata.axis(.z).cross(matb.axis(.y)),
                            mata.axis(.z).cross(matb.axis(.z)),
                        };

                        var besti: usize = 15;
                        var best_overlap = std.math.floatMax(f32);
                        for (&axes, 0..) |*axis, i| {
                            if (axis.length_sq() < 0.001) {
                                continue;
                            }
                            axis.* = axis.normalize3D();

                            const overlap = axis_overlap(ca, &mata, cb, &matb, axis.*);
                            if (overlap < 0) return null;
                            if (overlap < best_overlap) {
                                best_overlap = overlap;
                                besti = i;
                            }
                        }

                        switch (besti) {
                            0...2 => {
                                var normal = mata.axis(switch (besti) {
                                    0 => .x,
                                    1 => .y,
                                    2 => .z,
                                    else => unreachable,
                                });
                                if (normal.dot(cb.center.sub(ca.center)) < 0) {
                                    normal = normal.scale(-1);
                                }

                                var v = cb.half_extent;
                                if (matb.axis(.x).dot(normal) < 0) v.x *= -1;
                                if (matb.axis(.y).dot(normal) < 0) v.y *= -1;
                                if (matb.axis(.z).dot(normal) < 0) v.z *= -1;

                                v.z = 1;
                                v = matb.mul_vec4(v);

                                const contact = try handle.reserve(self);
                                contact.* = .{
                                    .pos = v,
                                    .normal = normal,
                                    .depth = @abs(best_overlap),
                                };
                            },
                            3...5 => {
                                var normal = matb.axis(switch (besti) {
                                    3 => .x,
                                    4 => .y,
                                    5 => .z,
                                    else => unreachable,
                                });
                                if (normal.dot(ca.center.sub(cb.center)) < 0) {
                                    normal = normal.scale(-1);
                                }

                                var v = ca.half_extent;
                                if (mata.axis(.x).dot(normal) < 0) v.x *= -1;
                                if (mata.axis(.y).dot(normal) < 0) v.y *= -1;
                                if (mata.axis(.z).dot(normal) < 0) v.z *= -1;

                                v.z = 1;
                                v = mata.mul_vec4(v);

                                const contact = try handle.reserve(self);
                                contact.* = .{
                                    .pos = v,
                                    .normal = normal,
                                    .depth = @abs(best_overlap),
                                };
                            },
                            6...14 => {
                                const aaxisi: math.Mat4x4.Axis = switch (besti) {
                                    6...8 => .x,
                                    9...11 => .y,
                                    12...14 => .z,
                                    else => unreachable,
                                };
                                const baxisi: math.Mat4x4.Axis = switch (besti) {
                                    6, 9, 12 => .x,
                                    7, 10, 13 => .y,
                                    8, 11, 14 => .z,
                                    else => unreachable,
                                };
                                const aaxis = mata.axis(aaxisi);
                                const baxis = matb.axis(baxisi);
                                var axis = aaxis.cross(baxis).normalize3D();

                                if (axis.dot(cb.center.sub(ca.center)) > 0) axis = axis.scale(-1);

                                var pedgea = ca.half_extent;
                                var pedgeb = cb.half_extent;

                                if (aaxisi == .x) {
                                    pedgea.x = 0;
                                } else if (mata.axis(.x).dot(axis) > 0) {
                                    pedgea.x *= -1;
                                }
                                if (baxisi == .x) {
                                    pedgeb.x = 0;
                                } else if (matb.axis(.x).dot(axis) < 0) {
                                    pedgeb.x *= -1;
                                }

                                if (aaxisi == .y) {
                                    pedgea.y = 0;
                                } else if (mata.axis(.y).dot(axis) > 0) {
                                    pedgea.y *= -1;
                                }
                                if (baxisi == .y) {
                                    pedgeb.y = 0;
                                } else if (matb.axis(.y).dot(axis) < 0) {
                                    pedgeb.y *= -1;
                                }

                                if (aaxisi == .z) {
                                    pedgea.z = 0;
                                } else if (mata.axis(.z).dot(axis) > 0) {
                                    pedgea.z *= -1;
                                }
                                if (baxisi == .z) {
                                    pedgeb.z = 0;
                                } else if (matb.axis(.z).dot(axis) < 0) {
                                    pedgeb.z *= -1;
                                }

                                pedgea = ca.center.add(pedgea);
                                pedgeb = cb.center.add(pedgeb);

                                const ab = pedgea.sub(pedgeb);
                                const dpa = aaxis.dot(ab);
                                const dpb = baxis.dot(ab);

                                const sma = aaxis.length_sq();
                                const smb = baxis.length_sq();
                                const dpe = aaxis.dot(baxis);
                                const denom = sma * smb - dpe * dpe;
                                if (@abs(denom) < 0.001) {
                                    return null;
                                }
                                const npa = pedgea.add(aaxis.scale((dpe * dpb - smb * dpa) / denom));
                                const npb = pedgeb.add(baxis.scale((sma * dpb - dpe * dpb) / denom));
                                const v = npa.scale(0.5).add(npb.scale(0.5));

                                const contact = try handle.reserve(self);
                                contact.* = .{
                                    .pos = v,
                                    .normal = aaxis.cross(baxis).normalize3D(),
                                    .depth = @abs(best_overlap),
                                };
                            },
                            else => return null,
                        }

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

        var depth: f32 = 0;
        var del = Vec4{};
        for (contacts[entity.entity.collision.contacts.index..][0..entity.entity.collision.contacts.len]) |contact| {
            if (contact.depth > depth) {
                del = contact.normal.scale(@max(contact.depth - constants.slop, 0) / (rba.invmass + rbb.invmass));
                depth = contact.depth;
            }
        }

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
