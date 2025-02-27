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

const main = @import("main.zig");
const allocator = main.allocator;

pub const World = struct {
    ecs: EntityComponentStore,

    pub fn init() !@This() {
        var self = @This(){ .ecs = try EntityComponentStore.init() };
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
            }
        }

        var collisions = std.ArrayList(EntityCollider.CollisionEntity).init(allocator.*);
        defer collisions.deinit();

        {
            var it1 = try self.ecs.iterator(EntityCollider.RigidEntity);
            var it2 = it1;
            while (it1.next()) |a| {
                it2.reset();
                while (it2.next()) |b| {
                    if (std.meta.eql(a.id, b.id)) {
                        break;
                    }
                    const collision = EntityCollider.collide(a, b, .{}) orelse continue;

                    try collisions.append(.{ .a = a, .b = b, .collision = collision });
                }
            }
        }
        {
            var it = try self.ecs.iterator(struct { cable: Components.CableAttached });
            while (it.next()) |e| {
                const a = try self.ecs.get(e.cable.a, EntityCollider.RigidEntity);
                const b = try self.ecs.get(e.cable.b, EntityCollider.RigidEntity);

                const ba = a.transform.pos.sub(b.transform.pos);
                if (ba.length() > e.cable.length) {
                    try collisions.append(.{
                        .a = a,
                        .b = b,
                        .collision = .{
                            .restitution = e.cable.restitution,
                            .depth = 0,
                            .normal = ba.normalize3D().scale(1),
                        },
                    });
                }
            }
        }
        {
            var it = try self.ecs.iterator(struct { rod: Components.RodAttached });
            while (it.next()) |e| {
                const a = try self.ecs.get(e.rod.a, EntityCollider.RigidEntity);
                const b = try self.ecs.get(e.rod.b, EntityCollider.RigidEntity);

                const ba = a.transform.pos.sub(b.transform.pos);
                const bal = ba.length();
                if (@abs(bal - e.rod.length) > 0.001) {
                    try collisions.append(.{
                        .a = a,
                        .b = b,
                        .collision = .{
                            .restitution = 0,
                            .depth = 0,
                            .normal = ba.normalize3D().scale(if (bal > e.rod.length) 1 else -1),
                        },
                    });
                }
            }
        }

        var positions = std.ArrayList(PositionSolver.PositionEntity).init(allocator.*);
        defer positions.deinit();

        for (collisions.items) |*e| {
            ImpulseSolver.solve(e, delta);

            try positions.append(.{ .entity = e.* });
        }

        for (positions.items) |*e| {
            PositionSolver.solve(e);
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
    pub const Collision = struct {
        // norm(a - b)
        normal: Vec4,

        // len(a - b)
        depth: f32,

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

    pub fn collide(a: RigidEntity.p, b: RigidEntity.p, v: struct { flip: bool = false }) ?Collision {
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

                        sa.radius *= a.transform.scale.max_v3();
                        sb.radius *= b.transform.scale.max_v3();

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

                            return .{
                                .normal = ba.normalize3D(),
                                .depth = @abs(dist),
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

                            return .{
                                .normal = ba.normalize3D().scale(-1),
                                .depth = @abs(dist),
                                .restitution = a.rigidbody.restitution * b.rigidbody.restitution,
                            };
                        }
                    },
                    .plane => |*pb| {
                        sa.center = sa.center.add(a.transform.pos);
                        sa.radius *= a.transform.scale.max_v3();

                        const ba = sa.center.sub(b.transform.pos);

                        pb.normal.w = 0;
                        pb.normal = b.transform.rotation.rotate_vector(pb.normal).normalize3D();

                        const dist = ba.dot(pb.normal) - sa.radius;

                        if (dist > 0) {
                            return null;
                        }

                        return .{
                            .normal = pb.normal.scale(-1),
                            .depth = @abs(dist),
                            .restitution = a.rigidbody.restitution * b.rigidbody.restitution,
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
    pub fn solve(entity: *EntityCollider.CollisionEntity, delta: f32) void {
        // - [Inelastic collision - Wikipedia](https://en.wikipedia.org/wiki/Inelastic_collision)

        const rba = entity.a.rigidbody;
        const rbb = entity.b.rigidbody;

        const vba = rbb.vel.sub(rba.vel);

        // normal velocity
        const nvel = entity.collision.normal.dot(vba);

        if (nvel >= 0) return;

        // impulse
        const e = entity.collision.restitution;
        const j = -(1 + e) * nvel / (rba.invmass + rbb.invmass);

        const fvel = (rbb.force.scale(rbb.invmass).sub(rba.force.scale(rba.invmass))).scale(delta).dot(entity.collision.normal);
        if (-fvel + 0.001 >= -nvel) {
            rba.vel = rba.vel.sub(entity.collision.normal.scale(-rba.vel.dot(entity.collision.normal)));
            rbb.vel = rbb.vel.add(entity.collision.normal.scale(-rbb.vel.dot(entity.collision.normal)));
        } else {
            const impulse = entity.collision.normal.scale(j);
            rba.vel = rba.vel.sub(impulse.scale(rba.invmass));
            rbb.vel = rbb.vel.add(impulse.scale(rbb.invmass));
        }

        // friction
        // var tangent = vba.sub(entity.collision.normal.scale(nvel));
        // if (tangent.length() > 0.0001) {
        //     tangent = tangent.normalize3D();
        // }

        // const dmu = (Vec4{ .x = rba.dynamic_friction, .y = rbb.dynamic_friction }).length();
        // const friction = tangent.scale(-j * dmu);
        // // std.debug.print("friction: {any}\n", .{friction});

        // rba.vel = rba.vel.sub(friction.scale(rba.invmass));
        // rbb.vel = rbb.vel.add(friction.scale(rbb.invmass));

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

    pub fn solve(entity: *PositionEntity) void {
        const rba = entity.entity.a.rigidbody;
        const rbb = entity.entity.b.rigidbody;
        const col = entity.entity.collision;

        const del = col.normal.scale(@max(col.depth - constants.slop, 0) / (rba.invmass + rbb.invmass));

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
                    s.radius *= transform.scale.max_v3();

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

        static_friction: f32 = 0.5,
        dynamic_friction: f32 = 0.5,
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

pub const Entity = struct {
    // i don't think i need generational indices here :/
    // these ids are not at all any sort of indices, these map to ArchetypeEntity using a hashmap.
    // thoughhh we could totally switch to a array. hmmmmmmmmm
    id: u32,
};

pub const ArchetypeEntity = struct {
    archetype: ArchetypeId,
    // entity's index into archetype's components[i].items
    entity_index: usize,
};

// index into self.archetypes
pub const ArchetypeId = u32;

// represents 1 component
pub const ComponentId = packed struct {
    id: u16,
    size: u16,

    pub fn lessThan(_: void, a: ComponentId, b: ComponentId) bool {
        return a.id < b.id;
    }
};

pub const Type = struct {
    // assume components are 0..128
    components: u128,

    const one: u128 = 1;

    pub inline fn from(components: []const ComponentId) @This() {
        var this: @This() = .{ .components = 0 };
        for (components) |comp| {
            this.components |= mask(comp);
        }
        return this;
    }

    inline fn mask(comp: anytype) u128 {
        return switch (@TypeOf(comp)) {
            ComponentId => @This().one << @intCast(comp.id),
            else => @This().one << @intCast(comp),
        };
    }

    // have a biset of components in each Type
    // mask out all components with bigger or equal ids: bitset & ((1 << compid.id) - 1)
    // count the number of bits still set
    pub fn index(self: *const @This(), component: ComponentId) ?usize {
        const m = mask(component);
        if (self.components & m > 0) {
            return @popCount(self.components & (m - 1));
        } else {
            return null;
        }
    }

    pub fn count(self: *const @This()) usize {
        return @popCount(self.components);
    }

    pub fn has_components(self: *const @This(), components: u128) bool {
        return (self.components & components) == components;
    }

    pub inline fn removed(self: *const @This(), compid: ComponentId) ?@This() {
        const typ = self.*;

        typ.components &= ~mask(compid);
        if (typ.components == self.components) {
            return null;
        }

        return typ;
    }

    pub inline fn inserted(self: *const @This(), compid: ComponentId) ?@This() {
        var typ = self.*;

        typ.components |= mask(compid);
        if (typ.components == self.components) {
            return null;
        }

        return typ;
    }

    pub fn iterator(self: *@This(), sizes: []u16) ComponentIterator {
        return .{ .components = self.components, .sizes = sizes };
    }

    // from a struct to a struct to the same struct but all fields are pointers
    pub fn pointer(comptime typ: type) type {
        const StructField = std.builtin.Type.StructField;
        var input_val: typ = undefined;
        const input_struct = @typeInfo(typ).Struct;

        comptime {
            var fields: [input_struct.fields.len]StructField = undefined;
            @memcpy(&fields, input_struct.fields);
            for (&fields) |*field| {
                field.default_value = null;
                const t: *field.type = &@field(input_val, field.name);
                field.type = @TypeOf(t);
            }

            return @Type(.{ .Struct = .{
                .layout = .auto,
                .fields = &fields,
                .decls = &[_]std.builtin.Type.Declaration{},
                .is_tuple = input_struct.is_tuple,
            } });
        }
    }

    pub const ComponentIterator = struct {
        components: u128,
        sizes: []u16,

        pub fn next(self: *@This()) ?ComponentId {
            if (self.components > 0) {
                const i = @ctz(self.components);
                self.components &= ~mask(i);
                return .{ .id = i, .size = self.sizes[i] };
            } else {
                return null;
            }
        }
    };
};

// represents a type of entity (a fixed sorted set of components)
pub const Archetype = struct {
    typ: Type,
    // type erased block of components (same length and order as self.typ.components)
    components: []std.ArrayListAligned(u8, 8),
    count: u32 = 0,

    // helpful for converting 1 type of entity to another
    // it is implicit whether we want to add or remove this component to this archetype
    edges: ArchetypeEdges,

    const ArchetypeEdges = std.AutoArrayHashMap(ComponentId, ArchetypeId);
    pub fn from_type(typ: Type) !@This() {
        const components = try allocator.alloc(std.ArrayListAligned(u8, 8), typ.count());
        errdefer allocator.free(components);
        for (components) |*comp| {
            comp.* = std.ArrayListAligned(u8, 8).init(allocator.*);
        }
        return .{
            .typ = typ,
            .components = components,
            .edges = ArchetypeEdges.init(allocator.*),
        };
    }

    pub fn swap_remove(self: *@This(), component_sizes: []u16, vtables: []EntityComponentStore.ComponentVtable, index: usize) void {
        var it = self.typ.iterator(component_sizes);

        for (self.components) |*comp| {
            const compid = it.next().?;

            if ((index + 1) * compid.size != comp.items.len) {
                const to_delete = comp.items[index * compid.size ..][0..compid.size];
                const last = comp.items[comp.items.len - compid.size ..];

                vtables[compid.id].maybe_deinit(@ptrCast(to_delete.ptr));

                @memcpy(to_delete, last);
            }

            // arraylist.pop() just does self.items.len -= 1
            comp.items.len -= compid.size;
        }
        self.count -= 1;
    }

    pub fn deinit(self: *@This(), component_sizes: []u16, vtables: []EntityComponentStore.ComponentVtable) void {
        var it = self.typ.iterator(component_sizes);

        for (self.components) |*comp| {
            const compid = it.next().?;

            var i: usize = 0;
            while ((i + 1) * compid.size < comp.items.len) : (i += 1) {
                vtables[compid.id].maybe_deinit(comp.items[i * compid.size ..][0..compid.size].ptr);
            }
            comp.deinit();
        }
        allocator.free(self.components);

        self.edges.deinit();
    }
};

// ECS lol
pub const EntityComponentStore = struct {
    // - [Building an ECS #1](https://ajmmertens.medium.com/building-an-ecs-1-where-are-my-entities-and-components-63d07c7da742)

    entities: Entities,
    archetypes: std.ArrayList(Archetype),
    // not all types are components. we register components so that we can do cooler comptime stuff
    // another idea could be to have a fixed enum of all possible components, and having a fixed enum varient per component
    // directly defined in the component as 'const component_type = .transform'
    //   - note: these enums can be merged into 1
    // TODO: entity can be a component by simply allowing Component id to be mapped by either a type or an entity.
    components: TypeComponents,
    component_sizes: std.ArrayList(u16),
    vtables: Vtables,
    // Entity's component id
    entityid_component_id: ComponentId,
    entity_id: u32 = 0,

    archetype_map: ArchetypeMap,
    component_map: std.AutoArrayHashMap(ComponentId, std.ArrayList(ArchetypeId)),

    const Entities = std.AutoArrayHashMap(Entity, ArchetypeEntity);
    const TypeComponents = std.AutoArrayHashMap(TypeId, ComponentId);
    const Vtables = std.ArrayList(ComponentVtable);
    const ComponentVtable = struct {
        name: []const u8,
        deinit: ?*const fn (ptr: *anyopaque) void,

        fn from(component: type) @This() {
            switch (component) {
                []const u8 => return .{ .name = @typeName(component), .deinit = null },
                else => return .{
                    .name = @typeName(component),
                    // TODO: these will not update when hot reloaded.
                    //  - this can be made to work by adding .reload() methods to things that have pointers.
                    //  - just regester the components again :P
                    // tho maybe unloading earlier dylib might invalidate these ptrs? not sure.
                    // are new dylibs loaded in the similar memory locations as the older ones?
                    //  - this will also break these ptrs.
                    .deinit = if (comptime @hasDecl(component, "deinit")) @ptrCast(&component.deinit) else null,
                },
            }
        }

        fn maybe_deinit(self: *const @This(), ptr: *anyopaque) void {
            if (self.deinit) |deinitfn| {
                deinitfn(ptr);
            }
        }
    };
    const ArchetypeMap = std.ArrayHashMap(Type, ArchetypeId, struct {
        pub fn hash(ctx: @This(), key: Type) u32 {
            _ = ctx;
            var hasher = std.hash.Wyhash.init(0);
            hasher.update(std.mem.asBytes(&key));
            return @truncate(hasher.final());
        }
        pub fn eql(ctx: @This(), a: Type, b: Type, b_index: usize) bool {
            _ = b_index;
            _ = ctx;
            return a.components == b.components;
        }
    }, true);

    pub fn init() !@This() {
        var self = @This(){
            .entities = std.AutoArrayHashMap(Entity, ArchetypeEntity).init(allocator.*),
            .archetypes = std.ArrayList(Archetype).init(allocator.*),
            .components = std.AutoArrayHashMap(TypeId, ComponentId).init(allocator.*),
            .component_sizes = std.ArrayList(u16).init(allocator.*),
            .archetype_map = ArchetypeMap.init(allocator.*),
            .component_map = std.AutoArrayHashMap(ComponentId, std.ArrayList(ArchetypeId)).init(allocator.*),
            .vtables = Vtables.init(allocator.*),
            .entityid_component_id = undefined,
        };
        errdefer self.deinit();

        self.entityid_component_id = try self.register(Entity);

        return self;
    }

    pub fn deinit(self: *@This()) void {
        self.entities.deinit();

        for (self.archetypes.items) |*a| {
            a.deinit(self.component_sizes.items, self.vtables.items);
        }
        self.archetypes.deinit();

        self.components.deinit();
        self.component_sizes.deinit();
        self.vtables.deinit();

        self.archetype_map.deinit();

        for (self.component_map.values()) |*list| {
            list.deinit();
        }
        self.component_map.deinit();
    }

    pub fn register(self: *@This(), component: type) !ComponentId {
        const type_id = TypeId.from_type(component);
        const comp_id = self.components.count();
        const comp = try self.components.getOrPut(type_id);
        const compid = ComponentId{
            .id = @intCast(comp_id),
            .size = @intCast(@sizeOf(component)),
        };
        if (!comp.found_existing) {
            comp.value_ptr.* = compid;
            try self.vtables.append(ComponentVtable.from(component));
            try self.component_sizes.append(compid.size);
        }
        return compid;
    }

    pub fn get_component_id(self: *@This(), component: type) !ComponentId {
        const type_id = TypeId.from_type(component);
        return self.components.get(type_id) orelse error.TypeNotAComponent;
    }

    fn component_ids_from(self: *@This(), typ: type) ![@typeInfo(typ).Struct.fields.len]ComponentId {
        const fields = @typeInfo(typ).Struct.fields;
        var components: [fields.len]ComponentId = undefined;

        inline for (fields, 0..) |*field, i| {
            const e = self.components.getEntry(TypeId.from_type(field.type));
            if (e) |entry| {
                components[i] = entry.value_ptr.*;
            } else {
                return error.FieldNotAComponent;
            }
        }

        return components;
    }

    fn components_from(self: *@This(), typ: type) !u128 {
        const components = try self.component_ids_from(typ);
        return Type.from(&components).components;
    }

    fn get_archetype(self: *@This(), typ: Type) !ArchetypeId {
        const archeid = self.archetype_map.get(typ) orelse blk: {
            var archetype = try Archetype.from_type(typ);
            errdefer archetype.deinit(self.component_sizes.items, self.vtables.items);

            const archeid: ArchetypeId = @intCast(self.archetypes.items.len);
            try self.archetypes.append(archetype);

            try self.archetype_map.put(archetype.typ, @intCast(archeid));

            break :blk archeid;
        };
        return archeid;
    }

    fn insert_reserved(self: *@This(), eid: Entity, components: anytype) !void {
        const component_ids = try self.components_from(@TypeOf(components));
        const typ = Type{ .components = component_ids | Type.mask(self.entityid_component_id) };

        const archeid = try self.get_archetype(typ);
        const archetype = &self.archetypes.items[archeid];
        defer archetype.count += 1;

        inline for (@typeInfo(@TypeOf(components)).Struct.fields) |field| {
            const compid = try self.get_component_id(field.type);
            const compi = archetype.typ.index(compid).?;
            const f = @field(components, field.name);
            const bytes = std.mem.asBytes(&f);
            try archetype.components[compi].appendSlice(bytes);
        }

        {
            const compi = typ.index(self.entityid_component_id).?;
            const bytes = std.mem.asBytes(&eid);
            try archetype.components[compi].appendSlice(bytes);
            try self.entities.put(eid, .{ .archetype = @intCast(archeid), .entity_index = @intCast(archetype.count) });
        }
    }

    pub fn insert(self: *@This(), components: anytype) !Entity {
        // TODO: think of a better way to generate unique entity ids? do i need to??
        const eid = Entity{ .id = self.entity_id };
        defer self.entity_id += 1;

        try self.insert_reserved(eid, components);

        return eid;
    }

    pub fn get(self: *@This(), entity: Entity, comptime T: type) !Type.pointer(T) {
        var t: Type.pointer(T) = undefined;

        const ae = self.entities.get(entity) orelse return error.EntityNotFound;
        const archetype = &self.archetypes.items[ae.archetype];

        inline for (@typeInfo(T).Struct.fields) |field| {
            const compid = try self.get_component_id(field.type);
            const compi = archetype.typ.index(compid).?;
            const val = std.mem.bytesAsValue(field.type, archetype.components[compi].items[ae.entity_index * compid.size ..][0..compid.size]);
            @field(t, field.name) = @alignCast(val);
        }

        return t;
    }

    fn swap_remove(self: *@This(), ae: ArchetypeEntity) void {
        const archetype = &self.archetypes.items[ae.archetype];
        defer archetype.swap_remove(self.component_sizes.items, self.vtables.items, ae.entity_index);

        if (archetype.count - 1 != ae.entity_index) {
            const entity_ci = archetype.typ.index(self.entityid_component_id).?;
            const val = std.mem.bytesAsValue(Entity, archetype.components[entity_ci].items[(archetype.count - 1) * self.entityid_component_id.size ..]);
            self.entities.getEntry(val.*).?.value_ptr.entity_index = ae.entity_index;
        }
    }

    pub fn delete(self: *@This(), entity: Entity) !void {
        const ae = self.entities.fetchSwapRemove(entity) orelse return error.EntityNotFound;
        self.swap_remove(ae.value);
    }

    pub fn add_component(self: *@This(), entity: Entity, component: anytype) !void {
        const compid = try self.get_component_id(@TypeOf(component));
        const ae = self.entities.getEntry(entity) orelse return error.EntityNotFound;
        const curr_archetype = &self.archetypes.items[ae.value_ptr.archetype];

        const edge = try curr_archetype.edges.getOrPut(compid);
        const archeid = blk: {
            if (edge.found_existing) {
                break :blk edge.value_ptr.*;
            } else {
                const typ = curr_archetype.typ.inserted(compid) orelse return error.ComponentAlreadyPresent;
                const archeid = try self.get_archetype(typ);
                edge.value_ptr.* = archeid;
                break :blk archeid;
            }
        };

        const archetype = &self.archetypes.items[archeid];
        defer {
            self.swap_remove(ae.value_ptr.*);
            ae.value_ptr.* = .{
                .archetype = archeid,
                .entity_index = archetype.count,
            };
            archetype.count += 1;
        }

        var i: usize = 0;
        var j: usize = 0;
        var it = archetype.typ.iterator(self.component_sizes.items);
        while (archetype.components.len > i) : (i += 1) {
            const curr_compid = it.next().?;

            if (std.meta.eql(compid, curr_compid)) {
                try archetype.components[i].appendSlice(std.mem.asBytes(&component));
            } else {
                defer j += 1;
                try archetype.components[i].appendSlice(curr_archetype.components[j].items[ae.value_ptr.entity_index * curr_compid.size ..][0..curr_compid.size]);
            }
        }
    }

    pub fn remove_component(self: *@This(), entity: Entity, component: type) !void {
        const compid = try self.get_component_id(component);
        const ae = self.entities.getEntry(entity) orelse return error.EntityNotFound;
        const curr_archetype = &self.archetypes.items[ae.value_ptr.archetype];

        const edge = try curr_archetype.edges.getOrPut(compid);
        const archeid = blk: {
            if (edge.found_existing) {
                break :blk edge.value_ptr.*;
            } else {
                const typ = (try curr_archetype.typ.removed(compid)) orelse return error.ComponentNotPresent;
                const archeid = try self.get_archetype(typ);
                edge.value_ptr.* = archeid;
                break :blk archeid;
            }
        };

        const archetype = &self.archetypes.items[archeid];
        defer {
            self.swap_remove(ae.value_ptr.*);
            ae.value_ptr.* = .{
                .archetype = archeid,
                .entity_index = archetype.count,
            };
            archetype.count += 1;
        }

        var i: usize = 0;
        var j: usize = 0;
        var it = curr_archetype.typ.iterator(self.component_sizes.items);
        while (curr_archetype.components.len > j) : (j += 1) {
            const curr_compid = it.next().?;

            if (!std.meta.eql(compid, curr_compid)) {
                defer i += 1;
                try archetype.components[i].appendSlice(curr_archetype.components[j].items[ae.value_ptr.entity_index * curr_compid.size ..][0..curr_compid.size]);
            }
        }
    }

    pub fn iterator(self: *@This(), comptime typ: type) !EntityIterator(typ) {
        const ids = try self.component_ids_from(typ);

        return .{
            .ids = ids,
            .typ = Type.from(&ids),
            .archetype_it = self.archetype_map.iterator(),
            .ecs = self,
        };
    }

    pub fn explorer(self: *@This(), entity: Entity) !EntityExplorer {
        const ae = self.entities.get(entity) orelse return error.EntityNotFound;
        const archetype = &self.archetypes.items[ae.archetype];

        return .{
            .ecs = self,
            .archetype = archetype,
            .entity_index = ae.entity_index,
        };
    }

    pub fn deferred(self: *@This()) CmdBuf {
        return CmdBuf.init(self);
    }

    pub const EntityExplorer = struct {
        // for queries like (does this entity have this component)
        // and for fast query of random components of this entity.

        ecs: *EntityComponentStore,
        archetype: *Archetype,
        entity_index: usize,

        pub fn get(self: *@This(), comptime T: type) !Type.pointer(T) {
            var t: Type.pointer(T) = undefined;

            inline for (@typeInfo(T).Struct.fields) |field| {
                const compid = try self.get_component_id(field.type);
                const compi = self.archetype.typ.index(compid).?;
                const val = std.mem.bytesAsValue(field.type, self.archetype.components[compi].items[self.entity_index * compid.size ..][0..compid.size]);
                @field(t, field.name) = @alignCast(val);
            }

            return t;
        }

        pub fn get_component(self: *@This(), typ: type) ?*typ {
            const compid = self.ecs.components.get(TypeId.from_type(typ)).?;
            const compi = self.archetype.typ.index(compid) orelse return null;
            const val = std.mem.bytesAsValue(typ, self.archetype.components[compi].items[self.entity_index * compid.size ..][0..compid.size]);
            return @alignCast(val);
        }

        pub fn iterator(self: *@This()) ComponentIterator {
            return .{ .explorer = self };
        }

        pub const ComponentIterator = struct {
            explorer: *EntityExplorer,
            it: Type.ComponentIterator,
            component_index: u8 = 0,

            pub const ComponentEntry = struct {
                component: []u8,
                vtable: *ComponentVtable,
                compid: ComponentId,
            };

            pub fn next(self: *@This()) ?ComponentEntry {
                const compid = self.it.next() orelse return null;
                defer self.component_index += 1;

                const component = self.explorer.archetype.components[self.component_index].items[self.explorer.entity_index * compid.size ..][0..compid.size];

                return .{
                    .component = component,
                    .vtable = &self.explorer.ecs.vtables.items[self.component_index],
                    .compid = compid,
                };
            }

            pub fn reset(self: *@This()) void {
                self.component_index = 0;
                self.it = self.explorer.archetype.typ.iterator(self.explorer.ecs.component_sizes);
            }
        };
    };

    pub fn EntityIterator(typ: type) type {
        return struct {
            const fields = @typeInfo(typ).Struct.fields;
            const len = fields.len;

            ecs: *EntityComponentStore,
            // same order as that of fields
            ids: [len]ComponentId,
            typ: Type,
            archetype_it: ArchetypeMap.Iterator,
            current: ?struct {
                archetype: ArchetypeId,
                // current index into archetype.components[].items
                index: usize = 0,
            } = null,

            pub fn current_entity_explorer(self: *@This()) EntityExplorer {
                return .{
                    .ecs = self.ecs,
                    .archetype = &self.ecs.archetypes.items[self.current.?.archetype],
                    .entity_index = self.current.?.index - 1,
                };
            }

            pub fn next(self: *@This()) ?Type.pointer(typ) {
                outer: while (true) {
                    if (self.current) |*curr| inner: {
                        var t: Type.pointer(typ) = undefined;
                        const archetype = &self.ecs.archetypes.items[curr.archetype];
                        inline for (fields, self.ids) |field, compid| {
                            const ci = archetype.typ.index(compid).?;
                            const slice = archetype.components[ci].items;

                            if ((curr.index + 1) * compid.size > slice.len) {
                                self.current = null;
                                break :inner;
                            }

                            const val = std.mem.bytesAsValue(field.type, slice[curr.index * compid.size ..][0..compid.size]);
                            @field(t, field.name) = @alignCast(val);
                        }
                        curr.index += 1;
                        return t;
                    }

                    while (self.archetype_it.next()) |e| {
                        if (!e.key_ptr.has_components(self.typ.components)) {
                            continue;
                        }

                        self.current = .{
                            .archetype = e.value_ptr.*,
                        };
                        continue :outer;
                    }

                    return null;
                }
            }

            pub fn reset(self: *@This()) void {
                self.archetype_it.reset();
                self.current = null;
            }
        };
    }

    pub const CmdBuf = struct {
        ecs: *EntityComponentStore,
        alloc: std.heap.ArenaAllocator,

        inserted: Inserted = .{},
        deleted: Deleted = .{},
        added_components: Added = .{},
        removed_components: Removed = .{},

        const Inserted = std.ArrayListUnmanaged(struct {
            entity: Entity,
            bundle: *anyopaque,
            insertfn: *const fn (*EntityComponentStore, Entity, *anyopaque) anyerror!void,
        });
        const Deleted = std.ArrayListUnmanaged(Entity);
        const Added = std.ArrayListUnmanaged(struct {
            entity: Entity,
            component: *anyopaque,
            addfn: *const fn (*EntityComponentStore, Entity, *anyopaque) anyerror!void,
        });
        const Removed = std.ArrayListUnmanaged(struct {
            entity: Entity,
            rmfn: *const fn (*EntityComponentStore, Entity) anyerror!void,
        });

        pub fn init(ecs: *EntityComponentStore) @This() {
            return .{
                .ecs = ecs,
                .alloc = std.heap.ArenaAllocator.init(allocator.*),
            };
        }

        pub fn deinit(self: *@This()) void {
            self.alloc.deinit();
        }

        fn reset(self: *@This()) void {
            _ = self.alloc.reset(.retain_capacity);
            self.inserted = .{};
            self.deleted = .{};
            self.added_components = .{};
            self.removed_components = .{};
        }

        pub fn apply(self: *@This()) !void {
            defer self.reset();

            for (self.inserted.items) |*t| {
                try t.insertfn(self.ecs, t.entity, t.bundle);
            }

            for (self.added_components.items) |*t| {
                try t.addfn(self.ecs, t.entity, t.component);
            }

            for (self.removed_components.items) |*t| {
                try t.rmfn(self.ecs, t.entity);
            }

            for (self.deleted.items) |t| {
                try self.ecs.delete(t);
            }
        }

        fn allocated(self: *@This(), thing: anytype) !*@TypeOf(thing) {
            const alloc = self.alloc.allocator();
            const mem = try alloc.create(@TypeOf(thing));
            errdefer alloc.free(mem);
            mem.* = thing;
            return mem;
        }

        pub fn insert(self: *@This(), components: anytype) !Entity {
            const e = Entity{ .id = self.ecs.entity_id };
            defer self.ecs.entity_id += 1;
            const alloc = self.alloc.allocator();

            try self.inserted.append(alloc, .{
                .entity = e,
                .bundle = @ptrCast(try self.allocated(components)),
                .insertfn = @ptrCast(&(struct {
                    fn insert(ecs: *EntityComponentStore, entity: Entity, comp: @TypeOf(components)) anyerror!void {
                        try ecs.insert_reserved(entity, comp);
                    }
                }).insert),
            });

            return e;
        }

        pub fn add_component(self: *@This(), entity: Entity, component: anytype) !void {
            const alloc = self.alloc.allocator();
            try self.added_components.append(alloc, .{
                .entity = entity,
                .component = @ptrCast(try self.allocated(component)),
                .addfn = @ptrCast(&(struct {
                    fn add_component(ecs: *EntityComponentStore, e: Entity, comp: @TypeOf(component)) !void {
                        try ecs.add_component(e, comp);
                    }
                }).add_component),
            });
        }

        pub fn remove_component(self: *@This(), entity: Entity, component: type) !void {
            const alloc = self.alloc.allocator();
            try self.removed_components.append(alloc, .{
                .entity = entity,
                .rmfn = @ptrCast(&(struct {
                    fn remove_component(ecs: *EntityComponentStore, e: Entity) !void {
                        try ecs.remove_component(e, component);
                    }
                }).remove_component),
            });
        }

        pub fn delete(self: *@This(), entity: Entity) !void {
            try self.deleted.append(self.alloc.allocator(), entity);
        }
    };
};
