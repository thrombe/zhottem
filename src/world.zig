const std = @import("std");

const math = @import("math.zig");
const Vec4 = math.Vec4;

const utils_mod = @import("utils.zig");
const TypeId = utils_mod.TypeId;

const app = @import("app.zig");
const AppState = app.AppState;

const resources_mod = @import("resources.zig");
const GpuResourceManager = resources_mod.GpuResourceManager;

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
                // e.transform.pos = state.camera.pos;
            } else if (!e.rigidbody.flags.pinned) {
                e.rigidbody.force = state.camera.world_basis.up.scale(-e.rigidbody.mass * 9.8);
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
            rb.vel = rb.vel.add(rb.force.scale(1 / rb.mass).scale(delta));
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
        // - [fix objects jittering in physics engine](https://gamedev.stackexchange.com/questions/53991/how-do-i-fix-objects-popping-or-jittering-in-physics-engine)
        const slop = 0.01;
    };

    pub fn solve(entity: *PositionEntity) void {
        const rba = &entity.entity.a.rigidbody;
        const rbb = &entity.entity.b.rigidbody;
        const col = &entity.entity.collision;

        const del = col.normal.scale(@max(col.depth - constants.slop, 0));

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

        pub fn raycast(self: @This(), transform: *const Transform, ro: Vec4, rd: Vec4) ?f32 {
            var this = self;
            switch (this) {
                .sphere => |*s| {
                    s.center = s.center.add(transform.pos);
                    s.radius *= transform.scale.max_v3();

                    const oc = ro.sub(s.center);
                    const a = 1.0; // assuming rd is normalized
                    const b = 2.0 * oc.dot(rd);
                    const c = oc.dot(oc) - s.radius * s.radius;

                    const d = b * b - 4 * a * c;

                    if (d < 0) {
                        return null;
                    }

                    if (-b - d > 0) {
                        return (-b - d) / (2 * a);
                    }

                    return (-b + d) / (2 * a);
                },
                .plane => |p| {
                    _ = p;
                    return null;
                },
            }
        }
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
    name: []const u8 = "none",
    typ: packed struct {
        player: bool = false,
        cube: bool = false,
        object: bool = false,
    },

    transform: Components.Transform = .{},
    collider: Components.Collider = .{ .sphere = .{ .radius = 1 } },
    rigidbody: Components.Rigidbody = .{ .flags = .{} },
    despawn_time: ?f32 = null,

    mesh: ?GpuResourceManager.MeshResourceHandle,
};

pub const EntityId = struct {
    generation: u32,
    index: u32,
};

pub const ArchetypeEntity = struct {
    archetype: ArchetypeId,
    // entity's index into archetype's components[i].items
    entity_index: usize,
};

// index into self.archetypes
pub const ArchetypeId = usize;

// represents 1 component
pub const ComponentId = u32;

pub const Type = struct {
    components: []const ComponentId,

    pub fn deinit(self: *@This()) void {
        allocator.free(self.components);
    }

    pub fn index(self: *const @This(), component: ComponentId) ?usize {
        // there will never be more than like 20 items here. linear search should be faster
        for (self.components, 0..) |comp, i| {
            if (std.meta.eql(comp, component)) {
                return i;
            }
        }

        return null;
    }

    // ids must be sorted
    pub fn has_components(self: *const @This(), ids: []const ComponentId) bool {
        var i: usize = 0;
        var j: usize = 0;
        while (self.components.len > i and ids.len > j) {
            const a = ids[j];
            const b = self.components[i];

            switch (std.math.order(a, b)) {
                .eq => {
                    i += 1;
                    j += 1;
                },
                .lt => {
                    return false;
                },
                .gt => {
                    i += 1;
                },
            }
        }

        return ids.len == j;
    }

    // from a struct to a struct to the same struct but all fields are pointers
    pub fn pointer(comptime typ: type) type {
        const StructField = std.builtin.Type.StructField;
        const input_val: typ = undefined;
        const input_struct = @typeInfo(typ).Struct;

        comptime {
            var fields: [input_struct.fields.len]StructField = undefined;
            @memcpy(&fields, input_struct.fields);
            for (&fields) |*field| {
                field.default_value = null;
                field.type = @TypeOf(&@field(input_val, field.name));
            }

            return @Type(.{ .Struct = .{
                .layout = .auto,
                .fields = &fields,
                .decls = &[_]std.builtin.Type.Declaration{},
                .is_tuple = input_struct.is_tuple,
            } });
        }
    }
};

// represents a type of entity (a fixed sorted set of components)
pub const Archetype = struct {
    typ: Type,
    // type erased block of components (same length and order as self.typ.components)
    components: []std.ArrayListAligned(u8, 4),

    fn from_type(typ: *const Type) !@This() {
        const components = try allocator.alloc(std.ArrayListAligned(u8, 4), typ.components.len);
        errdefer allocator.free(components);
        for (components) |*comp| {
            comp.* = std.ArrayListAligned(u8, 4).init(allocator);
        }
        return .{
            .typ = .{ .components = try allocator.dupe(ComponentId, typ.components) },
            .components = components,
        };
    }

    pub fn deinit(self: *@This()) void {
        for (self.components) |*comp| {
            comp.deinit();
        }
        allocator.free(self.components);

        self.edges.deinit();
    }
};

// ECS lol
pub const EntityComponentStore = struct {
    // - [Building an ECS #1](https://ajmmertens.medium.com/building-an-ecs-1-where-are-my-entities-and-components-63d07c7da742)

    entities: std.AutoArrayHashMap(EntityId, ArchetypeEntity),
    archetypes: std.ArrayList(Archetype),
    // not all types are components. we register components so that we can do cooler comptime stuff
    // another idea could be to have a fixed enum of all possible components, and having a fixed enum varient per component
    // directly defined in the component as 'const component_type = .transform'
    //   - note: these enums can be merged into 1
    components: std.AutoArrayHashMap(TypeId, ComponentId),

    archetype_map: ArchetypeMap,
    component_map: std.AutoArrayHashMap(ComponentId, std.ArrayList(ArchetypeId)),

    const ArchetypeMap = std.ArrayHashMap(Type, ArchetypeId, struct {
        pub fn hash(ctx: @This(), key: Type) u32 {
            _ = ctx;
            var hasher = std.hash.Wyhash.init(0);
            for (key.components) |*comp| {
                hasher.update(std.mem.asBytes(comp));
            }
            return @truncate(hasher.final());
        }
        pub fn eql(ctx: @This(), a: Type, b: Type, b_index: usize) bool {
            _ = b_index;
            _ = ctx;
            return std.mem.eql(ComponentId, a.components, b.components);
        }
    }, true);

    pub fn init() @This() {
        return @This(){
            .entities = std.AutoArrayHashMap(EntityId, ArchetypeEntity).init(allocator),
            .archetypes = std.ArrayList(Archetype).init(allocator),
            .components = std.AutoArrayHashMap(TypeId, ComponentId).init(allocator),
            .archetype_map = ArchetypeMap.init(allocator),
            .component_map = std.AutoArrayHashMap(ComponentId, std.ArrayList(ArchetypeId)).init(allocator),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.entities.deinit();
        self.components.deinit();

        for (self.archetypes.items) |*a| {
            a.deinit();
        }
        self.archetypes.deinit();

        // NOTE: these values are owned by self.archetypes[].typ
        // for (self.archetype_map.keys()) |*k| {
        //     k.deinit();
        // }
        self.archetype_map.deinit();

        for (self.component_map.values()) |*list| {
            list.deinit();
        }
        self.component_map.deinit();
    }

    pub fn register(self: *@This(), component: type) !ComponentId {
        const type_id = TypeId.from(component);
        const comp_id = self.components.count();
        const comp = try self.components.getOrPut(type_id);
        if (!comp.found_existing) {
            comp.value_ptr.* = @intCast(comp_id);
        }
        return comp.value_ptr.*;
    }

    pub fn get_component_id(self: *@This(), component: type) !ComponentId {
        const type_id = TypeId.from(component);
        return self.components.get(type_id) orelse error.TypeNotAComponent;
    }

    fn component_ids_from(self: *@This(), typ: type) ![@typeInfo(typ).Struct.fields.len]ComponentId {
        const fields = @typeInfo(typ).Struct.fields;
        var components: [fields.len]ComponentId = undefined;

        inline for (fields, 0..) |*field, i| {
            const e = self.components.getEntry(TypeId.from(field.type));
            if (e) |entry| {
                components[i] = entry.value_ptr.*;
            } else {
                return error.FieldNotAComponent;
            }
        }

        return components;
    }

    fn component_ids_sorted_from(self: *@This(), typ: type) ![@typeInfo(typ).Struct.fields.len]ComponentId {
        var components = try self.component_ids_from(typ);
        std.mem.sort(ComponentId, &components, {}, std.sort.asc(ComponentId));
        return components;
    }

    pub fn insert(self: *@This(), components: anytype) !EntityId {
        const component_ids = &try self.component_ids_sorted_from(@TypeOf(components));
        const typ = Type{ .components = component_ids };

        const archeid = self.archetype_map.get(typ) orelse blk: {
            var archetype = try Archetype.from_type(&typ);
            errdefer archetype.deinit();

            const archeid = self.archetypes.items.len;
            try self.archetypes.append(archetype);

            try self.archetype_map.put(archetype.typ, archeid);

            break :blk archeid;
        };
        const archetype = &self.archetypes.items[archeid];

        var ei: u32 = undefined;
        inline for (@typeInfo(@TypeOf(components)).Struct.fields) |field| {
            const compid = try self.get_component_id(field.type);
            const compi = typ.index(compid) orelse unreachable;
            const f = @field(components, field.name);
            const bytes = std.mem.asBytes(&f);
            try archetype.components[compi].appendSlice(bytes);

            ei = @intCast(archetype.components[compi].items.len / bytes.len);
        }

        const eid = EntityId{ .generation = 0, .index = @intCast(self.entities.count()) };
        try self.entities.put(eid, .{ .archetype = archeid, .entity_index = ei - 1 });
        errdefer self.entities.pop();

        return eid;
    }

    pub fn get(self: *@This(), entity: EntityId, comptime T: type) !Type.pointer(T) {
        const typ = Type{ .components = &try self.component_ids_sorted_from(T) };
        var t: Type.pointer(T) = undefined;

        const ae = self.entities.get(entity) orelse return error.EntityNotFound;
        const archetype = &self.archetypes.items[ae.archetype];

        inline for (@typeInfo(T).Struct.fields) |field| {
            const compid = try self.get_component_id(field.type);
            const compi = typ.index(compid) orelse unreachable;
            const slice = std.mem.bytesAsSlice(field.type, archetype.components[compi].items);
            @field(t, field.name) = &slice[entity.index];
        }

        return t;
    }

    pub fn iterator(self: *@This(), comptime typ: type) !EntityIterator(typ) {
        const ids = try self.component_ids_from(typ);
        var sorted_ids = ids;
        std.mem.sort(ComponentId, &sorted_ids, {}, std.sort.asc(ComponentId));

        return .{
            .ids = ids,
            .sorted_ids = sorted_ids,
            .archetype_it = self.archetype_map.iterator(),
            .archetypes = self.archetypes.items,
            .current = null,
        };
    }

    pub fn EntityIterator(typ: type) type {
        return struct {
            const fields = @typeInfo(typ).Struct.fields;
            const len = fields.len;

            // same order as that of fields
            ids: [len]ComponentId,
            sorted_ids: [len]ComponentId,
            archetype_it: ArchetypeMap.Iterator,
            archetypes: []Archetype,
            current: ?struct {
                archetype: ArchetypeId,
                // indices into archetype.components
                // same order as that of fields
                ids: [len]usize,
                // current index into archetype.components[].items
                index: usize = 0,
            },

            pub fn next(self: *@This()) ?Type.pointer(typ) {
                outer: while (true) {
                    if (self.current) |*curr| inner: {
                        var t: Type.pointer(typ) = undefined;
                        inline for (fields, curr.ids) |field, ci| {
                            const slice = std.mem.bytesAsSlice(field.type, self.archetypes[curr.archetype].components[ci].items);

                            if (curr.index >= slice.len) {
                                self.current = null;
                                break :inner;
                            }

                            @field(t, field.name) = &slice[curr.index];
                        }
                        curr.index += 1;
                        return t;
                    }

                    while (self.archetype_it.next()) |e| {
                        if (!e.key_ptr.has_components(&self.sorted_ids)) {
                            continue;
                        }

                        var ids: [len]usize = undefined;
                        inline for (self.ids, 0..) |cid, i| {
                            ids[i] = e.key_ptr.index(cid) orelse unreachable;
                        }

                        self.current = .{
                            .archetype = e.value_ptr.*,
                            .ids = ids,
                        };
                        continue :outer;
                    }

                    return null;
                }
            }

            pub fn reset(self: *@This()) void {
                self.archetypes.reset();
                self.current_archetype = null;
            }
        };
    }
};
