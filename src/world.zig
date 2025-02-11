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
    ecs: EntityComponentStore,

    pub fn init() !@This() {
        var self = @This(){ .ecs = try EntityComponentStore.init() };
        errdefer self.deinit();

        _ = try self.ecs.register([]const u8);
        _ = try self.ecs.register(Components.Transform);
        _ = try self.ecs.register(Components.Rigidbody);
        _ = try self.ecs.register(Components.Collider);
        _ = try self.ecs.register(Components.TimeDespawn);
        _ = try self.ecs.register(Components.StaticRender);
        _ = try self.ecs.register(Components.AnimatedRender);

        return self;
    }

    pub fn deinit(self: *@This()) void {
        self.ecs.deinit();
    }

    pub fn tick(self: *@This(), state: *AppState, delta: f32) !void {
        var it = try self.ecs.iterator(struct { r: Components.Rigidbody });
        while (it.next()) |e| {
            if (!e.r.flags.pinned and !e.r.flags.player) {
                const g = state.camera.world_basis.up.scale(-e.r.mass * 9.8);
                e.r.force = e.r.force.add(g);
            }
        }

        try self.step(delta);

        it.reset();
        while (it.next()) |e| {
            e.r.force = .{};
        }
    }

    pub fn step(self: *@This(), delta: f32) !void {
        {
            var it = try self.ecs.iterator(struct { r: Components.Rigidbody });
            while (it.next()) |e| {
                if (e.r.flags.pinned) continue;
                e.r.vel = e.r.vel.add(e.r.force.scale(1 / e.r.mass).scale(delta));
            }
        }

        var collisions = std.ArrayList(EntityCollider.CollisionEntity).init(allocator);
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

        const rba = entity.a.rigidbody;
        const rbb = entity.b.rigidbody;

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
        const rba = entity.entity.a.rigidbody;
        const rbb = entity.entity.b.rigidbody;
        const col = entity.entity.collision;

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

    pub const Rigidbody = struct {
        flags: packed struct {
            pinned: bool = false,
            player: bool = false,
        } = .{},
        vel: Vec4 = .{},
        force: Vec4 = .{},
        restitution: f32 = 1.0,
        mass: f32 = 1.0,
        static_friction: f32 = 0.5,
        dynamic_friction: f32 = 0.5,
    };

    pub const TimeDespawn = struct {
        despawn_time: f32,
    };

    pub const StaticRender = struct {
        mesh: GpuResourceManager.MeshResourceHandle,
    };

    pub const AnimatedRender = struct {
        model: GpuResourceManager.ModelHandle,

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
    generation: u32,
    index: u32,
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

            switch (std.math.order(a.id, b.id)) {
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
};

// represents a type of entity (a fixed sorted set of components)
pub const Archetype = struct {
    typ: Type,
    // type erased block of components (same length and order as self.typ.components)
    components: []std.ArrayListAligned(u8, 8),
    count: u32 = 0,
    generation: u32 = 0,

    pub fn from_type(typ: *const Type) !@This() {
        const components = try allocator.alloc(std.ArrayListAligned(u8, 8), typ.components.len);
        errdefer allocator.free(components);
        for (components) |*comp| {
            comp.* = std.ArrayListAligned(u8, 8).init(allocator);
        }
        return .{
            .typ = .{ .components = try allocator.dupe(ComponentId, typ.components) },
            .components = components,
        };
    }

    pub fn swap_remove(self: *@This(), vtables: []EntityComponentStore.ComponentVtable, index: usize) void {
        for (self.components, self.typ.components) |*comp, compid| {
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
        self.generation += 1;
    }

    pub fn deinit(self: *@This(), vtables: []EntityComponentStore.ComponentVtable) void {
        for (self.components, self.typ.components) |*comp, compid| {
            var i: usize = 0;
            while ((i + 1) * compid.size < comp.items.len) : (i += 1) {
                vtables[compid.id].maybe_deinit(comp.items[i * compid.size ..][0..compid.size].ptr);
            }
            comp.deinit();
        }
        allocator.free(self.components);

        self.typ.deinit();
    }
};

// ECS lol
pub const EntityComponentStore = struct {
    // - [Building an ECS #1](https://ajmmertens.medium.com/building-an-ecs-1-where-are-my-entities-and-components-63d07c7da742)

    entities: std.AutoArrayHashMap(Entity, ArchetypeEntity),
    archetypes: std.ArrayList(Archetype),
    // not all types are components. we register components so that we can do cooler comptime stuff
    // another idea could be to have a fixed enum of all possible components, and having a fixed enum varient per component
    // directly defined in the component as 'const component_type = .transform'
    //   - note: these enums can be merged into 1
    components: std.AutoArrayHashMap(TypeId, ComponentId),
    vtables: Vtables,
    // Entity's component id
    entityid_component_id: ComponentId,
    entity_id: u32 = 0,

    archetype_map: ArchetypeMap,
    component_map: std.AutoArrayHashMap(ComponentId, std.ArrayList(ArchetypeId)),

    const Vtables = std.ArrayList(ComponentVtable);
    const ComponentVtable = struct {
        deinit: ?*const fn (ptr: *anyopaque) void,

        fn from(component: type) @This() {
            switch (component) {
                []const u8 => return .{ .deinit = null },
                else => return .{
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

    pub fn init() !@This() {
        var self = @This(){
            .entities = std.AutoArrayHashMap(Entity, ArchetypeEntity).init(allocator),
            .archetypes = std.ArrayList(Archetype).init(allocator),
            .components = std.AutoArrayHashMap(TypeId, ComponentId).init(allocator),
            .archetype_map = ArchetypeMap.init(allocator),
            .component_map = std.AutoArrayHashMap(ComponentId, std.ArrayList(ArchetypeId)).init(allocator),
            .vtables = Vtables.init(allocator),
            .entityid_component_id = undefined,
        };
        errdefer self.deinit();

        self.entityid_component_id = try self.register(Entity);

        return self;
    }

    pub fn deinit(self: *@This()) void {
        self.entities.deinit();

        for (self.archetypes.items) |*a| {
            a.deinit(self.vtables.items);
        }
        self.archetypes.deinit();

        self.components.deinit();
        self.vtables.deinit();

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
        const compid = ComponentId{
            .id = @intCast(comp_id),
            .size = @intCast(@sizeOf(component)),
        };
        if (!comp.found_existing) {
            comp.value_ptr.* = compid;
            try self.vtables.append(ComponentVtable.from(component));
        }
        return compid;
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
        std.mem.sort(ComponentId, &components, {}, ComponentId.lessThan);
        return components;
    }

    pub fn insert(self: *@This(), components: anytype) !Entity {
        const component_ids = &try self.component_ids_sorted_from(@TypeOf(components));
        const typ = Type{ .components = [_]ComponentId{self.entityid_component_id} ++ component_ids };

        const archeid = self.archetype_map.get(typ) orelse blk: {
            var archetype = try Archetype.from_type(&typ);
            errdefer archetype.deinit(self.vtables.items);

            const archeid = self.archetypes.items.len;
            try self.archetypes.append(archetype);

            try self.archetype_map.put(archetype.typ, @intCast(archeid));

            break :blk archeid;
        };
        const archetype = &self.archetypes.items[archeid];
        defer archetype.count += 1;

        inline for (@typeInfo(@TypeOf(components)).Struct.fields) |field| {
            const compid = try self.get_component_id(field.type);
            const compi = archetype.typ.index(compid) orelse unreachable;
            const f = @field(components, field.name);
            const bytes = std.mem.asBytes(&f);
            try archetype.components[compi].appendSlice(bytes);
        }

        // TODO: think of a better way to generate unique entity ids?
        // generation kida does not do anything rn.
        const eid = Entity{ .generation = archetype.generation, .index = self.entity_id };
        self.entity_id += 1;

        {
            const compi = typ.index(self.entityid_component_id) orelse unreachable;
            const bytes = std.mem.asBytes(&eid);
            try archetype.components[compi].appendSlice(bytes);
            try self.entities.put(eid, .{ .archetype = @intCast(archeid), .entity_index = @intCast(archetype.count) });
        }

        return eid;
    }

    pub fn get(self: *@This(), entity: Entity, comptime T: type) !Type.pointer(T) {
        var t: Type.pointer(T) = undefined;

        const ae = self.entities.get(entity) orelse return error.EntityNotFound;
        const archetype = &self.archetypes.items[ae.archetype];

        inline for (@typeInfo(T).Struct.fields) |field| {
            const compid = try self.get_component_id(field.type);
            const compi = archetype.typ.index(compid) orelse unreachable;
            const val = std.mem.bytesAsValue(field.type, archetype.components[compi].items[ae.entity_index * compid.size ..][0..compid.size]);
            @field(t, field.name) = @alignCast(val);
        }

        return t;
    }

    pub fn remove(self: *@This(), entity: Entity) !void {
        const ae = self.entities.fetchSwapRemove(entity) orelse return error.EntityNotFound;
        const archetype = &self.archetypes.items[ae.value.archetype];
        defer archetype.swap_remove(self.vtables.items, ae.value.entity_index);

        if (archetype.count - 1 != ae.value.entity_index) {
            const entity_ci = archetype.typ.index(self.entityid_component_id) orelse unreachable;
            const val = std.mem.bytesAsValue(Entity, archetype.components[entity_ci].items[(archetype.count - 1) * self.entityid_component_id.size ..]);
            self.entities.getEntry(val.*).?.value_ptr.entity_index = ae.value.entity_index;
        }
    }

    pub fn iterator(self: *@This(), comptime typ: type) !EntityIterator(typ) {
        const ids = try self.component_ids_from(typ);
        var sorted_ids = ids;
        std.mem.sort(ComponentId, &sorted_ids, {}, ComponentId.lessThan);

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
                        inline for (fields, curr.ids, self.ids) |field, ci, compid| {
                            const slice = self.archetypes[curr.archetype].components[ci].items;

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
                self.archetype_it.reset();
                self.current = null;
            }
        };
    }
};
