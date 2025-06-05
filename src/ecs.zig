const std = @import("std");

const math = @import("math.zig");
const Vec4 = math.Vec4;

const utils_mod = @import("utils.zig");

const main = @import("main.zig");
const allocator = main.allocator;

// ECS lol
pub fn EntityComponentStore(_component_decls: type, context_type: type) type {
    return struct {
        // - [Building an ECS #1](https://ajmmertens.medium.com/building-an-ecs-1-where-are-my-entities-and-components-63d07c7da742)

        pub const component_decls = struct {
            pub usingnamespace _component_decls;
            pub const Entity = EntityId;
        };
        pub const component_list = &utils_mod.type_array_from_struct_decls(component_decls);
        pub const component_sizes = get_component_sizes(component_list);
        pub const component_vtables = get_component_vtables(component_list);
        pub const entityid_component_id = get_component_id(EntityId);

        entities: Entities,
        archetypes: std.ArrayList(Archetype),
        archetype_map: ArchetypeMap,
        // component_map: std.AutoHashMap(ComponentId, std.ArrayList(ArchetypeId)),

        const Ecs = @This();
        const Ctx = context_type;

        pub fn init(ctx: *anyopaque) !@This() {
            var self = @This(){
                .entities = try .init(allocator.*),
                .archetypes = .init(allocator.*),
                .archetype_map = .init(allocator.*),
                // .component_map = .init(allocator.*),
            };
            errdefer self.deinit(ctx);

            return self;
        }

        pub fn deinit(self: *@This(), ctx: Ctx) void {
            self.entities.deinit();

            for (self.archetypes.items) |*a| {
                a.deinit(ctx);
            }
            self.archetypes.deinit();

            self.archetype_map.deinit();

            // for (self.component_map.values()) |*list| {
            //     list.deinit();
            // }
            // self.component_map.deinit();
        }

        inline fn get_component_id(component: type) ComponentId {
            comptime {
                const index = utils_mod.indexof_type(component_list, component) orelse {
                    @compileError("type '" ++ @typeName(component) ++ "' is not a component");
                };
                return .{ .id = @intCast(index) };
            }
        }

        inline fn component_ids_from(typ: type) [@typeInfo(typ).@"struct".fields.len]ComponentId {
            comptime {
                const fields = @typeInfo(typ).@"struct".fields;
                var components: [fields.len]ComponentId = undefined;

                for (fields, 0..) |*field, i| {
                    components[i] = get_component_id(field.type);
                }

                return components;
            }
        }

        inline fn components_from(typ: type) Type.Components {
            comptime {
                const components = component_ids_from(typ);
                return Type.from(&components).components;
            }
        }

        inline fn get_component_sizes(components: anytype) [components.len]u16 {
            comptime {
                var sizes: [components.len]u16 = undefined;
                for (components, 0..) |typ, i| {
                    sizes[i] = @intCast(@sizeOf(typ));
                }
                return sizes;
            }
        }

        inline fn get_component_vtables(components: anytype) [components.len]ComponentVtable {
            comptime {
                var vtables: [components.len]ComponentVtable = undefined;
                for (components, 0..) |typ, i| {
                    vtables[i] = ComponentVtable.from(typ);
                }
                return vtables;
            }
        }

        fn get_archetype(self: *@This(), typ: Type, ctx: Ctx) !ArchetypeId {
            const archeid = self.archetype_map.get(typ) orelse blk: {
                var archetype = try Archetype.from_type(typ);
                errdefer archetype.deinit(ctx);

                const archeid: ArchetypeId = @intCast(self.archetypes.items.len);
                try self.archetypes.append(archetype);

                try self.archetype_map.put(archetype.typ, @intCast(archeid));

                break :blk archeid;
            };
            return archeid;
        }

        fn insert_reserved(self: *@This(), eid: EntityId, components: anytype, ctx: Ctx) !void {
            const component_ids = components_from(@TypeOf(components));
            const _typ = (Type{ .components = component_ids });
            const typ = _typ.inserted(entityid_component_id).?;

            const archeid = try self.get_archetype(typ, ctx);
            const archetype = &self.archetypes.items[archeid];
            defer archetype.count += 1;

            inline for (@typeInfo(@TypeOf(components)).@"struct".fields) |field| {
                const f = @field(components, field.name);
                if (comptime field.type == EntityId) {
                    std.debug.assert(std.meta.eql(eid, f));
                }

                const compid = get_component_id(field.type);
                const compi = archetype.typ.index(compid).?;
                const bytes = std.mem.asBytes(&f);
                try archetype.components[compi].appendSlice(bytes);
            }

            if (!_typ.components.eql(typ.components)) {
                const compi = archetype.typ.index(entityid_component_id).?;
                const bytes = std.mem.asBytes(&eid);
                try archetype.components[compi].appendSlice(bytes);
                try self.entities.set(eid, .{ .archetype = @intCast(archeid), .entity_index = @intCast(archetype.count) });
            }
        }

        pub fn insert(self: *@This(), components: anytype, ctx: Ctx) !EntityId {
            const eid = try self.entities.new();
            try self.insert_reserved(eid, components, ctx);
            return eid;
        }

        pub fn get(self: *@This(), entity: EntityId, comptime T: type) !Type.pointer(T) {
            var t: Type.pointer(T) = undefined;

            const ae = self.entities.get(entity) orelse return error.EntityNotFound;
            const archetype = &self.archetypes.items[ae.archetype];

            inline for (@typeInfo(T).@"struct".fields) |field| {
                const compid = get_component_id(field.type);
                const compi = archetype.typ.index(compid).?;
                const val = std.mem.bytesAsValue(field.type, archetype.components[compi].items[ae.entity_index * compid.size() ..][0..compid.size()]);
                @field(t, field.name) = @alignCast(val);
            }

            return t;
        }

        fn swap_remove(self: *@This(), ae: ArchetypeEntity, ctx: Ctx, deleted: bool) void {
            const archetype = &self.archetypes.items[ae.archetype];
            defer archetype.swap_remove(ae.entity_index, ctx, deleted);

            if (archetype.count - 1 != ae.entity_index) {
                const entity_ci = archetype.typ.index(entityid_component_id).?;
                const val = std.mem.bytesAsValue(EntityId, archetype.components[entity_ci].items[(archetype.count - 1) * entityid_component_id.size() ..]);
                self.entities.get(val.*).?.entity_index = ae.entity_index;
            }
        }

        pub fn delete(self: *@This(), entity: EntityId, ctx: Ctx) !void {
            const ae = (try self.entities.pop(entity)) orelse return error.EntityNotFound;
            self.swap_remove(ae, ctx, true);
        }

        pub fn add_component(self: *@This(), entity: EntityId, component: anytype, ctx: Ctx) !void {
            const compid = get_component_id(@TypeOf(component));
            const ae = self.entities.get(entity) orelse return error.EntityNotFound;

            const archeid = blk: {
                const curr_archetype = &self.archetypes.items[ae.archetype];

                const edge = try curr_archetype.edges.getOrPut(compid);
                if (edge.found_existing) {
                    break :blk edge.value_ptr.*;
                } else {
                    const typ = curr_archetype.typ.inserted(compid) orelse {
                        std.debug.print("duplicate component on entity: {s}\n", .{@typeName(@TypeOf(component))});
                        return error.ComponentAlreadyPresent;
                    };
                    const archeid = try self.get_archetype(typ, ctx);
                    edge.value_ptr.* = archeid;
                    break :blk archeid;
                }
            };

            const curr_archetype = &self.archetypes.items[ae.archetype];
            const archetype = &self.archetypes.items[archeid];
            defer {
                self.swap_remove(ae.*, ctx, false);
                ae.* = .{
                    .archetype = archeid,
                    .entity_index = archetype.count,
                };
                archetype.count += 1;
            }

            var i: usize = 0;
            var j: usize = 0;
            var it = archetype.typ.iterator();
            while (archetype.components.len > i) : (i += 1) {
                const curr_compid = it.next().?;

                if (std.meta.eql(compid, curr_compid)) {
                    try archetype.components[i].appendSlice(std.mem.asBytes(&component));
                } else {
                    defer j += 1;
                    const comp_mem = curr_archetype.components[j].items[ae.entity_index * curr_compid.size() ..][0..curr_compid.size()];
                    try archetype.components[i].appendSlice(comp_mem);
                }
            }
        }

        pub fn remove_component(self: *@This(), entity: EntityId, component: type, ctx: Ctx) !void {
            const compid = get_component_id(component);
            const ae = self.entities.get(entity) orelse return error.EntityNotFound;
            const archeid = blk: {
                const curr_archetype = &self.archetypes.items[ae.archetype];

                const edge = try curr_archetype.edges.getOrPut(compid);
                if (edge.found_existing) {
                    break :blk edge.value_ptr.*;
                } else {
                    const typ = (try curr_archetype.typ.removed(compid)) orelse return error.ComponentNotPresent;
                    const archeid = try self.get_archetype(typ, ctx);
                    edge.value_ptr.* = archeid;
                    break :blk archeid;
                }
            };

            const curr_archetype = &self.archetypes.items[ae.archetype];
            const archetype = &self.archetypes.items[archeid];
            defer {
                self.swap_remove(ae.*, ctx, false);
                ae.* = .{
                    .archetype = archeid,
                    .entity_index = archetype.count,
                };
                archetype.count += 1;
            }

            var i: usize = 0;
            var j: usize = 0;
            var it = curr_archetype.typ.iterator();
            while (curr_archetype.components.len > j) : (j += 1) {
                const curr_compid = it.next().?;

                const comp_mem = curr_archetype.components[j].items[ae.entity_index * curr_compid.size() ..][0..curr_compid.size()];
                if (std.meta.eql(compid, curr_compid)) {
                    component_vtables[compid.id].maybe_deinit(@ptrCast(comp_mem.ptr), ctx);
                } else {
                    defer i += 1;
                    try archetype.components[i].appendSlice(comp_mem);
                }
            }
        }

        pub fn overwrite_component(self: *@This(), entity: EntityId, component: anytype, ctx: Ctx) !void {
            const compid = get_component_id(@TypeOf(component));
            const ae = self.entities.get(entity) orelse return error.EntityNotFound;
            const archetype = &self.archetypes.items[ae.archetype];

            const ci = archetype.typ.index(compid).?;
            const comp_mem = archetype.components[ci].items[ae.entity_index * compid.size() ..][0..compid.size()];
            component_vtables[compid.id].maybe_deinit(@ptrCast(comp_mem.ptr), ctx);
            @memcpy(comp_mem, std.mem.asBytes(&component));
        }

        pub fn query(self: *@This(), comptime components: type) EntityQuery(components) {
            return .{ .ecs = self };
        }

        pub fn iterator(self: *@This(), comptime typ: type) EntityIterator(EntityQuery(typ)) {
            var q = self.query(typ);
            return q.iterator();
        }

        pub fn explorer(self: *@This(), entity: EntityId) !EntityExplorer {
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

        pub const EntityId = packed struct(u64) {
            index: u32,
            meta: Meta = .{},

            const Meta = packed struct(u32) {
                gen: u24 = 0,
                // for Entity struct outside the ecs, flags will always be 0
                // flags is there just for the ecs to track things.
                // flags will be set to 0 outside just to avoid desync between
                // actual Entity in ecs, and a random floating Entity struct
                flags: Flags = .{},
            };
            const Flags = packed struct(u8) {
                disabled: bool = false,
                _reserved: u7 = 0,
            };
        };

        // represents 1 component
        pub const ComponentId = packed struct {
            id: u16,

            pub fn lessThan(_: void, a: ComponentId, b: ComponentId) bool {
                return a.id < b.id;
            }

            pub fn size(self: @This()) usize {
                return @intCast(component_sizes[self.id]);
            }
        };

        // index into self.archetypes
        pub const ArchetypeId = u32;
        pub const ArchetypeEntity = struct {
            archetype: ArchetypeId,
            // entity's index into archetype's components[i].items
            entity_index: usize,
        };
        // arrayhashmap cuz ecs.iterate() needs to iterate this. (?)
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
                return a.components.eql(b.components);
            }
        }, true);

        pub const ComponentVtable = struct {
            name: []const u8,
            deinit: ?*const fn (ptr: *anyopaque) void = null,
            deinit_with_context: ?*const fn (ptr: *anyopaque, ctx: Ctx) void = null,

            fn from(component: type) @This() {
                const vtable: @This() = switch (component) {
                    []const u8 => .{ .name = @typeName(component) },
                    else => .{
                        .name = @typeName(component),
                        .deinit = if (comptime @hasDecl(component, "deinit")) @ptrCast(&component.deinit) else null,
                        .deinit_with_context = if (comptime @hasDecl(component, "deinit_with_context")) @ptrCast(&component.deinit_with_context) else null,
                    },
                };

                return vtable;
            }

            fn maybe_deinit(self: *const @This(), ptr: *anyopaque, ctx: Ctx) void {
                if (self.deinit_with_context) |deinitfn| {
                    deinitfn(ptr, ctx);
                } else if (self.deinit) |deinitfn| {
                    deinitfn(ptr);
                }
            }
        };

        // @ctz()
        // @clz()
        // @popCount()
        pub const Type = struct {
            components: Components,

            pub const Components = std.StaticBitSet(component_list.len);

            comptime {
                //  - it is a single vector instruction to do 'or' and 'and' with std.StaticBitSet(256).
                //    source: i checked assembly: https://godbolt.org/z/MMaej9Yvx
                std.debug.assert(component_list.len <= 256);
            }

            pub inline fn from(components: []const ComponentId) @This() {
                var this: @This() = .{ .components = .initEmpty() };
                for (components) |comp| {
                    this.components.set(@intCast(comp.id));
                }
                std.debug.assert(components.len == this.components.count());
                return this;
            }

            // have a biset of components in each Type
            // mask out all components with bigger or equal ids: bitset & ((1 << compid.id) - 1)
            // count the number of bits still set
            pub fn index(self: *const @This(), component: ComponentId) ?usize {
                if (self.components.isSet(@intCast(component.id))) {
                    var components = self.components;
                    components.setRangeValue(.{ .start = component.id, .end = self.components.capacity() }, false);
                    return components.count();
                } else {
                    return null;
                }
            }

            pub fn count(self: *const @This()) usize {
                return self.components.count();
            }

            pub fn has_components(self: *const @This(), components: Components) bool {
                return self.components.supersetOf(components);
            }

            pub inline fn removed(self: *const @This(), compid: ComponentId) ?@This() {
                var this = self.*;
                if (!this.components.isSet(@intCast(compid.id))) return null;
                this.components.setValue(@intCast(compid.id), false);
                return this;
            }

            pub inline fn inserted(self: *const @This(), compid: ComponentId) ?@This() {
                var this = self.*;
                if (this.components.isSet(@intCast(compid.id))) return null;
                this.components.setValue(@intCast(compid.id), true);
                return this;
            }

            pub fn iterator(self: *@This()) ComponentIterator {
                return .{ .components = self.components.iterator(.{}) };
            }

            // from a struct to a struct to the same struct but all fields are pointers
            pub fn pointer(comptime typ: type) type {
                const StructField = std.builtin.Type.StructField;
                var input_val: typ = undefined;
                const input_struct = @typeInfo(typ).@"struct";

                comptime {
                    var fields: [input_struct.fields.len]StructField = undefined;
                    @memcpy(&fields, input_struct.fields);
                    for (&fields) |*field| {
                        // field.default_value = null;
                        const t: *field.type = &@field(input_val, field.name);
                        field.type = @TypeOf(t);
                    }

                    return @Type(.{ .@"struct" = .{
                        .layout = .auto,
                        .fields = &fields,
                        .decls = &[_]std.builtin.Type.Declaration{},
                        .is_tuple = input_struct.is_tuple,
                    } });
                }
            }

            pub const ComponentIterator = struct {
                components: Components.Iterator(.{}),

                pub fn next(self: *@This()) ?ComponentId {
                    if (self.components.next()) |i| {
                        return .{ .id = @intCast(i) };
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

            const ArchetypeEdges = std.AutoHashMap(ComponentId, ArchetypeId);
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

            pub fn swap_remove(self: *@This(), index: usize, ctx: Ctx, deleted: bool) void {
                var it = self.typ.iterator();

                for (self.components) |*comp| {
                    const compid = it.next().?;

                    const to_delete = comp.items[index * compid.size() ..][0..compid.size()];
                    if (deleted) component_vtables[compid.id].maybe_deinit(@ptrCast(to_delete.ptr), ctx);

                    if ((index + 1) * compid.size() != comp.items.len) {
                        const last = comp.items[comp.items.len - compid.size() ..];
                        @memcpy(to_delete, last);
                    }

                    // arraylist.pop() just does self.items.len -= 1
                    comp.items.len -= compid.size();
                }
                self.count -= 1;
            }

            pub fn deinit(self: *@This(), ctx: Ctx) void {
                var it = self.typ.iterator();

                for (self.components) |*comp| {
                    const compid = it.next().?;

                    var i: usize = 0;
                    while ((i + 1) * compid.size() <= comp.items.len) : (i += 1) {
                        component_vtables[compid.id].maybe_deinit(comp.items[i * compid.size() ..][0..compid.size()].ptr, ctx);
                    }
                    comp.deinit();
                }
                allocator.free(self.components);

                self.edges.deinit();
            }
        };

        pub const Entities = struct {
            values: std.ArrayList(struct {
                meta: EntityId.Meta = .{},
                aid: ArchetypeEntity,
            }),
            free_list: utils_mod.Deque(u32),

            pub fn init(alloc: std.mem.Allocator) !@This() {
                return .{
                    .values = .init(alloc),
                    .free_list = try .init(alloc),
                };
            }

            pub fn deinit(self: *@This()) void {
                self.values.deinit();
                self.free_list.deinit();
            }

            pub fn new(self: *@This()) !EntityId {
                if (self.free_list.pop_front()) |index| {
                    const e = &self.values.items[index];
                    // we don't need protection against accessing null values right? right?
                    // - easy enough to add a entity.meta.flags.initialized. but meh.
                    e.aid = std.mem.zeroes(@TypeOf(e.aid));
                    e.meta.flags = .{};
                    return .{
                        .meta = .{ .gen = e.meta.gen },
                        .index = index,
                    };
                } else {
                    const index = self.values.items.len;
                    try self.values.append(.{ .aid = std.mem.zeroes(ArchetypeEntity) });
                    return .{ .index = @intCast(index), .meta = .{} };
                }
            }

            pub fn set(self: *@This(), entity: EntityId, aid: ArchetypeEntity) !void {
                const e = &self.values.items[entity.index];
                if (e.meta.gen != entity.meta.gen) return error.GenerationMismatch;
                e.aid = aid;
            }

            pub fn get(self: *@This(), entity: EntityId) ?*ArchetypeEntity {
                const e = &self.values.items[entity.index];
                if (e.meta.gen != entity.meta.gen) return null;
                return &e.aid;
            }

            pub fn pop(self: *@This(), entity: EntityId) !?ArchetypeEntity {
                const e = &self.values.items[entity.index];
                if (entity.meta.gen != e.meta.gen) return null;
                const aid = e.aid;
                e.aid = std.mem.zeroes(@TypeOf(e.aid));
                e.meta.gen += 1;
                e.meta.flags = .{};
                try self.free_list.push_back(entity.index);
                return aid;
            }

            pub fn count(self: *@This()) usize {
                return self.values.items.len;
            }
        };

        pub const EntityExplorer = struct {
            // for queries like (does this entity have this component)
            // and for fast query of random components of this entity.

            ecs: *Ecs,
            archetype: *Archetype,
            entity_index: usize,

            pub fn get(self: *@This(), comptime T: type) Type.pointer(T) {
                var t: Type.pointer(T) = undefined;

                inline for (@typeInfo(T).@"struct".fields) |field| {
                    const compid = get_component_id(field.type);
                    const compi = self.archetype.typ.index(compid).?;
                    const val = std.mem.bytesAsValue(field.type, self.archetype.components[compi].items[self.entity_index * compid.size() ..][0..compid.size()]);
                    @field(t, field.name) = @alignCast(val);
                }

                return t;
            }

            pub fn get_component(self: *@This(), typ: type) ?*typ {
                const compid = get_component_id(typ);
                const compi = self.archetype.typ.index(compid) orelse return null;
                const val = std.mem.bytesAsValue(typ, self.archetype.components[compi].items[self.entity_index * compid.size() ..][0..compid.size()]);
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
                    compid: ComponentId,
                };

                pub fn next(self: *@This()) ?ComponentEntry {
                    const compid = self.it.next() orelse return null;
                    defer self.component_index += 1;

                    const component = self.explorer.archetype.components[self.component_index].items[self.explorer.entity_index * compid.size() ..][0..compid.size()];

                    return .{
                        .component = component,
                        .compid = compid,
                    };
                }

                pub fn reset(self: *@This()) void {
                    self.component_index = 0;
                    self.it = self.explorer.archetype.typ.iterator();
                }
            };
        };

        pub fn EntityQuery(_get: type) type {
            return NewEntityQuery(_get, struct {}, struct {});
        }

        pub fn NewEntityQuery(_get: type, _with: type, _without: type) type {
            return struct {
                pub const Get = _get;
                pub const With = _with;
                pub const Without = _without;

                ecs: *Ecs,

                pub fn with(self: @This(), comptime components: type) NewEntityQuery(Get, utils_mod.tuple_union(With, components), Without) {
                    comptime {
                        const comp_ids_with = component_ids_from(utils_mod.tuple_union(Get, With));
                        const comp_with = Type.from(&comp_ids_with).components;
                        const comp_ids_without = component_ids_from(Without);
                        const comp_without = Type.from(&comp_ids_without).components;
                        if (comp_with.intersectWith(comp_without).count() > 0) @compileError("BadTypeQuery");
                        return .{ .ecs = self.ecs };
                    }
                }

                pub fn without(self: @This(), comptime components: type) NewEntityQuery(Get, With, utils_mod.tuple_union(Without, components)) {
                    comptime {
                        const comp_ids_with = component_ids_from(utils_mod.tuple_union(Get, With));
                        const comp_with = Type.from(&comp_ids_with).components;
                        const comp_ids_without = component_ids_from(Without);
                        const comp_without = Type.from(&comp_ids_without).components;
                        if (comp_with.intersectWith(comp_without).count() > 0) @compileError("BadTypeQuery");
                        return .{ .ecs = self.ecs };
                    }
                }

                pub fn filter(components: Type.Components) bool {
                    if (comptime std.meta.fields(Without).len > 0) {
                        const comp_ids_without = comptime component_ids_from(Without);
                        const comp_without = comptime Type.from(&comp_ids_without).components;
                        if (comp_without.intersectWith(components).count() > 0) return false;
                    }

                    const comp_ids_with = comptime component_ids_from(utils_mod.tuple_union(Get, With));
                    const comp_with = comptime Type.from(&comp_ids_with).components;
                    return comp_with.subsetOf(components);
                }

                pub fn iterator(self: *@This()) EntityIterator(@This()) {
                    return .{
                        .archetype_it = self.ecs.archetype_map.iterator(),
                        .ecs = self.ecs,
                    };
                }
            };
        }

        pub fn EntityIterator(_query: type) type {
            return struct {
                pub const Query = _query;
                const fields = @typeInfo(Query.Get).@"struct".fields;
                const component_ids = component_ids_from(Query.Get);

                ecs: *Ecs,
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

                pub fn next(self: *@This()) ?Type.pointer(_query.Get) {
                    outer: while (true) {
                        if (self.current) |*curr| inner: {
                            var t: Type.pointer(Query.Get) = undefined;
                            const archetype = &self.ecs.archetypes.items[curr.archetype];
                            inline for (fields, component_ids) |field, compid| {
                                const ci = archetype.typ.index(compid).?;
                                const slice = archetype.components[ci].items;

                                if ((curr.index + 1) * compid.size() > slice.len) {
                                    self.current = null;
                                    break :inner;
                                }

                                const val = std.mem.bytesAsValue(field.type, slice[curr.index * compid.size() ..][0..compid.size()]);
                                @field(t, field.name) = @alignCast(val);
                            }
                            curr.index += 1;
                            return t;
                        }

                        while (self.archetype_it.next()) |e| {
                            if (!Query.filter(e.key_ptr.components)) {
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
            ecs: *Ecs,
            alloc: std.heap.ArenaAllocator,

            impl: struct {
                inserted: Inserted = .{},
                deleted: Deleted = .{},
                added_components: Added = .{},
                overwritten_components: Overwritten = .{},
                removed_components: Removed = .{},
            } = .{},

            const Inserted = std.ArrayListUnmanaged(struct {
                entity: EntityId,
                bundle: *anyopaque,
                insertfn: *const fn (*Ecs, EntityId, *anyopaque, Ctx) anyerror!void,
            });
            const Deleted = std.ArrayListUnmanaged(EntityId);
            const Added = std.ArrayListUnmanaged(struct {
                entity: EntityId,
                component: *anyopaque,
                addfn: *const fn (*Ecs, EntityId, *anyopaque, Ctx) anyerror!void,
            });
            const Overwritten = std.ArrayListUnmanaged(struct {
                entity: EntityId,
                component: *anyopaque,
                overwritefn: *const fn (*Ecs, EntityId, *anyopaque, Ctx) anyerror!void,
            });
            const Removed = std.ArrayListUnmanaged(struct {
                entity: EntityId,
                rmfn: *const fn (*Ecs, EntityId, Ctx) anyerror!void,
            });

            pub fn init(ecs: *Ecs) @This() {
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
                self.impl = .{};
            }

            pub fn apply(self: *@This(), ctx: Ctx) !void {
                defer self.reset();

                for (self.impl.inserted.items) |*t| {
                    try t.insertfn(self.ecs, t.entity, t.bundle, ctx);
                }

                for (self.impl.added_components.items) |*t| {
                    try t.addfn(self.ecs, t.entity, t.component, ctx);
                }

                for (self.impl.overwritten_components.items) |*t| {
                    try t.overwritefn(self.ecs, t.entity, t.component, ctx);
                }

                for (self.impl.removed_components.items) |*t| {
                    try t.rmfn(self.ecs, t.entity, ctx);
                }

                for (self.impl.deleted.items) |t| {
                    try self.ecs.delete(t, ctx);
                }
            }

            fn allocated(self: *@This(), thing: anytype) !*@TypeOf(thing) {
                const alloc = self.alloc.allocator();
                const mem = try alloc.create(@TypeOf(thing));
                errdefer alloc.free(mem);
                mem.* = thing;
                return mem;
            }

            pub fn reserve(self: *@This()) !EntityId {
                return try self.ecs.entities.new();
            }

            pub fn insert(self: *@This(), components: anytype) !EntityId {
                const e = try self.reserve();
                try self.insert_reserved(e, components);
                return e;
            }

            pub fn insert_reserved(self: *@This(), e: EntityId, components: anytype) !void {
                const alloc = self.alloc.allocator();

                try self.impl.inserted.append(alloc, .{
                    .entity = e,
                    .bundle = @ptrCast(try self.allocated(components)),
                    .insertfn = @ptrCast(&(struct {
                        fn insert(ecs: *Ecs, entity: EntityId, comp: *@TypeOf(components), ctx: Ctx) anyerror!void {
                            try ecs.insert_reserved(entity, comp.*, ctx);
                        }
                    }).insert),
                });
            }

            pub fn add_component(self: *@This(), entity: EntityId, component: anytype) !void {
                const alloc = self.alloc.allocator();
                try self.impl.added_components.append(alloc, .{
                    .entity = entity,
                    .component = @ptrCast(try self.allocated(component)),
                    .addfn = @ptrCast(&(struct {
                        fn add_component(ecs: *Ecs, e: EntityId, comp: *@TypeOf(component), ctx: Ctx) !void {
                            try ecs.add_component(e, comp.*, ctx);
                        }
                    }).add_component),
                });
            }

            pub fn overwrite_component(self: *@This(), entity: EntityId, component: anytype) !void {
                const alloc = self.alloc.allocator();
                try self.impl.overwritten_components.append(alloc, .{
                    .entity = entity,
                    .component = @ptrCast(try self.allocated(component)),
                    .overwritefn = @ptrCast(&(struct {
                        fn overwrite_component(ecs: *Ecs, e: EntityId, comp: *@TypeOf(component), ctx: Ctx) !void {
                            try ecs.overwrite_component(e, comp.*, ctx);
                        }
                    }).overwrite_component),
                });
            }

            pub fn remove_component(self: *@This(), entity: EntityId, component: type) !void {
                const alloc = self.alloc.allocator();
                try self.impl.removed_components.append(alloc, .{
                    .entity = entity,
                    .rmfn = @ptrCast(&(struct {
                        fn remove_component(ecs: *Ecs, e: EntityId, ctx: Ctx) !void {
                            try ecs.remove_component(e, component, ctx);
                        }
                    }).remove_component),
                });
            }

            pub fn delete(self: *@This(), entity: EntityId) !void {
                try self.impl.deleted.append(self.alloc.allocator(), entity);
            }
        };
    };
}
