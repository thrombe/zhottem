const std = @import("std");

const math = @import("math.zig");

const main = @import("main.zig");
const allocator = main.allocator;

const utils = @import("utils.zig");

pub const Mesh = struct {
    vertices: [][3]f32,
    normals: [][3]f32,
    uvs: [][2]f32,
    faces: [][3]u32,

    pub fn cube() !@This() {
        var vertices = std.ArrayList([3]f32).init(allocator);
        errdefer vertices.deinit();
        var normals = std.ArrayList([3]f32).init(allocator);
        errdefer normals.deinit();
        var uvs = std.ArrayList([2]f32).init(allocator);
        errdefer uvs.deinit();
        var faces = std.ArrayList([3]u32).init(allocator);
        errdefer faces.deinit();

        try vertices.appendSlice(&[_][3]f32{
            .{ 1.0, -1.0, -1.0 },
            .{ 1.0, -1.0, 1.0 },
            .{ -1.0, -1.0, 1.0 },
            .{ -1.0, -1.0, -1.0 },
            .{ 1.0, 1.0, -1.0 },
            .{ 1.0, 1.0, 1.0 },
            .{ -1.0, 1.0, 1.0 },
            .{ -1.0, 1.0, -1.0 },
        });
        try normals.appendSlice(&[_][3]f32{
            .{ 0.0, 0.0, 1.0 },
            .{ 0.0, 0.0, 1.0 },
            .{ 0.0, 0.0, 1.0 },
            .{ 0.0, 0.0, 1.0 },
            .{ 0.0, 0.0, -1.0 },
            .{ 0.0, 0.0, -1.0 },
            .{ 0.0, 0.0, -1.0 },
            .{ 0.0, 0.0, -1.0 },
        });
        try uvs.appendSlice(&[_][2]f32{
            .{ 1.0, 0.0 },
            .{ 1.0, 1.0 },
            .{ 0.0, 1.0 },
            .{ 0.0, 0.0 },
            .{ 1.0, 0.0 },
            .{ 1.0, 1.0 },
            .{ 0.0, 1.0 },
            .{ 0.0, 0.0 },
        });
        try faces.appendSlice(&[_][3]u32{
            .{ 0, 1, 2 },
            .{ 0, 2, 3 },
            .{ 4, 5, 6 },
            .{ 4, 6, 7 },
            .{ 0, 4, 7 },
            .{ 0, 7, 3 },
            .{ 1, 5, 6 },
            .{ 1, 6, 2 },
            .{ 0, 1, 5 },
            .{ 0, 5, 4 },
            .{ 3, 2, 6 },
            .{ 3, 6, 7 },
        });

        return .{
            .vertices = try vertices.toOwnedSlice(),
            .normals = try normals.toOwnedSlice(),
            .uvs = try uvs.toOwnedSlice(),
            .faces = try faces.toOwnedSlice(),
        };
    }

    pub fn deinit(self: *@This()) void {
        allocator.free(self.vertices);
        allocator.free(self.normals);
        allocator.free(self.faces);
        allocator.free(self.uvs);
    }
};

pub const ObjParser = struct {
    const LineType = enum {
        comment,
        vertex,
        face,
        normal,
        texture_coord,

        fn str(self: @This()) []const u8 {
            return switch (self) {
                .comment => "#",
                .vertex => "v",
                .face => "f",
                .normal => "vn",
                .texture_coord => "vt",
            };
        }

        fn from_str(string: []const u8) ?@This() {
            inline for (comptime std.enums.values(@This())) |val| {
                if (std.mem.eql(u8, val.str(), string)) {
                    return val;
                }
            }

            return null;
        }
    };

    pub fn mesh_from_file(path: []const u8) !Mesh {
        var file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        var faces = std.ArrayList([3]u32).init(allocator);
        errdefer faces.deinit();
        var vertices = std.ArrayList([3]f32).init(allocator);
        errdefer vertices.deinit();
        var uvs = std.ArrayList([2]f32).init(allocator);
        errdefer uvs.deinit();
        var normals = std.ArrayList([3]f32).init(allocator);
        errdefer normals.deinit();

        var buf: [1024]u8 = undefined;
        var buf_reader = std.io.bufferedReader(file.reader());
        const reader = buf_reader.reader();
        while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |l| {
            var parts = std.mem.splitScalar(u8, l, ' ');
            while (parts.next()) |typ_str| {
                const typ = LineType.from_str(typ_str) orelse continue;

                switch (typ) {
                    .vertex => {
                        const x = parts.next() orelse return error.CouldNotParseVertex;
                        const y = parts.next() orelse return error.CouldNotParseVertex;
                        const z = parts.next() orelse return error.CouldNotParseVertex;

                        try vertices.append([3]f32{
                            try std.fmt.parseFloat(f32, x),
                            try std.fmt.parseFloat(f32, y),
                            try std.fmt.parseFloat(f32, z),
                        });
                    },
                    .face => {
                        const v1 = parts.next() orelse return error.CouldNotParseFace;
                        const v2 = parts.next() orelse return error.CouldNotParseFace;
                        const v3 = parts.next() orelse return error.CouldNotParseFace;

                        try faces.append([3]u32{
                            try std.fmt.parseInt(u32, std.mem.span(@as([*:'/']const u8, @ptrCast(v1.ptr))), 10) - 1,
                            try std.fmt.parseInt(u32, std.mem.span(@as([*:'/']const u8, @ptrCast(v2.ptr))), 10) - 1,
                            try std.fmt.parseInt(u32, std.mem.span(@as([*:'/']const u8, @ptrCast(v3.ptr))), 10) - 1,
                        });
                    },
                    .texture_coord => {
                        const u = parts.next() orelse return error.CouldNotParseTexCoord;
                        const v = parts.next() orelse return error.CouldNotParseTexCoord;

                        try uvs.append([2]f32{
                            try std.fmt.parseFloat(f32, u),
                            try std.fmt.parseFloat(f32, v),
                        });
                    },
                    .normal => {
                        const x = parts.next() orelse return error.CouldNotParseVertex;
                        const y = parts.next() orelse return error.CouldNotParseVertex;
                        const z = parts.next() orelse return error.CouldNotParseVertex;

                        try normals.append([3]f32{
                            try std.fmt.parseFloat(f32, x),
                            try std.fmt.parseFloat(f32, y),
                            try std.fmt.parseFloat(f32, z),
                        });
                    },
                    else => {},
                }
            }
        }

        return .{
            .vertices = try vertices.toOwnedSlice(),
            .normals = try normals.toOwnedSlice(),
            .faces = try faces.toOwnedSlice(),
            .uvs = try uvs.toOwnedSlice(),
        };
    }
};

pub const Gltf = struct {
    // - [glTF™ 2.0 Specification](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html)
    //   - all data is little endian
    //
    // NOTE: we don't handle big endian as it practically does not exist :} (i am lazy)

    header: Header,
    info: std.json.Parsed(Info),
    bin_chunk: Chunk,

    buf: []const u8,

    pub fn parse_glb(path: []const u8) !@This() {
        var reader = try Reader.load(path);
        errdefer reader.deinit();

        const header = try reader.read(Header);
        if (!std.mem.eql(u8, &header.magic, "glTF")) {
            return error.InvalidGltfHeader;
        }
        if (@as(usize, @intCast(header.length)) != reader.buf.len) {
            return error.CurruptedFile;
        }

        var json_chunk = try reader.chunk();
        if (!std.mem.eql(u8, &json_chunk.typ, "JSON")) {
            return error.InvalidJsonChunk;
        }

        const bin_chunk = try reader.chunk();
        if (!std.mem.eql(u8, &bin_chunk.typ, "BIN\x00")) {
            return error.InvalidBinChunk;
        }

        var info = try json_chunk.read_json(Info);
        errdefer info.deinit();

        return .{
            .header = header,
            .buf = reader.buf,
            .bin_chunk = bin_chunk,
            .info = info,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.info.deinit();
        allocator.free(self.buf);
    }

    pub fn get_bytes(self: *@This(), acc: *Info.Accessor) ![]const u8 {
        const view = &self.info.value.bufferViews[acc.bufferView];
        if (view.buffer != 0 or self.info.value.buffers[view.buffer].uri != null) {
            return error.ExternalBufferNotSupported;
        }

        const raw = self.bin_chunk.get_bytes(acc, view);
        return raw;
    }

    pub fn get_slice(self: *@This(), acc: *Info.Accessor, typ: type) ![]const typ {
        const view = &self.info.value.bufferViews[acc.bufferView];
        if (view.buffer != 0 or self.info.value.buffers[view.buffer].uri != null) {
            return error.ExternalBufferNotSupported;
        }

        return self.bin_chunk.get_slice(acc, view, typ);
    }

    const Header = extern struct {
        magic: [4]u8,
        version: u32,
        length: u32,
    };

    const Info = struct {
        scene: usize,
        scenes: []struct {
            name: []const u8,
            nodes: []usize,
        },
        nodes: []struct {
            name: ?[]const u8 = null,

            mesh: ?usize = null,
            camera: ?usize = null,
            skin: ?usize = null,

            rotation: ?[4]f32 = null,
            scale: ?[3]f32 = null,
            translation: ?[3]f32 = null,

            matrix: ?[16]f32 = null,

            weights: ?[]f32 = null, // only with morph targets

            children: ?[]usize = null,
        },
        meshes: []struct {
            name: []const u8,
            primitives: []struct {
                attributes: Attributes,
                indices: AccessorIndex,
                material: ?usize = null,
                mode: ?enum(u32) {
                    points = 0,
                    lines = 1,
                    line_loop = 2,
                    line_strip = 3,
                    triangles = 4,
                    triangle_strip = 5,
                    triangle_fan = 6,
                } = null,
            },
        },
        bufferViews: []BufferView,
        buffers: []struct {
            byteLength: usize,
            uri: ?[]const u8 = null,
        },
        animations: ?[]struct {
            name: ?[]const u8 = null,
            channels: []struct {
                sampler: usize,
                target: struct {
                    node: usize,
                    path: enum {
                        rotation,
                        scale,
                        translation,
                        weights, // only with morph targets

                        pub fn jsonParse(alloc: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) !@This() {
                            return try utils.JsonHelpers.parseEnumAsString(@This(), alloc, source, options);
                        }
                    },
                },
            },
            samplers: []struct {
                input: AccessorIndex,
                output: AccessorIndex,
                interpolation: enum {
                    LINEAR,
                    STEP,
                    CUBICSPLINE,

                    pub fn jsonParse(alloc: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) !@This() {
                        return try utils.JsonHelpers.parseEnumAsString(@This(), alloc, source, options);
                    }
                } = .LINEAR,
            },
        } = null,
        accessors: []Accessor,

        const PrimitiveAttribute = struct {
            typ: PrimitiveAttributeType,
            acc: AccessorIndex,

            fn parse_from_int_and_field_name(num: i64, fieldname: []const u8) !@This() {
                inline for (@typeInfo(PrimitiveAttributeType).Union.fields) |field| {
                    if (std.ascii.startsWithIgnoreCase(fieldname, field.name)) {
                        const typ = @unionInit(
                            PrimitiveAttributeType,
                            field.name,
                            switch (comptime std.meta.stringToEnum(std.meta.Tag(PrimitiveAttributeType), field.name).?) {
                                .texcoord, .color, .joint, .weight => blk: {
                                    var s = std.mem.split(u8, fieldname, "_");
                                    _ = s.next();
                                    const unparsed = s.next() orelse return error.MissingField;
                                    const parsed = std.fmt.parseInt(u32, unparsed, 10) catch return error.MissingField;
                                    break :blk .{ .set = parsed };
                                },
                                else => {},
                            },
                        );
                        return .{ .typ = typ, .acc = @intCast(num) };
                    }
                }

                return error.MissingField;
            }
        };
        const PrimitiveAttributeType = union(enum) {
            position,
            normal,
            tangent,

            texcoord: struct { set: u32 },
            color: struct { set: u32 },
            joint: struct { set: u32 },
            weight: struct { set: u32 },

            pub fn components(self: @This()) usize {
                return switch (self) {
                    .position => 3,
                    .normal => 3,
                    .tangent => 4,
                    .texcoord => 2,
                    .color => 3, // or 4 :/
                    .joint => 4,
                    .weight => 4,
                };
            }
        };
        const Attributes = struct {
            items: []PrimitiveAttribute,

            pub fn jsonParse(alloc: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) !@This() {
                const value = try std.json.innerParse(std.json.Value, alloc, source, options);
                if (std.meta.activeTag(value) != .object) {
                    return error.UnexpectedToken;
                }
                const obj = value.object;

                var items = std.ArrayList(PrimitiveAttribute).init(alloc);
                errdefer items.deinit();

                var it = obj.iterator();
                while (it.next()) |e| {
                    switch (e.value_ptr.*) {
                        .integer => |num| {
                            const attr = try PrimitiveAttribute.parse_from_int_and_field_name(num, e.key_ptr.*);

                            try items.append(attr);
                        },
                        else => return error.MissingField,
                    }
                }
                return .{
                    .items = try items.toOwnedSlice(),
                };
            }
        };
        const AccessorIndex = usize;
        const Accessor = struct {
            bufferView: usize,
            byteOffset: u32 = 0,
            componentType: enum(u32) {
                // https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#accessor-data-types
                byte = 5120,
                unsigned_byte = 5121,
                short = 5122,
                unsigned_short = 5123,
                unsigned_integer = 5125,
                float = 5126,

                pub fn typ(self: @This()) type {
                    return switch (self) {
                        .byte => i8,
                        .unsigned_byte => u8,
                        .short => i16,
                        .unsigned_short => u16,
                        .unsigned_integer => u32,
                        .float => f32,
                    };
                }

                pub fn stride(self: @This()) usize {
                    return switch (self) {
                        .byte => 1,
                        .unsigned_byte => 1,
                        .short => 2,
                        .unsigned_short => 2,
                        .unsigned_integer => 4,
                        .float => 4,
                    };
                }
            },
            count: usize,
            max: ?[]f32 = null,
            min: ?[]f32 = null,
            type: enum {
                SCALAR,
                VEC2,
                VEC3,
                VEC4,
                MAT2,
                MAT3,
                MAT4,

                pub fn components(self: @This()) usize {
                    return switch (self) {
                        .SCALAR => 1,
                        .VEC2 => 2,
                        .VEC3 => 3,
                        .VEC4 => 4,
                        .MAT2 => 4,
                        .MAT3 => 9,
                        .MAT4 => 16,
                    };
                }

                pub fn jsonParse(alloc: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) !@This() {
                    return try utils.JsonHelpers.parseEnumAsString(@This(), alloc, source, options);
                }
            },

            pub fn len(self: *@This()) usize {
                return self.count * self.type.components() * self.componentType.stride();
            }
        };
        const BufferView = struct {
            buffer: usize,
            byteLength: usize,
            target: enum(u32) {
                ARRAY_BUFFER = 34962,
                ELEMENT_ARRAY_BUFFER = 34963,
            },
            byteOffset: usize,
        };
    };

    const Chunk = struct {
        typ: [4]u8,
        buf: []const u8,

        fn read_json(self: *@This(), typ: type) !std.json.Parsed(typ) {
            return try std.json.parseFromSlice(typ, allocator, self.buf, .{
                .ignore_unknown_fields = true,
                .allocate = .alloc_always,
            });
        }

        fn get_bytes(self: *@This(), acc: *Info.Accessor, view: *Info.BufferView) []const u8 {
            return self.buf[view.byteOffset..][0..view.byteLength][acc.byteOffset..][0..acc.len()];
        }

        fn get_slice(self: *@This(), acc: *Info.Accessor, view: *Info.BufferView, typ: type) []const typ {
            const buf = self.get_bytes(acc, view);
            return std.mem.bytesAsSlice(typ, buf);
        }
    };

    const Reader = struct {
        buf: []const u8,
        head: usize = 0,

        fn load(path: []const u8) !@This() {
            const buf = try std.fs.cwd().readFileAllocOptions(
                allocator,
                path,
                100 * 1000 * 1000,
                null,
                8,
                null,
            );
            errdefer allocator.free(buf);

            return .{
                .buf = buf,
            };
        }

        fn deinit(self: *@This()) void {
            allocator.free(self.buf);
        }

        fn read(self: *@This(), typ: type) !typ {
            if (self.buf.len < self.head + @sizeOf(typ)) {
                return error.CouldNotParseHeader;
            }

            defer self.head += @sizeOf(typ);
            return @as(*const typ, @ptrCast(@alignCast(self.buf[self.head..].ptr))).*;
        }

        fn chunk(self: *@This()) !Chunk {
            const len: u32 = std.mem.bytesAsValue(u32, &try self.read([4]u8)).*;
            const typ = try self.read([4]u8);
            defer self.head += len;

            return .{
                .typ = typ,
                .buf = self.buf[self.head..][0..len],
            };
        }
    };
};
