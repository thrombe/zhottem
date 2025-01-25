const std = @import("std");

const math = @import("math.zig");

const main = @import("main.zig");
const allocator = main.allocator;

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
