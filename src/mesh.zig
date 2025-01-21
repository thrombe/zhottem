const std = @import("std");

const math = @import("math.zig");

const main = @import("main.zig");
const allocator = main.allocator;

pub const Mesh = struct {
    vertices: [][3]f32,
    faces: [][3]usize,

    pub fn cube() !@This() {
        var vertices = std.ArrayList([3]f32).init(allocator);
        errdefer vertices.deinit();
        var faces = std.ArrayList([3]usize).init(allocator);
        errdefer faces.deinit();

        try vertices.appendSlice(&[_][3]f32{
            .{ 1.0, -1.0, -1.0 },
            .{ 1.0, -1.0, 1.0 },
            .{ -1.0, -1.0, 1.0 },

            .{ 1.0, -1.0, -1.0 },
            .{ -1.0, -1.0, 1.0 },
            .{ -1.0, -1.0, -1.0 },

            .{ 1.0, 1.0, -1.0 },
            .{ 1.0, 1.0, 1.0 },
            .{ -1.0, 1.0, 1.0 },

            .{ 1.0, -1.0, -1.0 },
            .{ 1.0, 1.0, -1.0 },
            .{ -1.0, 1.0, -1.0 },

            .{ 1.0, -1.0, -1.0 },
            .{ -1.0, 1.0, -1.0 },
            .{ -1.0, -1.0, -1.0 },

            .{ 1.0, -1.0, 1.0 },
            .{ 1.0, 1.0, 1.0 },
            .{ -1.0, 1.0, 1.0 },

            .{ 1.0, -1.0, 1.0 },
            .{ -1.0, 1.0, 1.0 },
            .{ -1.0, -1.0, 1.0 },

            .{ 1.0, -1.0, -1.0 },
            .{ 1.0, -1.0, 1.0 },
            .{ 1.0, 1.0, 1.0 },

            .{ 1.0, -1.0, -1.0 },
            .{ 1.0, 1.0, 1.0 },
            .{ 1.0, 1.0, -1.0 },

            .{ -1.0, -1.0, -1.0 },
            .{ -1.0, 1.0, 1.0 },
            .{ -1.0, -1.0, 1.0 },

            .{ -1.0, -1.0, -1.0 },
            .{ -1.0, 1.0, 1.0 },
            .{ -1.0, 1.0, -1.0 },
        });

        return .{
            .vertices = try vertices.toOwnedSlice(),
            .faces = try faces.toOwnedSlice(),
        };
    }

    pub fn deinit(self: *@This()) void {
        allocator.free(self.vertices);
        allocator.free(self.faces);
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

        var vertices = std.ArrayList([3]f32).init(allocator);
        errdefer vertices.deinit();
        var faces = std.ArrayList([3]usize).init(allocator);
        errdefer faces.deinit();

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

                        try faces.append([3]usize{
                            try std.fmt.parseInt(usize, std.mem.span(@as([*:'/']const u8, @ptrCast(v1.ptr))), 10),
                            try std.fmt.parseInt(usize, std.mem.span(@as([*:'/']const u8, @ptrCast(v2.ptr))), 10),
                            try std.fmt.parseInt(usize, std.mem.span(@as([*:'/']const u8, @ptrCast(v3.ptr))), 10),
                        });
                    },
                    else => {},
                }
            }
        }

        return .{
            .vertices = try vertices.toOwnedSlice(),
            .faces = try faces.toOwnedSlice(),
        };
    }
};
