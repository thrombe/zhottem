const std = @import("std");

const math = @import("math.zig");

const main = @import("main.zig");
const allocator = main.allocator;

// assumes ok has ok.deinit()
pub fn Result(ok: type, err_typ: type) type {
    return union(enum) {
        Ok: ok,
        Err: Error,

        pub const Error = struct {
            err: err_typ,
            msg: []u8,

            // pub fn owned(err: err_typ, msg: []const u8) !@This() {
            //     return .{
            //         .err = err,
            //         .msg = try allocator.dupe(u8, msg),
            //     };
            // }
            // pub fn deinit(self: *@This()) void {
            //     allocator.free(self.msg);
            // }
        };

        // pub fn deinit(self: *@This()) void {
        //     switch (self) {
        //         .Ok => |res| {
        //             if (std.meta.hasMethod(ok, "deinit")) {
        //                 res.deinit();
        //             }
        //         },
        //         .Err => |err| {
        //             err.deinit();
        //         },
        //     }
        // }
    };
}

pub fn Deque(typ: type) type {
    return struct {
        allocator: std.mem.Allocator,
        buffer: []typ,
        size: usize,

        // fill this index next
        front: usize, // at
        back: usize, // one to the right

        pub fn init(alloc: std.mem.Allocator) !@This() {
            const len = 32;
            const buffer = try alloc.alloc(typ, len);
            return .{
                .allocator = alloc,
                .buffer = buffer,
                .front = 0,
                .back = 0,
                .size = 0,
            };
        }

        pub fn deinit(self: *@This()) void {
            self.allocator.free(self.buffer);
        }

        pub fn push_front(self: *@This(), value: typ) !void {
            if (self.size == self.buffer.len) {
                try self.resize();
                return self.push_front(value) catch unreachable;
            }
            self.front = (self.front + self.buffer.len - 1) % self.buffer.len;
            self.buffer[self.front] = value;
            self.size += 1;
        }

        pub fn push_back(self: *@This(), value: typ) !void {
            if (self.size == self.buffer.len) {
                try self.resize();
                return self.push_back(value) catch unreachable;
            }
            self.buffer[self.back] = value;
            self.back = (self.back + 1) % self.buffer.len;
            self.size += 1;
        }

        pub fn pop_front(self: *@This()) ?typ {
            if (self.size == 0) {
                return null;
            }
            const value = self.buffer[self.front];
            self.front = (self.front + 1) % self.buffer.len;
            self.size -= 1;
            return value;
        }

        pub fn pop_back(self: *@This()) ?typ {
            if (self.size == 0) {
                return null;
            }
            self.back = (self.back + self.buffer.len - 1) % self.buffer.len;
            const value = self.buffer[self.back];
            self.size -= 1;
            return value;
        }

        pub fn peek_front(self: *@This()) ?*const typ {
            if (self.size == 0) {
                return null;
            }
            return &self.buffer[self.front];
        }

        pub fn peek_back(self: *@This()) ?*const typ {
            if (self.size == 0) {
                return null;
            }
            const back = (self.back + self.buffer.len - 1) % self.buffer.len;
            return &self.buffer[back];
        }

        pub fn is_empty(self: *@This()) bool {
            return self.size == 0;
        }

        fn resize(self: *@This()) !void {
            std.debug.assert(self.size == self.buffer.len);

            const size = self.buffer.len * 2;
            const buffer = try self.allocator.alloc(typ, size);
            @memcpy(buffer[0 .. self.size - self.front], self.buffer[self.front..]);
            @memcpy(buffer[self.size - self.front .. self.size], self.buffer[0..self.front]);
            const new = @This(){
                .allocator = self.allocator,
                .buffer = buffer,
                .front = 0,
                .back = self.size,
                .size = self.size,
            };
            self.allocator.free(self.buffer);
            self.* = new;
        }
    };
}

// MAYBE: condvars + .block_recv()
pub fn Channel(typ: type) type {
    return struct {
        const Dq = Deque(typ);
        const Pinned = struct {
            dq: Dq,
            lock: std.Thread.Mutex = .{},
        };
        pinned: *Pinned,

        pub fn init(alloc: std.mem.Allocator) !@This() {
            const dq = try Dq.init(alloc);
            const pinned = try alloc.create(Pinned);
            pinned.* = .{
                .dq = dq,
            };
            return .{
                .pinned = pinned,
            };
        }

        pub fn deinit(self: *@This()) void {
            self.pinned.lock.lock();
            // defer self.pinned.lock.unlock();
            self.pinned.dq.deinit();
            self.pinned.dq.allocator.destroy(self.pinned);
        }

        pub fn send(self: *@This(), val: typ) !void {
            self.pinned.lock.lock();
            defer self.pinned.lock.unlock();
            try self.pinned.dq.push_back(val);
        }

        pub fn try_recv(self: *@This()) ?typ {
            self.pinned.lock.lock();
            defer self.pinned.lock.unlock();
            return self.pinned.dq.pop_front();
        }

        pub fn can_recv(self: *@This()) bool {
            self.pinned.lock.lock();
            defer self.pinned.lock.unlock();
            return self.pinned.dq.peek_front() != null;
        }
    };
}

pub const Fuse = struct {
    fused: std.atomic.Value(bool) = .{ .raw = false },

    pub fn fuse(self: *@This()) bool {
        return self.fused.swap(true, .release);
    }
    pub fn unfuse(self: *@This()) bool {
        const res = self.fused.swap(false, .release);
        return res;
    }
    pub fn check(self: *@This()) bool {
        return self.fused.load(.acquire);
    }
};

pub const FsFuse = struct {
    // - [emcrisostomo/fswatch](https://github.com/emcrisostomo/fswatch?tab=readme-ov-file#libfswatch)
    // - [libfswatch/c/libfswatch.h Reference](http://emcrisostomo.github.io/fswatch/doc/1.17.1/libfswatch.html/libfswatch_8h.html#ae465ef0618fb1dc6d8b70dee68359ea6)
    const c = @cImport({
        @cInclude("libfswatch/c/libfswatch.h");
    });

    const Event = struct {
        file: []const u8,
        real: []const u8,

        pub fn deinit(self: *const @This()) void {
            allocator.free(self.file);
            allocator.free(self.real);
        }
    };
    const Chan = Channel(Event);
    const Ctx = struct {
        handle: c.FSW_HANDLE,
        channel: Chan,
        path: [:0]const u8,
    };

    ctx: *Ctx,
    thread: std.Thread,

    pub fn init(path: [:0]const u8) !@This() {
        const rpath = try std.fs.cwd().realpathAlloc(allocator, path);
        defer allocator.free(rpath);
        const pathZ = try allocator.dupeZ(u8, rpath);

        const watch = try start(pathZ);
        return watch;
    }

    pub fn deinit(self: @This()) void {
        _ = c.fsw_stop_monitor(self.ctx.handle);
        _ = c.fsw_destroy_session(self.ctx.handle);

        // OOF: freezes the thread for a while
        // self.thread.join();

        self.ctx.channel.deinit();
        allocator.free(self.ctx.path);
        allocator.destroy(self.ctx);
    }

    pub fn can_recv(self: *@This()) bool {
        return self.ctx.channel.can_recv();
    }

    pub fn try_recv(self: *@This()) ?Event {
        return self.ctx.channel.try_recv();
    }

    pub fn restart(self: *@This(), path: [:0]const u8) !void {
        self.deinit();
        self.* = try init(path);
    }

    fn start(path: [:0]const u8) !@This() {
        const ok = c.fsw_init_library();
        if (ok != c.FSW_OK) {
            return error.CouldNotCreateFsWatcher;
        }

        const ctxt = try allocator.create(Ctx);
        ctxt.* = .{
            .channel = try Chan.init(allocator),
            .handle = null,
            .path = path,
        };

        const Callbacks = struct {
            fn spawn(ctx: *Ctx) !void {
                ctx.handle = c.fsw_init_session(c.filter_include) orelse return error.CouldNotInitFsWatcher;
                var oke = c.fsw_add_path(ctx.handle, ctx.path.ptr);
                if (oke != c.FSW_OK) {
                    return error.PathAdditionFailed;
                }

                oke = c.fsw_set_recursive(ctx.handle, true);
                if (oke != c.FSW_OK) {
                    return error.FswSetFailed;
                }
                oke = c.fsw_set_latency(ctx.handle, 0.2);
                if (oke != c.FSW_OK) {
                    return error.FswSetFailed;
                }
                oke = c.fsw_set_callback(ctx.handle, @ptrCast(&event_callback), ctx);
                if (oke != c.FSW_OK) {
                    return error.FswSetFailed;
                }

                std.debug.print("starting monitor\n", .{});
                oke = c.fsw_start_monitor(ctx.handle);
                if (oke != c.FSW_OK) {
                    return error.CouldNotStartWatcher;
                }
            }
            fn event_callback(events: [*c]const c.fsw_cevent, num: c_uint, ctx: ?*Ctx) callconv(.C) void {
                var flags = c.NoOp;
                // flags |= c.Created;
                flags |= c.Updated;
                // flags |= c.Removed;
                // flags |= c.Renamed;
                // flags |= c.OwnerModified;
                // flags |= c.AttributeModified;
                // flags |= c.MovedFrom;
                // flags |= c.MovedTo;

                for (events[0..@intCast(num)]) |event| {
                    for (event.flags[0..event.flags_num]) |f| {
                        if (flags & @as(c_int, @intCast(f)) == 0) {
                            continue;
                        }
                        const name = c.fsw_get_event_flag_name(f);
                        // std.debug.print("Path: {s}\n", .{event.path});
                        // std.debug.print("Event Type: {s}\n", .{std.mem.span(name)});

                        if (ctx) |cctx| {
                            const stripped = std.fs.path.relative(allocator, cctx.path, std.mem.span(event.path)) catch unreachable;
                            cctx.channel.send(.{
                                .file = stripped,
                                .real = allocator.dupe(u8, std.mem.span(event.path)) catch unreachable,
                            }) catch unreachable;
                        } else {
                            std.debug.print("Error: Event ignored! type: '{s}' path: '{s}'", .{ event.path, name });
                        }
                    }
                }
            }
        };

        const t = try std.Thread.spawn(.{ .allocator = allocator }, Callbacks.spawn, .{ctxt});
        return .{
            .ctx = ctxt,
            .thread = t,
        };
    }
};

pub const ImageMagick = struct {
    // - [ImageMagick – Sitemap](https://imagemagick.org/script/sitemap.php#program-interfaces)
    // - [ImageMagick – MagickWand, C API](https://imagemagick.org/script/magick-wand.php)
    // - [ImageMagick – MagickCore, Low-level C API](https://imagemagick.org/script/magick-core.php)
    const magick = @cImport({
        // @cInclude("MagickCore/MagickCore.h");
        @cDefine("MAGICKCORE_HDRI_ENABLE", "1");
        @cInclude("MagickWand/MagickWand.h");
    });

    pub fn Pixel(typ: type) type {
        return extern struct {
            r: typ,
            g: typ,
            b: typ,
            a: typ,
        };
    }
    pub fn Image(pix: type) type {
        return struct {
            buffer: []pix,
            height: usize,
            width: usize,

            pub fn deinit(self: *@This()) void {
                allocator.free(self.buffer);
            }
        };
    }

    pub const FloatImage = Image(Pixel(f32));
    pub const HalfImage = Image(Pixel(f16));
    pub const UnormImage = Image(Pixel(u8));

    pub fn decode_jpg(bytes: []u8, comptime typ: enum { unorm, half, float }) !(switch (typ) {
        .unorm => Image(Pixel(u8)),
        .half => Image(Pixel(f16)),
        .float => Image(Pixel(f32)),
    }) {
        magick.MagickWandGenesis();
        const wand = magick.NewMagickWand() orelse {
            return error.CouldNotGetWand;
        };
        defer _ = magick.DestroyMagickWand(wand);

        // const pwand = magick.NewPixelWand() orelse {
        //     return error.CouldNotGetWand;
        // };
        // defer _ = magick.DestroyPixelWand(pwand);
        // if (magick.PixelSetColor(pwand, "#28282800") == magick.MagickFalse) {
        //     return error.CouldNotSetPWandColor;
        // }
        // if (magick.MagickSetBackgroundColor(wand, pwand) == magick.MagickFalse) {
        //     return error.CouldNotSetBgColor;
        // }

        if (magick.MagickReadImageBlob(wand, bytes.ptr, bytes.len) == magick.MagickFalse) {
            return error.CouldNotReadImage;
        }

        const img_width = magick.MagickGetImageWidth(wand);
        const img_height = magick.MagickGetImageHeight(wand);

        const buffer = try allocator.alloc(Pixel(switch (typ) {
            .unorm => u8,
            .half => f32,
            .float => f32,
        }), img_width * img_height);
        errdefer allocator.free(buffer);

        if (magick.MagickExportImagePixels(
            wand,
            0,
            0,
            img_width,
            img_height,
            "RGBA",
            switch (typ) {
                .unorm => magick.CharPixel,
                .half => magick.FloatPixel,
                .float => magick.FloatPixel,
            },
            buffer.ptr,
        ) == magick.MagickFalse) {
            return error.CouldNotRenderToBuffer;
        }

        switch (typ) {
            .half => {
                const half = try allocator.alloc(Pixel(f16), img_width * img_height);
                for (buffer, 0..) |v, i| {
                    half[i] = .{
                        .r = @floatCast(v.r),
                        .g = @floatCast(v.g),
                        .b = @floatCast(v.b),
                        .a = @floatCast(v.a),
                    };
                }
                allocator.free(buffer);
                return .{
                    .buffer = half,
                    .height = img_height,
                    .width = img_width,
                };
            },
            else => {
                return .{
                    .buffer = buffer,
                    .height = img_height,
                    .width = img_width,
                };
            },
        }
    }

    pub fn encode_rgba_image(pixels: []f32, width: usize, height: usize) ![]u8 {
        magick.MagickWandGenesis();
        const wand = magick.NewMagickWand() orelse {
            return error.CouldNotGetWand;
        };
        defer _ = magick.DestroyMagickWand(wand);

        const pwand = magick.NewPixelWand();
        defer _ = magick.DestroyPixelWand(pwand);

        _ = magick.PixelSetColor(pwand, &[_]u8{ 0, 0, 0, 0 });

        if (magick.MagickNewImage(wand, width, height, pwand) == magick.MagickFalse) {
            return error.couldnotcreatenewimage;
        }

        if (magick.MagickImportImagePixels(wand, 0, 0, width, height, "RGBA", magick.FloatPixel, pixels.ptr) == magick.MagickFalse) {
            return error.CouldNotImportImage;
        }

        if (magick.MagickSetImageFormat(wand, "PNG") == magick.MagickFalse) {
            return error.CouldNotSetFormat;
        }
        var size: usize = 0;
        const blob = magick.MagickGetImageBlob(wand, &size);
        defer _ = magick.MagickRelinquishMemory(blob);

        const cloned_blob = try allocator.dupe(u8, blob[0..size]);
        errdefer allocator.free(cloned_blob);

        return cloned_blob;
    }
};

pub const ShaderUtils = struct {
    const Vec4 = math.Vec4;
    const Mat4x4 = math.Mat4x4;

    pub const Mouse = extern struct { x: i32, y: i32, left: u32, right: u32 };
    pub const Camera = extern struct {
        eye: Vec4,
        fwd: Vec4,
        right: Vec4,
        up: Vec4,
        meta: CameraMeta,

        pub const CameraMeta = extern struct {
            did_change: u32 = 0,
            did_move: u32 = 0,
            did_rotate: u32 = 0,
            pad: u32 = 0,
        };
    };
    pub const Frame = extern struct {
        frame: u32,
        time: f32,
        deltatime: f32,
        width: u32,
        height: u32,
        monitor_width: u32,
        monitor_height: u32,
    };

    pub fn create_extern_type(comptime uniform: type) type {
        const Type = std.builtin.Type;
        const Ut: Type = @typeInfo(uniform);
        const U = Ut.Struct;

        return @Type(.{
            .Struct = .{
                .layout = .@"extern",
                .fields = U.fields,
                .decls = &[_]Type.Declaration{},
                .is_tuple = false,
            },
        });
    }

    pub fn create_uniform_object(comptime uniform_type: type, uniform: anytype) create_extern_type(uniform_type) {
        const Type = std.builtin.Type;
        const Ut: Type = @typeInfo(uniform_type);
        const U = Ut.Struct;

        var uniform_object: create_extern_type(uniform_type) = undefined;

        inline for (U.fields) |field| {
            @field(uniform_object, field.name) = @field(uniform, field.name);
        }

        return uniform_object;
    }

    pub fn dump_glsl_uniform(ubo: anytype, path: []const u8) !void {
        const Dumper = struct {
            const Writer = std.ArrayList(u8).Writer;

            fn glsl_type(t: type) []const u8 {
                return switch (t) {
                    Vec4 => "vec4",
                    Mat4x4 => "mat4",
                    i32 => "int",
                    u32 => "uint",
                    f32 => "float",
                    Mouse => "Mouse",
                    Camera => "Camera",
                    Camera.CameraMeta => "CameraMeta",
                    Frame => "Frame",
                    @TypeOf(ubo) => "Uniforms",
                    else => switch (@typeInfo(t)) {
                        .Array => |child| glsl_type(child.child),
                        else => @compileError("cannot handle this type"),
                    },
                };
            }

            fn glsl_fieldname(field: std.builtin.Type.StructField) []const u8 {
                switch (@typeInfo(field.type)) {
                    .Array => |child| {
                        const len = std.fmt.comptimePrint("{d}", .{child.len});
                        return field.name ++ "[" ++ len ++ "]";
                    },
                    else => return field.name,
                }
            }

            fn dump_type(w: Writer, t: type) !void {
                switch (t) {
                    []Mat4x4, Mat4x4, Vec4, i32, u32, f32 => return,
                    else => switch (@typeInfo(t)) {
                        .Array => return,
                        else => {
                            const fields = @typeInfo(t).Struct.fields;
                            inline for (fields) |field| {
                                try dump_type(w, field.type);
                            }

                            try w.print(
                                \\ struct {s} {{
                                \\
                            , .{glsl_type(t)});

                            inline for (fields) |field| {
                                try w.print(
                                    \\     {s} {s};
                                    \\
                                , .{ glsl_type(field.type), glsl_fieldname(field) });
                            }

                            try w.print(
                                \\ }};
                                \\
                                \\
                            , .{});
                        },
                    },
                }
            }
        };

        var data = std.ArrayList(u8).init(allocator);
        defer data.deinit();

        try data.appendSlice(
            \\ // This file is generated from code. DO NOT EDIT.
            \\
            \\
        );
        try Dumper.dump_type(data.writer(), @TypeOf(ubo));

        const file = try std.fs.cwd().createFile(path, .{});
        defer file.close();

        try file.writeAll(data.items);
    }
};

pub fn ShaderCompiler(meta: type, stages: type) type {
    return struct {
        pub const StageMap = std.EnumArray(stages, Compiled);
        pub const StageSet = std.EnumSet(stages);

        pub const Stages = struct {
            map: StageMap,

            pub fn init(compiler: *Compiler) !@This() {
                var shaders = StageMap.initUndefined();
                var set = StageSet.initEmpty();
                errdefer {
                    var it = set.iterator();
                    while (it.next()) |key| {
                        shaders.get(key).deinit();
                    }
                }

                const set_full = StageSet.initFull();
                while (!set.eql(set_full)) {
                    while (compiler.ctx.compiled.try_recv()) |shader| {
                        if (set.contains(shader.typ)) {
                            shaders.get(shader.typ).deinit();
                        }
                        shaders.set(shader.typ, shader);
                        set.insert(shader.typ);
                    }

                    while (compiler.ctx.err_chan.try_recv()) |msg_| {
                        if (msg_) |msg| {
                            defer allocator.free(msg);
                            std.debug.print("{s}\n", .{msg});
                        }
                    }
                }
                return .{ .map = shaders };
            }

            pub fn update(self: *@This(), comp: *Compiler) bool {
                const can_recv = comp.has_updates();

                while (comp.ctx.compiled.try_recv()) |shader| {
                    self.map.get(shader.typ).deinit();

                    self.map.set(shader.typ, shader);
                }

                while (comp.ctx.err_chan.try_recv()) |msg_| {
                    if (msg_) |msg| {
                        defer allocator.free(msg);
                        std.debug.print("shader error: {s}\n", .{msg});
                    } else {
                        std.debug.print("no shader errors lesgo :)\n", .{});
                    }
                }

                return can_recv;
            }

            pub fn deinit(self: *@This()) void {
                for (self.map.values) |shader| {
                    shader.deinit();
                }
            }
        };

        pub const ShaderInfo = struct {
            typ: stages,
            stage: Glslc.Compiler.Stage,
            path: []const u8,
            include: []const []const u8,
            define: []const []const u8,

            fn compile(self: *const @This(), ctx: *Compiler.Ctx) ![]u32 {
                const shader: Glslc.Compiler.Code = .{ .path = .{
                    .main = self.path,
                    .include = self.include,
                    .definitions = self.define,
                } };
                if (ctx.dump_assembly) blk: {
                    // TODO: print this on screen instead of console
                    const res = ctx.comp.dump_assembly(allocator, &shader, self.stage) catch {
                        break :blk;
                    };
                    switch (res) {
                        .Err => |err| {
                            try ctx.err_chan.send(err.msg);
                            return err.err;
                        },
                        .Ok => {
                            try ctx.err_chan.send(null);
                        },
                    }
                }
                const frag_bytes = blk: {
                    const res = try ctx.comp.compile(
                        allocator,
                        &shader,
                        .spirv,
                        self.stage,
                    );
                    switch (res) {
                        .Err => |err| {
                            try ctx.err_chan.send(err.msg);
                            return err.err;
                        },
                        .Ok => |ok| {
                            errdefer allocator.free(ok);
                            try ctx.err_chan.send(null);
                            break :blk ok;
                        },
                    }
                };
                return frag_bytes;
            }
        };
        pub const Compiled = struct {
            typ: stages,
            code: []u32,
            metadata: meta,

            fn deinit(self: *const @This()) void {
                allocator.free(self.code);
            }
        };
        pub const Compiler = struct {
            const EventChan = Channel(Compiled);
            const Ctx = struct {
                comp: Glslc.Compiler,
                err_chan: Channel(?[]const u8),
                compiled: Channel(Compiled),
                shader_fuse: FsFuse,
                shaders: []ShaderInfo,
                dump_assembly: bool = false,
                has_updates: Fuse = .{},

                exit: Fuse = .{},

                fn update(self: *@This()) !void {
                    var has_updates_ = false;
                    defer if (has_updates_) {
                        _ = self.has_updates.fuse();
                    };

                    while (self.shader_fuse.try_recv()) |ev| {
                        defer ev.deinit();

                        var found_any = false;
                        for (self.shaders) |s| {
                            if (std.mem.eql(u8, s.path, ev.real)) {
                                found_any = true;
                                const res = try s.compile(self);
                                try self.compiled.send(.{
                                    .typ = s.typ,
                                    .code = res,
                                    .metadata = try meta.get_metadata(s),
                                });
                            }
                        }
                        has_updates_ = has_updates_ or found_any;

                        if (!found_any) {
                            std.debug.print("Unknown file update: {s}\n", .{ev.file});
                        }
                    }
                }

                fn deinit(self: *@This()) void {
                    while (self.err_chan.try_recv()) |err| {
                        if (err) |er| {
                            allocator.free(er);
                        }
                    }
                    self.err_chan.deinit();

                    while (self.compiled.try_recv()) |shader| {
                        shader.deinit();
                    }
                    self.compiled.deinit();
                    self.shader_fuse.deinit();

                    for (self.shaders) |s| {
                        allocator.free(s.path);
                        for (s.define) |def| {
                            allocator.free(def);
                        }
                        allocator.free(s.define);
                        for (s.include) |inc| {
                            allocator.free(inc);
                        }
                        allocator.free(s.include);
                    }
                    allocator.free(self.shaders);
                }
            };
            ctx: *Ctx,
            thread: std.Thread,

            pub fn init(comp: Glslc.Compiler, shader_info: []const ShaderInfo) !@This() {
                const shader_fuse = try FsFuse.init("./src");
                errdefer shader_fuse.deinit();

                const shaders = try allocator.dupe(ShaderInfo, shader_info);
                // OOF: not getting free-d if anything errors but meh
                for (shaders) |*s| {
                    var buf: [std.fs.MAX_PATH_BYTES:0]u8 = undefined;
                    const real = try std.fs.cwd().realpath(s.path, &buf);
                    s.path = try allocator.dupe(u8, real);

                    const define = try allocator.alloc([]const u8, s.define.len);
                    for (s.define, 0..) |def, i| {
                        define[i] = try allocator.dupe(u8, def);
                    }
                    s.define = define;

                    const include = try allocator.alloc([]const u8, s.include.len);
                    for (s.include, 0..) |inc, i| {
                        const real_inc = try std.fs.cwd().realpath(inc, &buf);
                        include[i] = try allocator.dupe(u8, real_inc);
                    }
                    s.include = include;
                }

                const ctxt = try allocator.create(Ctx);
                errdefer allocator.destroy(ctxt);
                ctxt.* = .{
                    .comp = comp,
                    .shaders = shaders,
                    .shader_fuse = shader_fuse,
                    .err_chan = try Channel(?[]const u8).init(allocator),
                    .compiled = try Channel(Compiled).init(allocator),
                };
                errdefer ctxt.deinit();

                var set = std.StringHashMap(void).init(allocator);
                defer set.deinit();
                for (shaders) |s| {
                    if (!set.contains(s.path)) {
                        try set.put(s.path, {});
                    }
                }

                var it = set.iterator();
                while (it.next()) |s| {
                    const real = try allocator.dupe(u8, s.key_ptr.*);
                    const stripped = try std.fs.path.relative(allocator, ctxt.shader_fuse.ctx.path, real);
                    try ctxt.shader_fuse.ctx.channel.send(.{
                        .file = stripped,
                        .real = real,
                    });
                }

                const Callbacks = struct {
                    fn spawn(ctx: *Ctx) void {
                        while (true) {
                            if (ctx.exit.unfuse()) {
                                break;
                            }
                            ctx.update() catch |e| {
                                const err = std.fmt.allocPrint(allocator, "{any}", .{e}) catch continue;
                                ctx.err_chan.send(err) catch continue;
                            };

                            std.time.sleep(std.time.ns_per_ms * 100);
                        }
                    }
                };
                const thread = try std.Thread.spawn(.{ .allocator = allocator }, Callbacks.spawn, .{ctxt});

                return .{
                    .ctx = ctxt,
                    .thread = thread,
                };
            }

            pub fn deinit(self: *@This()) void {
                _ = self.ctx.exit.fuse();
                self.thread.join();
                self.ctx.deinit();
                allocator.destroy(self.ctx);
            }

            pub fn has_updates(self: *@This()) bool {
                return self.ctx.has_updates.unfuse();
            }
        };
    };
}

pub const Glslc = struct {
    pub const Compiler = struct {
        pub const Opt = enum {
            none,
            small,
            fast,
        };
        pub const Stage = enum {
            vertex,
            fragment,
            compute,
        };
        opt: Opt = .none,
        lang: enum {
            glsl,
            hlsl,
        } = .glsl,
        env: enum {
            vulkan1_3,
            vulkan1_2,
            vulkan1_1,
            vulkan1_0,
        } = .vulkan1_0,
        pub const OutputType = enum {
            assembly,
            spirv,
        };
        pub const Code = union(enum) {
            code: struct {
                src: []const u8,
                definitions: []const []const u8,
            },
            path: struct {
                main: []const u8,
                include: []const []const u8,
                definitions: []const []const u8,
            },
        };

        pub const Err = error{
            GlslcErroredOut,
        };
        pub fn CompileResult(out: OutputType) type {
            return Result(switch (out) {
                .spirv => []u32,
                .assembly => []u8,
            }, Err);
        }

        pub fn dump_assembly(
            self: @This(),
            alloc: std.mem.Allocator,
            code: *const Code,
            stage: Stage,
        ) !Result(void, Err) {
            // std.debug.print("{s}\n", .{code});
            const res = try self.compile(alloc, code, .assembly, stage);
            switch (res) {
                .Ok => |bytes| {
                    defer alloc.free(bytes);
                    std.debug.print("{s}\n", .{bytes});
                    return .Ok;
                },
                .Err => |err| {
                    return .{ .Err = err };
                },
            }
        }

        pub fn compile(
            self: @This(),
            alloc: std.mem.Allocator,
            code: *const Code,
            comptime output_type: OutputType,
            stage: Stage,
        ) !CompileResult(output_type) {
            var args = std.ArrayList([]const u8).init(alloc);
            defer {
                for (args.items) |arg| {
                    alloc.free(arg);
                }
                args.deinit();
            }
            try args.append(try alloc.dupe(u8, "glslc"));
            try args.append(try alloc.dupe(u8, switch (stage) {
                .fragment => "-fshader-stage=fragment",
                .vertex => "-fshader-stage=vertex",
                .compute => "-fshader-stage=compute",
            }));
            try args.append(try alloc.dupe(u8, switch (self.lang) {
                .glsl => "-xglsl",
                .hlsl => "-xhlsl",
            }));
            try args.append(try alloc.dupe(u8, switch (self.env) {
                .vulkan1_3 => "--target-env=vulkan1.3",
                .vulkan1_2 => "--target-env=vulkan1.2",
                .vulkan1_1 => "--target-env=vulkan1.1",
                .vulkan1_0 => "--target-env=vulkan1.0",
            }));
            try args.append(try alloc.dupe(u8, switch (self.opt) {
                .fast => "-O",
                .small => "-Os",
                .none => "-O0",
            }));
            if (output_type == .assembly) {
                try args.append(try alloc.dupe(u8, "-S"));
            }
            try args.append(try alloc.dupe(u8, "-o-"));
            switch (code.*) {
                .code => |src| {
                    for (src.definitions) |def| {
                        try args.append(try std.fmt.allocPrint(alloc, "-D{s}", .{def}));
                    }
                    try args.append(try alloc.dupe(u8, "-"));
                },
                .path => |paths| {
                    for (paths.definitions) |def| {
                        try args.append(try std.fmt.allocPrint(alloc, "-D{s}", .{def}));
                    }
                    for (paths.include) |inc| {
                        try args.append(try alloc.dupe(u8, "-I"));
                        try args.append(try alloc.dupe(u8, inc));
                    }
                    try args.append(try alloc.dupe(u8, paths.main));
                },
            }

            // for (args.items) |arg| {
            //     std.debug.print("{s} ", .{arg});
            // }
            // std.debug.print("\n", .{});

            var child = std.process.Child.init(args.items, alloc);
            child.stdin_behavior = .Pipe;
            child.stdout_behavior = .Pipe;
            child.stderr_behavior = .Pipe;

            try child.spawn();

            const stdin = child.stdin orelse return error.NoStdin;
            child.stdin = null;
            const stdout = child.stdout orelse return error.NoStdout;
            child.stdout = null;
            const stderr = child.stderr orelse return error.NoStderr;
            child.stderr = null;
            defer stdout.close();
            defer stderr.close();

            switch (code.*) {
                .code => |src| {
                    try stdin.writeAll(src.src);
                },
                .path => {},
            }
            stdin.close();

            // similar to child.collectOutput
            const max_output_bytes = 1000 * 1000;
            var poller = std.io.poll(allocator, enum { stdout, stderr }, .{
                .stdout = stdout,
                .stderr = stderr,
            });
            defer poller.deinit();

            while (try poller.poll()) {
                if (poller.fifo(.stdout).count > max_output_bytes)
                    return error.StdoutStreamTooLong;
                if (poller.fifo(.stderr).count > max_output_bytes)
                    return error.StderrStreamTooLong;
            }

            const err = try child.wait();
            blk: {
                var err_buf = std.ArrayList(u8).init(alloc);

                switch (err) {
                    .Exited => |e| {
                        if (e != 0) {
                            _ = try err_buf.writer().print("exited with code: {}\n", .{e});
                        } else {
                            err_buf.deinit();
                            break :blk;
                        }
                    },
                    // .Signal => |code| {},
                    // .Stopped => |code| {},
                    // .Unknown => |code| {},
                    else => |e| {
                        try err_buf.writer().print("exited with code: {}\n", .{e});
                    },
                }

                const fifo = poller.fifo(.stderr);
                try err_buf.appendSlice(fifo.buf[fifo.head..][0..fifo.count]);
                return .{
                    .Err = .{
                        .err = Err.GlslcErroredOut,
                        .msg = try err_buf.toOwnedSlice(),
                    },
                };
            }

            const fifo = poller.fifo(.stdout);
            var aligned = std.ArrayListAligned(u8, 4).init(allocator);
            try aligned.appendSlice(fifo.buf[fifo.head..][0..fifo.count]);
            const bytes = try aligned.toOwnedSlice();
            return .{ .Ok = switch (output_type) {
                .spirv => std.mem.bytesAsSlice(u32, bytes),
                .assembly => bytes,
            } };
        }
    };
};
