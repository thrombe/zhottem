const std = @import("std");
const builtin = @import("builtin");

const utils_mod = @import("utils.zig");

const math = @import("math.zig");

const main = @import("main.zig");
const allocator = main.allocator;

const engine_mod = @import("engine.zig");

const posix = std.posix;

// windows does not support MSG.DONTWAIT
// so we only use non-blocking sockets on windows.
// on linux however - we can keep the blocking sockets and set them to non-blocking
// for one call to recvfrom using MSG.DONTWAIT
const is_windows = builtin.os.tag == .windows;

pub const Event = union(enum(u8)) {
    join,
    setid: packed struct {
        id: u8,
    },
    spawn_player: packed struct {
        id: u8,
        pos: Vec3,
    },
    despawn_player: packed struct {
        id: u8,
    },
    input: packed struct {
        id: u8,
        input: engine_mod.Window.InputState,
    },
    quit,

    const Vec3 = packed struct {
        x: f32,
        y: f32,
        z: f32,
    };
};

pub const NetworkingContext = struct {
    ctx: *Ctx,

    const constants = struct {
        // https://stackoverflow.com/a/35697810
        const max_payload_size = 1400;
    };

    pub fn init(options: Options) !@This() {
        const ctx = try allocator.create(Ctx);
        errdefer allocator.destroy(ctx);
        ctx.* = .{
            .options = options,
        };

        return .{
            .ctx = ctx,
        };
    }

    pub fn server(self: *@This()) !*Server {
        var s = try Server.init(self.ctx);
        errdefer s.deinit();
        return s;
    }

    pub fn client(self: *@This()) !*Client {
        var e = try Client.init(self.ctx);
        errdefer e.deinit();
        return e;
    }

    pub fn deinit(self: *@This()) void {
        const ctx = self.ctx;
        defer allocator.destroy(ctx);

        if (ctx.server) |*e| e.deinit();
        if (ctx.client) |*e| e.deinit();

        std.debug.assert(ctx.client == null);
        std.debug.assert(ctx.server == null);
    }

    pub fn tick(self: *@This()) !void {
        if (self.ctx.server) |*e| try e.tick();
        if (self.ctx.client) |*e| try e.tick();
    }

    pub fn pre_reload(self: *@This()) !void {
        if (self.ctx.server) |*e| try e.pre_reload();
        if (self.ctx.client) |*e| try e.pre_reload();
    }

    pub fn post_reload(self: *@This()) !void {
        if (self.ctx.server) |*e| try e.post_reload();
        if (self.ctx.client) |*e| try e.post_reload();
    }

    const Options = struct {
        tick_thread: bool = false,
        tick_fps_inv: u64 = 150,
        server_port: u16 = 27015,
    };

    const OutgoingMessage = struct {
        event: Event,
        flags: struct {
            reliable: bool = false,
            force_flush: bool = false,
            no_delay: bool = false,
            restart_broken_session: bool = false,
        } = .{},
    };

    const ClientMessage = struct {
        conn: u32,
        user_id: u64,
        message_number: i64,
        event: Event,
    };

    const RecvedMessage = struct {
        conn: u32,
        msg_num: i64,
        event: Event,
    };

    const Ctx = struct {
        options: Options,

        client: ?Client = null,
        server: ?Server = null,
    };

    pub const Server = struct {
        ctx: *Ctx,
        socket: posix.socket_t,
        next_conn_id: u32 = 1,
        address_map: std.AutoHashMap(u32, std.net.Address),
        conn_map: std.HashMap(std.net.Address, u32, struct {
            pub fn hash(ctx: @This(), key: std.net.Address) u32 {
                _ = ctx;
                var hasher = std.hash.Wyhash.init(0);
                const bytes = @as([*]const u8, @ptrCast(&key.any))[0..key.getOsSockLen()];
                hasher.update(bytes);
                return @truncate(hasher.final());
            }
            pub fn eql(ctx: @This(), a: std.net.Address, b: std.net.Address) bool {
                _ = ctx;
                return a.eql(b);
            }
        }, 80),
        messages: utils_mod.Channel(RecvedMessage),
        thread: ?std.Thread,
        quit: utils_mod.Fuse = .{},

        fn init(ctx: *Ctx) !*@This() {
            const sock = try posix.socket(
                posix.AF.INET,
                posix.SOCK.DGRAM | if (is_windows) posix.SOCK.NONBLOCK else 0,
                posix.IPPROTO.UDP,
            );
            errdefer posix.close(sock);

            try posix.setsockopt(sock, posix.SOL.SOCKET, posix.SO.RCVTIMEO, std.mem.asBytes(&posix.timeval{
                .sec = 0,
                .usec = @intCast(std.time.us_per_ms * ctx.options.tick_fps_inv),
            }));

            // sending udp packets is non-blocking
            // try posix.setsockopt(sock, posix.SOL.SOCKET, posix.SO.SNDTIMEO, std.mem.asBytes(&posix.timeval{
            //     .sec = 2,
            //     .usec = 500_000,
            // }));

            var addr = try std.net.Address.parseIp4("127.0.0.1", ctx.options.server_port);
            posix.bind(sock, &addr.any, addr.getOsSockLen()) catch |e| switch (e) {
                error.AddressInUse => return e,
                else => return e,
            };

            var s = @This(){
                .ctx = ctx,
                .socket = sock,
                .address_map = .init(allocator.*),
                .conn_map = .init(allocator.*),
                .messages = try .init(allocator.*),
                .thread = null,
            };
            errdefer {
                _ = s.quit.fuse();
                if (s.thread) |t| t.join();
                s.messages.deinit();
                s.address_map.deinit();
                s.conn_map.deinit();
            }

            if (ctx.options.tick_thread) {
                s.thread = try std.Thread.spawn(.{}, @This().start, .{ctx});
            }

            ctx.server = s;
            return &ctx.server.?;
        }

        fn start(ctx: *Ctx) void {
            while (true) if (ctx.server) |*e| if (e.quit.check()) break else e.tick_once(0) catch |err| switch (err) {
                error.WouldBlock => if (is_windows) std.Thread.sleep(ctx.options.tick_fps_inv),
                else => {
                    utils_mod.dump_error(err);
                    std.Thread.sleep(ctx.options.tick_fps_inv);
                },
            };
        }

        fn pre_reload(self: *@This()) !void {
            _ = self.quit.fuse();
            defer _ = self.quit.unfuse();

            if (self.thread) |t| t.join();
        }

        fn post_reload(self: *@This()) !void {
            if (self.ctx.options.tick_thread) {
                self.thread = try std.Thread.spawn(.{}, @This().start, .{self.ctx});
            }
        }

        pub fn deinit(self: *@This()) void {
            _ = self.quit.fuse();
            if (self.thread) |t| t.join();

            posix.close(self.socket);
            self.address_map.deinit();
            self.conn_map.deinit();
            self.messages.deinit();

            const ctx = self.ctx;
            ctx.server = null;
        }

        fn tick_once(self: *@This(), flags: u32) !void {
            var buf: [1500]u8 = undefined;
            var src_addrlen: posix.socklen_t = @sizeOf(posix.sockaddr);
            var src_addr: posix.sockaddr align(4) = std.mem.zeroes(posix.sockaddr);

            const n = try posix.recvfrom(
                self.socket,
                &buf,
                flags,
                &src_addr,
                &src_addrlen,
            );

            const addr = std.net.Address.initPosix(&src_addr);

            var conn: u32 = 0;
            if (self.conn_map.get(addr)) |c| {
                conn = c;
            } else {
                conn = self.next_conn_id;
                self.next_conn_id += 1;

                try self.address_map.put(conn, addr);
                try self.conn_map.put(addr, conn);
            }

            const event = std.mem.bytesToValue(Event, buf[0..n]);
            try self.messages.send(.{
                .conn = conn,
                .msg_num = 0,
                .event = event,
            });
        }

        fn tick(self: *@This()) !void {
            while (true) self.tick_once(if (is_windows) 0 else posix.MSG.DONTWAIT) catch |e| switch (e) {
                error.WouldBlock => break,
                else => return e,
            };
        }

        pub fn send_message(self: *@This(), conn: u32, from_conn: u32, msg: OutgoingMessage) !void {
            const addr = self.address_map.get(conn) orelse return error.InvalidConnection;
            const event = ClientMessage{
                .user_id = @intCast(from_conn),
                .message_number = 0,
                .conn = from_conn,
                .event = msg.event,
            };

            _ = try posix.sendto(
                self.socket,
                @as([*c]const u8, @ptrCast(&event))[0..@sizeOf(@TypeOf(event))],
                0,
                &addr.any,
                addr.getOsSockLen(),
            );
        }
    };

    pub const Client = struct {
        ctx: *Ctx,
        socket: posix.socket_t,
        server_addr: std.net.Address,
        messages: utils_mod.Channel(RecvedMessage),
        thread: ?std.Thread,
        quit: utils_mod.Fuse = .{},

        fn init(ctx: *Ctx) !*@This() {
            const sock = try posix.socket(
                posix.AF.INET,
                posix.SOCK.DGRAM | if (is_windows) posix.SOCK.NONBLOCK else 0,
                posix.IPPROTO.UDP,
            );
            errdefer posix.close(sock);

            try posix.setsockopt(sock, posix.SOL.SOCKET, posix.SO.RCVTIMEO, std.mem.asBytes(&posix.timeval{
                .sec = 0,
                .usec = @intCast(std.time.us_per_ms * ctx.options.tick_fps_inv),
            }));

            // Bind to any available port
            var addr = try std.net.Address.parseIp4("127.0.0.1", 0);
            try posix.bind(sock, &addr.any, addr.getOsSockLen());

            const server_addr = try std.net.Address.parseIp4("127.0.0.1", ctx.options.server_port);

            var s = @This(){
                .ctx = ctx,
                .socket = sock,
                .server_addr = server_addr,
                .messages = try .init(allocator.*),
                .thread = null,
            };
            errdefer {
                _ = s.quit.fuse();
                if (s.thread) |t| t.join();
                s.messages.deinit();
            }

            if (ctx.options.tick_thread) {
                s.thread = try std.Thread.spawn(.{}, @This().start, .{ctx});
            }

            ctx.client = s;
            return &ctx.client.?;
        }

        fn start(ctx: *Ctx) void {
            while (true) if (ctx.client) |*e| if (e.quit.check()) break else e.tick_once(0) catch |err| switch (err) {
                error.WouldBlock => if (is_windows) std.Thread.sleep(ctx.options.tick_fps_inv),
                else => {
                    utils_mod.dump_error(err);
                    std.Thread.sleep(ctx.options.tick_fps_inv);
                },
            };
        }

        fn pre_reload(self: *@This()) !void {
            _ = self.quit.fuse();
            defer _ = self.quit.unfuse();

            if (self.thread) |t| t.join();
        }

        fn post_reload(self: *@This()) !void {
            if (self.ctx.options.tick_thread) {
                self.thread = try std.Thread.spawn(.{}, @This().start, .{self.ctx});
            }
        }

        pub fn deinit(self: *@This()) void {
            _ = self.quit.fuse();
            if (self.thread) |t| t.join();

            posix.close(self.socket);
            self.messages.deinit();

            const ctx = self.ctx;
            ctx.client = null;
        }

        fn tick_once(self: *@This(), flags: u32) !void {
            var buf: [1500]u8 = undefined;
            var src_addrlen: posix.socklen_t = @sizeOf(posix.sockaddr);
            var src_addr: posix.sockaddr align(4) = std.mem.zeroes(posix.sockaddr);

            const n = try posix.recvfrom(
                self.socket,
                &buf,
                flags,
                &src_addr,
                &src_addrlen,
            );

            if (!std.net.Address.initPosix(&src_addr).eql(self.server_addr)) {
                return error.PacketNotFromServer;
            }

            const event = std.mem.bytesToValue(ClientMessage, buf[0..n]);
            try self.messages.send(.{
                .conn = event.conn,
                .msg_num = event.message_number,
                .event = event.event,
            });
        }

        fn tick(self: *@This()) !void {
            while (true) self.tick_once(if (is_windows) 0 else posix.MSG.DONTWAIT) catch |e| switch (e) {
                error.WouldBlock => break,
                else => return e,
            };
        }

        pub fn send_message(self: *@This(), msg: OutgoingMessage) !void {
            _ = try posix.sendto(
                self.socket,
                @as([*c]const u8, @ptrCast(&msg.event))[0..@sizeOf(@TypeOf(msg.event))],
                0,
                &self.server_addr.any,
                self.server_addr.getOsSockLen(),
            );

            // TODO: maybe support packets with arbitary buffers
            // const event = msg.event;
            // const T = @TypeOf(event);
            // switch (event) {
            //     inline else => |p, t| {
            //         var buf: [@sizeOf(std.meta.Tag(T)) + @sizeOf(@TypeOf(p))]u8 = undefined;
            //         buf[0] = std.mem.asBytes(&t)[0];
            //         @memcpy(buf[1..], std.mem.asBytes(&p));

            //         const n = try posix.sendto(
            //             self.socket,
            //             buf[0..],
            //             0,
            //             &addr.any,
            //             addr.getOsSockLen(),
            //         );

            //         if (n < @sizeOf(std.meta.Tag(T)) + @sizeOf(@TypeOf(p))) {
            //             return error.CouldNotSendIn1Call;
            //         }
            //     },
            // }
        }

        pub fn wait_for_connection(self: *@This()) !void {
            // no need to wait as there's no connection for raw udp sockets.
            _ = self;
        }
    };
};
