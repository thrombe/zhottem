const std = @import("std");

const utils = @import("utils.zig");

const math = @import("math.zig");

const main = @import("main.zig");
const allocator = main.allocator;

const posix = std.posix;

const Event = union(enum(u8)) {
    begin,
    end,
    myname: packed struct {
        id: u8,
        pos: Vec3,
    },
    yourname: packed struct {
        id: u8,
    },
    pos: packed struct {
        id: u8,
        pos: Vec3,
    },

    const Vec3 = packed struct {
        x: f32,
        y: f32,
        z: f32,
    };
};

pub const Socket = struct {
    ctx: *ListenCtx,
    listener: std.Thread,

    const ListenCtx = struct {
        sock: posix.socket_t,
        buf: []u8,
        channel: utils.Channel(Event),
        quit: utils.Fuse = .{},
        is_server: bool,
        has_received: bool,

        addr: std.net.Address,
        src_addr: posix.sockaddr = std.mem.zeroes(posix.sockaddr),
        src_addrlen: posix.socklen_t = @sizeOf(posix.sockaddr),

        fn listen(self: *@This()) !void {
            while (true) {
                if (self.quit.unfuse()) {
                    return;
                }

                const n = posix.recvfrom(
                    self.sock,
                    self.buf,
                    0, // posix.SOCK.NONBLOCK, for non blocking
                    &self.src_addr,
                    &self.src_addrlen,
                ) catch |e| switch (e) {
                    error.WouldBlock => {
                        continue;
                    },
                    else => return e,
                };
                self.has_received = true;
                const buf = self.buf[0..n];

                const tag: std.meta.Tag(Event) = @enumFromInt(self.buf[0]);
                switch (tag) {
                    inline else => |t| {
                        const p = std.mem.bytesToValue(std.meta.TagPayload(Event, t), buf[1..]);
                        try self.channel.send(@unionInit(Event, @tagName(t), p));
                    },
                }

                if (!self.is_server) {} else {}
            }
        }
    };

    const constants = struct {
        // https://stackoverflow.com/a/35697810
        const max_payload_size = 1400;
        const read_timeout = 1.0 / 60.0;
    };

    pub fn init(addr: std.net.Address) !@This() {
        const sock = try posix.socket(posix.AF.INET, posix.SOCK.DGRAM, posix.IPPROTO.UDP);
        errdefer posix.close(sock);

        try posix.setsockopt(sock, posix.SOL.SOCKET, posix.SO.RCVTIMEO, std.mem.asBytes(&posix.timeval{
            .tv_sec = 0,
            .tv_usec = @intFromFloat(std.time.us_per_s * constants.read_timeout),
        }));
        // try posix.setsockopt(sock, posix.SOL.SOCKET, posix.SO.SNDTIMEO, std.mem.asBytes(&posix.timeval{
        //     .tv_sec = 2,
        //     .tv_usec = 500_000,
        // }));

        const ctx = try allocator.create(ListenCtx);
        errdefer allocator.destroy(ctx);

        ctx.buf = try allocator.alloc(u8, 1500);
        errdefer allocator.free(ctx.buf);

        ctx.channel = try utils.Channel(Event).init(allocator.*);
        errdefer ctx.channel.deinit();

        ctx.quit = .{};
        ctx.sock = sock;
        ctx.is_server = true;
        ctx.addr = addr;
        ctx.src_addrlen = @sizeOf(posix.sockaddr);

        posix.bind(sock, &addr.any, addr.getOsSockLen()) catch |e| switch (e) {
            error.AddressInUse => {
                ctx.is_server = false;
            },
            else => return e,
        };

        return .{
            .ctx = ctx,
            .listener = try std.Thread.spawn(.{}, ListenCtx.listen, .{ctx}),
        };
    }

    pub fn deinit(self: *@This()) void {
        _ = self.ctx.quit.fuse();
        self.listener.join();
        posix.close(self.ctx.sock);
        allocator.free(self.ctx.buf);
        self.ctx.channel.deinit();
        allocator.destroy(self.ctx);
    }

    pub fn send(self: *@This(), val: Event) !void {
        const T = @TypeOf(val);
        switch (val) {
            inline else => |p, t| {
                var buf: [@sizeOf(std.meta.Tag(T)) + @sizeOf(@TypeOf(p))]u8 = undefined;
                buf[0] = std.mem.asBytes(&t)[0];
                @memcpy(buf[1..], std.mem.asBytes(&p));

                const n = try posix.sendto(
                    self.ctx.sock,
                    buf[0..],
                    0,
                    if (self.ctx.is_server) &self.ctx.src_addr else &self.ctx.addr.any,
                    if (self.ctx.is_server) self.ctx.src_addrlen else self.ctx.addr.getOsSockLen(),
                );

                if (n < @sizeOf(std.meta.Tag(T)) + @sizeOf(@TypeOf(p))) {
                    return error.CouldNotSendIn1Call;
                }
            },
        }
    }

    pub fn recv(self: *@This()) ?Event {
        return self.ctx.channel.try_recv();
    }
};

fn server(addr: std.net.Address) !void {
    const sock = try posix.socket(posix.AF.INET, posix.SOCK.DGRAM, posix.IPPROTO.UDP);
    defer posix.close(sock);

    // SO.REUSEADDR so that os does not keep it reserved for some time after the program is closed.
    // try posix.setsockopt(
    //     sock,
    //     posix.SOL.SOCKET,
    //     posix.SO.REUSEADDR,
    //     std.mem.asBytes(&@as(c_int, 1)),
    // );
    // try posix.setsockopt(sock, posix.SOL.SOCKET, posix.SO.RCVTIMEO, std.mem.asBytes(&posix.timeval{
    //     .tv_sec = 2,
    //     .tv_usec = 500_000,
    // }));
    // try posix.setsockopt(sock, posix.SOL.SOCKET, posix.SO.SNDTIMEO, std.mem.asBytes(&posix.timeval{
    //     .tv_sec = 2,
    //     .tv_usec = 500_000,
    // }));
    try posix.bind(sock, &addr.any, addr.getOsSockLen());

    var other_addr: posix.sockaddr = undefined;
    var other_addrlen: posix.socklen_t = @sizeOf(posix.sockaddr);

    var buf: [1024]u8 = undefined;

    std.debug.print("Listen on {any}...\n", .{addr});

    while (true) {
        const n_recv = try posix.recvfrom(
            sock,
            buf[0..],
            0, // posix.SOCK.NONBLOCK, for non blocking
            &other_addr,
            &other_addrlen,
        );
        std.debug.print(
            "received {d} byte(s) from {any};\n    string: {s}\n",
            .{ n_recv, other_addr, buf[0..n_recv] },
        );

        // we could extract the source address of the received data by
        // parsing the other_addr.data field

        const n_sent = try posix.sendto(
            sock,
            buf[0..n_recv],
            0,
            &other_addr,
            other_addrlen,
        );
        std.debug.print("echoed {d} byte(s) back\n", .{n_sent});
    }
}

fn client(addr: std.net.Address) !void {
    const sock = try posix.socket(posix.AF.INET, posix.SOCK.DGRAM, posix.IPPROTO.UDP);
    defer posix.close(sock);

    _ = try posix.sendto(sock, "heyo", 0, &addr.any, addr.getOsSockLen());

    var buf: [1024]u8 = undefined;
    const n_recv = try posix.recvfrom(
        sock,
        buf[0..],
        0, // posix.SOCK.NONBLOCK, for non blocking
        @constCast(&addr.any),
        @constCast(&addr.getOsSockLen()),
    );
    std.debug.print(
        "received {d} byte(s): {s}\n",
        .{ n_recv, buf[0..n_recv] },
    );
}

fn writeMessage(socket: posix.socket_t, msg: []const u8) !void {
    var buf: [4]u8 = undefined;
    std.mem.writeInt(u32, &buf, @intCast(msg.len), .little);

    var vec = [2]posix.iovec_const{
        .{ .len = 4, .base = &buf },
        .{ .len = msg.len, .base = msg.ptr },
    };

    try writeAllVectored(socket, &vec);
}

fn writeAllVectored(socket: posix.socket_t, vec: []posix.iovec_const) !void {
    var i: usize = 0;
    while (true) {
        var n = try posix.writev(socket, vec[i..]);
        while (n >= vec[i].len) {
            n -= vec[i].len;
            i += 1;
            if (i >= vec.len) return;
        }
        vec[i].base += n;
        vec[i].len -= n;
    }
}

const Reader = struct {
    buf: []u8,
    pos: usize = 0,
    start: usize = 0,
    sock: posix.socket_t,

    fn read_message(self: *@This()) ![]u8 {
        while (true) {
            if (try self.buffered_message()) |msg| {
                return msg;
            }

            // TODO: pos == buf.len-1??
            const n = try posix.read(self.sock, self.buf[self.pos..]);
            if (n == 0) {
                return error.SocketClosed;
            }

            self.pos += n;
        }
    }

    fn buffered_message(self: *@This()) !?[]u8 {
        _ = self;
    }

    fn ensure_space(self: *@This(), size: usize) !void {
        if (self.buf.len < size) {}
    }
};

pub fn testfn() !void {
    const addr = try std.net.Address.parseIp("127.0.0.1", 8072);

    // server(addr) catch |e| switch (e) {
    //     // error.AddressInUse => try client(addr),
    //     error.AddressInUse => {},
    //     else => return e,
    // };

    var sock = try Socket.init(addr);
    defer sock.deinit();

    if (!sock.ctx.is_server) {
        try sock.send(Event{ .begin = {} });
    }

    while (true) {
        while (sock.ctx.channel.try_recv()) |v| {
            switch (v) {
                .begin => {
                    try sock.send(.{ .yourname = .{ .id = 1 } });
                    try sock.send(.{ .myname = .{ .id = 0 } });
                },
                else => {},
            }
            std.debug.print("{any}\n", .{v});
        }
    }
}
