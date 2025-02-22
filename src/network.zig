const std = @import("std");

const utils = @import("utils.zig");

const math = @import("math.zig");

const main = @import("main.zig");
const allocator = main.allocator;

const Engine = @import("engine.zig");

const posix = std.posix;

const Event = union(enum(u8)) {
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
        input: Engine.Window.InputState,
    },
    quit,

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
        channel: PacketStream,
        quit: utils.Fuse = .{},
        is_server: bool,
        has_received: bool,

        const PacketStream = utils.Channel(struct { event: Event, addr: std.net.Address });

        fn listen(self: *@This()) !void {
            while (true) {
                if (self.quit.unfuse()) {
                    return;
                }

                var src_addrlen: posix.socklen_t = @sizeOf(posix.sockaddr);
                var src_addr: posix.sockaddr align(4) = std.mem.zeroes(posix.sockaddr);
                const n = posix.recvfrom(
                    self.sock,
                    self.buf,
                    0, // posix.SOCK.NONBLOCK, for non blocking
                    &src_addr,
                    &src_addrlen,
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
                        try self.channel.send(.{ .event = @unionInit(Event, @tagName(t), p), .addr = std.net.Address.initPosix(&src_addr) });
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

        ctx.channel = try ListenCtx.PacketStream.init(allocator.*);
        errdefer ctx.channel.deinit();

        ctx.quit = .{};
        ctx.sock = sock;
        ctx.is_server = true;

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

    pub fn send(self: *@This(), val: Event, addr: std.net.Address) !void {
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
                    &addr.any,
                    addr.getOsSockLen(),
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
