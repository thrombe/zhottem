const std = @import("std");

const utils_mod = @import("utils.zig");

const network_mod = @import("network.zig");
const Event = network_mod.Event;

const main = @import("main.zig");
const allocator = main.allocator;

const c = @cImport({
    @cInclude("steamworks/api.h");
});

pub const NetworkingContext = struct {
    ctx: *Ctx,
    thread: ?std.Thread,

    pub fn init(options: Options) !@This() {
        const path = try utils_mod.fspath.cwd_join(allocator.*, "steam_appid.txt");
        defer allocator.free(path);
        std.fs.accessAbsolute(path, .{}) catch |e| {
            switch (e) {
                error.FileNotFound => return error.SteamAppIdTxtNotFound,
                else => return e,
            }
        };

        const steam = c.steam_init() orelse return error.ErrorInitializingSteam;
        errdefer c.steam_deinit(steam);

        const ctx = try allocator.create(Ctx);
        errdefer allocator.destroy(ctx);
        ctx.* = .{
            .steam = steam,
            .options = options,
        };

        if (options.tick_thread) {
            const thread = try std.Thread.spawn(.{}, Ctx.start, .{ctx});
            errdefer {
                _ = ctx.quit.fuse();
                thread.join();
            }

            return .{
                .ctx = ctx,
                .thread = thread,
            };
        } else {
            return .{
                .ctx = ctx,
                .thread = null,
            };
        }
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

        _ = ctx.quit.fuse();
        if (self.thread) |t| t.join();

        ctx.ctx_mutex.lock();
        defer ctx.ctx_mutex.unlock();

        std.debug.assert(ctx.client == null);
        std.debug.assert(ctx.server == null);

        c.steam_deinit(ctx.steam);
    }

    pub fn tick(self: *@This()) !void {
        try self.ctx.tick();
    }

    pub fn pre_reload(self: *@This()) !void {
        _ = self.ctx.quit.fuse();
        defer _ = self.ctx.quit.unfuse();
        if (self.thread) |t| t.join();

        if (self.ctx.client) |*e| e.pre_reload();
        if (self.ctx.server) |*e| e.pre_reload();
    }

    pub fn post_reload(self: *@This()) !void {
        if (self.ctx.client) |*e| e.post_reload();
        if (self.ctx.server) |*e| e.post_reload();

        if (self.ctx.options.tick_thread) {
            self.thread = try std.Thread.spawn(.{}, Ctx.start, .{self.ctx});
        }
    }

    const Ctx = struct {
        steam: c.ZhottSteamCtx,
        options: Options,

        client: ?Client = null,
        server: ?Server = null,

        ctx_mutex: std.Thread.Mutex = .{},
        quit: utils_mod.Fuse = .{},

        fn start(self: *@This()) void {
            while (true) {
                if (self.quit.check()) {
                    return;
                }

                self.tick() catch |e| {
                    utils_mod.dump_error(e);
                };

                std.Thread.sleep(self.options.tick_fps_inv);
            }
        }

        fn tick(self: *@This()) !void {
            self.ctx_mutex.lock();
            defer self.ctx_mutex.unlock();

            if (self.client) |*e| e.tick();
            if (self.server) |*e| e.tick();
        }
    };

    const Options = struct {
        tick_thread: bool = false,
        tick_fps_inv: u64 = 150,
    };

    const OutgoingMessage = struct {
        event: Event,
        flags: @FieldType(c.OutgoingMessage, "flags") = .{
            .reliable = true,
            .force_flush = false,
            .no_delay = false,
            .restart_broken_session = true,
        },
    };
    const RecvedMessage = struct {
        conn: u32,
        msg_num: i64,
        event: Event,
    };

    pub const Server = struct {
        ctx: *Ctx,
        messages: utils_mod.Channel(RecvedMessage),

        fn init(ctx: *Ctx) !*@This() {
            ctx.ctx_mutex.lock();
            defer ctx.ctx_mutex.unlock();

            if (!c.server_init(ctx.steam, .{
                .ctx = &ctx.server,
                .msg_recv = @ptrCast(&msg_recv),
            })) {
                return error.CouldNotInitServer;
            }
            errdefer c.server_deinit(ctx.steam);

            ctx.server = .{ .ctx = ctx, .messages = try .init(allocator.*) };
            return &ctx.server.?;
        }

        pub fn deinit(self: *@This()) void {
            const ctx = self.ctx;
            ctx.ctx_mutex.lock();
            defer ctx.ctx_mutex.unlock();

            c.server_deinit(ctx.steam);
            self.messages.deinit();

            ctx.server = null;
        }

        fn tick(self: *@This()) void {
            c.server_tick(self.ctx.steam);
        }

        fn pre_reload(self: *@This()) void {
            c.server_pre_reload(self.ctx.steam);
        }

        fn post_reload(self: *@This()) void {
            c.server_post_reload(self.ctx.steam, .{
                .ctx = self,
                .msg_recv = @ptrCast(&msg_recv),
            });
        }

        fn msg_recv(self: *@This(), msg: c.NetworkMessage) callconv(.C) void {
            const event = std.mem.bytesToValue(Event, msg.data[0..msg.len]);
            self.messages.send(.{
                .conn = msg.conn,
                .msg_num = msg.message_number,
                .event = event,
            }) catch @panic("OOM");
        }

        pub fn send_message(self: *@This(), conn: u32, from_conn: u32, msg: OutgoingMessage) !void {
            const event = msg.event;
            c.server_msg_send(self.ctx.steam, conn, from_conn, .{
                .data = @ptrCast(&event),
                .len = @sizeOf(@TypeOf(msg.event)),
                .flags = msg.flags,
            });
        }
    };

    pub const Client = struct {
        ctx: *Ctx,
        messages: utils_mod.Channel(RecvedMessage),

        fn init(ctx: *Ctx) !*@This() {
            ctx.ctx_mutex.lock();
            defer ctx.ctx_mutex.unlock();

            c.client_init(ctx.steam, .{
                .ctx = &ctx.client,
                .msg_recv = @ptrCast(&msg_recv),
            });
            errdefer c.client_deinit(ctx.steam);

            ctx.client = .{ .ctx = ctx, .messages = try .init(allocator.*) };
            return &ctx.client.?;
        }

        pub fn deinit(self: *@This()) void {
            const ctx = self.ctx;
            ctx.ctx_mutex.lock();
            defer ctx.ctx_mutex.unlock();

            c.client_deinit(ctx.steam);
            self.messages.deinit();

            ctx.client = null;
        }

        fn tick(self: *@This()) void {
            c.client_tick(self.ctx.steam);
        }

        fn pre_reload(self: *@This()) void {
            c.client_pre_reload(self.ctx.steam);
        }

        fn post_reload(self: *@This()) void {
            c.client_post_reload(self.ctx.steam, .{
                .ctx = self,
                .msg_recv = @ptrCast(&msg_recv),
            });
        }

        fn msg_recv(self: *@This(), msg: c.NetworkMessage) callconv(.C) void {
            const event = std.mem.bytesToValue(Event, msg.data[0..msg.len]);
            self.messages.send(.{
                .conn = msg.conn,
                .msg_num = msg.message_number,
                .event = event,
            }) catch @panic("OOM");
        }

        pub fn send_message(self: *@This(), msg: OutgoingMessage) !void {
            const event = msg.event;
            c.client_msg_send(self.ctx.steam, .{
                .data = @ptrCast(&event),
                .len = @sizeOf(@TypeOf(msg.event)),
                .flags = msg.flags,
            });
        }

        pub fn wait_for_connection(self: *@This()) !void {
            while (!c.client_is_connected(self.ctx.steam)) {
                std.Thread.sleep(std.time.ns_per_ms * 50);
                try self.ctx.tick();
            }
        }
    };
};
