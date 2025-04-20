const std = @import("std");

const utils_mod = @import("utils.zig");

const main = @import("main.zig");
const allocator = main.allocator;

const c = @cImport({
    @cInclude("steamworks/api.h");
});

pub const NetworkingContext = struct {
    ctx: *Ctx,
    thread: std.Thread,

    const Ctx = struct {
        steam: c.ZhottSteamCtx,
        options: Options,

        client: ?Client = null,
        server: ?Server = null,

        ctx_mutex: std.Thread.Mutex = .{},
        quit: utils_mod.Fuse = .{},

        fn start(self: *@This()) void {
            while (true) {
                if (self.quit.unfuse()) {
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
        tick_fps_inv: u64 = 150,
    };

    pub fn init(options: Options) !@This() {
        std.fs.cwd().access("./steam_appid.txt", .{}) catch |e| {
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
        const thread = try std.Thread.spawn(.{}, Ctx.start, .{ctx});
        errdefer {
            _ = ctx.quit.fuse();
            thread.join();
        }

        return .{
            .ctx = ctx,
            .thread = thread,
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

        _ = ctx.quit.fuse();
        self.thread.join();

        ctx.ctx_mutex.lock();
        defer ctx.ctx_mutex.unlock();

        std.debug.assert(ctx.client == null);
        std.debug.assert(ctx.server == null);

        c.steam_deinit(ctx.steam);
    }

    pub const Server = struct {
        ctx: *Ctx,
        messages: utils_mod.Channel(struct {}),

        fn init(ctx: *Ctx) !*@This() {
            ctx.ctx_mutex.lock();
            defer ctx.ctx_mutex.unlock();

            if (!c.server_init(ctx.steam)) {
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
    };

    pub const Client = struct {
        ctx: *Ctx,
        messages: utils_mod.Channel(struct {}),

        fn init(ctx: *Ctx) !*@This() {
            ctx.ctx_mutex.lock();
            defer ctx.ctx_mutex.unlock();

            c.client_init(ctx.steam);
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
    };
};
