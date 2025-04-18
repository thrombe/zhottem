const std = @import("std");

const c = @cImport({
    @cInclude("steamworks.h");
});

pub fn testfn() !void {
    std.fs.cwd().access("./steam_appid.txt", .{}) catch |e| {
        switch (e) {
            error.FileNotFound => return error.SteamAppIdTxtNotFound,
            else => return e,
        }
    };

    const steam = c.steam_init() orelse return error.ErrorInitializingSteam;
    defer c.steam_deinit(steam);

    c.server_init(steam);
    defer c.server_deinit(steam);

    c.client_init(steam);
    defer c.client_deinit(steam);

    // while (!c.client_connect_to_server(steam)) {
    //     std.Thread.sleep(std.time.ns_per_ms * 100);
    // }

    while (true) {
        c.client_tick(steam);
        c.server_tick(steam);
        std.Thread.sleep(std.time.ns_per_ms * 300);
    }

    return error.Oof;
}
