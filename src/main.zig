const std = @import("std");

const options = @import("build-options");
const utils_mod = @import("utils.zig");

var _allocator: std.mem.Allocator = undefined;

// pub const is required so that this can directly be imported and used in other files
pub const allocator: *std.mem.Allocator = &_allocator;

const HotAction = enum(u8) {
    nothing,
    errored,
    quit,
};

const HotVtable = struct {
    const Init = *const fn () callconv(.c) ?*anyopaque;
    const Deinit = *const fn (app: *anyopaque) callconv(.c) HotAction;
    const Tick = *const fn (app: *anyopaque) callconv(.c) HotAction;
    const PreReload = *const fn (app: *anyopaque) callconv(.c) HotAction;
    const PostReload = *const fn (app: *anyopaque) callconv(.c) HotAction;

    init: Init,
    deinit: Deinit,
    tick: Tick,
    pre_reload: PreReload,
    post_reload: PostReload,

    fn from_dyn(dyn: *std.DynLib) !@This() {
        return .{
            .init = dyn.lookup(Init, "hot_init") orelse return error.FuncNotFound,
            .deinit = dyn.lookup(Deinit, "hot_deinit") orelse return error.FuncNotFound,
            .tick = dyn.lookup(Tick, "hot_tick") orelse return error.FuncNotFound,
            .pre_reload = dyn.lookup(PreReload, "hot_pre_reload") orelse return error.FuncNotFound,
            .post_reload = dyn.lookup(PostReload, "hot_post_reload") orelse return error.FuncNotFound,
        };
    }
};

// NOTE: global state breaks when hot reloading is turned on.
// things that use global state must be separated into another dylib (like glfw)
const HotReloader = struct {
    path: [:0]const u8,
    hot_cache: []const u8,
    libpath: []const u8,

    dylib: std.DynLib,
    vtable: HotVtable,
    app: *anyopaque,
    fs: utils_mod.FsFuse,
    count: u32 = 0,

    fn init(comptime path: [:0]const u8, comptime hotcache_path: [:0]const u8) !@This() {
        const libpath = path ++ options.hotlib_name;
        const hot_cache = hotcache_path ++ options.hotlib_name;

        const hot_lib = try utils_mod.fspath.cwd_join(allocator.*, hot_cache);
        errdefer allocator.free(hot_lib);

        const hot_lib_src = try utils_mod.fspath.cwd_join(allocator.*, libpath);
        errdefer allocator.free(hot_lib_src);
        try std.fs.copyFileAbsolute(hot_lib_src, hot_lib, .{});

        var dyn = try std.DynLib.open(hot_lib);
        errdefer dyn.close();

        const vtable = try HotVtable.from_dyn(&dyn);

        const app = vtable.init() orelse return error.CouldNotInitApp;
        errdefer _ = vtable.deinit(app);

        var fs = try utils_mod.FsFuse.init(path);
        errdefer fs.deinit();

        return .{
            .libpath = hot_lib_src,
            .path = path,
            .hot_cache = hot_lib,
            .dylib = dyn,
            .vtable = vtable,
            .app = app,
            .fs = fs,
        };
    }

    fn deinit(self: *@This()) void {
        // NOTE: closing the dylib makes the app crash at __run_exit_handlers when exiting
        // defer self.dylib.close();

        defer _ = self.vtable.deinit(self.app);
        defer self.fs.deinit();
        defer allocator.free(self.hot_cache);
        defer allocator.free(self.libpath);
    }

    fn tick(self: *@This()) !bool {
        {
            // we don't actually care about the exact event in this case.
            while (self.fs.try_recv()) |event| {
                defer event.deinit();

                // event.file.len == 0 cuz fswatch keeps returning "" when libhot.so changes :/
                if (std.mem.eql(u8, options.hotlib_name, event.file) or event.file.len == 0) {
                    std.Thread.sleep(std.time.ns_per_ms * 500);
                    std.debug.print("reloading\n", .{});

                    // dlopen() caches library loads.
                    // passing diff paths defeats this caching.
                    const new_path = try std.fmt.allocPrint(allocator.*, "{s}{d}", .{ self.hot_cache, self.count });
                    std.debug.print("loading: {s}\n", .{new_path});
                    defer self.count += 1;
                    defer allocator.free(new_path);
                    try std.fs.copyFileAbsolute(self.libpath, new_path, .{});

                    switch (self.vtable.pre_reload(self.app)) {
                        .quit => return false,
                        .nothing => {},
                        .errored => return error.Errored,
                    }
                    errdefer _ = self.vtable.post_reload(self.app);

                    var dyn = try std.DynLib.open(new_path);
                    errdefer dyn.close();

                    const vtable = HotVtable.from_dyn(&dyn) catch |e| {
                        utils_mod.dump_error(e);
                        return true;
                    };

                    switch (vtable.post_reload(self.app)) {
                        .quit => return false,
                        .nothing => {},
                        .errored => return error.Errored,
                    }

                    // NOTE: closing this dylib before we open the new one crashes somewhere i can't debug
                    //   so don't do that.
                    // NOTE: closing the dylib at all crashes on windows. se we don't get to do that either.
                    //   luckily we don't need to care.
                    // self.dylib.close();
                    self.dylib = dyn;
                    self.vtable = vtable;
                }
            }
        }

        switch (self.vtable.tick(self.app)) {
            .quit => return false,
            .nothing => return true,
            .errored => return error.Errored,
        }
    }
};

// TODO:
// set segfault handlers and call app.panic_exit() then ask user if they want to restart
// use fixed buff allocator to avoid leaks.
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    allocator.* = gpa.allocator();

    switch (options.mode) {
        .hotlib => {},
        .exe => {
            var app = try HotApp.init();
            defer app.deinit() catch |e| {
                utils_mod.dump_error(e);
            };

            while (try app.tick()) {}
        },
        .hotexe => {
            var app = try HotReloader.init(
                if (@import("builtin").target.os.tag == .windows) "zig-out/bin/" else "zig-out/lib/",
                "zig-out/hot-cache/",
            );
            defer app.deinit();
            while (try app.tick()) {}
        },
    }

    // no defer if we don't want to print leaks when we error out
    // _ = gpa.deinit();
}

export fn hot_init() callconv(.c) ?*anyopaque {
    switch (options.mode) {
        .exe, .hotexe => return null,
        .hotlib => {
            const app = HotApp.init() catch |e| {
                utils_mod.dump_error(e);
                return null;
            };

            return @ptrCast(app);
        },
    }
}

export fn hot_tick(_app: *anyopaque) callconv(.c) HotAction {
    switch (options.mode) {
        .exe, .hotexe => return .quit,
        .hotlib => {
            const app: *HotApp = @ptrCast(@alignCast(_app));

            const res = app.tick() catch |e| {
                utils_mod.dump_error(e);
                return .errored;
            };

            return if (res) .nothing else .quit;
        },
    }
}

export fn hot_pre_reload(_app: *anyopaque) callconv(.c) HotAction {
    switch (options.mode) {
        .exe, .hotexe => return .nothing,
        .hotlib => {
            const app: *HotApp = @ptrCast(@alignCast(_app));

            app.pre_reload() catch |e| {
                utils_mod.dump_error(e);
                return .errored;
            };

            return .nothing;
        },
    }
}

export fn hot_post_reload(_app: *anyopaque) callconv(.c) HotAction {
    switch (options.mode) {
        .exe, .hotexe => return .nothing,
        .hotlib => {
            const app: *HotApp = @ptrCast(@alignCast(_app));

            app.post_reload() catch |e| {
                utils_mod.dump_error(e);
                return .errored;
            };

            return .nothing;
        },
    }
}

export fn hot_deinit(_app: *anyopaque) callconv(.c) HotAction {
    switch (options.mode) {
        .exe, .hotexe => return .quit,
        .hotlib => {
            const app: *HotApp = @ptrCast(@alignCast(_app));

            app.deinit() catch |e| {
                utils_mod.dump_error(e);
                return .errored;
            };

            return .nothing;
        },
    }
}

// this function is here just so that glfw, portaudio, etc are linked directly into the hot bin.
// we never actually call this function
// export fn hot_ignore() callconv(.c) void {
//     switch (options.mode) {
//         .hotexe => {
//             const c = @cImport({
//                 @cInclude("GLFW/glfw3.h");
//                 @cInclude("portaudio.h");
//             });
//             const Extern = struct {
//                 extern fn SteamAPI_Shutdown() callconv(.c) void;
//                 extern fn cImGui_ImplGlfw_Shutdown() callconv(.c) void;
//             };
//             _ = c.glfwInit();
//             _ = c.Pa_Initialize();
//             Extern.SteamAPI_Shutdown();
//             Extern.cImGui_ImplGlfw_Shutdown();
//         },
//         else => {},
//     }
// }

const HotApp = struct {
    const vk = @import("vulkan");

    const utils = @import("utils.zig");
    const Fuse = utils.Fuse;

    const math = @import("math.zig");
    const Vec4 = math.Vec4;
    const Mat4x4 = math.Mat4x4;

    const Engine = @import("engine.zig");
    const c = Engine.c;

    const gui = @import("gui.zig");
    const GuiEngine = gui.GuiEngine;

    const application = @import("app.zig");
    const App = application.App;
    const AppState = application.AppState;
    const GuiState = application.GuiState;
    const RendererState = application.RendererState;

    gpa: Gpa,
    engine: Engine,
    gui_engine: GuiEngine,
    app_state: AppState,
    gui_state: GuiState,
    app: App,
    renderer_state: RendererState,
    gui_renderer: GuiEngine.GuiRenderer,

    const Gpa = std.heap.GeneralPurposeAllocator(.{});

    fn init() !*@This() {
        const self = blk: {
            var gpa = Gpa{};
            const self = try gpa.allocator().create(@This());
            self.gpa = gpa;

            allocator.* = self.gpa.allocator();
            break :blk self;
        };
        errdefer {
            var gpa = self.gpa;
            gpa.allocator().destroy(self);
        }

        var args = try std.process.argsWithAllocator(allocator.*);
        defer args.deinit();

        var engine_args: Engine.Window.Args = .{};
        while (args.next()) |arg| {
            if (std.mem.eql(u8, arg, "-x11")) {
                engine_args.platform_hint = .x11;
            }
            if (std.mem.eql(u8, arg, "-wayland")) {
                engine_args.platform_hint = .wayland;
            }
        }

        self.engine = try Engine.init(engine_args);
        errdefer self.engine.deinit();

        std.debug.print("using device: {s}\n", .{self.engine.graphics.props.device_name});

        self.gui_engine = try GuiEngine.init(self.engine.window);
        errdefer self.gui_engine.deinit();

        self.gui_state = GuiState{};

        self.app = try App.init(&self.engine);
        errdefer self.app.deinit(&self.engine.graphics.device);

        self.app_state = try AppState.init(self.engine.window, &self.app);
        errdefer self.app_state.deinit();

        self.renderer_state = try RendererState.init(&self.app, &self.engine, &self.app_state);
        errdefer self.renderer_state.deinit(&self.engine.graphics.device);

        self.gui_renderer = try GuiEngine.GuiRenderer.init(&self.engine, &self.renderer_state.swapchain);
        errdefer self.gui_renderer.deinit(&self.engine.graphics.device);

        return self;
    }

    fn deinit(self: *@This()) !void {
        defer {
            var gpa = self.gpa;
            gpa.allocator().destroy(self);
            _ = gpa.deinit();
        }
        defer self.engine.deinit();
        defer self.gui_engine.deinit();
        defer self.app_state.deinit();
        defer self.app.deinit(&self.engine.graphics.device);
        defer self.renderer_state.deinit(&self.engine.graphics.device);
        defer self.gui_renderer.deinit(&self.engine.graphics.device);

        try self.renderer_state.swapchain.waitForAllFences(&self.engine.graphics.device);
        try self.engine.graphics.device.deviceWaitIdle();
    }

    fn pre_reload(self: *@This()) !void {
        {
            // imgui bugs out on reload, so we deinit it on pre_reload, and reinit it on load :/

            try self.renderer_state.swapchain.waitForAllFences(&self.engine.graphics.device);
            try self.engine.graphics.device.deviceWaitIdle();

            self.gui_renderer.pre_reload();
            self.gui_engine.deinit();
        }

        try self.app_state.pre_reload();
        try self.app.pre_reload();
        try self.engine.pre_reload();
    }

    fn post_reload(self: *@This()) !void {
        allocator.* = self.gpa.allocator();
        try self.engine.post_reload();
        try self.app.post_reload();
        try self.app_state.post_reload();

        self.gui_engine = try GuiEngine.init(self.engine.window);
        errdefer self.gui_engine.deinit();
        self.gui_renderer.post_reload(&self.engine, &self.renderer_state.swapchain);
    }

    fn tick(self: *@This()) !bool {
        return try self.app.tick(&self.engine, &self.app_state, &self.gui_renderer, &self.gui_state, &self.renderer_state);
    }
};
