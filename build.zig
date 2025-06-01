const std = @import("std");

const win_export = "__declspec(dllexport)";
const compile_commands_flags = &[_][]const u8{ "-gen-cdb-fragment-path", ".cache/cdb" };
const strict_cxx_flags = &[_][]const u8{ "-Werror", "-Wall", "-Wno-unused-variable", "-Wno-unused-function" };

const Flags = std.ArrayList([]const u8);

fn compile_commands_step(b: *std.Build, v: struct {
    cdb_dir: []const u8,
    compile_commands_dir: []const u8,
    alloc: std.mem.Allocator,
}) !struct { step: *std.Build.Step } {
    const cdb = try b.build_root.join(v.alloc, &.{v.cdb_dir});
    const compile_commands_dir = try b.build_root.join(v.alloc, &.{v.compile_commands_dir});
    const command = try std.fmt.allocPrint(
        v.alloc,
        "(echo \\[ ; cat {s}/* ; echo {{}}\\]) | jq 'map(select(length > 0)) | map(select(. != \"no-default-config\"))' > {s}/compile_commands.json",
        .{ cdb, compile_commands_dir },
    );

    // https://github.com/ziglang/zig/issues/9323#issuecomment-1646590552
    const gen = b.addSystemCommand(&.{
        "sh",
        "-c",
        command,
    });

    return .{ .step = &gen.step };
}

fn vulkan_step(b: *std.Build, v: struct {
    vulkan_headers: *std.Build.Dependency,
}) struct { mod: *std.Build.Module } {
    // generate vulkan bindings from vk.xml and create a zig module from the generated code
    const registry = v.vulkan_headers.path("registry/vk.xml");
    const vk_gen = b.dependency("vulkan_zig", .{}).artifact("vulkan-zig-generator");
    const vk_generate_cmd = b.addRunArtifact(vk_gen);
    vk_generate_cmd.addFileArg(registry);
    const vulkan_zig = b.addModule("vulkan-zig", .{
        .root_source_file = vk_generate_cmd.addOutputFileArg("vk.zig"),
    });

    return .{ .mod = vulkan_zig };
}

fn imgui_step(b: *std.Build, v: struct {
    dear: *std.Build.Dependency,
    imgui: *std.Build.Dependency,
    vulkan_headers: *std.Build.Dependency,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    alloc: std.mem.Allocator,
}) !struct { generated: *std.Build.Step.WriteFile, lib: *std.Build.Step.Compile } {
    const is_windows = v.target.result.os.tag == .windows;
    const dear = v.dear.path("dear_bindings.py");

    const cimgui = b.addSharedLibrary(.{
        .name = "cimgui",
        .target = v.target,
        .optimize = v.optimize,
    });

    const dcimgui_generated = b.addWriteFiles();
    cimgui.step.dependOn(&dcimgui_generated.step);

    const dcimgui = b.addSystemCommand(&[_][]const u8{"python"});
    dcimgui.addFileArg(dear);
    dcimgui.addArgs(&[_][]const u8{ "-o", "dcimgui" });
    dcimgui.addFileArg(v.imgui.path("imgui.h"));
    dcimgui.setCwd(dcimgui_generated.getDirectory());
    cimgui.step.dependOn(&dcimgui.step);

    const dcimgui_glfw = b.addSystemCommand(&[_][]const u8{"python"});
    dcimgui_glfw.addFileArg(dear);
    dcimgui_glfw.addArgs(&[_][]const u8{ "-o", "dcimgui_impl_glfw", "--backend", "--include" });
    dcimgui_glfw.addFileArg(v.imgui.path("imgui.h"));
    dcimgui_glfw.addFileArg(v.imgui.path("backends/imgui_impl_glfw.h"));
    dcimgui_glfw.step.dependOn(&dcimgui.step);
    dcimgui_glfw.setCwd(dcimgui_generated.getDirectory());
    cimgui.step.dependOn(&dcimgui_glfw.step);

    const dcimgui_vulkan = b.addSystemCommand(&[_][]const u8{"python"});
    dcimgui_vulkan.addFileArg(dear);
    dcimgui_vulkan.addArgs(&[_][]const u8{ "-o", "dcimgui_impl_vulkan", "--backend", "--include" });
    dcimgui_vulkan.addFileArg(v.imgui.path("imgui.h"));
    dcimgui_vulkan.addFileArg(v.imgui.path("backends/imgui_impl_vulkan.h"));
    dcimgui_vulkan.step.dependOn(&dcimgui.step);
    dcimgui_vulkan.setCwd(dcimgui_generated.getDirectory());
    cimgui.step.dependOn(&dcimgui_vulkan.step);

    var flags = Flags.init(v.alloc);
    if (is_windows) try flags.appendSlice(&.{
        "-DCIMGUI_API=" ++ win_export,
        "-DCIMGUI_IMPL_API=" ++ win_export,
    });
    const owned_flags = try flags.toOwnedSlice();

    if (is_windows) {
        cimgui.addIncludePath(b.path("./zig-out/vendor/include"));
        cimgui.addLibraryPath(b.path("./zig-out/vendor/lib"));
    }

    cimgui.addCSourceFiles(.{ .root = v.imgui.path("./"), .files = &[_][]const u8{
        "imgui.cpp",
        "imgui_draw.cpp",
        "imgui_widgets.cpp",
        "imgui_tables.cpp",
        "imgui_demo.cpp",
        "backends/imgui_impl_glfw.cpp",
        "backends/imgui_impl_vulkan.cpp",
    }, .flags = owned_flags });
    cimgui.addCSourceFiles(.{ .root = dcimgui_generated.getDirectory(), .files = &[_][]const u8{
        "dcimgui.cpp",
        "dcimgui_impl_glfw.cpp",
        "dcimgui_impl_vulkan.cpp",
    }, .flags = owned_flags });
    cimgui.addIncludePath(v.imgui.path("./"));
    cimgui.addIncludePath(v.imgui.path("./backends"));
    cimgui.addIncludePath(v.vulkan_headers.path("./include"));
    cimgui.addIncludePath(dcimgui_generated.getDirectory());
    cimgui.linkSystemLibrary("vulkan");
    cimgui.linkSystemLibrary("glfw");
    cimgui.linkLibC();
    cimgui.linkLibCpp();

    return .{
        .generated = dcimgui_generated,
        .lib = cimgui,
    };
}

const JoltOptions = struct {
    use_double_precision: bool,
    enable_asserts: bool,
    enable_cross_platform_determinism: bool,
    enable_debug_renderer: bool,
};

fn jolt_step(b: *std.Build, v: struct {
    jolt: *std.Build.Dependency,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    alloc: std.mem.Allocator,
}) !struct { lib: *std.Build.Step.Compile, boptions: *std.Build.Step.Options, options: JoltOptions } {
    const is_windows = v.target.result.os.tag == .windows;
    const options = b.addOptions();
    const jolt_options = JoltOptions{
        .use_double_precision = false,
        // .enable_asserts = v.optimize == .Debug,
        .enable_asserts = false,
        .enable_cross_platform_determinism = false,
        .enable_debug_renderer = true,
    };

    inline for (std.meta.fields(@TypeOf(jolt_options))) |field| {
        options.addOption(field.type, field.name, @field(jolt_options, field.name));
    }

    const jolt = b.addSharedLibrary(.{
        .name = "jolt",
        .target = v.target,
        .optimize = v.optimize,
    });

    var files = std.ArrayList([]const u8).init(v.alloc);
    const jolt_src = try v.jolt.path("./Jolt").getPath3(b, &jolt.step).joinString(v.alloc, "");
    const dir = try std.fs.openDirAbsolute(jolt_src, .{ .iterate = true });
    var it = try dir.walk(v.alloc);
    while (try it.next()) |f| {
        if (f.kind != .file) continue;
        if (!std.mem.endsWith(u8, f.path, ".cpp")) continue;

        try files.append(try v.alloc.dupe(u8, f.path));
    }

    var flags = Flags.init(v.alloc);
    try flags.appendSlice(compile_commands_flags);
    try flags.appendSlice(&[_][]const u8{
        "-std=c++17",
        "-fno-exceptions",
        "-fno-sanitize=undefined",
        "-fno-access-control",
        "-fno-rtti",
    });
    try flags.appendSlice(&[_][]const u8{
        "-DJPH_SHARED_LIBRARY",
        "-DJPH_BUILD_SHARED_LIBRARY",
    });
    if (is_windows) try flags.append("-DJPH_EXPORT=" ++ win_export);
    if (jolt_options.enable_cross_platform_determinism) try flags.append("-DJPH_CROSS_PLATFORM_DETERMINISTIC");
    if (jolt_options.enable_debug_renderer) try flags.append("-DJPH_DEBUG_RENDERER");
    if (jolt_options.use_double_precision) try flags.append("-DJPH_DOUBLE_PRECISION");
    if (jolt_options.enable_asserts) try flags.append("-DJPH_ENABLE_ASSERTS");

    jolt.addIncludePath(v.jolt.path("./"));
    jolt.addCSourceFiles(.{
        .root = v.jolt.path("./Jolt"),
        .flags = try flags.toOwnedSlice(),
        .files = files.items,
    });

    jolt.linkLibC();
    jolt.linkLibCpp();

    return .{ .lib = jolt, .boptions = options, .options = jolt_options };
}

fn remotery_step(b: *std.Build, v: struct {
    remotery: *std.Build.Dependency,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    alloc: std.mem.Allocator,
}) !struct { lib: *std.Build.Step.Compile } {
    const remotery = b.addSharedLibrary(.{
        .name = "remotery",
        .target = v.target,
        .optimize = v.optimize,
    });

    var flags = Flags.init(v.alloc);
    try flags.appendSlice(compile_commands_flags);
    try flags.appendSlice(&[_][]const u8{
        "-DRMT_ENABLED=1",
        "-DRMT_USE_VULKAN=1",
    });

    remotery.addIncludePath(v.remotery.path("lib"));
    remotery.addCSourceFiles(.{
        .root = v.remotery.path("lib"),
        .flags = try flags.toOwnedSlice(),
        .files = &[_][]const u8{"Remotery.c"},
    });
    remotery.linkSystemLibrary("vulkan");
    remotery.linkLibC();

    return .{ .lib = remotery };
}

const TracyOptions = struct {
    // Enable profiling
    tracy_enable: bool = true,
    // On-demand profiling
    on_demand: bool = false,
    // Enfore callstack collection for tracy regions
    callstack: bool = false,
    // Disable all callstack related functionality
    no_callstack: bool = false,
    // Disables the inline functions in callstacks
    no_callstack_inlines: bool = false,
    // Only listen on the localhost interface
    only_localhost: bool = false,
    // Disable client discovery by broadcast to local network
    no_broadcast: bool = false,
    // Tracy will only accept connections on IPv4 addresses (disable IPv6)
    only_ipv4: bool = false,
    // Disable collection of source code
    no_code_transfer: bool = false,
    // Disable capture of context switches
    no_context_switch: bool = false,
    // Client executable does not exit until all profile data is sent to server
    no_exit: bool = false,
    // Disable call stack sampling
    no_sampling: bool = false,
    // Disable zone validation for C API
    no_verify: bool = false,
    // Disable capture of hardware Vsync events
    no_vsync_capture: bool = false,
    // Disable the frame image support and its thread
    no_frame_image: bool = false,
    // Disable systrace sampling
    no_system_tracing: bool = false,
    // Enable nopsleds for efficient patching by system-level tools (e.g. rr)
    patchable_nopsleds: bool = false,
    // Use lower resolution timers
    timer_fallback: bool = false,
    // Use libunwind backtracing where supported
    libunwind_backtrace: bool = false,
    // Instead of full runtime symbol resolution, only resolve the image path and offset to enable offline symbol resolution
    symbol_offline_resolve: bool = false,
    // Enable libbacktrace to support dynamically loaded elfs in symbol resolution resolution after the first symbol resolve operation
    libbacktrace_elf_dynload_support: bool = false,
    // Enable delayed initialization of the library (init on first call)
    delayed_init: bool = false,
    // Enable the manual lifetime management of the profile
    manual_lifetime: bool = false,
    // Enable fibers support
    fibers: bool = false,
    // Disable crash handling
    no_crash_handler: bool = false,
    // Enable verbose logging
    verbose: bool = false,
    // Enable debuginfod support
    debuginfod: bool = false,

    shared: bool = true,
};

fn tracy_step(b: *std.Build, v: struct {
    dep: *std.Build.Dependency,
    options: TracyOptions,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    alloc: std.mem.Allocator,
}) !struct { lib: *std.Build.Step.Compile, boptions: *std.Build.Step.Options, options: TracyOptions } {
    const is_windows = v.target.result.os.tag == .windows;

    const tracy = b.addSharedLibrary(.{
        .name = "tracy",
        .target = v.target,
        .optimize = v.optimize,
    });

    var flags = Flags.init(v.alloc);
    try flags.appendSlice(compile_commands_flags);

    if (v.options.tracy_enable) try flags.append("-DTRACY_ENABLE");
    if (v.options.on_demand) try flags.append("-DTRACY_ON_DEMAND");
    if (v.options.callstack) try flags.append("-DTRACY_CALLSTACK");
    if (v.options.no_callstack) try flags.append("-DTRACY_NO_CALLSTACK");
    if (v.options.no_callstack_inlines) try flags.append("-DTRACY_NO_CALLSTACK_INLINES");
    if (v.options.only_localhost) try flags.append("-DTRACY_ONLY_LOCALHOST");
    if (v.options.no_broadcast) try flags.append("-DTRACY_NO_BROADCAST");
    if (v.options.only_ipv4) try flags.append("-DTRACY_ONLY_IPV4");
    if (v.options.no_code_transfer) try flags.append("-DTRACY_NO_CODE_TRANSFER");
    if (v.options.no_context_switch) try flags.append("-DTRACY_NO_CONTEXT_SWITCH");
    if (v.options.no_exit) try flags.append("-DTRACY_NO_EXIT");
    if (v.options.no_sampling) try flags.append("-DTRACY_NO_SAMPLING");
    if (v.options.no_verify) try flags.append("-DTRACY_NO_VERIFY");
    if (v.options.no_vsync_capture) try flags.append("-DTRACY_NO_VSYNC_CAPTURE");
    if (v.options.no_frame_image) try flags.append("-DTRACY_NO_FRAME_IMAGE");
    if (v.options.no_system_tracing) try flags.append("-DTRACY_NO_SYSTEM_TRACING");
    if (v.options.patchable_nopsleds) try flags.append("-DTRACY_PATCHABLE_NOPSLEDS");
    if (v.options.delayed_init) try flags.append("-DTRACY_DELAYED_INIT");
    if (v.options.manual_lifetime) try flags.append("-DTRACY_MANUAL_LIFETIME");
    if (v.options.fibers) try flags.append("-DTRACY_FIBERS");
    if (v.options.timer_fallback) try flags.append("-DTRACY_TIMER_FALLBACK");
    if (v.options.no_crash_handler) try flags.append("-DTRACY_NO_CRASH_HANDLER");
    if (v.options.libunwind_backtrace) try flags.append("-DTRACY_LIBUNWIND_BACKTRACE");
    if (v.options.libunwind_backtrace) tracy.linkSystemLibrary("libunwind");
    if (v.options.symbol_offline_resolve) try flags.append("-DTRACY_SYMBOL_OFFLINE_RESOLVE");
    if (v.options.libbacktrace_elf_dynload_support) try flags.append("-DTRACY_LIBBACKTRACE_ELF_DYNLOAD_SUPPORT");
    if (v.options.verbose) try flags.append("-DTRACY_VERBOSE");
    if (v.options.debuginfod) try flags.append("-DTRACY_DEBUGINFOD");
    if (v.options.debuginfod) tracy.linkSystemLibrary("libdebuginfod");
    if (v.options.shared) try flags.append("-DDTRACY_EXPORTS");
    if (is_windows and v.options.shared) try flags.appendSlice(&.{
        "-DWINVER=0x0601",
        "-D_WIN32_WINNT=0x0601",
    });
    // try flags.append("-std=c++14");

    if (is_windows) {
        tracy.linkSystemLibrary("dbghelp");
        tracy.linkSystemLibrary("ws2_32");
    }

    const options = b.addOptions();
    inline for (std.meta.fields(@TypeOf(v.options))) |field| {
        options.addOption(field.type, field.name, @field(v.options, field.name));
    }

    tracy.addIncludePath(v.dep.path("public"));
    tracy.addCSourceFiles(.{
        .root = v.dep.path("public"),
        .flags = try flags.toOwnedSlice(),
        .files = &[_][]const u8{"TracyClient.cpp"},
    });
    tracy.linkLibCpp();

    return .{ .lib = tracy, .options = v.options, .boptions = options };
}

const CompileMode = enum {
    exe,
    hotexe,
    hotlib,
};

fn step(b: *std.Build, v: struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    alloc: std.mem.Allocator,
    mode: CompileMode,
    vulkan_zig: *std.Build.Module,
    cimgui: *std.Build.Step.Compile,
    imgui_dep: *std.Build.Dependency,
    dcimgui_generated: *std.Build.Step.WriteFile,
    jolt: *std.Build.Step.Compile,
    jolt_boptions: *std.Build.Step.Options,
    jolt_options: JoltOptions,
    jolt_dep: *std.Build.Dependency,
    stb_dep: *std.Build.Dependency,
    steamworks_dep: *std.Build.Dependency,
    remotery: *std.Build.Step.Compile,
    remotery_dep: *std.Build.Dependency,
    tracy: *std.Build.Step.Compile,
    tracy_dep: *std.Build.Dependency,
    tracy_boptions: *std.Build.Step.Options,
}) !*std.Build.Step.Compile {
    const is_windows = v.target.result.os.tag == .windows;

    // see b.addSharedLibrary()
    // see b.addStaticLibrary()
    // see b.addExecutable()
    const compile_step = std.Build.Step.Compile.create(b, .{
        .name = switch (v.mode) {
            .exe => "zhottem",
            .hotexe => "hottem",
            .hotlib => "hot",
        },
        .root_module = b.createModule(.{
            .target = v.target,
            .optimize = v.optimize,
            .root_source_file = b.path("src/main.zig"),
        }),
        .kind = switch (v.mode) {
            .exe, .hotexe => .exe,
            .hotlib => .lib,
        },
        .linkage = switch (v.mode) {
            .hotlib => .dynamic,
            .exe, .hotexe => null,
        },
    });

    const options = b.addOptions();
    options.addOption(bool, "hot_reload", v.mode != .exe);
    options.addOption(bool, "is_lib", v.mode == .hotlib);
    options.addOption([]const u8, "hotlib_name", if (is_windows) "hot.dll" else "libhot.so");
    options.addOption(CompileMode, "mode", v.mode);
    compile_step.root_module.addImport("build-options", options.createModule());
    compile_step.root_module.addImport("jolt-options", v.jolt_boptions.createModule());
    compile_step.root_module.addImport("tracy-options", v.tracy_boptions.createModule());

    if (is_windows) {
        compile_step.addIncludePath(b.path("./zig-out/vendor/include"));
        compile_step.addLibraryPath(b.path("./zig-out/vendor/lib"));
        compile_step.addLibraryPath(b.path("./zig-out/lib"));
        compile_step.addRPath(b.path("./zig-out/vendor/lib"));
    }

    switch (v.mode) {
        .exe, .hotlib => {
            compile_step.root_module.addImport("vulkan", v.vulkan_zig);

            compile_step.addIncludePath(v.dcimgui_generated.getDirectory());
            compile_step.addIncludePath(v.imgui_dep.path("./"));
            compile_step.addIncludePath(v.imgui_dep.path("./backends"));
            compile_step.addIncludePath(v.jolt_dep.path("./"));
            compile_step.addIncludePath(v.stb_dep.path("./"));
            compile_step.addIncludePath(v.remotery_dep.path("./lib"));
            compile_step.addIncludePath(b.path("./src"));

            compile_step.addIncludePath(v.steamworks_dep.path("./public"));
            if (is_windows) {
                compile_step.addLibraryPath(v.steamworks_dep.path("./redistributable_bin/win64"));
                compile_step.linkSystemLibrary("steam_api64");
            } else {
                compile_step.addLibraryPath(v.steamworks_dep.path("./redistributable_bin/linux64"));
                compile_step.addRPath(v.steamworks_dep.path("./redistributable_bin/linux64"));
                compile_step.linkSystemLibrary("steam_api");
            }

            var steam_flags = Flags.init(v.alloc);
            try steam_flags.appendSlice(compile_commands_flags);
            try steam_flags.append("-Wno-invalid-offsetof");
            try steam_flags.append("-Wgnu-alignof-expression");
            if (!is_windows) try steam_flags.appendSlice(strict_cxx_flags);
            compile_step.addCSourceFiles(.{
                .root = b.path("./src/steamworks"),
                .files = &[_][]const u8{
                    "steam.cpp",
                    "server.cpp",
                    "client.cpp",
                },
                .flags = try steam_flags.toOwnedSlice(),
            });

            const files = b.addWriteFiles();
            compile_step.step.dependOn(&files.step);

            // TODO: consider not depending on zphysics alloc
            // NOTE: if the zig code for this allocator is not compiled (cuz it's lazy compilation)
            //    it will give linker errors complaining about missing functions. might be useful to keep this under
            //    a config flag
            _ = files.add("stb_image.c",
                \\extern void* zphysicsAlloc(unsigned long long sz);
                \\extern void* zphysicsRealloc(void* ptr, unsigned long long old_size, unsigned long long new_size);
                \\extern void zphysicsFree(void* ptr);
                \\
                \\#define STB_IMAGE_IMPLEMENTATION
                \\#define STBI_NO_STDIO
                \\#define STBI_MALLOC(sz) zphysicsAlloc(sz)
                \\#define STBI_REALLOC_SIZED(p,oldsz,newsz) zphysicsRealloc(p,oldsz,newsz)
                \\#define STBI_FREE(p) zphysicsFree(p)
                \\
                \\#include <stb_image.h>
                \\
                \\
                \\#define STBI_WRITE_NO_STDIO
                \\#define STB_IMAGE_WRITE_IMPLEMENTATION
                \\#define STBIW_MALLOC(sz) zphysicsAlloc(sz)
                \\#define STBIW_REALLOC_SIZED(p,oldsz,newsz) zphysicsRealloc(p,oldsz,newsz)
                \\#define STBIW_FREE(p) zphysicsFree(p)
                \\
                \\#include <stb_image_write.h>
            );
            compile_step.addCSourceFiles(.{
                .root = files.getDirectory(),
                .files = &[_][]const u8{
                    "stb_image.c",
                },
                .flags = &[_][]const u8{
                    // we don't want a separate dll rn
                    // if (is_windows) "-DSTBIDEF=" ++ win_export else "",
                } ++ compile_commands_flags,
            });

            // compile_step.linkLibrary(v.cimgui);
            // compile_step.linkLibrary(v.jolt);

            // if (is_windows) {
            //     compile_step.want_lto = false;
            // }

            compile_step.addRPath(b.path("./zig-out/lib"));
            if (is_windows) {
                compile_step.addObjectFile(b.path("./zig-out/lib/cimgui.lib"));
                compile_step.addObjectFile(b.path("./zig-out/lib/jolt.lib"));
                compile_step.addObjectFile(b.path("./zig-out/lib/remotery.lib"));
                compile_step.addObjectFile(b.path("./zig-out/lib/tracy.lib"));
            } else {
                compile_step.addObjectFile(b.path("./zig-out/lib/libcimgui.so"));
                compile_step.addObjectFile(b.path("./zig-out/lib/libjolt.so"));
                compile_step.addObjectFile(b.path("./zig-out/lib/libremotery.so"));
                compile_step.addObjectFile(b.path("./zig-out/lib/libtracy.so"));
            }

            var jolt_flags = Flags.init(v.alloc);
            try jolt_flags.appendSlice(compile_commands_flags);
            try jolt_flags.appendSlice(&[_][]const u8{
                "-std=c++17",
                "-fno-exceptions",
                // "-fno-sanitize=undefined",
                "-fno-access-control",
                "-fno-rtti",
            });
            try jolt_flags.append("-DJPH_SHARED_LIBRARY");
            if (is_windows) try jolt_flags.append("-DJPH_EXPORT=" ++ win_export);
            if (v.jolt_options.enable_cross_platform_determinism) try jolt_flags.append("-DJPH_CROSS_PLATFORM_DETERMINISTIC");
            if (v.jolt_options.enable_debug_renderer) try jolt_flags.append("-DJPH_DEBUG_RENDERER");
            if (v.jolt_options.use_double_precision) try jolt_flags.append("-DJPH_DOUBLE_PRECISION");
            if (v.jolt_options.enable_asserts) try jolt_flags.append("-DJPH_ENABLE_ASSERTS");
            if (!is_windows) try jolt_flags.appendSlice(strict_cxx_flags);
            compile_step.addCSourceFiles(.{
                .root = b.path("./src"),
                .flags = try jolt_flags.toOwnedSlice(),
                .files = &[_][]const u8{
                    "jolt/c.cpp",
                    "jolt/extensions.cpp",
                },
            });

            compile_step.linkSystemLibrary("glfw");
            compile_step.linkSystemLibrary("portaudio");
            compile_step.linkSystemLibrary("fswatch");
            // compile_step.linkSystemLibrary2("ImageMagick", .{});
            // compile_step.linkSystemLibrary2("MagickWand", .{});
            // compile_step.linkSystemLibrary2("MagickCore", .{});
            compile_step.linkLibC();
            compile_step.linkLibCpp();
        },
        .hotexe => {
            compile_step.linkSystemLibrary("fswatch");
            compile_step.linkLibC();
        },
    }

    return compile_step;
}

pub fn build(b: *std.Build) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const vulkan_headers = b.dependency("vulkan_headers", .{});
    const imgui = b.dependency("imgui", .{});
    const jolt = b.dependency("jolt", .{});
    const stb = b.dependency("stb", .{});
    const steamworks = b.dependency("steamworks", .{});
    const remotery = b.dependency("remotery", .{});
    const tracy = b.dependency("tracy", .{});

    const vulkan = vulkan_step(b, .{
        .vulkan_headers = vulkan_headers,
    });
    const libimgui = try imgui_step(b, .{
        .dear = b.dependency("dear_bindings", .{}),
        .imgui = imgui,
        .vulkan_headers = vulkan_headers,
        .target = target,
        .optimize = optimize,
        .alloc = alloc,
    });
    const libjolt = try jolt_step(b, .{
        .jolt = jolt,
        .target = target,
        .optimize = optimize,
        .alloc = alloc,
    });
    const libremotery = try remotery_step(b, .{
        .remotery = remotery,
        .target = target,
        .optimize = optimize,
        .alloc = alloc,
    });
    const libtracy = try tracy_step(b, .{
        .dep = tracy,
        .target = target,
        .optimize = optimize,
        .alloc = alloc,
        .options = .{
            .shared = true,
            // if false, client saves all traces until it connects to a server
            // and 1 client can only connect to 1 server. server restarts not possible
            // setting it to true does not save traces, and connects to servers on demand
            .on_demand = true,
            .only_localhost = true,
            .only_ipv4 = true,
        },
    });
    const compile_commands = try compile_commands_step(b, .{
        .cdb_dir = ".cache/cdb",
        .compile_commands_dir = "",
        .alloc = alloc,
    });
    // run this after building everything
    compile_commands.step.dependOn(b.getInstallStep());

    const exe = try step(b, .{
        .target = target,
        .optimize = optimize,
        .alloc = alloc,
        .mode = .exe,
        .vulkan_zig = vulkan.mod,
        .imgui_dep = imgui,
        .cimgui = libimgui.lib,
        .dcimgui_generated = libimgui.generated,
        .jolt = libjolt.lib,
        .jolt_boptions = libjolt.boptions,
        .jolt_options = libjolt.options,
        .jolt_dep = jolt,
        .stb_dep = stb,
        .steamworks_dep = steamworks,
        .remotery = libremotery.lib,
        .remotery_dep = remotery,
        .tracy = libtracy.lib,
        .tracy_dep = tracy,
        .tracy_boptions = libtracy.boptions,
    });
    const hotlib = try step(b, .{
        .target = target,
        .optimize = optimize,
        .alloc = alloc,
        .mode = .hotlib,
        .vulkan_zig = vulkan.mod,
        .imgui_dep = imgui,
        .cimgui = libimgui.lib,
        .dcimgui_generated = libimgui.generated,
        .jolt = libjolt.lib,
        .jolt_boptions = libjolt.boptions,
        .jolt_options = libjolt.options,
        .jolt_dep = jolt,
        .stb_dep = stb,
        .steamworks_dep = steamworks,
        .remotery = libremotery.lib,
        .remotery_dep = remotery,
        .tracy = libtracy.lib,
        .tracy_dep = tracy,
        .tracy_boptions = libtracy.boptions,
    });
    const hotexe = try step(b, .{
        .target = target,
        .optimize = optimize,
        .alloc = alloc,
        .mode = .hotexe,
        .vulkan_zig = vulkan.mod,
        .imgui_dep = imgui,
        .cimgui = libimgui.lib,
        .dcimgui_generated = libimgui.generated,
        .jolt = libjolt.lib,
        .jolt_boptions = libjolt.boptions,
        .jolt_options = libjolt.options,
        .jolt_dep = jolt,
        .stb_dep = stb,
        .steamworks_dep = steamworks,
        .remotery = libremotery.lib,
        .remotery_dep = remotery,
        .tracy = libtracy.lib,
        .tracy_dep = tracy,
        .tracy_boptions = libtracy.boptions,
    });

    compile_commands.step.dependOn(&libimgui.lib.step);
    compile_commands.step.dependOn(&libjolt.lib.step);
    compile_commands.step.dependOn(&libremotery.lib.step);
    compile_commands.step.dependOn(&libtracy.lib.step);
    compile_commands.step.dependOn(&hotlib.step);

    const build_libs_step = b.step("build-libs", "Build the libs required for the app");
    build_libs_step.dependOn(&libimgui.lib.step);
    build_libs_step.dependOn(&b.addInstallArtifact(libimgui.lib, .{}).step);
    build_libs_step.dependOn(&libjolt.lib.step);
    build_libs_step.dependOn(&b.addInstallArtifact(libjolt.lib, .{}).step);
    build_libs_step.dependOn(&libremotery.lib.step);
    build_libs_step.dependOn(&b.addInstallArtifact(libremotery.lib, .{}).step);
    build_libs_step.dependOn(&libtracy.lib.step);
    build_libs_step.dependOn(&b.addInstallArtifact(libtracy.lib, .{}).step);
    build_libs_step.dependOn(&hotlib.step);
    build_libs_step.dependOn(&b.addInstallArtifact(hotlib, .{}).step);
    build_libs_step.dependOn(compile_commands.step);

    const hot_build_step = b.step("build-hot", "Build the hot app");
    hot_build_step.dependOn(&b.addInstallArtifact(hotlib, .{}).step);
    hot_build_step.dependOn(b.getInstallStep());
    // hot_build_step.dependOn(compile_commands.step);

    const hot_run_cmd = b.addRunArtifact(hotexe);
    hot_run_cmd.step.dependOn(&b.addInstallArtifact(hotlib, .{}).step);
    hot_run_cmd.step.dependOn(&b.addInstallArtifact(hotexe, .{}).step);
    if (b.args) |args| {
        hot_run_cmd.addArgs(args);
    }
    const hot_run_step = b.step("run-hot", "Run the hot app");
    hot_run_step.dependOn(&hot_run_cmd.step);

    const exe_run_cmd = b.addRunArtifact(exe);
    b.enable_wine = true;
    exe_run_cmd.step.dependOn(&b.addInstallArtifact(exe, .{}).step);
    if (b.args) |args| {
        exe_run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&exe_run_cmd.step);
}
