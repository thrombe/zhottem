const std = @import("std");

const compile_commands_flags = &[_][]const u8{ "-gen-cdb-fragment-path", ".cache/cdb" };
const win_export = "__declspec(dllexport)";

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
}) struct { generated: *std.Build.Step.WriteFile, lib: *std.Build.Step.Compile } {
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

    const flags = &[_][]const u8{
        if (is_windows) "-DCIMGUI_API=" ++ win_export else "",
        if (is_windows) "-DCIMGUI_IMPL_API=" ++ win_export else "",
    };

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
    }, .flags = flags });
    cimgui.addCSourceFiles(.{ .root = dcimgui_generated.getDirectory(), .files = &[_][]const u8{
        "dcimgui.cpp",
        "dcimgui_impl_glfw.cpp",
        "dcimgui_impl_vulkan.cpp",
    }, .flags = flags });
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

fn jolt_step(b: *std.Build, v: struct {
    jolt: *std.Build.Dependency,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    alloc: std.mem.Allocator,
}) !struct { lib: *std.Build.Step.Compile, options: *std.Build.Step.Options } {
    const is_windows = v.target.result.os.tag == .windows;
    const options = b.addOptions();
    const jolt_options = .{
        .use_double_precision = false,
        // .enable_asserts = v.optimize == .Debug,
        .enable_asserts = false,
        .enable_cross_platform_determinism = false,
        .enable_debug_renderer = false,
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

    jolt.addIncludePath(v.jolt.path("./"));
    jolt.addCSourceFiles(.{
        .root = v.jolt.path("./Jolt"),
        .flags = &[_][]const u8{
            "-std=c++17",
            "-fno-exceptions",
            "-fno-sanitize=undefined",
            "-fno-access-control",
            "-fno-rtti",

            "-DJPH_SHARED_LIBRARY",
            "-DJPH_BUILD_SHARED_LIBRARY",
            if (is_windows) "-DJPH_EXPORT=" ++ win_export else "",
            if (jolt_options.enable_cross_platform_determinism) "-DJPH_CROSS_PLATFORM_DETERMINISTIC" else "",
            if (jolt_options.enable_debug_renderer) "-DJPH_DEBUG_RENDERER" else "",
            if (jolt_options.use_double_precision) "-DJPH_DOUBLE_PRECISION" else "",
            if (jolt_options.enable_asserts) "-DJPH_ENABLE_ASSERTS" else "",
        } ++ compile_commands_flags,
        .files = files.items,
    });

    jolt.linkLibC();
    jolt.linkLibCpp();

    return .{ .lib = jolt, .options = options };
}

const CompileMode = enum {
    exe,
    hotexe,
    hotlib,
};

fn step(b: *std.Build, v: struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    mode: CompileMode,
    vulkan_zig: *std.Build.Module,
    cimgui: *std.Build.Step.Compile,
    imgui_dep: *std.Build.Dependency,
    dcimgui_generated: *std.Build.Step.WriteFile,
    jolt: *std.Build.Step.Compile,
    jolt_options: *std.Build.Step.Options,
    jolt_dep: *std.Build.Dependency,
    stb_dep: *std.Build.Dependency,
    steamworks_dep: *std.Build.Dependency,
}) *std.Build.Step.Compile {
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
    compile_step.root_module.addImport("jolt-options", v.jolt_options.createModule());

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
            compile_step.addIncludePath(b.path("./src"));

            compile_step.addIncludePath(v.steamworks_dep.path("./public/steam"));
            if (is_windows) {
                compile_step.addLibraryPath(v.steamworks_dep.path("./redistributable_bin/win64"));
                compile_step.linkSystemLibrary("steam_api64");
            } else {
                compile_step.addLibraryPath(v.steamworks_dep.path("./redistributable_bin/linux64"));
                compile_step.addRPath(v.steamworks_dep.path("./redistributable_bin/linux64"));
                compile_step.linkSystemLibrary("steam_api");
            }

            compile_step.addCSourceFiles(.{
                .root = b.path("./src/steamworks"),
                .files = &[_][]const u8{
                    "steam.cpp",
                    "server.cpp",
                    "client.cpp",
                },
                .flags = &[_][]const u8{
                    "-Wno-invalid-offsetof",
                    // "-fno-exceptions",
                    // "-fno-rtti",
                } ++ compile_commands_flags,
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
            } else {
                compile_step.addObjectFile(b.path("./zig-out/lib/libcimgui.so"));
                compile_step.addObjectFile(b.path("./zig-out/lib/libjolt.so"));
            }

            compile_step.addCSourceFiles(.{
                .root = b.path("./src"),
                .flags = &[_][]const u8{
                    "-std=c++17",
                    "-fno-exceptions",
                    "-fno-sanitize=undefined",
                    "-fno-access-control",
                    "-fno-rtti",
                    if (is_windows) "" else "-Werror",
                    if (is_windows) "" else "-Wall",
                    if (is_windows) "" else "-Wno-unused-variable",
                    if (is_windows) "" else "-Wno-unused-function",

                    "-DJPH_SHARED_LIBRARY",
                    if (is_windows) "-DJPH_EXPORT=" ++ win_export else "",
                } ++ compile_commands_flags,
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

    const vulkan = vulkan_step(b, .{
        .vulkan_headers = vulkan_headers,
    });
    const libimgui = imgui_step(b, .{
        .dear = b.dependency("dear_bindings", .{}),
        .imgui = imgui,
        .vulkan_headers = vulkan_headers,
        .target = target,
        .optimize = optimize,
    });
    const libjolt = try jolt_step(b, .{
        .jolt = jolt,
        .target = target,
        .optimize = optimize,
        .alloc = alloc,
    });
    const compile_commands = try compile_commands_step(b, .{
        .cdb_dir = ".cache/cdb",
        .compile_commands_dir = "",
        .alloc = alloc,
    });
    // run this after building everything
    compile_commands.step.dependOn(b.getInstallStep());

    const exe = step(b, .{
        .target = target,
        .optimize = optimize,
        .mode = .exe,
        .vulkan_zig = vulkan.mod,
        .imgui_dep = imgui,
        .cimgui = libimgui.lib,
        .dcimgui_generated = libimgui.generated,
        .jolt = libjolt.lib,
        .jolt_options = libjolt.options,
        .jolt_dep = jolt,
        .stb_dep = stb,
        .steamworks_dep = steamworks,
    });
    const hotlib = step(b, .{
        .target = target,
        .optimize = optimize,
        .mode = .hotlib,
        .vulkan_zig = vulkan.mod,
        .imgui_dep = imgui,
        .cimgui = libimgui.lib,
        .dcimgui_generated = libimgui.generated,
        .jolt = libjolt.lib,
        .jolt_options = libjolt.options,
        .jolt_dep = jolt,
        .stb_dep = stb,
        .steamworks_dep = steamworks,
    });
    const hotexe = step(b, .{
        .target = target,
        .optimize = optimize,
        .mode = .hotexe,
        .vulkan_zig = vulkan.mod,
        .imgui_dep = imgui,
        .cimgui = libimgui.lib,
        .dcimgui_generated = libimgui.generated,
        .jolt = libjolt.lib,
        .jolt_options = libjolt.options,
        .jolt_dep = jolt,
        .stb_dep = stb,
        .steamworks_dep = steamworks,
    });

    const build_libs_step = b.step("build-libs", "Build the libs required for the app");
    build_libs_step.dependOn(&libimgui.lib.step);
    build_libs_step.dependOn(&b.addInstallArtifact(libimgui.lib, .{}).step);
    build_libs_step.dependOn(&libjolt.lib.step);
    build_libs_step.dependOn(&b.addInstallArtifact(libjolt.lib, .{}).step);
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
    exe_run_cmd.step.dependOn(&b.addInstallArtifact(exe, .{}).step);
    if (b.args) |args| {
        exe_run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&exe_run_cmd.step);
}
