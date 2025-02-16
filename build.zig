const std = @import("std");

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
    cimgui_dep: *std.Build.Dependency,
}) *std.Build.Step.Compile {
    // see b.addSharedLibrary()
    // see b.addStaticLibrary()
    // see b.addExecutable()
    const compile_step = std.Build.Step.Compile.create(b, .{
        .name = switch (v.mode) {
            .exe => "zhottem",
            .hotexe => "hottem",
            .hotlib => "hot",
        },
        .root_module = .{
            .target = v.target,
            .optimize = v.optimize,
            .root_source_file = b.path("src/main.zig"),
        },
        .kind = switch (v.mode) {
            .exe, .hotexe => .exe,
            .hotlib => .lib,
        },
        .linkage = switch (v.mode) {
            .hotlib => .dynamic,
            .exe, .hotexe => null,
        },
        .zig_lib_dir = b.zig_lib_dir,
    });

    const options = b.addOptions();
    options.addOption(bool, "hot_reload", v.mode != .exe);
    options.addOption(bool, "is_lib", v.mode == .hotlib);
    options.addOption([]const u8, "hotlib_name", "libhot.so");
    options.addOption(CompileMode, "mode", v.mode);
    compile_step.root_module.addImport("build-options", options.createModule());

    switch (v.mode) {
        .exe, .hotlib => {
            compile_step.root_module.addImport("vulkan", v.vulkan_zig);
            compile_step.linkLibrary(v.cimgui_dep.artifact("cimgui"));
            compile_step.addIncludePath(v.cimgui_dep.path("dcimgui/backends"));
            compile_step.linkSystemLibrary("fswatch");
            compile_step.linkSystemLibrary2("ImageMagick", .{});
            compile_step.linkSystemLibrary2("MagickWand", .{});
            compile_step.linkSystemLibrary2("MagickCore", .{});
            compile_step.linkLibC();
        },
        .hotexe => {
            compile_step.linkSystemLibrary("fswatch");
            compile_step.linkLibC();
        },
    }

    b.installArtifact(compile_step);

    return compile_step;
}

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // generate vulkan bindings from vk.xml and create a zig module from the generated code
    const registry = b.dependency("vulkan_headers", .{}).path("registry/vk.xml");
    const vk_gen = b.dependency("vulkan_zig", .{}).artifact("vulkan-zig-generator");
    const vk_generate_cmd = b.addRunArtifact(vk_gen);
    vk_generate_cmd.addFileArg(registry);
    const vulkan_zig = b.addModule("vulkan-zig", .{
        .root_source_file = vk_generate_cmd.addOutputFileArg("vk.zig"),
    });

    const cimgui_dep = b.dependency("cimgui", .{
        .target = target,
        .optimize = optimize,
        .platform = .GLFW,
        .renderer = .Vulkan,
    });

    const exe = step(b, .{
        .target = target,
        .optimize = optimize,
        .mode = .exe,
        .vulkan_zig = vulkan_zig,
        .cimgui_dep = cimgui_dep,
    });
    const hotlib = step(b, .{
        .target = target,
        .optimize = optimize,
        .mode = .hotlib,
        .vulkan_zig = vulkan_zig,
        .cimgui_dep = cimgui_dep,
    });
    const hotexe = step(b, .{
        .target = target,
        .optimize = optimize,
        .mode = .hotexe,
        .vulkan_zig = vulkan_zig,
        .cimgui_dep = cimgui_dep,
    });

    hotexe.step.dependOn(&hotlib.step);

    const hot_build_step = b.step("build-hot", "Build the hot app");
    hot_build_step.dependOn(&hotlib.step);

    const hot_run_cmd = b.addRunArtifact(hotexe);
    hot_run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        hot_run_cmd.addArgs(args);
    }
    const hot_run_step = b.step("run-hot", "Run the hot app");
    hot_run_step.dependOn(&hot_run_cmd.step);

    const exe_run_cmd = b.addRunArtifact(exe);
    exe_run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        exe_run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&exe_run_cmd.step);
}
