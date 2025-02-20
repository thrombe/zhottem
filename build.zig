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
    cimgui: *std.Build.Step.Compile,
    imgui_dep: *std.Build.Dependency,
    dcimgui_generated: *std.Build.Step.WriteFile,
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

            compile_step.addIncludePath(v.dcimgui_generated.getDirectory());
            compile_step.addIncludePath(v.imgui_dep.path("./"));
            compile_step.addIncludePath(v.imgui_dep.path("./backends"));

            // compile_step.linkLibrary(v.cimgui);
            compile_step.addObjectFile(b.path("./zig-out/lib/libcimgui.so"));
            compile_step.addLibraryPath(b.path("./zig-out/lib"));

            compile_step.linkSystemLibrary("glfw");
            compile_step.linkSystemLibrary("sndfile");
            compile_step.linkSystemLibrary("portaudio");
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

    return compile_step;
}

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // generate vulkan bindings from vk.xml and create a zig module from the generated code
    const vulkan_headers = b.dependency("vulkan_headers", .{});
    const registry = vulkan_headers.path("registry/vk.xml");
    const vk_gen = b.dependency("vulkan_zig", .{}).artifact("vulkan-zig-generator");
    const vk_generate_cmd = b.addRunArtifact(vk_gen);
    vk_generate_cmd.addFileArg(registry);
    const vulkan_zig = b.addModule("vulkan-zig", .{
        .root_source_file = vk_generate_cmd.addOutputFileArg("vk.zig"),
    });

    const dear = b.dependency("dear_bindings", .{}).path("dear_bindings.py");
    const imgui = b.dependency("imgui", .{});

    const cimgui = b.addSharedLibrary(.{
        .name = "cimgui",
        .target = target,
        .optimize = optimize,
    });

    const dcimgui_generated = b.addWriteFiles();
    cimgui.step.dependOn(&dcimgui_generated.step);

    const dcimgui = b.addSystemCommand(&[_][]const u8{"python"});
    dcimgui.addFileArg(dear);
    dcimgui.addArgs(&[_][]const u8{ "-o", "dcimgui" });
    dcimgui.addFileArg(imgui.path("imgui.h"));
    dcimgui.setCwd(dcimgui_generated.getDirectory());
    cimgui.step.dependOn(&dcimgui.step);

    const dcimgui_glfw = b.addSystemCommand(&[_][]const u8{"python"});
    dcimgui_glfw.addFileArg(dear);
    dcimgui_glfw.addArgs(&[_][]const u8{ "-o", "dcimgui_impl_glfw", "--backend", "--include" });
    dcimgui_glfw.addFileArg(imgui.path("imgui.h"));
    dcimgui_glfw.addFileArg(imgui.path("backends/imgui_impl_glfw.h"));
    dcimgui_glfw.step.dependOn(&dcimgui.step);
    dcimgui_glfw.setCwd(dcimgui_generated.getDirectory());
    cimgui.step.dependOn(&dcimgui_glfw.step);

    const dcimgui_vulkan = b.addSystemCommand(&[_][]const u8{"python"});
    dcimgui_vulkan.addFileArg(dear);
    dcimgui_vulkan.addArgs(&[_][]const u8{ "-o", "dcimgui_impl_vulkan", "--backend", "--include" });
    dcimgui_vulkan.addFileArg(imgui.path("imgui.h"));
    dcimgui_vulkan.addFileArg(imgui.path("backends/imgui_impl_vulkan.h"));
    dcimgui_vulkan.step.dependOn(&dcimgui.step);
    dcimgui_vulkan.setCwd(dcimgui_generated.getDirectory());
    cimgui.step.dependOn(&dcimgui_vulkan.step);

    cimgui.addCSourceFiles(.{ .root = imgui.path("./"), .files = &[_][]const u8{
        "imgui.cpp",
        "imgui_draw.cpp",
        "imgui_widgets.cpp",
        "imgui_tables.cpp",
        "imgui_demo.cpp",
        "backends/imgui_impl_glfw.cpp",
        "backends/imgui_impl_vulkan.cpp",
    }, .flags = &[_][]const u8{} });
    cimgui.addCSourceFiles(.{ .root = dcimgui_generated.getDirectory(), .files = &[_][]const u8{
        "dcimgui.cpp",
        "dcimgui_impl_glfw.cpp",
        "dcimgui_impl_vulkan.cpp",
    }, .flags = &[_][]const u8{} });
    cimgui.addIncludePath(imgui.path("./"));
    cimgui.addIncludePath(imgui.path("./backends"));
    cimgui.addIncludePath(vulkan_headers.path("./include"));
    cimgui.addIncludePath(dcimgui_generated.getDirectory());
    cimgui.linkSystemLibrary("vulkan");
    cimgui.linkSystemLibrary("glfw");
    cimgui.linkLibC();
    cimgui.linkLibCpp();

    const exe = step(b, .{
        .target = target,
        .optimize = optimize,
        .mode = .exe,
        .vulkan_zig = vulkan_zig,
        .cimgui = cimgui,
        .imgui_dep = imgui,
        .dcimgui_generated = dcimgui_generated,
    });
    const hotlib = step(b, .{
        .target = target,
        .optimize = optimize,
        .mode = .hotlib,
        .vulkan_zig = vulkan_zig,
        .cimgui = cimgui,
        .imgui_dep = imgui,
        .dcimgui_generated = dcimgui_generated,
    });
    const hotexe = step(b, .{
        .target = target,
        .optimize = optimize,
        .mode = .hotexe,
        .vulkan_zig = vulkan_zig,
        .cimgui = cimgui,
        .imgui_dep = imgui,
        .dcimgui_generated = dcimgui_generated,
    });

    const build_libs_step = b.step("build-libs", "Build the libs required for the app");
    build_libs_step.dependOn(&cimgui.step);
    build_libs_step.dependOn(&b.addInstallArtifact(cimgui, .{}).step);

    const hot_build_step = b.step("build-hot", "Build the hot app");
    hot_build_step.dependOn(&b.addInstallArtifact(hotlib, .{}).step);
    hot_build_step.dependOn(b.getInstallStep());

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
