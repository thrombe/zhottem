const std = @import("std");

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
    const cgltf = b.dependency("cgltf", .{});

    const exe = b.addExecutable(.{
        .name = "zhottem",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("vulkan", vulkan_zig);
    exe.linkLibrary(cimgui_dep.artifact("cimgui"));
    exe.addIncludePath(cimgui_dep.path("dcimgui/backends"));
    exe.linkSystemLibrary("fswatch");
    exe.linkSystemLibrary2("ImageMagick", .{});
    exe.linkSystemLibrary2("MagickWand", .{});
    exe.linkSystemLibrary2("MagickCore", .{});

    // OOF: zig can't compile .h files as c source?? .h compiles to "precompiled header file" something something
    // exe.addCSourceFile(.{
    //     .file = cgltf.path("./cgltf.h"),
    //     .flags = &.{ "-std=c99", "-DCGLTF_IMPLEMENTATION" },
    // });
    exe.addCSourceFile(.{
        .file = b.path("./src/c_imports.c"),
        .flags = &.{ "-std=c99", "-DCGLTF_IMPLEMENTATION" },
    });
    exe.addIncludePath(.{ .dependency = .{
        .dependency = cgltf,
        .sub_path = "./",
    } });

    exe.linkLibC();
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
