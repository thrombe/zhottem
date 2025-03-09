const std = @import("std");

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
    target: ?std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
}) struct { generated: *std.Build.Step.WriteFile, lib: *std.Build.Step.Compile } {
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

    cimgui.addCSourceFiles(.{ .root = v.imgui.path("./"), .files = &[_][]const u8{
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
    target: ?std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
}) struct { lib: *std.Build.Step.Compile } {
    const jolt = b.addSharedLibrary(.{
        .name = "jolt",
        .target = v.target,
        .optimize = v.optimize,
    });

    jolt.addIncludePath(v.jolt.path("./"));
    jolt.addCSourceFiles(.{
        .root = v.jolt.path("./Jolt"),
        .flags = &[_][]const u8{
            "-std=c++17",
            "-fno-exceptions",
            "-fno-sanitize=undefined",

            // "-DJPH_CROSS_PLATFORM_DETERMINISTIC",
            // "-DJPH_DEBUG_RENDERER",
            // "-DJPH_DOUBLE_PRECISION",
            // "-DJPH_ENABLE_ASSERTS",
        },
        .files = &[_][]const u8{
            "AABBTree/AABBTreeBuilder.cpp",
            "Core/Color.cpp",
            "Core/Factory.cpp",
            "Core/IssueReporting.cpp",
            "Core/JobSystemSingleThreaded.cpp",
            "Core/JobSystemThreadPool.cpp",
            "Core/JobSystemWithBarrier.cpp",
            "Core/LinearCurve.cpp",
            "Core/Memory.cpp",
            "Core/Profiler.cpp",
            "Core/RTTI.cpp",
            "Core/Semaphore.cpp",
            "Core/StringTools.cpp",
            "Core/TickCounter.cpp",
            "Geometry/ConvexHullBuilder.cpp",
            "Geometry/ConvexHullBuilder2D.cpp",
            "Geometry/Indexify.cpp",
            "Geometry/OrientedBox.cpp",
            "Math/Vec3.cpp",
            "ObjectStream/ObjectStream.cpp",
            "ObjectStream/ObjectStreamBinaryIn.cpp",
            "ObjectStream/ObjectStreamBinaryOut.cpp",
            "ObjectStream/ObjectStreamIn.cpp",
            "ObjectStream/ObjectStreamOut.cpp",
            "ObjectStream/ObjectStreamTextIn.cpp",
            "ObjectStream/ObjectStreamTextOut.cpp",
            "ObjectStream/SerializableObject.cpp",
            "ObjectStream/TypeDeclarations.cpp",
            "Physics/Body/Body.cpp",
            "Physics/Body/BodyCreationSettings.cpp",
            "Physics/Body/BodyInterface.cpp",
            "Physics/Body/BodyManager.cpp",
            "Physics/Body/MassProperties.cpp",
            "Physics/Body/MotionProperties.cpp",
            "Physics/Character/Character.cpp",
            "Physics/Character/CharacterBase.cpp",
            "Physics/Character/CharacterVirtual.cpp",
            "Physics/Collision/BroadPhase/BroadPhase.cpp",
            "Physics/Collision/BroadPhase/BroadPhaseBruteForce.cpp",
            "Physics/Collision/BroadPhase/BroadPhaseQuadTree.cpp",
            "Physics/Collision/BroadPhase/QuadTree.cpp",
            "Physics/Collision/CastConvexVsTriangles.cpp",
            "Physics/Collision/CastSphereVsTriangles.cpp",
            "Physics/Collision/CollideConvexVsTriangles.cpp",
            "Physics/Collision/CollideSphereVsTriangles.cpp",
            "Physics/Collision/CollisionDispatch.cpp",
            "Physics/Collision/CollisionGroup.cpp",
            "Physics/Collision/EstimateCollisionResponse.cpp",
            "Physics/Collision/GroupFilter.cpp",
            "Physics/Collision/GroupFilterTable.cpp",
            "Physics/Collision/ManifoldBetweenTwoFaces.cpp",
            "Physics/Collision/NarrowPhaseQuery.cpp",
            "Physics/Collision/NarrowPhaseStats.cpp",
            "Physics/Collision/PhysicsMaterial.cpp",
            "Physics/Collision/PhysicsMaterialSimple.cpp",
            "Physics/Collision/Shape/BoxShape.cpp",
            "Physics/Collision/Shape/CapsuleShape.cpp",
            "Physics/Collision/Shape/CompoundShape.cpp",
            "Physics/Collision/Shape/ConvexHullShape.cpp",
            "Physics/Collision/Shape/ConvexShape.cpp",
            "Physics/Collision/Shape/CylinderShape.cpp",
            "Physics/Collision/Shape/DecoratedShape.cpp",
            "Physics/Collision/Shape/EmptyShape.cpp",
            "Physics/Collision/Shape/HeightFieldShape.cpp",
            "Physics/Collision/Shape/MeshShape.cpp",
            "Physics/Collision/Shape/MutableCompoundShape.cpp",
            "Physics/Collision/Shape/OffsetCenterOfMassShape.cpp",
            "Physics/Collision/Shape/PlaneShape.cpp",
            "Physics/Collision/Shape/RotatedTranslatedShape.cpp",
            "Physics/Collision/Shape/ScaledShape.cpp",
            "Physics/Collision/Shape/Shape.cpp",
            "Physics/Collision/Shape/SphereShape.cpp",
            "Physics/Collision/Shape/StaticCompoundShape.cpp",
            "Physics/Collision/Shape/TaperedCapsuleShape.cpp",
            "Physics/Collision/Shape/TaperedCylinderShape.cpp",
            "Physics/Collision/Shape/TriangleShape.cpp",
            "Physics/Collision/TransformedShape.cpp",
            "Physics/Constraints/ConeConstraint.cpp",
            "Physics/Constraints/Constraint.cpp",
            "Physics/Constraints/ConstraintManager.cpp",
            "Physics/Constraints/ContactConstraintManager.cpp",
            "Physics/Constraints/DistanceConstraint.cpp",
            "Physics/Constraints/FixedConstraint.cpp",
            "Physics/Constraints/GearConstraint.cpp",
            "Physics/Constraints/HingeConstraint.cpp",
            "Physics/Constraints/MotorSettings.cpp",
            "Physics/Constraints/PathConstraint.cpp",
            "Physics/Constraints/PathConstraintPath.cpp",
            "Physics/Constraints/PathConstraintPathHermite.cpp",
            "Physics/Constraints/PointConstraint.cpp",
            "Physics/Constraints/PulleyConstraint.cpp",
            "Physics/Constraints/RackAndPinionConstraint.cpp",
            "Physics/Constraints/SixDOFConstraint.cpp",
            "Physics/Constraints/SliderConstraint.cpp",
            "Physics/Constraints/SpringSettings.cpp",
            "Physics/Constraints/SwingTwistConstraint.cpp",
            "Physics/Constraints/TwoBodyConstraint.cpp",
            "Physics/DeterminismLog.cpp",
            "Physics/IslandBuilder.cpp",
            "Physics/LargeIslandSplitter.cpp",
            "Physics/PhysicsScene.cpp",
            "Physics/PhysicsSystem.cpp",
            "Physics/PhysicsUpdateContext.cpp",
            "Physics/Ragdoll/Ragdoll.cpp",
            "Physics/SoftBody/SoftBodyCreationSettings.cpp",
            "Physics/SoftBody/SoftBodyMotionProperties.cpp",
            "Physics/SoftBody/SoftBodyShape.cpp",
            "Physics/SoftBody/SoftBodySharedSettings.cpp",
            "Physics/StateRecorderImpl.cpp",
            "Physics/Vehicle/MotorcycleController.cpp",
            "Physics/Vehicle/TrackedVehicleController.cpp",
            "Physics/Vehicle/VehicleAntiRollBar.cpp",
            "Physics/Vehicle/VehicleCollisionTester.cpp",
            "Physics/Vehicle/VehicleConstraint.cpp",
            "Physics/Vehicle/VehicleController.cpp",
            "Physics/Vehicle/VehicleDifferential.cpp",
            "Physics/Vehicle/VehicleEngine.cpp",
            "Physics/Vehicle/VehicleTrack.cpp",
            "Physics/Vehicle/VehicleTransmission.cpp",
            "Physics/Vehicle/Wheel.cpp",
            "Physics/Vehicle/WheeledVehicleController.cpp",
            "RegisterTypes.cpp",
            "Renderer/DebugRenderer.cpp",
            "Renderer/DebugRendererPlayback.cpp",
            "Renderer/DebugRendererRecorder.cpp",
            "Renderer/DebugRendererSimple.cpp",
            "Skeleton/SkeletalAnimation.cpp",
            "Skeleton/Skeleton.cpp",
            "Skeleton/SkeletonMapper.cpp",
            "Skeleton/SkeletonPose.cpp",
            "TriangleSplitter/TriangleSplitter.cpp",
            "TriangleSplitter/TriangleSplitterBinning.cpp",
            "TriangleSplitter/TriangleSplitterMean.cpp",
        },
    });

    jolt.linkLibC();
    jolt.linkLibCpp();

    return .{ .lib = jolt };
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
    jolt_dep: *std.Build.Dependency,
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
    options.addOption([]const u8, "hotlib_name", "libhot.so");
    options.addOption(CompileMode, "mode", v.mode);
    compile_step.root_module.addImport("build-options", options.createModule());

    switch (v.mode) {
        .exe, .hotlib => {
            compile_step.root_module.addImport("vulkan", v.vulkan_zig);

            compile_step.addIncludePath(v.dcimgui_generated.getDirectory());
            compile_step.addIncludePath(v.imgui_dep.path("./"));
            compile_step.addIncludePath(v.imgui_dep.path("./backends"));
            compile_step.addIncludePath(v.jolt_dep.path("./"));
            compile_step.addIncludePath(b.path("./src"));

            // compile_step.linkLibrary(v.cimgui);
            // compile_step.linkLibrary(v.jolt);

            compile_step.addLibraryPath(b.path("./zig-out/lib"));
            compile_step.addObjectFile(b.path("./zig-out/lib/libcimgui.so"));
            compile_step.addObjectFile(b.path("./zig-out/lib/libjolt.so"));

            compile_step.addCSourceFiles(.{
                .root = b.path("./src"),
                .flags = &[_][]const u8{},
                .files = &[_][]const u8{
                    "jolt.cpp",
                },
            });

            compile_step.linkSystemLibrary("glfw");
            compile_step.linkSystemLibrary("portaudio");
            compile_step.linkSystemLibrary("fswatch");
            compile_step.linkSystemLibrary2("ImageMagick", .{});
            compile_step.linkSystemLibrary2("MagickWand", .{});
            compile_step.linkSystemLibrary2("MagickCore", .{});
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

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const vulkan_headers = b.dependency("vulkan_headers", .{});
    const imgui = b.dependency("imgui", .{});
    const jolt = b.dependency("jolt", .{});

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
    const libjolt = jolt_step(b, .{
        .jolt = jolt,
        .target = target,
        .optimize = optimize,
    });

    const exe = step(b, .{
        .target = target,
        .optimize = optimize,
        .mode = .exe,
        .vulkan_zig = vulkan.mod,
        .imgui_dep = imgui,
        .cimgui = libimgui.lib,
        .dcimgui_generated = libimgui.generated,
        .jolt = libjolt.lib,
        .jolt_dep = jolt,
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
        .jolt_dep = jolt,
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
        .jolt_dep = jolt,
    });

    const build_libs_step = b.step("build-libs", "Build the libs required for the app");
    build_libs_step.dependOn(&libimgui.lib.step);
    build_libs_step.dependOn(&b.addInstallArtifact(libimgui.lib, .{}).step);
    build_libs_step.dependOn(&libjolt.lib.step);
    build_libs_step.dependOn(&b.addInstallArtifact(libjolt.lib, .{}).step);

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
