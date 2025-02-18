const std = @import("std");

const vk = @import("vulkan");

const utils = @import("utils.zig");
const Fuse = utils.Fuse;
const ShaderUtils = utils.ShaderUtils;

const math = @import("math.zig");
const Vec4 = math.Vec4;

const mesh_mod = @import("mesh.zig");

const Engine = @import("engine.zig");
const c = Engine.c;
const Device = Engine.VulkanContext.Api.Device;

const gui = @import("gui.zig");
const GuiEngine = gui.GuiEngine;

const render_utils = @import("render_utils.zig");
const Swapchain = render_utils.Swapchain;
const UniformBuffer = render_utils.UniformBuffer;
const DynamicUniformBuffer = render_utils.DynamicUniformBuffer;
const Buffer = render_utils.Buffer;
const Image = render_utils.Image;
const GraphicsPipeline = render_utils.GraphicsPipeline;
const RenderPass = render_utils.RenderPass;
const DescriptorPool = render_utils.DescriptorPool;
const DescriptorSet = render_utils.DescriptorSet;
const CmdBuffer = render_utils.CmdBuffer;

const world_mod = @import("world.zig");
const World = world_mod.World;
const Components = world_mod.Components;
const Entity = world_mod.Entity;

const resources_mod = @import("resources.zig");
const GpuResourceManager = resources_mod.GpuResourceManager;
const InstanceManager = resources_mod.InstanceManager;
const InstanceAllocator = resources_mod.InstanceAllocator;

const main = @import("main.zig");
const allocator = main.allocator;

pub const App = @This();

world: World,

uniforms: UniformBuffer,
model_uniforms: ModelUniformBuffer,
screen_image: Image,
depth_image: Image,
cpu_resources: GpuResourceManager.CpuResources,
gpu_resources: GpuResourceManager.GpuResources,
instance_manager: InstanceManager,
descriptor_pool: DescriptorPool,
camera_descriptor_set: DescriptorSet,
model_descriptor_set: DescriptorSet,
command_pool: vk.CommandPool,
stages: ShaderStageManager,

texture_img: utils.ImageMagick.UnormImage,
texture: Image,

handles: struct {
    player: Entity,
    model: struct {
        sphere: GpuResourceManager.ModelHandle,
    },
    mesh: struct {
        cube: GpuResourceManager.MeshResourceHandle,
    },
},

var matrices = std.mem.zeroes([2]math.Mat4x4);
const ModelUniformBuffer = DynamicUniformBuffer(math.Mat4x4);

pub fn init(engine: *Engine, app_state: *AppState) !@This() {
    var ctx = &engine.graphics;
    const device = &ctx.device;

    const cmd_pool = try device.createCommandPool(&.{
        .queue_family_index = ctx.graphics_queue.family,
        .flags = .{
            .reset_command_buffer_bit = true,
        },
    }, null);
    errdefer device.destroyCommandPool(cmd_pool, null);

    var model_uniform = try ModelUniformBuffer.new(&matrices, ctx);
    errdefer model_uniform.deinit(device);
    model_uniform.uniform_buffer[0] = math.Mat4x4.scaling_mat(math.Vec4.splat3(0.3)).mul_mat(math.Mat4x4.translation_mat(.{ .x = 1.5 }));
    model_uniform.uniform_buffer[1] = math.Mat4x4.scaling_mat(math.Vec4.splat3(0.5)).mul_mat(math.Mat4x4.translation_mat(.{ .y = 3 }));
    try model_uniform.upload(device);

    var screen = try Image.new(ctx, cmd_pool, .{
        .img_type = .@"2d",
        .img_view_type = .@"2d",
        .format = .r16g16b16a16_sfloat,
        .layout = .color_attachment_optimal,
        .extent = .{
            .width = app_state.monitor_rez.width,
            .height = app_state.monitor_rez.height,
            .depth = 1,
        },
        .usage = .{
            .transfer_src_bit = true,
            .color_attachment_bit = true,
        },
        .view_aspect_mask = .{
            .color_bit = true,
        },
    });
    errdefer screen.deinit(device);

    var depth = try Image.new(ctx, cmd_pool, .{
        .img_type = .@"2d",
        .img_view_type = .@"2d",
        .format = .d32_sfloat,
        .layout = .depth_stencil_attachment_optimal,
        .extent = .{
            .width = app_state.monitor_rez.width,
            .height = app_state.monitor_rez.height,
            .depth = 1,
        },
        .usage = .{
            .depth_stencil_attachment_bit = true,
        },
        .view_aspect_mask = .{
            .depth_bit = true,
        },
    });
    errdefer depth.deinit(device);

    var image = try utils.ImageMagick.from_file("./assets/img.png", .unorm);
    errdefer image.deinit();
    const slice = std.mem.bytesAsSlice([4]u8, std.mem.sliceAsBytes(image.buffer));

    var gpu_img = try Image.new_from_slice(ctx, cmd_pool, .{
        .extent = .{ .width = @intCast(image.width), .height = @intCast(image.height) },
        .bind_desc_type = .combined_image_sampler,
        .layout = .shader_read_only_optimal,
        .usage = .{
            .sampled_bit = true,
        },
    }, slice);
    errdefer gpu_img.deinit(device);

    var gltf = try mesh_mod.Gltf.parse_glb("./assets/dance.glb");
    // defer gltf.deinit();
    // const model = try gltf.to_model("Cube.015", "metarig");
    const sphere = try gltf.to_model("Cube.015", "metarig");

    // var object = try mesh.ObjParser.mesh_from_file("./assets/object.obj");
    // defer object.deinit();

    var cube = try mesh_mod.Mesh.cube();
    defer cube.deinit();

    var plane = try mesh_mod.Mesh.plane();
    defer plane.deinit();

    // var sphere_gltf = try mesh_mod.Gltf.parse_glb("./assets/sphere.glb");
    // defer sphere_gltf.deinit();
    // const sphere = try sphere_gltf.to_model();
    // defer sphere.deinit();

    var cpu = GpuResourceManager.CpuResources.init();
    errdefer cpu.deinit();

    const bones_handle = try cpu.reserve_bones(1500);

    var instance_manager = InstanceManager.init(bones_handle);
    errdefer instance_manager.deinit();

    const cube_instance_handle = try cpu.batch_reserve(1000);
    const cube_mesh_handle = try cpu.add_mesh(&cube);
    try instance_manager.instances.append(.{ .mesh = cube_mesh_handle, .instances = cube_instance_handle });

    const plane_mesh_handle = try cpu.add_mesh(&plane);
    const plane_instance_handle = try cpu.batch_reserve(10);
    try instance_manager.instances.append(.{ .mesh = plane_mesh_handle, .instances = plane_instance_handle });

    const sphere_model_handle = try cpu.add_model(sphere);
    const sphere_mesh_handle = sphere_model_handle.mesh;
    const sphere_instance_handle = try cpu.batch_reserve(100);
    try instance_manager.instances.append(.{ .mesh = sphere_mesh_handle, .instances = sphere_instance_handle });

    var world = try World.init();
    errdefer world.deinit();
    const player_id = try world.ecs.insert(.{
        @as([]const u8, "player"),
        math.Camera.init(
            math.Camera.constants.basis.vulkan,
            math.Camera.constants.basis.opengl,
        ),
        Components.Controller{},
        Components.Transform{ .pos = .{} },
        Components.LastTransform{},
        Components.Rigidbody{
            .flags = .{ .player = true },
            .mass = 2,
            .dynamic_friction = 1.0,
        },
        Components.Collider{ .sphere = .{ .radius = 1 } },
    });

    // _ = try world.ecs.insert(.{
    //     Components.Rigidbody{
    //         .flags = .{ .pinned = true },
    //     },
    //     Components.Transform{ .pos = .{ .y = -3 } },
    //     Components.LastTransform{},
    //     Components.Collider{ .sphere = .{ .radius = -10 } },
    // });
    _ = try world.ecs.insert(.{
        Components.Rigidbody{ .flags = .{ .pinned = true }, .dynamic_friction = 1 },
        Components.Transform{
            .pos = .{ .y = -3 },
            .scale = Vec4.splat3(50),
        },
        Components.LastTransform{},
        Components.Collider{ .plane = .{ .normal = .{ .y = 1 } } },
        Components.StaticRender{ .mesh = plane_mesh_handle },
    });
    _ = try world.ecs.insert(.{
        Components.Rigidbody{ .flags = .{ .pinned = true } },
        Components.Transform{
            .pos = .{ .y = 50, .x = 50 },
            .rotation = Vec4.quat_angle_axis(std.math.pi / 2.0, .{ .z = 1 }),
            .scale = Vec4.splat3(50),
        },
        Components.LastTransform{},
        Components.Collider{ .plane = .{ .normal = .{ .y = 1 } } },
        Components.StaticRender{ .mesh = plane_mesh_handle },
    });
    _ = try world.ecs.insert(.{
        Components.Rigidbody{ .flags = .{ .pinned = true } },
        Components.Transform{
            .pos = .{ .y = 50, .x = -50 },
            .rotation = Vec4.quat_angle_axis(-std.math.pi / 2.0, .{ .z = 1 }),
            .scale = Vec4.splat3(50),
        },
        Components.LastTransform{},
        Components.Collider{ .plane = .{ .normal = .{ .y = 1 } } },
        Components.StaticRender{ .mesh = plane_mesh_handle },
    });
    _ = try world.ecs.insert(.{
        Components.Rigidbody{ .flags = .{ .pinned = true } },
        Components.Transform{
            .pos = .{ .y = 50, .z = 50 },
            .rotation = Vec4.quat_angle_axis(-std.math.pi / 2.0, .{ .x = 1 }),
            .scale = Vec4.splat3(50),
        },
        Components.LastTransform{},
        Components.Collider{ .plane = .{ .normal = .{ .y = 1 } } },
        Components.StaticRender{ .mesh = plane_mesh_handle },
    });
    _ = try world.ecs.insert(.{
        Components.Rigidbody{ .flags = .{ .pinned = true } },
        Components.Transform{
            .pos = .{ .y = 50, .z = -50 },
            .rotation = Vec4.quat_angle_axis(std.math.pi / 2.0, .{ .x = 1 }),
            .scale = Vec4.splat3(50),
        },
        Components.LastTransform{},
        Components.Collider{ .plane = .{ .normal = .{ .y = 1 } } },
        Components.StaticRender{ .mesh = plane_mesh_handle },
    });

    // const object_mesh_handle = try cpu.add_mesh(&object);
    // const object_instance_handle = try cpu.batch_reserve(1);
    // try drawcalls.append(.{ .mesh = object_mesh_handle, .instances = object_instance_handle });
    // try world.entities.append(.{
    //     .name = "object",
    //     .typ = .{ .object = true },
    //     .transform = .{
    //         .pos = .{ .y = 2 },
    //         .scale = Vec4.splat3(0.2),
    //     },
    //     .mesh = object_mesh_handle,
    // });

    // for (sphere_instance_handle.first..(sphere_instance_handle.first + sphere_instance_handle.count), 0..) |_, i| {
    //     if (i > 2) break;
    //     _ = try world.ecs.insert(.{
    //         @as([]const u8, "persistent balls"),
    //         Components.Transform{ .pos = .{ .x = @floatFromInt(i * 3), .y = 5 } },
    //         Components.Rigidbody{},
    //         Components.Collider{ .sphere = .{ .radius = 1 } },
    //         Components.AnimatedRender{ .model = sphere_model_handle },
    //     });
    // }

    const player = try world.ecs.get(player_id, struct { transform: Components.Transform, camera: math.Camera, controller: Components.Controller });
    var uniforms = try UniformBuffer.new(try app_state.uniforms(engine.window, player.transform, player.camera, player.controller), ctx);
    errdefer uniforms.deinit(device);

    var gpu = try cpu.upload(engine, cmd_pool);
    errdefer gpu.deinit(device);

    var desc_pool = try DescriptorPool.new(device);
    errdefer desc_pool.deinit(device);

    var camera_desc_set_builder = desc_pool.set_builder();
    defer camera_desc_set_builder.deinit();
    try camera_desc_set_builder.add(&uniforms);
    var camera_desc_set = try camera_desc_set_builder.build(device);
    errdefer camera_desc_set.deinit(device);

    var model_desc_set_builder = desc_pool.set_builder();
    defer model_desc_set_builder.deinit();
    try model_desc_set_builder.add(&model_uniform);
    try model_desc_set_builder.add(&gpu_img);
    try model_desc_set_builder.add(&gpu.bone_buffer);
    var model_desc_set = try model_desc_set_builder.build(device);
    errdefer model_desc_set.deinit(device);

    const stages = try ShaderStageManager.init();
    errdefer stages.deinit();

    return @This(){
        .world = world,
        .uniforms = uniforms,
        .model_uniforms = model_uniform,
        .screen_image = screen,
        .depth_image = depth,
        .gpu_resources = gpu,
        .cpu_resources = cpu,
        .instance_manager = instance_manager,
        .descriptor_pool = desc_pool,
        .camera_descriptor_set = camera_desc_set,
        .model_descriptor_set = model_desc_set,
        .command_pool = cmd_pool,
        .stages = stages,

        .texture_img = image,
        .texture = gpu_img,

        .handles = .{
            .player = player_id,
            .model = .{
                .sphere = sphere_model_handle,
            },
            .mesh = .{
                .cube = cube_mesh_handle,
            },
        },
    };
}

pub fn deinit(self: *@This(), device: *Device) void {
    defer device.destroyCommandPool(self.command_pool, null);
    defer self.world.deinit();
    defer self.uniforms.deinit(device);
    defer self.model_uniforms.deinit(device);
    defer self.screen_image.deinit(device);
    defer self.depth_image.deinit(device);
    defer self.cpu_resources.deinit();
    defer self.gpu_resources.deinit(device);
    defer self.instance_manager.deinit();
    defer self.camera_descriptor_set.deinit(device);
    defer self.model_descriptor_set.deinit(device);
    defer self.descriptor_pool.deinit(device);
    defer self.stages.deinit();
    defer self.texture_img.deinit();
    defer self.texture.deinit(device);
}

pub fn present(
    self: *@This(),
    dynamic_state: *RendererState,
    gui_renderer: *GuiEngine.GuiRenderer,
    ctx: *Engine.VulkanContext,
) !Swapchain.PresentState {
    const cmdbuf = dynamic_state.cmdbuffer.bufs[dynamic_state.swapchain.image_index];
    const gui_cmdbuf = gui_renderer.cmd_bufs[dynamic_state.swapchain.image_index];

    const current_si = try dynamic_state.swapchain.present_start(ctx);

    try self.uniforms.upload(&ctx.device);
    try self.gpu_resources.update_instances(&ctx.device, self.cpu_resources.instances.items);
    try self.gpu_resources.update_bones(&ctx.device, self.cpu_resources.bones.items);

    return dynamic_state.swapchain.present_end(&[_]vk.CommandBuffer{ cmdbuf, gui_cmdbuf }, ctx, current_si) catch |err| switch (err) {
        error.OutOfDateKHR => return .suboptimal,
        else => |narrow| return narrow,
    };
}

pub const RendererState = struct {
    swapchain: Swapchain,
    cmdbuffer: CmdBuffer,
    pipeline: GraphicsPipeline,
    bg_pipeline: GraphicsPipeline,

    // not owned
    pool: vk.CommandPool,

    pub fn init(app: *App, engine: *Engine, app_state: *AppState) !@This() {
        _ = app_state;
        const ctx = &engine.graphics;
        const device = &ctx.device;

        var swapchain = try Swapchain.init(ctx, engine.window.extent, .{});
        errdefer swapchain.deinit(device);

        var self: @This() = .{
            .pipeline = undefined,
            .bg_pipeline = undefined,
            .swapchain = swapchain,
            .pool = app.command_pool,
            .cmdbuffer = undefined,
        };

        const pipelines = try self.create_pipelines(engine, app);
        self.pipeline = pipelines.pipeline;
        self.bg_pipeline = pipelines.bg_pipeline;
        errdefer self.pipeline.deinit(device);
        errdefer self.bg_pipeline.deinit(device);

        self.cmdbuffer = try self.create_cmdbuf(engine, app);
        errdefer self.cmdbuffer.deinit(device);

        return self;
    }

    pub fn recreate_pipelines(self: *@This(), engine: *Engine, app: *App, app_state: *AppState) !void {
        const ctx = &engine.graphics;
        const device = &ctx.device;

        const pipelines = try self.create_pipelines(engine, app);

        self.pipeline.deinit(device);
        self.bg_pipeline.deinit(device);
        self.pipeline = pipelines.pipeline;
        self.bg_pipeline = pipelines.bg_pipeline;

        _ = app_state.cmdbuf_fuse.fuse();
    }

    pub fn recreate_swapchain(self: *@This(), engine: *Engine, app_state: *AppState) !void {
        try self.swapchain.recreate(&engine.graphics, engine.window.extent, .{});
        _ = app_state.cmdbuf_fuse.fuse();
    }

    pub fn recreate_cmdbuf(self: *@This(), engine: *Engine, app: *App) !void {
        const ctx = &engine.graphics;
        const device = &ctx.device;

        const cmdbuffer = try self.create_cmdbuf(engine, app);
        self.cmdbuffer.deinit(device);
        self.cmdbuffer = cmdbuffer;
    }

    pub fn create_pipelines(self: *@This(), engine: *Engine, app: *App) !struct { bg_pipeline: GraphicsPipeline, pipeline: GraphicsPipeline } {
        _ = self;
        const ctx = &engine.graphics;
        const device = &ctx.device;

        var pipeline = try GraphicsPipeline.new(device, .{
            .vert = app.stages.shaders.map.get(.vert).code,
            .frag = app.stages.shaders.map.get(.frag).code,
            .vertex_info = .{
                .binding_desc = &(resources_mod.Vertex.binding_description ++ resources_mod.Instance.binding_desc),
                .attr_desc = &(resources_mod.Vertex.attribute_description ++ resources_mod.Instance.attribute_desc),
            },
            .dynamic_info = .{
                .image_format = app.screen_image.format,
                .depth_format = app.depth_image.format,
            },
            .desc_set_layouts = &[_]vk.DescriptorSetLayout{
                app.camera_descriptor_set.layout,
                app.model_descriptor_set.layout,
            },
        });
        errdefer pipeline.deinit(device);

        var bg_pipeline = try GraphicsPipeline.new(device, .{
            .vert = app.stages.shaders.map.get(.bg_vert).code,
            .frag = app.stages.shaders.map.get(.bg_frag).code,
            .vertex_info = .{
                .binding_desc = &[_]vk.VertexInputBindingDescription{},
                .attr_desc = &[_]vk.VertexInputAttributeDescription{},
            },
            .dynamic_info = .{
                .image_format = app.screen_image.format,
                .depth_format = app.depth_image.format,
            },
            .desc_set_layouts = &[_]vk.DescriptorSetLayout{
                app.camera_descriptor_set.layout,
            },
        });
        errdefer bg_pipeline.deinit(device);

        return .{ .pipeline = pipeline, .bg_pipeline = bg_pipeline };
    }

    pub fn create_cmdbuf(self: *@This(), engine: *Engine, app: *App) !CmdBuffer {
        const ctx = &engine.graphics;
        const device = &ctx.device;

        var cmdbuf = try CmdBuffer.init(device, .{
            .pool = app.command_pool,
            .size = self.swapchain.swap_images.len,
        });
        errdefer cmdbuf.deinit(device);

        try cmdbuf.begin(device);
        cmdbuf.dynamic_render_begin(device, .{
            .image = app.screen_image.view,
            .depth = app.depth_image.view,
            .extent = engine.window.extent,
        });

        // these offsets are for each dynamic descriptor.
        // basically - you bind a buffer of objects in the desc set, and just tell
        // it what offset you want for that data here.
        for (app.instance_manager.instances.items) |call| {
            call.draw(
                &self.pipeline,
                &[_]vk.DescriptorSet{
                    app.camera_descriptor_set.set,
                    app.model_descriptor_set.set,
                },
                &[_]u32{0},
                &cmdbuf,
                device,
                &app.gpu_resources,
            );
        }

        cmdbuf.draw(device, .{
            .pipeline = &self.bg_pipeline,
            .desc_sets = &[_]vk.DescriptorSet{
                app.camera_descriptor_set.set,
            },
            .dynamic_offsets = &[_]u32{},
            .vertices = .{
                .buffer = null,
                .count = 6,
                .first = 0,
            },
            .indices = .{
                .buffer = null,
                .count = undefined,
                .first = undefined,
            },
            .instances = .{
                .buffer = null,
                .count = 1,
                .first = 0,
            },
        });

        cmdbuf.dynamic_render_end(device);
        cmdbuf.drawIntoSwapchain(device, .{
            .image = app.screen_image.image,
            .image_layout = .color_attachment_optimal,
            .size = self.swapchain.extent,
            .swapchain = &self.swapchain,
            .queue_family = ctx.graphics_queue.family,
        });
        try cmdbuf.end(device);

        return cmdbuf;
    }

    pub fn deinit(self: *@This(), device: *Device) void {
        try self.swapchain.waitForAllFences(device);

        defer self.swapchain.deinit(device);
        defer self.cmdbuffer.deinit(device);

        defer self.pipeline.deinit(device);
        defer self.bg_pipeline.deinit(device);
    }
};

const ShaderStageManager = struct {
    shaders: CompilerUtils.Stages,
    compiler: CompilerUtils.Compiler,

    const ShaderStage = enum {
        bg_vert,
        bg_frag,
        vert,
        frag,
    };
    const CompilerUtils = utils.ShaderCompiler(struct {
        pub fn get_metadata(_: CompilerUtils.ShaderInfo) !@This() {
            return .{};
        }
    }, ShaderStage);

    pub fn init() !@This() {
        var comp = try CompilerUtils.Compiler.init(.{ .opt = .fast, .env = .vulkan1_3 }, &[_]CompilerUtils.ShaderInfo{
            .{
                .typ = .bg_vert,
                .stage = .vertex,
                .path = "./src/shader.glsl",
                .define = &[_][]const u8{"BG_VERT_PASS"},
                .include = &[_][]const u8{"./src"},
            },
            .{
                .typ = .bg_frag,
                .stage = .fragment,
                .path = "./src/shader.glsl",
                .define = &[_][]const u8{"BG_FRAG_PASS"},
                .include = &[_][]const u8{"./src"},
            },
            .{
                .typ = .vert,
                .stage = .vertex,
                .path = "./src/shader.glsl",
                .define = &[_][]const u8{"VERT_PASS"},
                .include = &[_][]const u8{"./src"},
            },
            .{
                .typ = .frag,
                .stage = .fragment,
                .path = "./src/shader.glsl",
                .define = &[_][]const u8{"FRAG_PASS"},
                .include = &[_][]const u8{"./src"},
            },
        });
        errdefer comp.deinit();

        return .{
            .shaders = try CompilerUtils.Stages.init(&comp),
            .compiler = comp,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.shaders.deinit();
        self.compiler.deinit();
    }

    pub fn update(self: *@This()) bool {
        return self.shaders.update(&self.compiler);
    }
};

pub const AppState = struct {
    monitor_rez: struct { width: u32, height: u32 },
    mouse: extern struct { x: i32 = 0, y: i32 = 0, left: bool = false, right: bool = false } = .{},

    frame: u32 = 0,
    time: f32 = 0,
    deltatime: f32 = 0,
    fps_cap: u32 = 60,

    physics: struct {
        step: f32 = 1.0 / 30.0,
        acctime: f32 = 0,
        interpolation_acctime: f32 = 0,

        fn interpolated(self: *const @This(), lt: *const Components.LastTransform, t: *const Components.Transform) Components.Transform {
            return lt.lerp(t, self.interpolation_acctime / self.step);
        }
    } = .{},

    rng: std.Random.Xoshiro256,
    resize_fuse: Fuse = .{},
    cmdbuf_fuse: Fuse = .{},
    shader_fuse: Fuse = .{},
    uniform_buffer: []u8,
    uniform_shader_dumped: bool = false,
    focus: bool = false,

    pub fn init(window: *Engine.Window) !@This() {
        const mouse = window.poll_mouse();
        const sze = try window.get_res();

        const rng = std.Random.DefaultPrng.init(@intCast(std.time.timestamp()));

        return .{
            .monitor_rez = .{ .width = sze.width, .height = sze.height },
            .mouse = .{ .x = mouse.x, .y = mouse.y, .left = mouse.left },
            .rng = rng,
            .uniform_buffer = try allocator.alloc(u8, 0),
        };
    }

    pub fn deinit(self: *@This()) void {
        allocator.free(self.uniform_buffer);
    }

    pub fn tick(self: *@This(), lap: u64, engine: *Engine, app: *App) !void {
        const window = engine.window;
        const delta = @as(f32, @floatFromInt(lap)) / @as(f32, @floatFromInt(std.time.ns_per_s));

        const input = window.input();

        var mouse = input.mouse;
        var kb = input.keys;

        const imgui_io = &c.ImGui_GetIO()[0];
        if (imgui_io.WantCaptureMouse) {
            mouse = std.mem.zeroes(@TypeOf(mouse));
            mouse.x = input.mouse.x;
            mouse.y = input.mouse.y;
        }
        if (imgui_io.WantCaptureKeyboard) {
            kb = std.mem.zeroes(@TypeOf(kb));
        }

        if (kb.p.just_pressed()) {
            try render_utils.dump_image_to_file(
                &app.screen_image,
                &engine.graphics,
                app.command_pool,
                window.extent,
                "./images",
            );
        }

        if (mouse.left.just_pressed() and !self.focus) {
            self.focus = true;
            imgui_io.ConfigFlags |= c.ImGuiConfigFlags_NoMouse;
            window.hide_cursor(true);
        }
        if (kb.escape.just_pressed() and !self.focus) {
            window.queue_close();
        }
        if (kb.escape.just_pressed() and self.focus) {
            self.focus = false;
            imgui_io.ConfigFlags &= ~c.ImGuiConfigFlags_NoMouse;
            window.hide_cursor(false);
        }

        self.mouse.left = mouse.left.pressed();
        self.mouse.x = @intFromFloat(mouse.x);
        self.mouse.y = @intFromFloat(mouse.y);

        self.frame += 1;
        self.time += delta;
        self.deltatime = delta;

        {
            var player = try app.world.ecs.get(app.handles.player, struct { camera: math.Camera, controller: Components.Controller, t: Components.Transform, r: Components.Rigidbody, lt: Components.LastTransform });

            player.controller.did_move = kb.w.pressed() or kb.a.pressed() or kb.s.pressed() or kb.d.pressed();
            player.controller.did_rotate = @abs(mouse.dx) + @abs(mouse.dy) > 0.0001 and self.focus;

            // rotation should not be multiplied by deltatime. if mouse moves by 3cm, it should always rotate the same amount.
            if (player.controller.did_rotate) {
                player.controller.yaw += mouse.dx * player.controller.sensitivity_scale * player.controller.sensitivity;
                player.controller.pitch += mouse.dy * player.controller.sensitivity_scale * player.controller.sensitivity;
                player.controller.pitch = std.math.clamp(player.controller.pitch, math.Camera.constants.pitch_min, math.Camera.constants.pitch_max);
            }

            const rot = player.camera.rot_quat(player.controller.pitch, player.controller.yaw);
            const fwd = rot.rotate_vector(player.camera.world_basis.fwd);
            const right = rot.rotate_vector(player.camera.world_basis.right);

            player.t.rotation = rot;

            var speed = player.controller.speed;
            if (kb.shift.pressed()) {
                speed *= 2.0;
            }
            if (kb.ctrl.pressed()) {
                speed *= 0.1;
            }

            speed *= 50 * player.controller.speed;
            speed *= player.r.mass;

            if (kb.w.pressed()) {
                player.r.force = fwd.scale(speed);
            }
            if (kb.a.pressed()) {
                player.r.force = right.scale(-speed);
            }
            if (kb.s.pressed()) {
                player.r.force = fwd.scale(-speed);
            }
            if (kb.d.pressed()) {
                player.r.force = right.scale(speed);
            }
            if (kb.space.pressed()) {
                player.r.force = .{};
                player.r.vel = .{};
            }

            {
                const T = struct { t: Components.Transform, r: Components.Rigidbody, c: Components.Collider };
                var t_min: ?f32 = null;
                var closest: ?world_mod.Type.pointer(T) = null;
                var it = try app.world.ecs.iterator(T);
                while (it.next()) |e| {
                    if (!e.r.flags.player and !e.r.flags.pinned and mouse.left.pressed()) {
                        if (e.c.raycast(e.t, self.physics.interpolated(player.lt, player.t).pos.add(fwd.scale(1.1)), fwd)) |t| {
                            if (t_min == null) {
                                t_min = t;
                                closest = e;
                            } else if (t_min.? > t) {
                                t_min = t;
                                closest = e;
                            }
                        }
                    }
                }
                if (closest) |e| {
                    e.r.vel = e.r.vel.add(fwd.scale(50));
                }
            }

            if (mouse.right.pressed()) {
                // const bones = try allocator.alloc(math.Mat4x4, app.cpu_resources.models.items[app.handles.model.sphere.index].bones.len);
                // errdefer allocator.free(bones);
                // const indices = try allocator.alloc(Components.AnimatedRender.AnimationIndices, app.cpu_resources.models.items[app.handles.model.sphere.index].bones.len);
                // errdefer allocator.free(indices);
                // @memset(bones, .{});
                // @memset(indices, std.mem.zeroes(Components.AnimatedRender.AnimationIndices));

                const rng = math.Rng.init(self.rng.random()).with(.{ .min = 0.2, .max = 0.4 });
                const t = Components.Transform{ .pos = self.physics.interpolated(player.lt, player.t).pos.add(fwd.scale(3.0)), .scale = Vec4.splat3(rng.next()) };
                _ = try app.world.ecs.insert(.{
                    @as([]const u8, "bullet"),
                    t,
                    Components.LastTransform{ .t = t },
                    Components.Rigidbody{ .flags = .{}, .vel = fwd.scale(50.0), .mass = 1, .dynamic_friction = 1 },
                    Components.Collider{ .sphere = .{ .radius = 1.0 } },
                    // Components.AnimatedRender{ .model = app.handles.model.sphere, .bones = bones, .indices = indices },
                    Components.StaticRender{ .mesh = app.handles.mesh.cube },
                    Components.TimeDespawn{ .despawn_time = self.time + 20 },
                });
            }
        }

        {
            self.physics.acctime += delta;
            self.physics.interpolation_acctime += delta;

            const player = try app.world.ecs.get(app.handles.player, struct { camera: math.Camera });
            {
                var it = try app.world.ecs.iterator(struct { r: Components.Rigidbody });
                while (it.next()) |e| {
                    if (!e.r.flags.pinned) {
                        const g = player.camera.world_basis.up.scale(-e.r.mass * 9.8);
                        e.r.force = e.r.force.add(g);
                    }
                }
            }

            while (self.physics.acctime >= self.physics.step) {
                self.physics.acctime -= self.physics.step;

                if (self.physics.acctime < self.physics.step) {
                    self.physics.interpolation_acctime = self.physics.acctime;
                    var it = try app.world.ecs.iterator(struct { t: Components.Transform, ft: Components.LastTransform });
                    while (it.next()) |e| {
                        e.ft.t = e.t.*;
                    }
                }

                try app.world.step(self.physics.step);
            }

            {
                var it = try app.world.ecs.iterator(struct { r: Components.Rigidbody });
                while (it.next()) |e| {
                    e.r.force = .{};
                }
            }
        }

        {
            var to_remove = std.ArrayList(Entity).init(allocator.*);
            defer to_remove.deinit();

            var it = try app.world.ecs.iterator(struct { id: Entity, ds: Components.TimeDespawn });
            while (it.next()) |e| {
                if (e.ds.despawn_time < self.time) {
                    try to_remove.append(e.id.*);
                }
            }

            for (to_remove.items) |e| {
                try app.world.ecs.remove(e);
            }
        }

        {
            const animate = struct {
                fn animate(ar: *Components.AnimatedRender, model: *mesh_mod.Model, time: f32) bool {
                    const animation = model.animations[0];
                    const bones = model.bones;
                    const indices = ar.indices;

                    for (ar.bones) |*t| {
                        t.* = math.Mat4x4.scaling_mat(Vec4.splat3(1));
                    }

                    var updated = false;
                    for (ar.bones, 0..) |*arb, i| {
                        var out_t = math.Vec4{ .w = 1 };
                        if (get_lerped(&out_t, animation.bones[i].translation_keyframes.items, &indices[i].translation, time)) {
                            updated = true;
                        }
                        var out_r = math.Vec4.quat_identity_rot();
                        if (get_lerped(&out_r, animation.bones[i].rotation_keyframes.items, &indices[i].rotation, time)) {
                            updated = true;
                        }
                        var out_s = math.Vec4.splat4(1);
                        if (get_lerped(&out_s, animation.bones[i].scale_keyframes.items, &indices[i].scale, time)) {
                            updated = true;
                        }

                        arb.* = math.Mat4x4.translation_mat(out_t).mul_mat(math.Mat4x4.rot_mat_from_quat(out_r)).mul_mat(math.Mat4x4.scaling_mat(out_s));
                    }

                    for (bones, 0..) |*b, i| {
                        if (b.parent == null) {
                            apply(ar, b.children, bones, ar.bones[i]);
                        }
                    }

                    return updated;
                }

                fn get_lerped(out: *math.Vec4, keyframes: []mesh_mod.Keyframe, curr: *u32, time: f32) bool {
                    // check if i have to increment keyframe index
                    // calculate dt for this keyframe
                    // lerp
                    // store

                    if (keyframes.len <= curr.*) return false;
                    if (time > keyframes[curr.*].time and keyframes.len > curr.* + 1) curr.* += 1;

                    const curr_v = keyframes[curr.*];

                    if (keyframes.len == curr.* + 1) {
                        out.* = curr_v.value;
                        return false;
                    }
                    const next_v = keyframes[curr.* + 1];

                    const t = (time - curr_v.time) / (next_v.time - curr_v.time);

                    out.* = curr_v.value.mix(next_v.value, std.math.clamp(t, 0, 1));
                    return true;
                }

                fn apply(ar: *Components.AnimatedRender, ids: []const mesh_mod.BoneId, bones: []mesh_mod.Bone, t: math.Mat4x4) void {
                    for (ids) |bone_id| {
                        ar.bones[bone_id] = t.mul_mat(ar.bones[bone_id]);
                        apply(ar, bones[bone_id].children, bones, ar.bones[bone_id]);
                    }
                }
            }.animate;

            var it = try app.world.ecs.iterator(struct { m: Components.AnimatedRender });
            while (it.next()) |e| {
                const model = &app.cpu_resources.models.items[app.handles.model.sphere.index];
                const a: *Components.AnimatedRender = e.m;
                a.time += delta;

                if (!animate(a, model, a.time)) {
                    a.time = 0;
                    @memset(a.indices, std.mem.zeroes(Components.AnimatedRender.AnimationIndices));
                }
            }
        }

        {
            app.instance_manager.reset();
            defer if (app.instance_manager.update()) {
                _ = self.cmdbuf_fuse.fuse();
            };

            {
                var it = try app.world.ecs.iterator(struct { t: Components.Transform, ft: Components.LastTransform, m: Components.StaticRender });
                while (it.next()) |e| {
                    const instance = app.instance_manager.reserve_instance(e.m.mesh);
                    if (instance) |instance_index| {
                        const first_bone = app.instance_manager.reserve_bones(1);

                        if (first_bone) |bone_index| {
                            app.cpu_resources.instances.items[instance_index].bone_offset = bone_index;
                            app.cpu_resources.bones.items[bone_index] = self.physics.interpolated(e.ft, e.t).mat4();
                        } else {
                            app.cpu_resources.instances.items[instance_index].bone_offset = 0;
                        }
                    }
                }
            }

            {
                var it = try app.world.ecs.iterator(struct { t: Components.Transform, ft: Components.LastTransform, m: Components.AnimatedRender });
                while (it.next()) |e| {
                    const instance = app.instance_manager.reserve_instance(e.m.model.mesh);
                    const model = &app.cpu_resources.models.items[e.m.model.index];
                    if (instance) |instance_index| {
                        const first_bone = app.instance_manager.reserve_bones(@intCast(model.bones.len));

                        if (first_bone) |bone_index| {
                            app.cpu_resources.instances.items[instance_index].bone_offset = bone_index;
                            for (0..model.bones.len) |index| {
                                app.cpu_resources.bones.items[bone_index + index] = self.physics.interpolated(e.ft, e.t).mat4().mul_mat(e.m.bones[index]).mul_mat(model.bones[index].inverse_bind_matrix);
                            }
                        } else {
                            app.cpu_resources.instances.items[instance_index].bone_offset = 0;
                        }
                    }
                }
            }
        }

        {
            const player = try app.world.ecs.get(app.handles.player, struct { camera: math.Camera, controller: Components.Controller, lt: Components.LastTransform, transform: Components.Transform });
            app.uniforms.uniform_buffer = try self.uniforms(window, &self.physics.interpolated(player.lt, player.transform), player.camera, player.controller);
        }
    }

    fn uniforms(
        self: *@This(),
        window: *Engine.Window,
        transform: *const Components.Transform,
        camera: *const math.Camera,
        controller: *const Components.Controller,
    ) ![]u8 {
        const rot = camera.rot_quat(controller.pitch, controller.yaw);

        const fwd = rot.rotate_vector(camera.world_basis.fwd);
        const right = rot.rotate_vector(camera.world_basis.right);
        const up = rot.rotate_vector(camera.world_basis.up);
        const eye = transform.pos;

        const uniform = .{
            .camera = ShaderUtils.Camera{
                .eye = eye,
                .fwd = fwd,
                .right = right,
                .up = up,
                .meta = .{
                    .did_move = @intCast(@intFromBool(controller.did_move)),
                    .did_rotate = @intCast(@intFromBool(controller.did_rotate)),
                    .did_change = @intCast(@intFromBool(controller.did_rotate or controller.did_move)),
                },
            },
            .mouse = ShaderUtils.Mouse{
                .x = self.mouse.x,
                .y = self.mouse.y,
                .left = @intCast(@intFromBool(self.mouse.left)),
                .right = @intCast(@intFromBool(self.mouse.right)),
            },
            .world_to_screen = camera.world_to_screen_mat(.{
                .width = window.extent.width,
                .height = window.extent.height,
                .pos = eye,
                .pitch = controller.pitch,
                .yaw = controller.yaw,
            }),
            .frame = self.frame,
            .time = self.time,
            .deltatime = self.deltatime,
            .width = @as(i32, @intCast(window.extent.width)),
            .height = @as(i32, @intCast(window.extent.height)),
            .monitor_width = @as(i32, @intCast(self.monitor_rez.width)),
            .monitor_height = @as(i32, @intCast(self.monitor_rez.height)),
        };
        const ubo = ShaderUtils.create_uniform_object(@TypeOf(uniform), uniform);
        const ubo_buffer = std.mem.asBytes(&ubo);

        if (self.uniform_buffer.len != ubo_buffer.len) {
            allocator.free(self.uniform_buffer);
            self.uniform_buffer = try allocator.alloc(u8, ubo_buffer.len);
        }

        if (!self.uniform_shader_dumped) {
            self.uniform_shader_dumped = true;

            var gen = try ShaderUtils.GlslBindingGenerator.init();
            defer gen.deinit();

            try gen.add_uniform(ubo);
            try gen.add_bind_enum(resources_mod.UniformBinds);
            try gen.add_bind_enum(resources_mod.VertexBinds);
            try gen.add_bind_enum(resources_mod.VertexInputLocations);
            try gen.dump_shader("./src/uniforms.glsl");
        }

        @memcpy(self.uniform_buffer, ubo_buffer);

        return self.uniform_buffer;
    }

    pub fn reset_time(self: *@This()) void {
        self.time = 0;
        self.deltatime = 0;
        self.frame = 0;
    }
};

pub const GuiState = struct {
    frame_times: [10]f32 = std.mem.zeroes([10]f32),
    frame_times_i: usize = 10,

    pub fn tick(self: *@This(), app: *App, state: *AppState, lap: u64) !void {
        const delta = @as(f32, @floatFromInt(lap)) / @as(f32, @floatFromInt(std.time.ns_per_s));

        const player = try app.world.ecs.get(app.handles.player, struct { controller: Components.Controller });

        self.frame_times_i += 1;
        self.frame_times_i = @rem(self.frame_times_i, self.frame_times.len);
        self.frame_times[self.frame_times_i] = delta * std.time.ms_per_s;
        const frametime = std.mem.max(f32, &self.frame_times);

        c.ImGui_SetNextWindowPos(.{ .x = 5, .y = 5 }, c.ImGuiCond_Once);
        defer c.ImGui_End();
        if (c.ImGui_Begin("SIKE", null, c.ImGuiWindowFlags_None)) {
            c.ImGui_Text("Application average %.3f ms/frame (%.1f FPS)", frametime, std.time.ms_per_s / frametime);

            c.ImGui_Text("State");
            self.editState(state, player.controller);
        }
    }

    fn editState(self: *@This(), state: *AppState, controller: *Components.Controller) void {
        _ = self;

        var reset = false;

        _ = c.ImGui_SliderFloat("Speed", &controller.speed, 0.1, 10.0);
        _ = c.ImGui_SliderFloat("Sensitivity", &controller.sensitivity, 0.001, 2.0);
        _ = c.ImGui_SliderInt("FPS cap", @ptrCast(&state.fps_cap), 5, 500);

        reset = reset or c.ImGui_Button("Reset render state");

        if (reset) {
            _ = state.cmdbuf_fuse.fuse();
            state.reset_time();
        }
    }
};
