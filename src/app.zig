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

const resources_mod = @import("resources.zig");
const GpuResourceManager = resources_mod.GpuResourceManager;
const DrawCallReserve = resources_mod.DrawCallReserve;

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
drawcalls: std.ArrayList(DrawCallReserve),
descriptor_pool: DescriptorPool,
camera_descriptor_set: DescriptorSet,
model_descriptor_set: DescriptorSet,
command_pool: vk.CommandPool,
stages: ShaderStageManager,

texture_img: utils.ImageMagick.UnormImage,
texture: Image,

handles: struct {
    mesh: struct {
        sphere: GpuResourceManager.MeshResourceHandle,
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

    var uniforms = try UniformBuffer.new(try app_state.uniforms(engine.window), ctx);
    errdefer uniforms.deinit(device);

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

    var gltf = try mesh_mod.Gltf.parse_glb("./assets/well.glb");
    defer gltf.deinit();
    var object = try gltf.to_mesh();
    defer object.deinit();

    // var object = try mesh.ObjParser.mesh_from_file("./assets/object.obj");
    // defer object.deinit();

    // var cube = try mesh.Mesh.cube();
    // defer cube.deinit();

    var plane = try mesh_mod.Mesh.plane();
    defer plane.deinit();

    var sphere_gltf = try mesh_mod.Gltf.parse_glb("./assets/sphere.glb");
    defer sphere_gltf.deinit();
    var sphere = try sphere_gltf.to_mesh();
    defer sphere.deinit();

    var cpu = GpuResourceManager.CpuResources.init();
    errdefer cpu.deinit();

    var world = World.init();
    errdefer world.deinit();
    try world.entities.append(.{
        .name = "player",
        .typ = .{ .player = true },
        .transform = .{ .pos = .{} },
        .rigidbody = .{
            .flags = .{},
            .mass = 100,
        },
        .collider = .{ .sphere = .{ .radius = 2 } },
        .mesh = null,
    });

    var drawcalls = std.ArrayList(DrawCallReserve).init(allocator);
    errdefer drawcalls.deinit();

    const plane_mesh_handle = try cpu.add_mesh(&plane);
    const plane_instance_handle = try cpu.batch_reserve(1);
    try drawcalls.append(.{ .mesh = plane_mesh_handle, .instances = plane_instance_handle });
    try world.entities.append(.{
        .name = "floor",
        .typ = .{},
        .rigidbody = .{
            .flags = .{ .pinned = true },
        },
        // .transform = .{ .pos = .{ .y = -3 }, .scale = Vec4.splat3(100) },
        // .collider = .{ .plane = .{ .normal = .{ .y = 1 } } },
        // .mesh = plane_mesh_handle,
        .transform = .{ .pos = .{ .y = -3 } },
        .collider = .{ .sphere = .{ .radius = -10 } },
        .mesh = null,
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

    const sphere_mesh_handle = try cpu.add_mesh(&sphere);
    const sphere_instance_handle = try cpu.batch_reserve(5000);
    try drawcalls.append(.{ .mesh = sphere_mesh_handle, .instances = sphere_instance_handle });
    for (sphere_instance_handle.first..(sphere_instance_handle.first + sphere_instance_handle.count), 0..) |_, i| {
        if (i > 2) break;
        try world.entities.append(.{
            .name = "persistent balls",
            .typ = .{ .cube = true },
            .transform = .{ .pos = .{ .x = @floatFromInt(i * 3), .y = 5 } },
            .mesh = sphere_mesh_handle,
        });
    }

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
    var model_desc_set = try model_desc_set_builder.build(device);
    errdefer model_desc_set.deinit(device);

    const stages = try ShaderStageManager.init();
    errdefer stages.deinit();

    return .{
        .world = world,
        .uniforms = uniforms,
        .model_uniforms = model_uniform,
        .screen_image = screen,
        .depth_image = depth,
        .gpu_resources = gpu,
        .cpu_resources = cpu,
        .drawcalls = drawcalls,
        .descriptor_pool = desc_pool,
        .camera_descriptor_set = camera_desc_set,
        .model_descriptor_set = model_desc_set,
        .command_pool = cmd_pool,
        .stages = stages,

        .texture_img = image,
        .texture = gpu_img,

        .handles = .{
            .mesh = .{
                .sphere = sphere_mesh_handle,
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
    defer self.drawcalls.deinit();
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
        for (app.drawcalls.items) |call| {
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
    camera: math.Camera,
    camera_meta: ShaderUtils.Camera.CameraMeta = .{},

    frame: u32 = 0,
    time: f32 = 0,
    deltatime: f32 = 0,
    fps_cap: u32 = 60,

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
            .camera = math.Camera.init(
                Vec4{ .z = -4 },
                math.Camera.constants.basis.vulkan,
                math.Camera.constants.basis.opengl,
            ),
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

        if (c.ImGui_GetIO().*.WantCaptureMouse) {
            mouse = std.mem.zeroes(@TypeOf(mouse));
            mouse.x = input.mouse.x;
            mouse.y = input.mouse.y;
        }
        if (c.ImGui_GetIO().*.WantCaptureKeyboard) {
            kb = std.mem.zeroes(@TypeOf(kb));
        }

        if (mouse.left.just_pressed() and !self.focus) {
            self.focus = true;
            window.hide_cursor(true);
        }
        if (kb.escape.just_pressed() and self.focus) {
            self.focus = false;
            window.hide_cursor(false);
        }
        if (kb.q.just_pressed()) {
            window.queue_close();
        }

        var dx: i32 = 0;
        var dy: i32 = 0;
        if (self.focus) {
            dx = @intFromFloat(mouse.dx);
            dy = @intFromFloat(mouse.dy);
        }
        self.camera_meta.did_move = @intCast(@intFromBool(kb.w.pressed() or kb.a.pressed() or kb.s.pressed() or kb.d.pressed()));
        self.camera_meta.did_rotate = @intCast(@intFromBool((dx | dy) != 0));
        self.camera_meta.did_change = @intCast(@intFromBool((self.camera_meta.did_move | self.camera_meta.did_rotate) > 0));

        self.mouse.left = mouse.left.pressed();
        self.mouse.x = @intFromFloat(mouse.x);
        self.mouse.y = @intFromFloat(mouse.y);

        self.frame += 1;
        self.time += delta;
        self.deltatime = delta;

        if (kb.p.just_pressed()) {
            try render_utils.dump_image_to_file(
                &app.screen_image,
                &engine.graphics,
                app.command_pool,
                window.extent,
                "./images",
            );
        }

        // rotation should not be multiplied by deltatime. if mouse moves by 3cm, it should always rotate the same amount.
        self.camera.yaw += @as(f32, @floatFromInt(dx)) * self.camera.sensitivity_scale * self.camera.sensitivity;
        self.camera.pitch += @as(f32, @floatFromInt(dy)) * self.camera.sensitivity_scale * self.camera.sensitivity;
        self.camera.pitch = std.math.clamp(self.camera.pitch, math.Camera.constants.pitch_min, math.Camera.constants.pitch_max);

        const rot = self.camera.rot_quat();
        const fwd = rot.rotate_vector(self.camera.world_basis.fwd);
        const right = rot.rotate_vector(self.camera.world_basis.right);

        var t_min: ?f32 = null;
        var closest: ?*world_mod.Entity = null;
        for (app.world.entities.items) |*e| {
            if (e.typ.player) {
                var speed = self.camera.speed;
                if (kb.shift.pressed()) {
                    speed *= 2.0;
                }
                if (kb.ctrl.pressed()) {
                    speed *= 0.1;
                }

                speed *= 10 * self.camera.speed;
                speed = std.math.clamp(speed, -10, 10);
                speed *= e.rigidbody.mass;

                if (kb.w.pressed()) {
                    e.rigidbody.force = fwd.scale(speed);
                }
                if (kb.a.pressed()) {
                    e.rigidbody.force = right.scale(-speed);
                }
                if (kb.s.pressed()) {
                    e.rigidbody.force = fwd.scale(-speed);
                }
                if (kb.d.pressed()) {
                    e.rigidbody.force = right.scale(speed);
                }

                e.transform.pos = self.camera.pos;
                e.transform.rotation = rot;
            }
            if (e.typ.cube and mouse.left.pressed()) {
                if (e.collider.raycast(&e.transform, self.camera.pos, fwd)) |t| {
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
            e.rigidbody.vel = e.rigidbody.vel.add(fwd.scale(50));
        }

        try app.world.tick(self, delta);

        if (mouse.right.pressed()) {
            const dirs = self.camera.dirs();
            try app.world.entities.append(.{
                .name = "bullet",
                .typ = .{ .cube = true },
                .transform = .{ .pos = self.camera.pos.add(dirs.fwd.scale(3.0)), .scale = Vec4.splat3(0.2) },
                .rigidbody = .{ .flags = .{}, .vel = dirs.fwd.scale(50.0), .mass = 1 },
                .collider = .{ .sphere = .{ .radius = 1.0 } },
                .mesh = app.handles.mesh.sphere,
                .despawn_time = self.time + 5,
            });
            _ = self.cmdbuf_fuse.fuse();
        }

        for (app.drawcalls.items) |*dc| {
            dc.reset();
        }

        var i: usize = 0;
        while (app.world.entities.items.len > i) : (i += 1) {
            const e = &app.world.entities.items[i];

            if (e.typ.player) {
                self.camera.pos = e.transform.pos;
            }

            if (e.despawn_time) |t| {
                if (t < self.time) {
                    _ = app.world.entities.swapRemove(i);
                    i -= 1;
                    _ = self.cmdbuf_fuse.fuse();
                    continue;
                }
            }

            const instance = blk: {
                const mesh = e.mesh orelse break :blk 0;
                var count: u32 = 0;
                for (app.drawcalls.items) |*dc| {
                    count += dc.maybe_reserve(mesh);
                }
                break :blk count;
            };
            app.cpu_resources.instances.items[instance].transform = e.transform.mat4();
        }
    }

    pub fn uniforms(self: *@This(), window: *Engine.Window) ![]u8 {
        const rot = self.camera.rot_quat();

        const fwd = rot.rotate_vector(self.camera.world_basis.fwd);
        const right = rot.rotate_vector(self.camera.world_basis.right);
        const up = rot.rotate_vector(self.camera.world_basis.up);
        const eye = self.camera.pos;

        const uniform = .{
            .camera = ShaderUtils.Camera{
                .eye = eye,
                .fwd = fwd,
                .right = right,
                .up = up,
                .meta = self.camera_meta,
            },
            .mouse = ShaderUtils.Mouse{
                .x = self.mouse.x,
                .y = self.mouse.y,
                .left = @intCast(@intFromBool(self.mouse.left)),
                .right = @intCast(@intFromBool(self.mouse.right)),
            },
            .world_to_screen = self.camera.world_to_screen_mat(.{ .width = window.extent.width, .height = window.extent.height }),
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

    pub fn tick(self: *@This(), state: *AppState, lap: u64) void {
        const delta = @as(f32, @floatFromInt(lap)) / @as(f32, @floatFromInt(std.time.ns_per_s));

        self.frame_times_i += 1;
        self.frame_times_i = @rem(self.frame_times_i, self.frame_times.len);
        self.frame_times[self.frame_times_i] = delta * std.time.ms_per_s;
        const frametime = std.mem.max(f32, &self.frame_times);

        c.ImGui_SetNextWindowPos(.{ .x = 5, .y = 5 }, c.ImGuiCond_Once);
        defer c.ImGui_End();
        if (c.ImGui_Begin("SIKE", null, c.ImGuiWindowFlags_None)) {
            c.ImGui_Text("Application average %.3f ms/frame (%.1f FPS)", frametime, std.time.ms_per_s / frametime);

            c.ImGui_Text("State");
            self.editState(state);
        }
    }

    fn editState(self: *@This(), state: *AppState) void {
        _ = self;

        var reset = false;

        _ = c.ImGui_SliderFloat("Speed", &state.camera.speed, 0.1, 10.0);
        _ = c.ImGui_SliderFloat("Sensitivity", &state.camera.sensitivity, 0.001, 2.0);
        _ = c.ImGui_SliderInt("FPS cap", @ptrCast(&state.fps_cap), 5, 500);

        reset = reset or c.ImGui_Button("Reset render state");

        if (reset) {
            _ = state.cmdbuf_fuse.fuse();
            state.reset_time();
        }
    }
};
