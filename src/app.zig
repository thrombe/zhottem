const std = @import("std");

const vk = @import("vulkan");

const utils = @import("utils.zig");
const Fuse = utils.Fuse;
const ShaderUtils = utils.ShaderUtils;

const math = @import("math.zig");
const Vec4 = math.Vec4;
const Vec3 = math.Vec3;

const assets_mod = @import("assets.zig");
const loader_mod = @import("loader.zig");

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

const ecs_mod = @import("ecs.zig");
const Entity = ecs_mod.Entity;

const world_mod = @import("world.zig");
const jolt = world_mod.Jphysics.jolt;
const World = world_mod.World;
const Components = world_mod.Components;
const C = world_mod.C;

const resources_mod = @import("resources.zig");
const ResourceManager = resources_mod.ResourceManager;
const InstanceManager = resources_mod.InstanceManager;
const InstanceAllocator = resources_mod.InstanceAllocator;

const steamworks_mod = @import("steamworks.zig");
// const NetworkingContext = steamworks_mod.NetworkingContext;

const network_mod = @import("network.zig");
const NetworkingContext = network_mod.NetworkingContext;

const main = @import("main.zig");
const allocator = main.allocator;

pub const App = @This();

world: World,

uniforms: UniformBuffer,
screen_image: Image,
depth_image: Image,
resources: ResourceManager,
descriptor_pool: DescriptorPool,
command_pool: vk.CommandPool,
recorder: AudioRecorder,
audio: AudioPlayer,

net_ctx: NetworkingContext,
net_server: ?*NetworkingContext.Server = null,
net_client: *NetworkingContext.Client,

texture: Image,

handles: Handles,
entities: Entities,

const Entities = struct {
    player: Entity,
};

const Handles = struct {
    material: struct {
        background: ResourceManager.MaterialHandle,
        models: ResourceManager.MaterialHandle,
    },
    gltf: struct {
        library: ResourceManager.GltfHandle,
    },
    mesh: struct {
        cube: ResourceManager.MeshHandle,
        plane: ResourceManager.MeshHandle,
    },
    image: struct {
        mandlebulb: ResourceManager.ImageHandle,
    },
    audio: AudioHandles,

    const AudioHandles = struct {
        boom: ResourceManager.AudioHandle,
        bruh: ResourceManager.AudioHandle,
        long_reload: ResourceManager.AudioHandle,
        short_reload: ResourceManager.AudioHandle,
        scream1: ResourceManager.AudioHandle,
        scream2: ResourceManager.AudioHandle,
        shot: ResourceManager.AudioHandle,

        fn init(cpu: *ResourceManager.Assets) !@This() {
            return .{
                .boom = try cpu.add(try assets_mod.Wav.parse_wav("assets/audio/boom.wav")),
                .bruh = try cpu.add(try assets_mod.Wav.parse_wav("assets/audio/bruh.wav")),
                .long_reload = try cpu.add(try assets_mod.Wav.parse_wav("assets/audio/long-reload.wav")),
                .short_reload = try cpu.add(try assets_mod.Wav.parse_wav("assets/audio/short-reload.wav")),
                .scream1 = try cpu.add(try assets_mod.Wav.parse_wav("assets/audio/scream1.wav")),
                .scream2 = try cpu.add(try assets_mod.Wav.parse_wav("assets/audio/scream2.wav")),
                .shot = try cpu.add(try assets_mod.Wav.parse_wav("assets/audio/shot.wav")),
            };
        }
    };

    fn init(cpu: *ResourceManager.Assets) !@This() {
        var library = try assets_mod.Gltf.parse_glb("assets/exports/library.glb");
        errdefer library.deinit();

        var image = try utils.StbImage.from_file(.unorm, "assets/images/mandlebulb.png");
        errdefer image.deinit();

        var cube = try assets_mod.Mesh.cube(allocator.*);
        errdefer cube.deinit();

        var plane = try assets_mod.Mesh.plane(allocator.*);
        errdefer plane.deinit();

        const Material = ResourceManager.Assets.Material;

        return .{
            .audio = try .init(cpu),
            .material = .{
                .background = try cpu.add(Material{
                    .name = "BG",
                    .vert = "bg_vert",
                    .frag = "bg_frag",
                    .src = "src/shader.glsl",
                }),
                .models = try cpu.add(Material{
                    .name = "MODEL",
                    .vert = "vert",
                    .frag = "frag",
                    .src = "src/shader.glsl",
                }),
            },
            .image = .{
                .mandlebulb = try cpu.add(image),
            },
            .gltf = .{
                .library = try cpu.add(library),
            },
            .mesh = .{
                .cube = try cpu.add(cube),
                .plane = try cpu.add(plane),
            },
        };
    }
};

const AudioPlayer = Engine.Audio.Stream(.output, struct {
    // owned by ResourceManager.CpuResources
    samples: []assets_mod.Wav,
    frame_count: u64 = 0,

    playing: struct {
        // only swap in callback.
        // self.samples accessed only in callback.
        // other thread can fill only when not fused.
        // callback can only swap when fused

        swap_fuse: Fuse = .{},

        // NOTE: only audio thread may lock
        lock: Fuse = .{},

        samples: Samples,
        samples_buf2: Samples,

        fn fused_swap(self: *@This()) void {
            _ = self.lock.fuse();
            defer _ = self.lock.unfuse();

            if (self.swap_fuse.unfuse()) {
                std.mem.swap(Samples, &self.samples, &self.samples_buf2);
            }
        }
    },

    pub fn init(samples: []assets_mod.Wav) !@This() {
        return @This(){
            .samples = samples,
            .playing = .{
                .samples = try Samples.initCapacity(allocator.*, 200),
                .samples_buf2 = try Samples.initCapacity(allocator.*, 200),
            },
        };
    }

    pub fn deinit(self: *@This()) void {
        _ = self.playing.lock.fuse();
        defer _ = self.playing.lock.unfuse();

        self.playing.samples.deinit();
        self.playing.samples_buf2.deinit();
    }

    pub fn callback(
        self: *AudioPlayer.CallbackContext,
        output: [][2]f32,
        timeinfo: *c.PaStreamCallbackTimeInfo,
        flags: c.PaStreamCallbackFlags,
    ) !void {
        _ = flags;
        _ = timeinfo;

        defer self.ctx.frame_count += output.len;

        @memset(output, [2]f32{ 0, 0 });

        self.ctx.playing.fused_swap();
        for (self.ctx.playing.samples.items) |*ps| {
            _ = ps.fill(self.ctx.frame_count, self.ctx.samples, output);
        }
    }

    pub const Samples = std.ArrayList(PlayingSample);
    pub const PlayingSample = struct {
        handle: ResourceManager.AudioHandle,
        start_frame: u64,

        // relative position of the sound. (listener +z fwd, +x right, -y up)
        pos: Vec3,
        volume: f32,

        // the float values supplied to audio apis is the instantaneous amplitude of the wave
        // power is proportional to the square of amplitude
        // intensity is power carried by wave per unit area (perp to the area)
        // percieved loudness is logarithmic in power
        // delta dB = 10*log10(p2/p1)
        // "twice as loud" is a diff of 10dB
        //
        // with distance - sound's intensity decreases (as area increases)
        // so twice as far means a quarter the intensity
        // which means quarter the power (per unit area (which is what we end up hearing ig. cuz the ear drums are constant in size))
        // which means we just divide the amplitude by 2 to account for the distance

        pub fn fill(self: *@This(), frame_count: u64, samples: []assets_mod.Wav, output: [][2]f32) bool {
            const min = 1.0;
            const max = 100.0;
            const original_dist = self.pos.length();
            const dist = @min(max, @max(min, original_dist));

            const att = self.volume / dist;

            const right_dot = 0.5 * if (original_dist > 0.01) self.pos.x / original_dist else 0.0;
            var left = 0.5 - right_dot;
            var right = 0.5 + right_dot;

            // rescale in [0, 1]
            left *= 2;
            right *= 2;

            // 20% audio always leaks into the other ear
            left = left * 0.8 + 0.2;
            right = right * 0.8 + 0.2;

            // if close, we leak more
            left = @min(1.0, left + 1.0 / @max(0.01, original_dist));
            right = @min(1.0, right + 1.0 / @max(0.01, original_dist));

            const sample = samples[self.handle.index].data;
            var index = frame_count - self.start_frame;
            for (output) |*oframe| {
                defer index += 1;

                if (sample.len <= index) {
                    return true;
                }

                const frame = sample[index];
                oframe[0] = std.math.clamp(oframe[0] + frame[0] * left * att, -1, 1);
                oframe[1] = std.math.clamp(oframe[1] + frame[1] * right * att, -1, 1);
            }

            return false;
        }
    };
});

const AudioRecorder = Engine.Audio.Stream(.input, struct {
    recorded: utils.Channel([256]f32),

    pub fn callback(self: *AudioRecorder.CallbackContext, input: []const f32, timeinfo: *c.PaStreamCallbackTimeInfo, flags: c.PaStreamCallbackFlags) !void {
        _ = flags;
        _ = timeinfo;
        _ = self;

        var buf: [256]f32 = undefined;
        @memcpy(&buf, input);
        // try self.ctx.recorded.send(buf);
    }

    pub fn deinit(self: *@This()) void {
        self.recorded.deinit();
    }
});

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

    var world = try World.init(math.Camera.constants.basis.opengl.up);
    errdefer world.deinit();
    try loader_mod.generate_type_registry();

    // start net context early
    var net_ctx = try NetworkingContext.init(.{});
    errdefer net_ctx.deinit();
    const net_server: ?*NetworkingContext.Server = net_ctx.server() catch |e| blk: {
        utils.dump_error(e);
        break :blk null;
    };
    errdefer if (net_server) |s| s.deinit();
    const net_client = try net_ctx.client();
    errdefer net_client.deinit();

    var assets = ResourceManager.Assets.init();
    errdefer assets.deinit();

    const handles = try Handles.init(&assets);

    const mandlebulb = assets.ref(handles.image.mandlebulb);
    var gpu_img = try Image.new_from_slice(ctx, cmd_pool, .{
        .extent = .{
            .width = @intCast(mandlebulb.width),
            .height = @intCast(mandlebulb.height),
        },
        .bind_desc_type = .combined_image_sampler,
        .layout = .shader_read_only_optimal,
        .usage = .{
            .sampled_bit = true,
        },
    }, std.mem.bytesAsSlice([4]u8, std.mem.sliceAsBytes(mandlebulb.buffer)));
    errdefer gpu_img.deinit(device);

    var cmdbuf = world.ecs.deferred();
    defer cmdbuf.deinit();

    var t: C.GlobalTransform = .{ .transform = .{
        .pos = .{ .y = 5 },
    } };
    const player_id = try cmdbuf.insert(.{
        try C.Name.from("player"),
        math.Camera.init(
            math.Camera.constants.basis.vulkan,
            math.Camera.constants.basis.opengl,
        ),
        C.Controller{},
        t,
        C.Shooter{
            .audio = handles.audio.shot,
            .ticker = try utils.Ticker.init(std.time.ns_per_ms * 100),
            .hold = true,
        },
        C.PlayerId{ .id = 0, .conn = 0 },
        try world.phy.add_character(.{
            .pos = t.transform.pos,
            .rot = t.transform.rotation,
        }),
    });

    try loader_mod.spawn_node(
        &world,
        &assets,
        &cmdbuf,
        handles.gltf.library,
        "person_ig",
        .{ .pos = .{ .z = 5, .y = 1 }, .scale = .splat(4) },
        handles.material.models,
    );
    try loader_mod.spawn_node(
        &world,
        &assets,
        &cmdbuf,
        handles.gltf.library,
        "background_quad",
        .{},
        handles.material.background,
    );

    t.transform = .{
        .pos = .{ .y = -5.5 },
        .scale = .{ .x = 50, .y = 0.1, .z = 50 },
    };
    _ = try cmdbuf.insert(.{
        try C.Name.from("floor"),
        t,
        C.StaticMesh{ .mesh = handles.mesh.plane, .material = handles.material.models },
        C.BatchedRender{},
        try world.phy.add_body(.{
            .shape = .{ .box = .{ .size = t.transform.scale } },
            .motion_type = .static,
            .friction = 0.4,
            .rotation = t.transform.rotation,
        }),
    });

    try cmdbuf.apply(@ptrCast(&world));

    const player = try world.ecs.get(player_id, struct { pid: C.PlayerId, t: C.GlobalTransform, camera: math.Camera, controller: C.Controller });
    var uniforms = try UniformBuffer.new(try app_state.uniforms(engine.window, &player.t.transform, player.camera, player.controller), ctx);
    errdefer uniforms.deinit(device);

    var resources = try ResourceManager.init(assets, engine, cmd_pool);
    errdefer resources.deinit(device);

    var desc_pool = try DescriptorPool.new(device);
    errdefer desc_pool.deinit(device);

    var recorder = try AudioRecorder.init(.{
        .recorded = try utils.Channel([256]f32).init(allocator.*),
    }, .{});
    errdefer recorder.deinit() catch |e| utils.dump_error(e);
    try recorder.start();

    // var audio = try AudioPlayer.init(.{
    //     .recorded = &recorder.ctx.ctx.recorded,
    // }, .{});
    // errdefer audio.deinit() catch |e| utils.dump_error(e);
    // try audio.start();

    var audio = try AudioPlayer.init(try AudioPlayer.Ctx.init(assets.audio.items), .{});
    errdefer audio.deinit() catch |e| utils.dump_error(e);
    try audio.start();

    std.debug.print("waiting to connect to server...\n", .{});
    try net_client.wait_for_connection();
    std.debug.print("connected to server...\n", .{});
    try net_client.send_message(.{
        .flags = .{ .reliable = true },
        .event = .{ .join = {} },
    });
    while (!net_client.messages.can_recv()) {
        if (net_server) |s| if (s.messages.try_recv()) |msg| switch (msg.event) {
            .join => try s.send_message(msg.conn, msg.conn, .{ .event = .{ .setid = .{ .id = 0 } } }),
            else => @panic("unexpected message"),
        };
        try net_ctx.tick();
        std.Thread.sleep(std.time.ns_per_ms * net_ctx.ctx.options.tick_fps_inv);
    }
    std.debug.print("waiting for server message...\n", .{});

    const msg = net_client.messages.try_recv().?;
    switch (msg.event) {
        .setid => |e| {
            player.pid.id = e.id;
            player.pid.conn = msg.conn;

            try net_client.send_message(.{ .event = .{ .spawn_player = .{ .id = e.id, .pos = .{
                .x = player.t.transform.pos.x,
                .y = player.t.transform.pos.y,
                .z = player.t.transform.pos.z,
            } } } });
        },
        else => @panic("unexpected message"),
    }
    std.debug.print("starting game...\n", .{});

    return @This(){
        .world = world,
        .uniforms = uniforms,
        .screen_image = screen,
        .depth_image = depth,
        .resources = resources,
        .descriptor_pool = desc_pool,
        .command_pool = cmd_pool,
        .recorder = recorder,
        .audio = audio,

        .net_ctx = net_ctx,
        .net_server = net_server,
        .net_client = net_client,

        .texture = gpu_img,

        .handles = handles,
        .entities = .{
            .player = player_id,
        },
    };
}

pub fn deinit(self: *@This(), device: *Device) void {
    defer device.destroyCommandPool(self.command_pool, null);
    defer self.world.deinit();
    defer self.uniforms.deinit(device);
    defer self.screen_image.deinit(device);
    defer self.depth_image.deinit(device);
    defer self.resources.deinit(device);
    defer self.descriptor_pool.deinit(device);
    defer self.recorder.deinit() catch |e| utils.dump_error(e);
    defer self.audio.deinit() catch |e| utils.dump_error(e);
    defer self.texture.deinit(device);

    defer self.net_ctx.deinit();
    defer if (self.net_server) |s| s.deinit();
    defer self.net_client.deinit();
}

pub fn pre_reload(self: *@This()) !void {
    self.world.phy.pre_reload();
    try self.recorder.pre_reload();
    try self.audio.pre_reload();
    try self.net_ctx.pre_reload();
}

pub fn post_reload(self: *@This()) !void {
    self.world.phy.post_reload();
    try self.recorder.post_reload();
    try self.audio.post_reload();
    try self.net_ctx.post_reload();
}

pub fn present(
    self: *@This(),
    dynamic_state: *RendererState,
    app_state: *AppState,
    gui_renderer: *GuiEngine.GuiRenderer,
    engine: *Engine,
) !Swapchain.PresentState {
    const ctx = &engine.graphics;

    if (engine.window.resize_fuse.unfuse()) {
        _ = app_state.resize_fuse.fuse();
    }

    // TODO: might be useful to create some kinda double buffered setup for
    //  cmdbuffers so that i can queue them before .queueWaitIdle()
    // multiple framebuffers => multiple descriptor sets => different buffers
    // big buffers that depends on the last frame's big buffer + multiple framebuffers => me sad
    // so just wait for one frame's queue to be empty before trying to render another frame
    try ctx.device.queueWaitIdle(ctx.graphics_queue.handle);

    try self.uniforms.upload(&ctx.device);
    const updates = try self.resources.instances.update(ctx, self.command_pool);
    if (updates.buffer_invalid) {
        _ = app_state.shader_fuse.fuse();
    }
    if (updates.cmdbuf_invalid) {
        _ = app_state.cmdbuf_fuse.fuse();
    }

    if (dynamic_state.stages.update()) {
        _ = app_state.shader_fuse.fuse();
    }

    if (app_state.shader_fuse.unfuse()) {
        try dynamic_state.recreate_pipelines(engine, self, app_state);
    }

    if (app_state.cmdbuf_fuse.unfuse()) {
        try dynamic_state.recreate_cmdbuf(engine, self);
    }

    const current_si = try dynamic_state.swapchain.present_start(ctx);
    const present_state = dynamic_state.swapchain.present_end(
        &[_]vk.CommandBuffer{
            dynamic_state.cmdbuffer.bufs[dynamic_state.swapchain.image_index],
            gui_renderer.cmd_bufs[dynamic_state.swapchain.image_index],
        },
        ctx,
        current_si,
    ) catch |err| switch (err) {
        error.OutOfDateKHR => blk: {
            _ = app_state.resize_fuse.fuse();
            break :blk .suboptimal;
        },
        else => |narrow| return narrow,
    };

    // this has to happen before the next app/gui tick
    if (app_state.resize_fuse.unfuse()) {
        // this is not good :/
        // we have to wait for queue to be idle before creating swapchain again
        try ctx.device.queueWaitIdle(ctx.graphics_queue.handle);

        try dynamic_state.recreate_swapchain(engine, app_state);

        gui_renderer.deinit(&engine.graphics.device);
        gui_renderer.* = try GuiEngine.GuiRenderer.init(engine, &dynamic_state.swapchain);
    }

    return present_state;
}

pub const RendererState = struct {
    swapchain: Swapchain,
    cmdbuffer: CmdBuffer,
    descriptor_set: DescriptorSet,

    stages: ShaderStageManager,
    pipelines: Pipelines,

    // not owned
    pool: vk.CommandPool,

    const Pipelines = std.AutoArrayHashMap(ResourceManager.MaterialHandle, GraphicsPipeline);

    pub fn init(app: *App, engine: *Engine, app_state: *AppState) !@This() {
        _ = app_state;
        const ctx = &engine.graphics;
        const device = &ctx.device;

        var arena = std.heap.ArenaAllocator.init(allocator.*);
        defer arena.deinit();
        const alloc = arena.allocator();

        var shader_stages = std.ArrayList(utils.ShaderCompiler.ShaderInfo).init(alloc);
        for (app.resources.assets.materials.items) |material| {
            const frag = try std.fmt.allocPrint(alloc, "{s}_FRAG_PASS", .{material.name});
            const vert = try std.fmt.allocPrint(alloc, "{s}_VERT_PASS", .{material.name});
            try shader_stages.append(.{
                .name = material.frag,
                .stage = .fragment,
                .path = material.src,
                .include = try alloc.dupe([]const u8, &[_][]const u8{"src"}),
                .define = try alloc.dupe([]const u8, &[_][]const u8{frag}),
            });
            try shader_stages.append(.{
                .name = material.vert,
                .stage = .vertex,
                .path = material.src,
                .include = try alloc.dupe([]const u8, &[_][]const u8{"src"}),
                .define = try alloc.dupe([]const u8, &[_][]const u8{vert}),
            });
        }

        var stages = try ShaderStageManager.init(shader_stages.items);
        errdefer stages.deinit();

        var swapchain = try Swapchain.init(ctx, engine.window.extent, .{});
        errdefer swapchain.deinit(device);

        var pipelines = Pipelines.init(allocator.*);
        errdefer pipelines.deinit();

        for (app.resources.assets.materials.items, 0..) |_, i| {
            const handle = ResourceManager.MaterialHandle{ .index = @intCast(i) };
            try pipelines.put(handle, undefined);
        }

        var self: @This() = .{
            .stages = stages,
            .pipelines = pipelines,
            .descriptor_set = undefined,
            .swapchain = swapchain,
            .pool = app.command_pool,
            .cmdbuffer = undefined,
        };

        try self.create_pipelines(engine, app, false);
        errdefer self.descriptor_set.deinit(device);
        errdefer {
            var it = self.pipelines.iterator();
            while (it.next()) |p| {
                p.value_ptr.deinit(device);
            }
        }

        self.cmdbuffer = try self.create_cmdbuf(engine, app);
        errdefer self.cmdbuffer.deinit(device);

        return self;
    }

    pub fn recreate_pipelines(self: *@This(), engine: *Engine, app: *App, app_state: *AppState) !void {
        try self.create_pipelines(engine, app, true);
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

    fn create_pipelines(self: *@This(), engine: *Engine, app: *App, initialized: bool) !void {
        const ctx = &engine.graphics;
        const device = &ctx.device;

        var desc_set_builder = app.descriptor_pool.set_builder();
        defer desc_set_builder.deinit();
        try desc_set_builder.add(&app.uniforms, resources_mod.UniformBinds.camera.bind());
        try desc_set_builder.add(&app.texture, resources_mod.UniformBinds.texture.bind());
        try app.resources.add_binds(&desc_set_builder);
        var desc_set = try desc_set_builder.build(device);
        errdefer desc_set.deinit(device);

        var it = self.pipelines.iterator();
        while (it.next()) |pipeline| {
            if (initialized) pipeline.value_ptr.deinit(device);

            const material = app.resources.assets.ref(pipeline.key_ptr.*);
            pipeline.value_ptr.* = try GraphicsPipeline.new(device, .{
                .vert = self.stages.shaders.map.get(material.vert).?.code,
                .frag = self.stages.shaders.map.get(material.frag).?.code,
                .dynamic_info = .{
                    .image_format = app.screen_image.format,
                    .depth_format = app.depth_image.format,
                },
                .desc_set_layouts = &.{
                    desc_set.layout,
                },
                .push_constant_ranges = &[_]vk.PushConstantRange{.{
                    .stage_flags = .{
                        .vertex_bit = true,
                        .fragment_bit = true,
                    },
                    .offset = 0,
                    .size = @sizeOf(resources_mod.PushConstants),
                }},
            });
        }

        if (initialized) {
            self.descriptor_set.deinit(device);
        }
        self.descriptor_set = desc_set;
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

        app.resources.instances.draw(
            device,
            &cmdbuf,
            &[_]vk.DescriptorSet{
                self.descriptor_set.set,
            },
            &self.pipelines,
        );

        cmdbuf.dynamic_render_end(device);
        cmdbuf.draw_into_swapchain(device, .{
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

        defer self.descriptor_set.deinit(device);

        defer self.stages.deinit();
        defer {
            var it = self.pipelines.iterator();
            while (it.next()) |p| {
                p.value_ptr.deinit(device);
            }
            self.pipelines.deinit();
        }
    }
};

const ShaderStageManager = struct {
    shaders: utils.ShaderCompiler.Stages,
    compiler: utils.ShaderCompiler.Compiler,

    pub fn init(stages: []const utils.ShaderCompiler.ShaderInfo) !@This() {
        var comp = try utils.ShaderCompiler.Compiler.init(.{ .opt = .fast, .env = .vulkan1_3 }, stages);
        errdefer comp.deinit();

        return .{
            .shaders = try utils.ShaderCompiler.Stages.init(&comp, stages),
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
    ts: u64,
    time: f32 = 0,
    deltatime: f32 = 0,
    fps_cap: u32 = 60,

    physics: struct {
        step: f32 = 1.0 / 60.0,
        acctime: f32 = 0,
        interpolation_acctime: f32 = 0,

        fn interpolated(self: *const @This(), lt: *const C.LastTransform, t: *const C.GlobalTransform) C.Transform {
            return lt.transform.lerp(&t.transform, self.interpolation_acctime / self.step);
        }
    } = .{},

    rng: std.Random.Xoshiro256,
    resize_fuse: Fuse = .{},
    cmdbuf_fuse: Fuse = .{},
    shader_fuse: Fuse = .{},
    uniform_buffer: []u8,
    uniform_shader_dumped: bool = false,
    focus: bool = false,

    cmdbuf: ecs_mod.EntityComponentStore.CmdBuf,

    client_count: u8 = 0,

    pub fn init(window: *Engine.Window, start_ts: u64, app: *App) !@This() {
        const mouse = window.poll_mouse();
        const sze = try window.get_res();

        const rng = std.Random.DefaultPrng.init(@intCast(std.time.timestamp()));

        return .{
            .monitor_rez = .{ .width = sze.width, .height = sze.height },
            .mouse = .{ .x = mouse.x, .y = mouse.y, .left = mouse.left },
            .rng = rng,
            .uniform_buffer = try allocator.alloc(u8, 0),
            .ts = start_ts,
            .cmdbuf = app.world.ecs.deferred(),
        };
    }

    pub fn deinit(self: *@This()) void {
        allocator.free(self.uniform_buffer);
        self.cmdbuf.deinit();
    }

    pub fn pre_reload(self: *@This()) !void {
        _ = self;
    }

    pub fn post_reload(self: *@This()) !void {
        _ = self.resize_fuse.fuse();
        _ = self.shader_fuse.fuse();
        _ = self.cmdbuf_fuse.fuse();
    }

    pub fn tick(self: *@This(), lap: u64, engine: *Engine, app: *App) !void {
        const assets = &app.resources.assets;
        const window = engine.window;
        const delta = @as(f32, @floatFromInt(lap)) / @as(f32, @floatFromInt(std.time.ns_per_s));

        var input = window.input();

        var pexp = try app.world.ecs.explorer(app.entities.player);
        const camera = pexp.get_component(math.Camera).?;
        const pid = pexp.get_component(C.PlayerId).?;
        const player_id = pid.id;

        // local input tick
        {
            var mouse = &input.mouse;
            var kb = &input.keys;

            const imgui_io = &c.ImGui_GetIO()[0];
            if (imgui_io.WantCaptureMouse) {
                // mouse.* = std.mem.zeroes(@TypeOf(mouse));
                mouse.x = input.mouse.x;
                mouse.y = input.mouse.y;
                mouse.left = .none;
                mouse.right = .none;
            }
            if (imgui_io.WantCaptureKeyboard) {
                // kb.* = std.mem.zeroes(@TypeOf(kb));
            }

            if (kb.p.just_pressed()) {
                try render_utils.dump_image_to_file(
                    &app.screen_image,
                    &engine.graphics,
                    app.command_pool,
                    window.extent,
                    "images",
                );
            }

            if (mouse.left.just_pressed() and !self.focus) {
                self.focus = true;
                imgui_io.ConfigFlags |= c.ImGuiConfigFlags_NoMouse;
                window.hide_cursor(true);
            }
            if (kb.escape.just_pressed() and !self.focus) {
                window.queue_close();

                if (app.net_server) |_| {
                    try app.net_client.send_message(.{ .event = .{ .quit = {} } });
                } else {
                    try app.net_client.send_message(.{ .event = .{ .despawn_player = .{ .id = pid.id } } });
                }
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
            self.ts += lap;
            self.time += delta;
            self.deltatime = delta;

            if (!self.focus) {
                mouse.dx = 0;
                mouse.dy = 0;
            }
        }

        // networking tick
        {
            try app.net_client.send_message(.{ .event = .{ .input = .{ .id = pid.id, .input = input } } });

            try app.net_ctx.tick();
            if (app.net_server) |s| while (s.messages.try_recv()) |e| {
                switch (e.event) {
                    .join => {
                        self.client_count += 1;
                        try s.send_message(e.conn, 0, .{ .event = .{ .setid = .{ .id = self.client_count } } });
                    },
                    .spawn_player => {
                        var it = try app.world.ecs.iterator(struct { p: C.PlayerId, t: C.GlobalTransform });
                        while (it.next()) |p| {
                            // notify new player about all other players.
                            try s.send_message(e.conn, p.p.conn, .{ .event = .{ .spawn_player = .{ .id = p.p.id, .pos = .{
                                .x = p.t.transform.pos.x,
                                .y = p.t.transform.pos.y,
                                .z = p.t.transform.pos.z,
                            } } } });

                            // notify other players about this player
                            try s.send_message(p.p.conn, e.conn, .{ .event = e.event });
                        }
                    },
                    .despawn_player => {
                        // tell everyone this player left
                        var it = try app.world.ecs.iterator(struct { p: C.PlayerId });
                        while (it.next()) |p| {
                            try s.send_message(p.p.conn, e.conn, .{ .event = e.event });
                        }
                    },
                    .input => {
                        // send this player's inputs to everyone
                        var it = try app.world.ecs.iterator(struct { p: C.PlayerId });
                        while (it.next()) |p| {
                            try s.send_message(p.p.conn, e.conn, .{ .event = e.event });
                        }
                    },
                    .quit => {
                        // tell everyone to quit themselves
                        var it = try app.world.ecs.iterator(struct { p: C.PlayerId });
                        while (it.next()) |p| {
                            try s.send_message(p.p.conn, e.conn, .{ .event = e.event });
                        }
                    },
                    .setid => {
                        std.debug.print("YIKES server received: {any} event\n", .{std.meta.activeTag(e.event)});
                        continue;
                    },
                }
            };

            try app.net_ctx.tick();
            while (app.net_client.messages.try_recv()) |e| {
                switch (e.event) {
                    .spawn_player => |id| {
                        if (player_id == id.id) {
                            continue;
                        }
                        const t = C.Transform{ .pos = .{
                            .x = id.pos.x,
                            .y = id.pos.y,
                            .z = id.pos.z,
                        } };
                        _ = try self.cmdbuf.insert(.{
                            try C.Name.from("player"),
                            t,
                            C.Controller{},
                            C.StaticMesh{ .mesh = app.handles.mesh.cube, .material = app.handles.material.models },
                            C.BatchedRender{},
                            C.PlayerId{ .id = id.id, .conn = e.conn },
                            C.Shooter{
                                .audio = app.handles.audio.shot,
                                .ticker = try utils.Ticker.init(std.time.ns_per_ms * 100),
                                .hold = true,
                            },
                            try app.world.phy.add_character(.{
                                .pos = t.pos,
                                .rot = t.rotation,
                            }),
                        });
                    },
                    .despawn_player => |id| {
                        var it = try app.world.ecs.iterator(struct { p: C.PlayerId, entity: Entity });
                        while (it.next()) |p| {
                            if (p.p.id == id.id) {
                                try self.cmdbuf.delete(p.entity.*);
                            }
                        }
                    },
                    .input => |pinput| {
                        var pit = try app.world.ecs.iterator(struct {
                            pid: C.PlayerId,
                            controller: C.Controller,
                            t: C.GlobalTransform,
                            lt: C.LastTransform,
                            shooter: C.Shooter,
                            char: C.CharacterBody,
                        });

                        while (pit.next()) |player| {
                            if (pinput.id != player.pid.id) continue;

                            const kb = pinput.input.keys;
                            const mouse = pinput.input.mouse;

                            const char: *C.CharacterBody = player.char;
                            player.controller.did_move = kb.w.pressed() or kb.a.pressed() or kb.s.pressed() or kb.d.pressed();
                            player.controller.did_rotate = @abs(mouse.dx) + @abs(mouse.dy) > 0.0001;

                            // rotation should not be multiplied by deltatime. if mouse moves by 3cm, it should always rotate the same amount.
                            if (player.controller.did_rotate) {
                                player.controller.yaw += mouse.dx * player.controller.sensitivity_scale * player.controller.sensitivity;
                                player.controller.pitch += mouse.dy * player.controller.sensitivity_scale * player.controller.sensitivity;
                                player.controller.pitch = std.math.clamp(player.controller.pitch, math.Camera.constants.pitch_min, math.Camera.constants.pitch_max);
                            }

                            const rot = camera.rot_quat(player.controller.pitch, player.controller.yaw);
                            const up = rot.rotate_vector(camera.world_basis.up);
                            const fwd = rot.rotate_vector(camera.world_basis.fwd);
                            const right = rot.rotate_vector(camera.world_basis.right);

                            player.t.transform.rotation = rot.normalize();
                            // char.character.setRotation(player.t.rotation.to_buf());

                            var speed = player.controller.speed;
                            if (kb.shift.pressed()) {
                                speed *= 2.0;
                            }
                            if (kb.ctrl.pressed()) {
                                speed *= 0.1;
                            }

                            speed *= 50 * player.controller.speed;

                            if (char.character.getGroundState() == .on_ground) {
                                if (kb.w.pressed()) {
                                    player.char.force = player.char.force.add(fwd.scale(speed));
                                }
                                if (kb.a.pressed()) {
                                    player.char.force = player.char.force.add(right.scale(-speed));
                                }
                                if (kb.s.pressed()) {
                                    player.char.force = player.char.force.add(fwd.scale(-speed));
                                }
                                if (kb.d.pressed()) {
                                    player.char.force = player.char.force.add(right.scale(speed));
                                }
                            }
                            if (kb.space.pressed()) {
                                if (char.character.getGroundState() == .on_ground) {
                                    char.impulse = up.scale(10);
                                }
                            }

                            // {
                            //     const T = struct { t: C.Transform, c: C.Collider };
                            //     var t_min: ?f32 = null;
                            //     var closest: ?ecs_mod.Type.pointer(T) = null;
                            //     var it = try app.world.ecs.iterator(T);
                            //     while (it.next()) |ent| {
                            //         if (!ent.r.flags.player and !ent.r.flags.pinned and mouse.left.pressed()) {
                            //             if (ent.c.raycast(ent.t, self.physics.interpolated(player.lt, player.t).pos.add(fwd.scale(1.1)), fwd)) |t| {
                            //                 if (t_min == null) {
                            //                     t_min = t;
                            //                     closest = ent;
                            //                 } else if (t_min.? > t) {
                            //                     t_min = t;
                            //                     closest = ent;
                            //                 }
                            //             }
                            //         }
                            //     }
                            //     if (closest) |ent| {
                            //         ent.r.vel = ent.r.vel.add(fwd.scale(50));
                            //     }
                            // }

                            if (player.shooter.try_shoot(mouse.right)) {
                                // const bones = try allocator.alloc(math.Mat4x4, assets.ref(app.handles.model.sphere).bones.len);
                                // errdefer allocator.free(bones);
                                // const indices = try allocator.alloc(C.AnimatedRender.AnimationIndices, assets.ref(app.handles.model.sphere).bones.len);
                                // errdefer allocator.free(indices);
                                // @memset(bones, .{});
                                // @memset(indices, std.mem.zeroes(C.AnimatedRender.AnimationIndices));

                                const rng = math.Rng.init(self.rng.random()).with(.{ .min = 0.4, .max = 0.7 });
                                const t = C.GlobalTransform{ .transform = .{ .pos = self.physics.interpolated(player.lt, player.t).pos.add(fwd.scale(3.0)), .scale = .splat(rng.next()) } };
                                _ = try self.cmdbuf.insert(.{
                                    try C.Name.from("bullet"),
                                    t,
                                    // C.Rigidbody{ .flags = .{}, .vel = fwd.scale(50.0), .invmass = 1, .friction = 1 },
                                    // C.AnimatedRender{ .model = app.handles.model.sphere, .bones = bones, .indices = indices },
                                    C.StaticMesh{ .mesh = app.handles.mesh.cube, .material = app.handles.material.models },
                                    C.BatchedRender{},
                                    C.TimeDespawn{ .despawn_time = self.time + 10, .state = .alive },
                                    try app.world.phy.add_body(.{
                                        .shape = .{ .box = .{ .size = t.transform.scale } },
                                        .pos = t.transform.pos,
                                        .velocity = fwd.scale(50),
                                        .friction = 0.4,
                                        .rotation = t.transform.rotation,
                                        .motion_quality = .linear_cast,
                                    }),
                                });
                                _ = try self.cmdbuf.insert(.{
                                    C.TimeDespawn{
                                        .despawn_time = self.time + assets.ref(app.handles.audio.shot).duration_sec(),
                                        .state = .alive,
                                    },
                                    C.StaticSound{ .audio = app.handles.audio.shot, .pos = player.t.transform.pos, .start_frame = app.audio.ctx.ctx.frame_count, .volume = 0.4 },
                                });
                            }
                        }
                    },
                    .quit => {
                        window.queue_close();
                    },
                    .setid, .join => {},
                }
            }
        }

        // physics tick
        {
            self.physics.acctime += delta;
            self.physics.interpolation_acctime += delta;

            var steps = @divFloor(self.physics.acctime, self.physics.step);
            steps = @min(steps, 5); // no more than 5 steps per frame
            if (steps >= 1) {
                self.physics.acctime -= self.physics.step * steps;

                // {
                //     var it = try app.world.ecs.iterator(struct { r: C.Rigidbody });
                //     while (it.next()) |e| {
                //         if (!e.r.flags.pinned) {
                //             const g = camera.world_basis.up.scale(-9.8 / e.r.invmass);
                //             e.r.force = e.r.force.add(g);
                //         }
                //     }
                // }

                var player_it = try app.world.ecs.iterator(struct { char: C.CharacterBody });
                while (player_it.next()) |e| {
                    const char: *C.CharacterBody = e.char;
                    char.force = char.force.add(Vec3.from_buf(app.world.phy.phy.getGravity()));

                    var vel = Vec3.from_buf(char.character.getLinearVelocity());
                    vel = vel.add(char.force.scale(self.physics.step));

                    if (char.character.getGroundState() == .on_ground) {
                        const ground = Vec3.from_buf(char.character.getGroundNormal());
                        vel = vel.sub(ground.scale(vel.dot(ground)));

                        // friction
                        vel = vel.scale(0.9);
                    }

                    vel = vel.add(char.impulse);
                    char.character.setLinearVelocity(vel.to_buf());

                    const update_settings: jolt.CharacterVirtual.ExtendedUpdateSettings = .{};
                    char.character.extendedUpdate(
                        self.physics.step * steps,
                        (Vec3{ .y = -1 }).to_buf(),
                        &update_settings,
                        .{
                            // .broad_phase_layer_filter = app.world.phy.state.broadphase_layer_filter.interface(),
                            // .object_layer_filter = app.world.phy.state.object_layer_filter.interface(),
                            // body_filter: ?*const BodyFilter = null,
                            // shape_filter: ?*const ShapeFilter = null,
                        },
                    );
                }
                try app.world.step(self.physics.step * steps, @intFromFloat(steps));

                self.physics.interpolation_acctime = self.physics.acctime;
                var it = try app.world.ecs.iterator(struct { t: C.GlobalTransform, ft: C.LastTransform });
                while (it.next()) |e| {
                    e.ft.transform = e.t.transform;
                }
            }
        }

        // alive state tick
        {
            var it = try app.world.ecs.iterator(struct { id: Entity, ds: C.TimeDespawn });
            while (it.next()) |e| {
                switch (e.ds.state) {
                    .alive => {
                        if (e.ds.despawn_time < self.time) {
                            e.ds.state = .dying;

                            try self.cmdbuf.add_component(e.id.*, C.Sound{
                                .start_frame = app.audio.ctx.ctx.frame_count,
                                .audio = if (self.rng.random().boolean()) app.handles.audio.scream1 else app.handles.audio.scream2,
                                .volume = 2.0,
                            });
                        }
                    },
                    .dying => {
                        var exp = it.current_entity_explorer();
                        if (exp.get_component(C.Sound)) |s| {
                            if (s.start_frame + assets.ref(s.audio).data.len <= app.audio.ctx.ctx.frame_count) {
                                e.ds.state = .dead;
                            }
                        } else {
                            e.ds.state = .dead;
                        }
                    },
                    .dead => {
                        try self.cmdbuf.delete(e.id.*);
                    },
                }
            }
        }

        // animation tick
        {
            const animate = struct {
                fn animate(ar: *C.AnimatedMesh, armature: *assets_mod.Armature, time: f32) bool {
                    const animation = &armature.animations[4];

                    for (ar.bones) |*t| {
                        t.* = math.Mat4x4.scaling_mat(.splat(1));
                    }

                    var updated = false;
                    for (ar.bones, ar.indices, 0..) |*arb, *index, i| {
                        const bone = &animation.bones[i];
                        var out_t = math.Vec4{};
                        if (get_lerped(&out_t, bone.translation_keyframes.items, &index.translation, time)) {
                            updated = true;
                        }
                        var out_r = math.Vec4.quat_identity_rot();
                        if (get_lerped(&out_r, bone.rotation_keyframes.items, &index.rotation, time)) {
                            updated = true;
                        }
                        var out_s = math.Vec4.splat(1);
                        if (get_lerped(&out_s, bone.scale_keyframes.items, &index.scale, time)) {
                            updated = true;
                        }

                        arb.* = math.Mat4x4.translation_mat(out_t.xyz()).mul_mat(math.Mat4x4.rot_mat_from_quat(out_r)).mul_mat(math.Mat4x4.scaling_mat(out_s.xyz()));
                    }

                    const bones = armature.bones;
                    for (bones, 0..) |*b, i| {
                        if (b.parent == null) {
                            apply(ar, b.children, bones, ar.bones[i]);
                        }
                    }

                    return updated;
                }

                fn get_lerped(out: *math.Vec4, keyframes: []assets_mod.Keyframe, curr: *u32, time: f32) bool {
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

                fn apply(ar: *C.AnimatedMesh, ids: []const assets_mod.BoneId, bones: []assets_mod.Bone, t: math.Mat4x4) void {
                    for (ids) |bone_id| {
                        ar.bones[bone_id] = t.mul_mat(ar.bones[bone_id]);
                        apply(ar, bones[bone_id].children, bones, ar.bones[bone_id]);
                    }
                }
            }.animate;

            var it = try app.world.ecs.iterator(struct { m: C.AnimatedMesh });
            while (it.next()) |e| {
                const a: *C.AnimatedMesh = e.m;
                const armature = assets.ref(a.armature);
                a.time += delta;

                if (!animate(a, armature, a.time)) {
                    a.time = 0;
                    @memset(a.indices, std.mem.zeroes(C.AnimatedMesh.AnimationIndices));
                }
            }
        }

        // add missing last transforms
        {
            var it = try app.world.ecs.iterator(struct { entity: Entity, t: C.GlobalTransform });
            while (it.next()) |e| {
                var ee = it.current_entity_explorer();
                if (ee.get_component(C.LastTransform) == null) {
                    try self.cmdbuf.add_component(e.entity.*, C.LastTransform{ .transform = e.t.transform });
                }
            }
        }

        try self.cmdbuf.apply(@ptrCast(&app.world));

        // render instance tick
        {
            const instances = &app.resources.instances;
            instances.reset();

            {
                var it = try app.world.ecs.iterator(struct { t: C.GlobalTransform, ft: C.LastTransform, m: C.StaticMesh, b: C.BatchedRender });
                while (it.next()) |e| {
                    if (e.b.batch == null) {
                        e.b.batch = try instances.get_batch(e.m.mesh, e.m.material);
                    }
                    const instance = try instances.reserve_instance(e.b.batch.?);
                    if (instance) |ptr| {
                        const bones = try instances.reserve_bones(1);
                        ptr.bone_offset = bones.first;
                        bones.buf[0] = self.physics.interpolated(e.ft, e.t).mat4();
                    }
                }
            }

            {
                var it = try app.world.ecs.iterator(struct { t: C.GlobalTransform, ft: C.LastTransform, m: C.AnimatedMesh, b: C.BatchedRender });
                while (it.next()) |e| {
                    if (e.b.batch == null) {
                        e.b.batch = try instances.get_batch(e.m.mesh, e.m.material);
                    }
                    const instance = try instances.reserve_instance(e.b.batch.?);
                    const armature = assets.ref(e.m.armature);
                    if (instance) |ptr| {
                        const bones = try instances.reserve_bones(@intCast(armature.bones.len));
                        @memset(bones.buf, .{});
                        ptr.bone_offset = bones.first;
                        for (0..armature.bones.len) |index| {
                            bones.buf[index] = self.physics.interpolated(e.ft, e.t).mat4().mul_mat(e.m.bones[index]).mul_mat(armature.bones[index].inverse_bind_matrix);
                        }
                    }
                }
            }
        }

        // audio tick
        {
            const playing = &app.audio.ctx.ctx.playing;

            // if true, the audio thread won't know it had to swap
            // if false, the audio thread is swapping or has swapped.
            // but we don't know it it is currently swapping, so we need another lock for that. (hence playing.lock)
            _ = playing.swap_fuse.unfuse();
            defer _ = playing.swap_fuse.fuse();

            // this will not lock for more than the tiniest amount of time.
            while (playing.lock.check()) {}

            playing.samples_buf2.clearRetainingCapacity();

            const player = try app.world.ecs.get(app.entities.player, struct { t: C.GlobalTransform });
            {
                var it = try app.world.ecs.iterator(struct { sound: C.Sound, t: C.GlobalTransform });
                while (it.next()) |e| {
                    try playing.samples_buf2.append(.{
                        .handle = e.sound.audio,
                        .volume = e.sound.volume,
                        .start_frame = e.sound.start_frame,
                        .pos = player.t.transform.rotation.inverse_rotate_vector(e.t.transform.pos.sub(player.t.transform.pos)),
                    });
                }
            }
            {
                var it = try app.world.ecs.iterator(struct { sound: C.StaticSound });
                while (it.next()) |e| {
                    try playing.samples_buf2.append(.{
                        .handle = e.sound.audio,
                        .volume = e.sound.volume,
                        .start_frame = e.sound.start_frame,
                        .pos = player.t.transform.rotation.inverse_rotate_vector(e.sound.pos.sub(player.t.transform.pos)),
                    });
                }
            }
        }

        // camera tick
        {
            const player = try app.world.ecs.get(app.entities.player, struct {
                camera: math.Camera,
                controller: C.Controller,
                lt: C.LastTransform,
                t: C.GlobalTransform,
            });
            app.uniforms.uniform_buffer = try self.uniforms(
                window,
                &self.physics.interpolated(player.lt, player.t),
                player.camera,
                player.controller,
            );
        }
    }

    fn uniforms(
        self: *@This(),
        window: *Engine.Window,
        transform: *const C.Transform,
        camera: *const math.Camera,
        controller: *const C.Controller,
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
            try gen.add_struct("Vertex", resources_mod.Vertex);
            try gen.add_struct("Instance", resources_mod.Instance);
            try gen.add_struct("DrawCtx", resources_mod.ResourceManager.InstanceResources.DrawCtx);
            try gen.add_struct("PushConstants", resources_mod.PushConstants);
            try gen.add_bind_enum(resources_mod.UniformBinds);
            try gen.dump_shader("src/uniforms.glsl");
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

        const player = try app.world.ecs.get(app.entities.player, struct { controller: C.Controller });

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

    fn editState(self: *@This(), state: *AppState, controller: *C.Controller) void {
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
