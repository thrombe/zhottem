const std = @import("std");

pub const c = @cImport({
    @cDefine("GLFW_INCLUDE_VULKAN", "1");
    // @cDefine("GLFW_INCLUDE_NONE", "1");
    @cInclude("GLFW/glfw3.h");
    @cInclude("GLFW/glfw3native.h");

    @cInclude("dcimgui.h");
    @cInclude("cimgui_impl_glfw.h");
    @cInclude("cimgui_impl_vulkan.h");
});

const vk = @import("vulkan");

const utils = @import("utils.zig");
const Fuse = utils.Fuse;

const main = @import("main.zig");
const allocator = main.allocator;

const Engine = @This();

window: *Window,
graphics: VulkanContext,

pub fn init() !@This() {
    var window = try Window.init();
    errdefer window.deinit();

    var ctx = try VulkanContext.init(window);
    errdefer ctx.deinit();

    return .{
        .window = window,
        .graphics = ctx,
    };
}

pub fn deinit(self: *@This()) void {
    self.graphics.deinit();
    self.window.deinit();
}

pub const Window = struct {
    // last known size
    extent: vk.Extent2D = .{ .width = 800, .height = 600 },
    handle: *c.GLFWwindow,
    resize_fuse: Fuse = .{},
    input_state: InputState = .{},

    pub const Action = enum(u2) {
        none,
        press,
        repeat,
        release,

        fn key_tick(self: anytype) void {
            self.* = switch (self.*) {
                .none => .none,
                .press => .press,
                .repeat => .repeat,
                .release => .none,
            };
        }

        fn mouse_tick(self: anytype) void {
            self.* = switch (self.*) {
                .none => .none,
                .press => .repeat,
                .repeat => .repeat,
                .release => .none,
            };
        }

        fn from_int(k: c_int) ?@This() {
            return switch (k) {
                c.GLFW_RELEASE => .release,
                c.GLFW_PRESS => .press,
                c.GLFW_REPEAT => .repeat,
                else => return null,
            };
        }
    };
    pub const InputState = packed struct {
        mouse: packed struct {
            left: Action = .none,
            right: Action = .none,
            middle: Action = .none,

            fn tick(self: anytype) void {
                inline for (@typeInfo(@This()).Struct.fields) |field| {
                    @field(self, field.name).mouse_tick();
                }
            }
        } = .{},
        keys: packed struct {
            num_0: Action = .none,
            num_1: Action = .none,
            num_2: Action = .none,
            num_3: Action = .none,
            num_4: Action = .none,
            num_5: Action = .none,
            num_6: Action = .none,
            num_7: Action = .none,
            num_8: Action = .none,
            num_9: Action = .none,
            a: Action = .none,
            b: Action = .none,
            c: Action = .none,
            d: Action = .none,
            e: Action = .none,
            f: Action = .none,
            g: Action = .none,
            h: Action = .none,
            i: Action = .none,
            j: Action = .none,
            k: Action = .none,
            l: Action = .none,
            m: Action = .none,
            n: Action = .none,
            o: Action = .none,
            p: Action = .none,
            q: Action = .none,
            r: Action = .none,
            s: Action = .none,
            t: Action = .none,
            u: Action = .none,
            v: Action = .none,
            w: Action = .none,
            x: Action = .none,
            y: Action = .none,
            z: Action = .none,
            shift: Action = .none,
            shift_l: Action = .none,
            shift_r: Action = .none,
            ctrl: Action = .none,
            ctrl_l: Action = .none,
            ctrl_r: Action = .none,
            alt: Action = .none,
            alt_l: Action = .none,
            alt_r: Action = .none,
            super: Action = .none,
            super_l: Action = .none,
            super_r: Action = .none,

            fn tick(self: anytype) void {
                inline for (@typeInfo(@This()).Struct.fields) |field| {
                    @field(self, field.name).key_tick();
                }
            }
        } = .{},
    };

    const Callbacks = struct {
        var global_window: *Window = undefined;

        fn err(code: c_int, msg: [*c]const u8) callconv(.C) void {
            _ = code;
            std.debug.print("GLFW Error: {s}\n", .{msg});
        }

        fn resize(_: ?*c.GLFWwindow, width: c_int, height: c_int) callconv(.C) void {
            global_window.extent = .{
                .width = @intCast(width),
                .height = @intCast(height),
            };
            _ = global_window.resize_fuse.fuse();
        }

        fn mouse(_: ?*c.GLFWwindow, button: c_int, action: c_int, mods: c_int) callconv(.C) void {
            _ = mods;
            const a = Action.from_int(action) orelse return;
            switch (button) {
                c.GLFW_MOUSE_BUTTON_LEFT => global_window.input_state.mouse.left = a,
                c.GLFW_MOUSE_BUTTON_RIGHT => global_window.input_state.mouse.right = a,
                c.GLFW_MOUSE_BUTTON_MIDDLE => global_window.input_state.mouse.middle = a,
                else => return,
            }
        }

        fn key(_: ?*c.GLFWwindow, button: c_int, scancode: c_int, action: c_int, mods: c_int) callconv(.C) void {
            _ = mods;
            _ = scancode;

            const a = Action.from_int(action) orelse return;
            switch (button) {
                c.GLFW_KEY_0 => global_window.input_state.keys.num_0 = a,
                c.GLFW_KEY_1 => global_window.input_state.keys.num_1 = a,
                c.GLFW_KEY_2 => global_window.input_state.keys.num_2 = a,
                c.GLFW_KEY_3 => global_window.input_state.keys.num_3 = a,
                c.GLFW_KEY_4 => global_window.input_state.keys.num_4 = a,
                c.GLFW_KEY_5 => global_window.input_state.keys.num_5 = a,
                c.GLFW_KEY_6 => global_window.input_state.keys.num_6 = a,
                c.GLFW_KEY_7 => global_window.input_state.keys.num_7 = a,
                c.GLFW_KEY_8 => global_window.input_state.keys.num_8 = a,
                c.GLFW_KEY_9 => global_window.input_state.keys.num_9 = a,
                c.GLFW_KEY_A => global_window.input_state.keys.a = a,
                c.GLFW_KEY_B => global_window.input_state.keys.b = a,
                c.GLFW_KEY_C => global_window.input_state.keys.c = a,
                c.GLFW_KEY_D => global_window.input_state.keys.d = a,
                c.GLFW_KEY_E => global_window.input_state.keys.e = a,
                c.GLFW_KEY_F => global_window.input_state.keys.f = a,
                c.GLFW_KEY_G => global_window.input_state.keys.g = a,
                c.GLFW_KEY_H => global_window.input_state.keys.h = a,
                c.GLFW_KEY_I => global_window.input_state.keys.i = a,
                c.GLFW_KEY_J => global_window.input_state.keys.j = a,
                c.GLFW_KEY_K => global_window.input_state.keys.k = a,
                c.GLFW_KEY_L => global_window.input_state.keys.l = a,
                c.GLFW_KEY_M => global_window.input_state.keys.m = a,
                c.GLFW_KEY_N => global_window.input_state.keys.n = a,
                c.GLFW_KEY_O => global_window.input_state.keys.o = a,
                c.GLFW_KEY_P => global_window.input_state.keys.p = a,
                c.GLFW_KEY_Q => global_window.input_state.keys.q = a,
                c.GLFW_KEY_R => global_window.input_state.keys.r = a,
                c.GLFW_KEY_S => global_window.input_state.keys.s = a,
                c.GLFW_KEY_T => global_window.input_state.keys.t = a,
                c.GLFW_KEY_U => global_window.input_state.keys.u = a,
                c.GLFW_KEY_V => global_window.input_state.keys.v = a,
                c.GLFW_KEY_W => global_window.input_state.keys.w = a,
                c.GLFW_KEY_X => global_window.input_state.keys.x = a,
                c.GLFW_KEY_Y => global_window.input_state.keys.y = a,
                c.GLFW_KEY_Z => global_window.input_state.keys.z = a,
                c.GLFW_KEY_LEFT_SHIFT => {
                    global_window.input_state.keys.shift = a;
                    global_window.input_state.keys.shift_l = a;
                },
                c.GLFW_KEY_LEFT_CONTROL => {
                    global_window.input_state.keys.ctrl = a;
                    global_window.input_state.keys.ctrl_l = a;
                },
                c.GLFW_KEY_LEFT_ALT => {
                    global_window.input_state.keys.alt = a;
                    global_window.input_state.keys.alt_l = a;
                },
                c.GLFW_KEY_LEFT_SUPER => {
                    global_window.input_state.keys.super = a;
                    global_window.input_state.keys.super_l = a;
                },
                c.GLFW_KEY_RIGHT_SHIFT => {
                    global_window.input_state.keys.shift = a;
                    global_window.input_state.keys.shift_r = a;
                },
                c.GLFW_KEY_RIGHT_CONTROL => {
                    global_window.input_state.keys.ctrl = a;
                    global_window.input_state.keys.ctrl_r = a;
                },
                c.GLFW_KEY_RIGHT_ALT => {
                    global_window.input_state.keys.alt = a;
                    global_window.input_state.keys.alt_r = a;
                },
                c.GLFW_KEY_RIGHT_SUPER => {
                    global_window.input_state.keys.super = a;
                    global_window.input_state.keys.super_r = a;
                },
                else => return,
            }
        }
    };

    pub fn init() !*@This() {
        _ = c.glfwSetErrorCallback(&Callbacks.err);

        if (c.glfwInit() != c.GLFW_TRUE) return error.GlfwInitFailed;
        errdefer c.glfwTerminate();

        if (c.glfwVulkanSupported() != c.GLFW_TRUE) {
            return error.VulkanNotSupported;
        }

        const extent = .{ .width = 800, .height = 600 };
        c.glfwWindowHint(c.GLFW_CLIENT_API, c.GLFW_NO_API);
        const window = c.glfwCreateWindow(
            extent.width,
            extent.height,
            "yaaaaaaaaaa",
            null,
            null,
        ) orelse return error.WindowInitFailed;
        errdefer c.glfwDestroyWindow(window);

        _ = c.glfwSetFramebufferSizeCallback(window, &Callbacks.resize);
        _ = c.glfwSetMouseButtonCallback(window, &Callbacks.mouse);
        _ = c.glfwSetKeyCallback(window, &Callbacks.key);

        const self = try allocator.create(@This());
        errdefer allocator.destroy(self);
        self.* = .{ .extent = extent, .handle = window };
        Callbacks.global_window = self;
        return self;
    }

    pub fn tick(self: *@This()) void {
        var w: c_int = undefined;
        var h: c_int = undefined;
        c.glfwGetFramebufferSize(self.handle, &w, &h);

        self.input_state.keys.tick();
        self.input_state.mouse.tick();

        // polls events and calls callbacks
        c.glfwPollEvents();
    }

    pub fn is_pressed(self: *@This(), key: c_int) bool {
        return c.glfwGetKey(self.handle, key) == c.GLFW_PRESS;
    }

    pub fn poll_mouse(self: *@This()) struct { left: bool, x: i32, y: i32 } {
        var x: f64 = 0;
        var y: f64 = 0;
        c.glfwGetCursorPos(self.handle, @ptrCast(&x), @ptrCast(&y));
        const left = c.glfwGetMouseButton(self.handle, c.GLFW_MOUSE_BUTTON_LEFT);
        return .{ .left = left == c.GLFW_PRESS, .x = @intFromFloat(x), .y = @intFromFloat(y) };
    }

    pub fn set_cursor_pos(self: *@This(), x: f64, y: f64) void {
        c.glfwSetCursorPos(self.handle, x, y);
    }

    pub fn hide_cursor(self: *@This(), hide: bool) void {
        c.glfwSetInputMode(self.handle, c.GLFW_CURSOR, if (hide) c.GLFW_CURSOR_DISABLED else c.GLFW_CURSOR_NORMAL);
    }

    pub fn get_res(self: *@This()) !struct { width: u32, height: u32 } {
        const monitor = c.glfwGetWindowMonitor(self.handle) orelse c.glfwGetPrimaryMonitor() orelse return error.CouldNotGetMonitor;
        const mode = c.glfwGetVideoMode(monitor);
        return .{
            .width = @intCast(mode.*.width),
            .height = @intCast(mode.*.height),
        };
    }

    pub fn should_close(self: *@This()) bool {
        return c.glfwWindowShouldClose(self.handle) == c.GLFW_TRUE;
    }

    pub fn queue_close(self: *@This()) void {
        c.glfwSetWindowShouldClose(self.handle, c.GLFW_TRUE);
    }

    pub fn is_minimized(self: *@This()) bool {
        var w: c_int = undefined;
        var h: c_int = undefined;
        c.glfwGetFramebufferSize(self.handle, &w, &h);
        return w == 0 or h == 0;
    }

    pub fn deinit(self: *@This()) void {
        c.glfwDestroyWindow(self.handle);
        c.glfwTerminate();
        allocator.destroy(self);
    }

    pub fn input(self: *@This()) Window.InputState {
        return self.input_state;
    }
};

pub const VulkanContext = struct {
    const required_device_extensions = [_][*:0]const u8{
        vk.extensions.khr_swapchain.name,
        vk.extensions.khr_dynamic_rendering.name,
    };
    pub const Api = struct {
        const apis: []const vk.ApiInfo = &.{
            // .{
            //     .base_commands = .{
            //         .createInstance = true,
            //     },
            //     .instance_commands = .{
            //         .createDevice = true,
            //     },
            // },
            vk.features.version_1_0,
            vk.features.version_1_1,
            vk.features.version_1_2,
            vk.features.version_1_3,
            vk.extensions.khr_surface,
            vk.extensions.khr_swapchain,
            vk.extensions.khr_dynamic_rendering,

            // EH: ?what are these
            // vk.extensions.ext_validation_features,
            // vk.extensions.ext_validation_flags,
            // vk.extensions.ext_validation_cache,
        };

        pub const BaseDispatch = vk.BaseWrapper(apis);
        pub const InstanceDispatch = vk.InstanceWrapper(apis);
        pub const DeviceDispatch = vk.DeviceWrapper(apis);

        pub const Instance = vk.InstanceProxy(apis);
        pub const Device = vk.DeviceProxy(apis);
        pub const CommandBuffer = vk.CommandBufferProxy(apis);
        pub const Queue = vk.QueueProxy(apis);
    };

    vkb: Api.BaseDispatch,
    instance: Api.Instance,
    surface: vk.SurfaceKHR,
    pdev: vk.PhysicalDevice,
    props: vk.PhysicalDeviceProperties,
    mem_props: vk.PhysicalDeviceMemoryProperties,

    device: Api.Device,
    graphics_queue: Queue,
    present_queue: Queue,

    pub fn init(window: *Window) !@This() {
        const vkb = try Api.BaseDispatch.load(@as(vk.PfnGetInstanceProcAddr, @ptrCast(&c.glfwGetInstanceProcAddress)));

        var glfw_exts_count: u32 = 0;
        const glfw_exts = c.glfwGetRequiredInstanceExtensions(&glfw_exts_count);
        const layers = [_][*c]const u8{
            "VK_LAYER_KHRONOS_validation",
        };
        const instance = vkb.createInstance(&.{
            .p_application_info = &.{
                .p_application_name = "yaaaaaaaaaaaaaaa",
                .api_version = vk.API_VERSION_1_3,
                .application_version = vk.makeApiVersion(0, 0, 1, 0),
                .engine_version = vk.makeApiVersion(0, 0, 1, 0),
            },
            .enabled_extension_count = glfw_exts_count,
            .pp_enabled_extension_names = @ptrCast(glfw_exts),
            .enabled_layer_count = @intCast(layers.len),
            .pp_enabled_layer_names = @ptrCast(&layers),
        }, null) catch |e| {
            std.debug.print("{any}\n", .{e});
            return e;
        };

        const vki = try allocator.create(Api.InstanceDispatch);
        errdefer allocator.destroy(vki);
        vki.* = try Api.InstanceDispatch.load(instance, vkb.dispatch.vkGetInstanceProcAddr);
        const vkinstance: Api.Instance = Api.Instance.init(instance, vki);
        errdefer vkinstance.destroyInstance(null);

        var surface: vk.SurfaceKHR = undefined;
        if (c.glfwCreateWindowSurface(@as(*const c.VkInstance, @ptrCast(&vkinstance.handle)).*, window.handle, null, @ptrCast(&surface)) != c.VK_SUCCESS) {
            return error.SurfaceInitFailed;
        }

        const candidate = try DeviceCandidate.pick(vkinstance, surface);
        const pdev = candidate.pdev;
        const props = candidate.props;

        const dev = blk: {
            const priority = [_]f32{1};
            const qci = [_]vk.DeviceQueueCreateInfo{
                .{
                    .queue_family_index = candidate.queues.graphics_family,
                    .queue_count = 1,
                    .p_queue_priorities = &priority,
                },
                .{
                    .queue_family_index = candidate.queues.present_family,
                    .queue_count = 1,
                    .p_queue_priorities = &priority,
                },
            };

            const queue_count: u32 = if (candidate.queues.graphics_family == candidate.queues.present_family)
                1
            else
                2;

            const device = try vkinstance.createDevice(candidate.pdev, &.{
                .p_next = @ptrCast(&vk.PhysicalDeviceVulkan13Features{
                    .dynamic_rendering = vk.TRUE,
                    .synchronization_2 = vk.TRUE,
                }),
                .queue_create_info_count = queue_count,
                .p_queue_create_infos = &qci,
                .enabled_extension_count = required_device_extensions.len,
                .pp_enabled_extension_names = @ptrCast(&required_device_extensions),
                .p_enabled_features = &.{
                    .fill_mode_non_solid = vk.TRUE,
                    // .vertex_pipeline_stores_and_atomics = vk.TRUE,
                },
            }, null);
            break :blk device;
        };

        const vkd = try allocator.create(Api.DeviceDispatch);
        errdefer allocator.destroy(vkd);
        vkd.* = try Api.DeviceDispatch.load(dev, vkinstance.wrapper.dispatch.vkGetDeviceProcAddr);
        const device = Api.Device.init(dev, vkd);
        errdefer device.destroyDevice(null);

        const graphics_queue = Queue.init(device, candidate.queues.graphics_family);
        const present_queue = Queue.init(device, candidate.queues.present_family);

        const mem_props = vkinstance.getPhysicalDeviceMemoryProperties(pdev);

        return .{
            .vkb = vkb,
            .instance = vkinstance,
            .surface = surface,
            .device = device,
            .pdev = pdev,
            .props = props,
            .mem_props = mem_props,
            .graphics_queue = graphics_queue,
            .present_queue = present_queue,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.device.destroyDevice(null);
        self.instance.destroySurfaceKHR(self.surface, null);
        self.instance.destroyInstance(null);

        // Don't forget to free the tables to prevent a memory leak.
        allocator.destroy(self.device.wrapper);
        allocator.destroy(self.instance.wrapper);
    }

    pub fn allocate(self: @This(), requirements: vk.MemoryRequirements, flags: vk.MemoryPropertyFlags) !vk.DeviceMemory {
        return try self.device.allocateMemory(&.{
            .allocation_size = requirements.size,
            .memory_type_index = blk: {
                for (self.mem_props.memory_types[0..self.mem_props.memory_type_count], 0..) |mem_type, i| {
                    if (requirements.memory_type_bits & (@as(u32, 1) << @truncate(i)) != 0 and mem_type.property_flags.contains(flags)) {
                        break :blk @truncate(i);
                    }
                }

                return error.NoSuitableMemoryType;
            },
        }, null);
    }

    pub const Queue = struct {
        handle: vk.Queue,
        family: u32,

        fn init(device: Api.Device, family: u32) Queue {
            return .{
                .handle = device.getDeviceQueue(family, 0),
                .family = family,
            };
        }
    };

    const DeviceCandidate = struct {
        pdev: vk.PhysicalDevice,
        props: vk.PhysicalDeviceProperties,
        queues: QueueAllocation,

        const QueueAllocation = struct {
            graphics_family: u32,
            present_family: u32,
        };

        fn pick(
            instance: Api.Instance,
            surface: vk.SurfaceKHR,
        ) !DeviceCandidate {
            const pdevs = try instance.enumeratePhysicalDevicesAlloc(allocator);
            defer allocator.free(pdevs);

            for (pdevs) |pdev| {
                const props = instance.getPhysicalDeviceProperties(pdev);
                std.debug.print("{s}\n", .{props.device_name});
            }

            for (pdevs) |pdev| {
                if (try checkSuitable(instance, pdev, surface)) |candidate| {
                    return candidate;
                }
            }

            return error.NoSuitableDevice;
        }

        fn checkSuitable(
            instance: Api.Instance,
            pdev: vk.PhysicalDevice,
            surface: vk.SurfaceKHR,
        ) !?DeviceCandidate {
            const propsv = try instance.enumerateDeviceExtensionPropertiesAlloc(pdev, null, allocator);
            defer allocator.free(propsv);
            for (required_device_extensions) |ext| {
                for (propsv) |props| {
                    if (std.mem.eql(u8, std.mem.span(ext), std.mem.sliceTo(&props.extension_name, 0))) {
                        break;
                    }
                } else {
                    return null;
                }
            }

            var format_count: u32 = undefined;
            _ = try instance.getPhysicalDeviceSurfaceFormatsKHR(pdev, surface, &format_count, null);
            var present_mode_count: u32 = undefined;
            _ = try instance.getPhysicalDeviceSurfacePresentModesKHR(pdev, surface, &present_mode_count, null);
            if (!(format_count > 0 and present_mode_count > 0)) {
                return null;
            }

            const families = try instance.getPhysicalDeviceQueueFamilyPropertiesAlloc(pdev, allocator);
            defer allocator.free(families);

            var graphics_family: ?u32 = null;
            var present_family: ?u32 = null;
            for (families, 0..) |properties, i| {
                const family: u32 = @intCast(i);
                if (graphics_family == null and properties.queue_flags.graphics_bit) {
                    graphics_family = family;
                }
                if (present_family == null and (try instance.getPhysicalDeviceSurfaceSupportKHR(pdev, family, surface)) == vk.TRUE) {
                    present_family = family;
                }
            }

            if (graphics_family == null or present_family == null) {
                return null;
            }

            const props = instance.getPhysicalDeviceProperties(pdev);
            if (props.device_type != .discrete_gpu) {
                return null;
            }
            return DeviceCandidate{
                .pdev = pdev,
                .props = props,
                .queues = .{
                    .graphics_family = graphics_family.?,
                    .present_family = present_family.?,
                },
            };
        }
    };
};
