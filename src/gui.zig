const std = @import("std");

const vk = @import("vulkan");

const main = @import("main.zig");
const allocator = main.allocator;

const Engine = @import("engine.zig");
const c = Engine.c;

const math = @import("math.zig");
const Vec4 = math.Vec4;
const Mat4x4 = math.Mat4x4;

const render_utils = @import("render_utils.zig");
const Swapchain = render_utils.Swapchain;

pub const GuiEngine = struct {
    ctx: *c.ImGuiContext,

    const Device = Engine.VulkanContext.Api.Device;

    pub fn loader(name: [*c]const u8, instance: ?*anyopaque) callconv(.C) ?*const fn () callconv(.C) void {
        return c.glfwGetInstanceProcAddress(@ptrCast(instance), name);
    }

    pub fn init(window: *Engine.Window) !@This() {
        const ctx = c.ImGui_CreateContext(null) orelse return error.ErrorCreatingImguiContext;
        errdefer c.ImGui_DestroyContext(ctx);

        _ = c.cImGui_ImplVulkan_LoadFunctions(loader);

        const io = c.ImGui_GetIO();
        io.*.ConfigFlags |= c.ImGuiConfigFlags_NavEnableKeyboard;
        io.*.ConfigFlags |= c.ImGuiConfigFlags_NavEnableGamepad;

        const color = math.ColorParse.hex_xyzw;
        const style = c.ImGui_GetStyle();
        style.*.Colors[c.ImGuiCol_Text] = c.ImVec4{ .x = 0.93, .y = 0.93, .z = 0.93, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_TextDisabled] = c.ImVec4{ .x = 0.5, .y = 0.5, .z = 0.5, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_WindowBg] = c.ImVec4{ .x = 0.11, .y = 0.11, .z = 0.11, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_ChildBg] = c.ImVec4{ .x = 0.15, .y = 0.15, .z = 0.15, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_Border] = c.ImVec4{ .x = 0.30, .y = 0.30, .z = 0.30, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_FrameBg] = c.ImVec4{ .x = 0.20, .y = 0.20, .z = 0.20, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_FrameBgHovered] = c.ImVec4{ .x = 0.40, .y = 0.40, .z = 0.40, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_FrameBgActive] = c.ImVec4{ .x = 0.50, .y = 0.50, .z = 0.50, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_TitleBg] = c.ImVec4{ .x = 0.00, .y = 0.00, .z = 0.00, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_TitleBgActive] = c.ImVec4{ .x = 0.20, .y = 0.20, .z = 0.20, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_TitleBgCollapsed] = c.ImVec4{ .x = 0.10, .y = 0.10, .z = 0.10, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_Button] = c.ImVec4{ .x = 0.80, .y = 0.20, .z = 0.20, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_ButtonHovered] = c.ImVec4{ .x = 1.00, .y = 0.50, .z = 0.50, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_ButtonActive] = c.ImVec4{ .x = 1.00, .y = 0.30, .z = 0.30, .w = 1.00 };
        style.*.Colors[c.ImGuiCol_Header] = color(c.ImVec4, "#3c3836ff");
        style.*.Colors[c.ImGuiCol_HeaderHovered] = color(c.ImVec4, "#504945ff");
        style.*.Colors[c.ImGuiCol_HeaderActive] = color(c.ImVec4, "#7c6f64ff");
        style.*.WindowRounding = 5;
        style.*.FrameRounding = 3;

        _ = c.cImGui_ImplGlfw_InitForVulkan(window.handle, true);
        errdefer c.cImGui_ImplGlfw_Shutdown();

        return .{
            .ctx = ctx,
        };
    }

    pub fn deinit(self: *@This()) void {
        c.cImGui_ImplGlfw_Shutdown();
        c.ImGui_DestroyContext(self.ctx);
    }

    pub const GuiRenderer = struct {
        desc_pool: vk.DescriptorPool,
        pass: vk.RenderPass,

        cmd_pool: vk.CommandPool,
        cmd_bufs: []vk.CommandBuffer,

        framebuffers: []vk.Framebuffer,

        pub fn init(engine: *Engine, swapchain: *Swapchain) !@This() {
            const device = &engine.graphics.device;

            const cmd_pool = try device.createCommandPool(&.{
                .queue_family_index = engine.graphics.graphics_queue.family,
                .flags = .{
                    .reset_command_buffer_bit = true,
                },
            }, null);
            errdefer device.destroyCommandPool(cmd_pool, null);

            var desc_pool = try device.createDescriptorPool(&.{
                .flags = .{
                    .free_descriptor_set_bit = true,
                },
                .max_sets = 1,
                .pool_size_count = 1,
                .p_pool_sizes = &[_]vk.DescriptorPoolSize{.{
                    .type = .combined_image_sampler,
                    .descriptor_count = 1,
                }},
            }, null);
            errdefer device.destroyDescriptorPool(desc_pool, null);

            const color_attachment = vk.AttachmentDescription{
                .format = swapchain.surface_format.format,
                .samples = .{ .@"1_bit" = true },
                .load_op = .load,
                .store_op = .store,
                .stencil_load_op = .dont_care,
                .stencil_store_op = .dont_care,
                .initial_layout = .color_attachment_optimal,
                .final_layout = .present_src_khr,
            };

            const color_attachment_ref = vk.AttachmentReference{
                .attachment = 0,
                .layout = .color_attachment_optimal,
            };

            const subpass = vk.SubpassDescription{
                .pipeline_bind_point = .graphics,
                .color_attachment_count = 1,
                .p_color_attachments = @ptrCast(&color_attachment_ref),
            };
            const pass = try device.createRenderPass(&.{
                .attachment_count = 1,
                .p_attachments = @ptrCast(&color_attachment),
                .subpass_count = 1,
                .p_subpasses = @ptrCast(&subpass),
            }, null);
            errdefer device.destroyRenderPass(pass, null);

            const framebuffers = blk: {
                const framebuffers = try allocator.alloc(vk.Framebuffer, swapchain.swap_images.len);
                errdefer allocator.free(framebuffers);

                var i_2: usize = 0;
                errdefer for (framebuffers[0..i_2]) |fb| device.destroyFramebuffer(fb, null);
                for (framebuffers) |*fb| {
                    const attachments = [_]vk.ImageView{swapchain.swap_images[i_2].view};
                    fb.* = try device.createFramebuffer(&.{
                        .render_pass = pass,
                        .attachment_count = attachments.len,
                        .p_attachments = &attachments,
                        .width = swapchain.extent.width,
                        .height = swapchain.extent.height,
                        .layers = 1,
                    }, null);
                    i_2 += 1;
                }

                break :blk framebuffers;
            };
            errdefer {
                for (framebuffers) |fb| device.destroyFramebuffer(fb, null);
                allocator.free(framebuffers);
            }

            const cmdbufs = try allocator.alloc(vk.CommandBuffer, swapchain.swap_images.len);
            errdefer allocator.free(cmdbufs);

            try device.allocateCommandBuffers(&.{
                .command_pool = cmd_pool,
                .level = .primary,
                .command_buffer_count = @intCast(cmdbufs.len),
            }, cmdbufs.ptr);
            errdefer device.freeCommandBuffers(cmd_pool, @intCast(cmdbufs.len), cmdbufs.ptr);

            var info = c.ImGui_ImplVulkan_InitInfo{
                .Instance = @as(*c.VkInstance, @ptrCast(&engine.graphics.instance.handle)).*,
                .PhysicalDevice = @as(*c.VkPhysicalDevice, @ptrCast(&engine.graphics.pdev)).*,
                .Device = @as(*c.VkDevice, @ptrCast(&engine.graphics.device.handle)).*,
                .QueueFamily = engine.graphics.graphics_queue.family,
                .Queue = @as(*c.VkQueue, @ptrCast(&engine.graphics.graphics_queue.handle)).*,
                .DescriptorPool = @as(*c.VkDescriptorPool, @ptrCast(&desc_pool)).*,
                .RenderPass = @as(*c.VkRenderPass, @ptrCast(@constCast(&pass))).*,
                .MinImageCount = 2,
                .ImageCount = @intCast(swapchain.swap_images.len),
                // .MSAASamples: VkSampleCountFlagBits = @import("std").mem.zeroes(VkSampleCountFlagBits),
                // .PipelineCache: VkPipelineCache = @import("std").mem.zeroes(VkPipelineCache),
                // .Subpass: u32 = @import("std").mem.zeroes(u32),
                // .UseDynamicRendering: bool = @import("std").mem.zeroes(bool),
                // .PipelineRenderingCreateInfo: VkPipelineRenderingCreateInfoKHR = @import("std").mem.zeroes(VkPipelineRenderingCreateInfoKHR),
                // .Allocator: [*c]const VkAllocationCallbacks = @import("std").mem.zeroes([*c]const VkAllocationCallbacks),
                // .CheckVkResultFn: ?*const fn (VkResult) callconv(.C) void = @import("std").mem.zeroes(?*const fn (VkResult) callconv(.C) void),
                // .MinAllocationSize: VkDeviceSize = @import("std").mem.zeroes(VkDeviceSize),
            };
            _ = c.cImGui_ImplVulkan_Init(&info);
            errdefer c.cImGui_ImplVulkan_Shutdown();

            return .{
                .desc_pool = desc_pool,
                .cmd_pool = cmd_pool,
                .pass = pass,
                .framebuffers = framebuffers,
                .cmd_bufs = cmdbufs,
            };
        }

        pub fn render_start(_: *@This()) void {
            c.cImGui_ImplVulkan_NewFrame();
            c.cImGui_ImplGlfw_NewFrame();
            c.ImGui_NewFrame();
        }

        pub fn render_end(self: *@This(), device: *Device, swapchain: *Swapchain) !void {
            c.ImGui_Render();

            const draw_data = c.ImGui_GetDrawData();

            const index = swapchain.image_index;
            const cmdbuf = self.cmd_bufs[index];
            const framebuffer = self.framebuffers[index];

            try device.resetCommandBuffer(cmdbuf, .{ .release_resources_bit = true });
            try device.beginCommandBuffer(cmdbuf, &.{});

            device.cmdBeginRenderPass(cmdbuf, &.{
                .render_pass = self.pass,
                .framebuffer = framebuffer,
                .render_area = .{
                    .offset = .{ .x = 0, .y = 0 },
                    .extent = swapchain.extent,
                },
            }, .@"inline");

            c.cImGui_ImplVulkan_RenderDrawData(draw_data, @as(*c.VkCommandBuffer, @ptrCast(@constCast(&cmdbuf))).*);

            device.cmdEndRenderPass(cmdbuf);
            try device.endCommandBuffer(cmdbuf);
        }

        pub fn deinit(self: *@This(), device: *Device) void {
            defer device.destroyDescriptorPool(self.desc_pool, null);
            defer device.destroyRenderPass(self.pass, null);
            defer device.destroyCommandPool(self.cmd_pool, null);
            defer {
                for (self.framebuffers) |fb| device.destroyFramebuffer(fb, null);
                allocator.free(self.framebuffers);
            }
            defer {
                device.freeCommandBuffers(self.cmd_pool, @intCast(self.cmd_bufs.len), self.cmd_bufs.ptr);
                allocator.free(self.cmd_bufs);
            }
            defer c.cImGui_ImplVulkan_Shutdown();
        }
    };
};

fn enum_dropdown(enum_ptr: anytype, title: [*:0]const u8) void {
    const opt_modes = comptime blk: {
        const fields = @typeInfo(@TypeOf(enum_ptr.*)).Enum.fields;
        var arr: [fields.len][*:0]const u8 = undefined;
        for (fields, 0..) |field, i| {
            arr[i] = field.name;
        }
        break :blk arr;
    };
    var opt_index: c_int = 0;
    for (opt_modes, 0..) |mode, i| {
        if (std.mem.eql(u8, std.mem.span(mode), @tagName(enum_ptr.*))) {
            opt_index = @intCast(i);
        }
    }
    c.ImGui_SetNextItemWidth(200);
    _ = c.ImGui_ComboChar(title, &opt_index, &opt_modes, opt_modes.len);
    enum_ptr.* = std.meta.stringToEnum(@TypeOf(enum_ptr.*), std.mem.span(opt_modes[@intCast(opt_index)])).?;
}
