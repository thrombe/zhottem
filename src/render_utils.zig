const std = @import("std");

const vk = @import("vulkan");

const utils = @import("utils.zig");

const Engine = @import("engine.zig");

const main = @import("main.zig");
const allocator = main.allocator;

const Device = Engine.VulkanContext.Api.Device;

pub const DescriptorPool = struct {
    pool: vk.DescriptorPool,

    pub fn new(device: *Device) !@This() {
        const pool_sizes = [_]vk.DescriptorPoolSize{
            .{
                .type = .uniform_buffer,
                .descriptor_count = 100,
            },
            .{
                .type = .storage_buffer,
                .descriptor_count = 100,
            },
            .{
                .type = .storage_image,
                .descriptor_count = 100,
            },
        };
        const desc_pool = try device.createDescriptorPool(&.{
            .max_sets = 100,
            .pool_size_count = pool_sizes.len,
            .p_pool_sizes = &pool_sizes,
        }, null);
        errdefer device.destroyDescriptorPool(desc_pool, null);

        return .{
            .pool = desc_pool,
        };
    }

    pub fn reset(self: *@This(), device: *Device) !void {
        try device.resetDescriptorPool(self.pool, .{});
    }

    pub fn deinit(self: *@This(), device: *Device) void {
        device.destroyDescriptorPool(self.pool, null);
    }

    pub fn set_builder(self: *const @This()) DescriptorSet.Builder {
        return .{
            .pool = self.pool,
        };
    }
};

pub const DescriptorSet = struct {
    // not owned
    pool: vk.DescriptorPool,

    set: vk.DescriptorSet,
    layout: vk.DescriptorSetLayout,

    pub fn deinit(self: *@This(), device: *Device) void {
        device.destroyDescriptorSetLayout(self.layout, null);

        // NOTE: self.set is deinited from it's pool
        // try device.freeDescriptorSets(self.pool, 1, @ptrCast(&self.set));
    }

    pub const Builder = struct {
        pool: vk.DescriptorPool,
        layout_binding: std.ArrayListUnmanaged(vk.DescriptorSetLayoutBinding) = .{},
        desc_set_update: std.ArrayListUnmanaged(vk.WriteDescriptorSet) = .{},

        pub fn add(self: *@This(), binding: anytype) !void {
            try self.layout_binding.append(allocator, binding.layout_binding(@intCast(self.layout_binding.items.len)));

            // NOTE: undefined is written to in .build()
            try self.desc_set_update.append(allocator, binding.write_desc_set(@intCast(self.desc_set_update.items.len), undefined));
        }

        pub fn deinit(self: *@This()) void {
            self.layout_binding.deinit(allocator);
            self.desc_set_update.deinit(allocator);
        }

        pub fn build(self: *@This(), device: *Device) !DescriptorSet {
            const layouts = [_]vk.DescriptorSetLayout{try device.createDescriptorSetLayout(&.{
                .flags = .{},
                .binding_count = @intCast(self.layout_binding.items.len),
                .p_bindings = self.layout_binding.items.ptr,
            }, null)};
            errdefer device.destroyDescriptorSetLayout(layouts[0], null);

            var desc_set: vk.DescriptorSet = undefined;
            try device.allocateDescriptorSets(&.{
                .descriptor_pool = self.pool,
                .descriptor_set_count = @intCast(layouts.len),
                .p_set_layouts = &layouts,
            }, @ptrCast(&desc_set));

            // NOTE: overwriting undefined
            for (self.desc_set_update.items) |*s| {
                s.dst_set = desc_set;
            }

            device.updateDescriptorSets(@intCast(self.desc_set_update.items.len), self.desc_set_update.items.ptr, 0, null);

            return .{
                .pool = self.pool,
                .set = desc_set,
                .layout = layouts[0],
            };
        }
    };
};

pub const ComputePipeline = struct {
    pipeline: vk.Pipeline,
    layout: vk.PipelineLayout,

    const Args = struct {
        shader: []u32,
        desc_set_layouts: []const vk.DescriptorSetLayout,
    };

    pub fn new(device: *Device, v: Args) !@This() {
        const layout = try device.createPipelineLayout(&.{
            // .flags: PipelineLayoutCreateFlags = .{},
            .set_layout_count = @intCast(v.desc_set_layouts.len),
            .p_set_layouts = v.desc_set_layouts.ptr,
        }, null);
        errdefer device.destroyPipelineLayout(layout, null);

        const compute = try device.createShaderModule(&.{
            .code_size = v.shader.len * @sizeOf(u32),
            .p_code = @ptrCast(v.shader.ptr),
        }, null);
        defer device.destroyShaderModule(compute, null);

        const cpci = vk.ComputePipelineCreateInfo{
            .stage = .{
                .stage = .{
                    .compute_bit = true,
                },
                .module = compute,
                .p_name = "main",
            },
            .layout = layout,
            .base_pipeline_index = undefined,
        };

        var pipeline: vk.Pipeline = undefined;
        _ = try device.createComputePipelines(.null_handle, 1, @ptrCast(&cpci), null, @ptrCast(&pipeline));

        return .{
            .pipeline = pipeline,
            .layout = layout,
        };
    }

    pub fn deinit(self: *const @This(), device: *Device) void {
        device.destroyPipeline(self.pipeline, null);
        device.destroyPipelineLayout(self.layout, null);
    }
};

pub const GraphicsPipeline = struct {
    pipeline: vk.Pipeline,
    layout: vk.PipelineLayout,

    const Args = struct {
        vert: []u32,
        frag: []u32,
        pass: vk.RenderPass,
        desc_set_layouts: []const vk.DescriptorSetLayout,
    };

    pub fn new(device: *Device, v: Args) !@This() {
        const layout = try device.createPipelineLayout(&.{
            .flags = .{},
            .set_layout_count = @intCast(v.desc_set_layouts.len),
            .p_set_layouts = v.desc_set_layouts.ptr,
            .push_constant_range_count = 0,
            .p_push_constant_ranges = undefined,
        }, null);
        errdefer device.destroyPipelineLayout(layout, null);

        const vert = try device.createShaderModule(&.{
            .code_size = v.vert.len * @sizeOf(u32),
            .p_code = @ptrCast(v.vert.ptr),
        }, null);
        defer device.destroyShaderModule(vert, null);

        const frag = try device.createShaderModule(&.{
            .code_size = v.frag.len * @sizeOf(u32),
            .p_code = @ptrCast(v.frag.ptr),
        }, null);
        defer device.destroyShaderModule(frag, null);

        const pssci = [_]vk.PipelineShaderStageCreateInfo{
            .{
                .stage = .{ .vertex_bit = true },
                .module = vert,
                .p_name = "main",
            },
            .{
                .stage = .{ .fragment_bit = true },
                .module = frag,
                .p_name = "main",
            },
        };

        const pvisci = vk.PipelineVertexInputStateCreateInfo{
            // .vertex_binding_description_count = 0,
            // .p_vertex_binding_descriptions = @ptrCast(&Vertex.binding_description),
            // .vertex_attribute_description_count = Vertex.attribute_description.len,
            // .p_vertex_attribute_descriptions = &Vertex.attribute_description,
        };

        const piasci = vk.PipelineInputAssemblyStateCreateInfo{
            .topology = .triangle_list,
            .primitive_restart_enable = vk.FALSE,
        };

        const pvsci = vk.PipelineViewportStateCreateInfo{
            .viewport_count = 1,
            .p_viewports = undefined, // set in createCommandBuffers with cmdSetViewport
            .scissor_count = 1,
            .p_scissors = undefined, // set in createCommandBuffers with cmdSetScissor
        };

        const prsci = vk.PipelineRasterizationStateCreateInfo{
            .depth_clamp_enable = vk.FALSE,
            .rasterizer_discard_enable = vk.FALSE,
            .polygon_mode = .fill,
            .cull_mode = .{ .back_bit = false },
            .front_face = .clockwise,
            .depth_bias_enable = vk.FALSE,
            .depth_bias_constant_factor = 0,
            .depth_bias_clamp = 0,
            .depth_bias_slope_factor = 0,
            .line_width = 1,
        };

        const pmsci = vk.PipelineMultisampleStateCreateInfo{
            .rasterization_samples = .{ .@"1_bit" = true },
            .sample_shading_enable = vk.FALSE,
            .min_sample_shading = 1,
            .alpha_to_coverage_enable = vk.FALSE,
            .alpha_to_one_enable = vk.FALSE,
        };

        const pcbas = vk.PipelineColorBlendAttachmentState{
            .blend_enable = vk.FALSE,
            .src_color_blend_factor = .one,
            .dst_color_blend_factor = .zero,
            .color_blend_op = .add,
            .src_alpha_blend_factor = .one,
            .dst_alpha_blend_factor = .zero,
            .alpha_blend_op = .add,
            .color_write_mask = .{ .r_bit = true, .g_bit = true, .b_bit = true, .a_bit = true },
        };

        const pcbsci = vk.PipelineColorBlendStateCreateInfo{
            .logic_op_enable = vk.FALSE,
            .logic_op = .copy,
            .attachment_count = 1,
            .p_attachments = @ptrCast(&pcbas),
            .blend_constants = [_]f32{ 0, 0, 0, 0 },
        };

        const dynstate = [_]vk.DynamicState{ .viewport, .scissor };
        const pdsci = vk.PipelineDynamicStateCreateInfo{
            .flags = .{},
            .dynamic_state_count = dynstate.len,
            .p_dynamic_states = &dynstate,
        };

        const depth_stencil_info = vk.PipelineDepthStencilStateCreateInfo{
            .depth_test_enable = vk.TRUE,
            .depth_write_enable = vk.TRUE,
            .depth_compare_op = .less,
            .depth_bounds_test_enable = vk.FALSE,
            .stencil_test_enable = vk.FALSE,
            .front = .{
                .fail_op = .keep,
                .pass_op = .replace,
                .depth_fail_op = .keep,
                .compare_op = .always,
                .compare_mask = 0xFF,
                .write_mask = 0xFF,
                .reference = 1,
            },
            .back = .{
                .fail_op = .keep,
                .pass_op = .replace,
                .depth_fail_op = .keep,
                .compare_op = .always,
                .compare_mask = 0xFF,
                .write_mask = 0xFF,
                .reference = 1,
            },
            .min_depth_bounds = 0.0,
            .max_depth_bounds = 1.0,
        };

        const gpci = vk.GraphicsPipelineCreateInfo{
            .flags = .{},
            .stage_count = 2,
            .p_stages = &pssci,
            .p_vertex_input_state = &pvisci,
            .p_input_assembly_state = &piasci,
            .p_tessellation_state = null,
            .p_viewport_state = &pvsci,
            .p_rasterization_state = &prsci,
            .p_multisample_state = &pmsci,
            .p_depth_stencil_state = &depth_stencil_info,
            .p_color_blend_state = &pcbsci,
            .p_dynamic_state = &pdsci,
            .layout = layout,
            .render_pass = v.pass,
            .subpass = 0,
            .base_pipeline_handle = .null_handle,
            .base_pipeline_index = -1,
        };

        var pipeline: vk.Pipeline = undefined;
        _ = try device.createGraphicsPipelines(
            .null_handle,
            1,
            @ptrCast(&gpci),
            null,
            @ptrCast(&pipeline),
        );
        return .{ .pipeline = pipeline, .layout = layout };
    }

    pub fn deinit(self: *@This(), device: *Device) void {
        device.destroyPipeline(self.pipeline, null);
        device.destroyPipelineLayout(self.layout, null);
    }
};

pub const RenderPass = struct {
    pass: vk.RenderPass,

    const Args = struct {
        color_attachment_format: vk.Format,
    };

    pub fn new(device: *Device, v: Args) !@This() {
        const subpass = vk.SubpassDescription{
            .pipeline_bind_point = .graphics,
            .color_attachment_count = 1,
            .p_color_attachments = @ptrCast(&vk.AttachmentReference{
                .attachment = 0,
                .layout = .color_attachment_optimal,
            }),
            .p_depth_stencil_attachment = @ptrCast(&vk.AttachmentReference{
                .attachment = 1,
                .layout = .depth_stencil_attachment_optimal,
            }),
        };

        const color_attachment = vk.AttachmentDescription{
            .format = v.color_attachment_format,
            .samples = .{ .@"1_bit" = true },
            .load_op = .clear,
            .store_op = .store,
            .stencil_load_op = .dont_care,
            .stencil_store_op = .dont_care,
            .initial_layout = .undefined,
            .final_layout = .color_attachment_optimal,
        };

        const depth_attachment = vk.AttachmentDescription{
            .format = .d32_sfloat,
            .samples = .{ .@"1_bit" = true },
            .load_op = .clear,
            .store_op = .dont_care,
            .stencil_load_op = .dont_care,
            .stencil_store_op = .dont_care,
            .initial_layout = .undefined,
            .final_layout = .depth_stencil_attachment_optimal,
        };

        const attachments = [_]vk.AttachmentDescription{ color_attachment, depth_attachment };

        const deps = [_]vk.SubpassDependency{
            .{
                .src_subpass = vk.SUBPASS_EXTERNAL,
                .dst_subpass = 0,
                .src_stage_mask = .{
                    .color_attachment_output_bit = true,
                },
                .dst_stage_mask = .{
                    .color_attachment_output_bit = true,
                },
                // src_access_mask: AccessFlags = .{},
                .dst_access_mask = .{
                    .color_attachment_write_bit = true,
                },
                // dependency_flags: DependencyFlags = .{},
            },
            .{
                .src_subpass = vk.SUBPASS_EXTERNAL,
                .dst_subpass = 0,
                .src_stage_mask = .{
                    .early_fragment_tests_bit = true,
                },
                .dst_stage_mask = .{
                    .early_fragment_tests_bit = true,
                },
                // src_access_mask: AccessFlags = .{},
                .dst_access_mask = .{
                    .depth_stencil_attachment_write_bit = true,
                },
                // dependency_flags: DependencyFlags = .{},
            },
        };

        return .{
            .pass = try device.createRenderPass(&.{
                .attachment_count = @intCast(attachments.len),
                .p_attachments = &attachments,
                .subpass_count = 1,
                .p_subpasses = @ptrCast(&subpass),
                .dependency_count = @intCast(deps.len),
                .p_dependencies = &deps,
            }, null),
        };
    }

    pub fn deinit(self: *@This(), device: *Device) void {
        device.destroyRenderPass(self.pass, null);
    }
};

pub const Image = struct {
    size: u64,
    extent: vk.Extent3D,
    image: vk.Image,
    format: vk.Format,
    memory: vk.DeviceMemory,
    view: vk.ImageView,
    sampler: vk.Sampler,
    io: vk.DescriptorImageInfo,

    pub const BufferView = struct {
        buf: Buffer,

        pub fn deinit(self: *@This(), device: *Device) void {
            device.destroyBuffer(self.buf.buffer, null);
        }
    };

    pub const Args = struct {
        img_type: vk.ImageType,
        img_view_type: vk.ImageViewType,
        format: vk.Format,
        extent: vk.Extent3D,
        usage: vk.ImageUsageFlags = .{},
        view_aspect_mask: vk.ImageAspectFlags = .{},
    };

    pub fn new(ctx: *Engine.VulkanContext, v: Args) !@This() {
        const device = &ctx.device;

        const img = try device.createImage(&.{
            .image_type = v.img_type,
            .format = v.format,
            .extent = v.extent,
            .mip_levels = 1,
            .array_layers = 1,
            .samples = .{ .@"1_bit" = true },
            .tiling = .optimal,
            .usage = v.usage,
            .sharing_mode = .exclusive,
            .initial_layout = .undefined,
        }, null);
        errdefer device.destroyImage(img, null);

        const mem_reqs = device.getImageMemoryRequirements(img);
        const memory = try ctx.allocate(mem_reqs, .{ .device_local_bit = true });
        errdefer device.freeMemory(memory, null);
        try device.bindImageMemory(img, memory, 0);

        const view = try device.createImageView(&.{
            .image = img,
            .view_type = v.img_view_type,
            .format = v.format,
            .components = .{
                .r = .identity,
                .g = .identity,
                .b = .identity,
                .a = .identity,
            },
            .subresource_range = .{
                .aspect_mask = v.view_aspect_mask,
                .base_mip_level = 0,
                .level_count = 1,
                .base_array_layer = 0,
                .layer_count = 1,
            },
        }, null);
        errdefer device.destroyImageView(view, null);

        const sampler = try device.createSampler(&.{
            .flags = .{},
            .mag_filter = .nearest,
            .min_filter = .nearest,
            .mipmap_mode = .nearest,
            .address_mode_u = .clamp_to_edge,
            .address_mode_v = .clamp_to_edge,
            .address_mode_w = .clamp_to_edge,
            .mip_lod_bias = 0.0,
            .anisotropy_enable = 0,
            .max_anisotropy = 16.0,
            .compare_enable = 0,
            .compare_op = .never,
            .min_lod = 0.0,
            .max_lod = 1.0,
            .border_color = .int_opaque_black,
            .unnormalized_coordinates = 0,
        }, null);
        errdefer device.destroySampler(sampler, null);

        return .{
            .image = img,
            .memory = memory,
            .view = view,
            .size = mem_reqs.size,
            .sampler = sampler,
            .extent = v.extent,
            .format = v.format,
            .io = .{
                .sampler = sampler,
                .image_view = view,
                .image_layout = .general,
            },
        };
    }

    pub fn transition(self: *@This(), ctx: *Engine.VulkanContext, pool: vk.CommandPool, layout: vk.ImageLayout, new_layout: vk.ImageLayout) !void {
        const device = &ctx.device;

        var cmdbuf_handle: vk.CommandBuffer = undefined;
        try device.allocateCommandBuffers(&.{
            .command_pool = pool,
            .level = .primary,
            .command_buffer_count = 1,
        }, @ptrCast(&cmdbuf_handle));
        defer device.freeCommandBuffers(pool, 1, @ptrCast(&cmdbuf_handle));

        // OOF: so you are saying i could have been using a nicer api all this time? :mous
        const cmdbuf = Engine.VulkanContext.Api.CommandBuffer.init(cmdbuf_handle, device.wrapper);

        try cmdbuf.beginCommandBuffer(&.{
            .flags = .{ .one_time_submit_bit = true },
        });

        {
            transitionImage(cmdbuf.handle, device, self.image, layout, new_layout, ctx.graphics_queue.family);
        }

        try cmdbuf.endCommandBuffer();

        const si = vk.SubmitInfo{
            .command_buffer_count = 1,
            .p_command_buffers = (&cmdbuf.handle)[0..1],
            .p_wait_dst_stage_mask = undefined,
        };
        try device.queueSubmit(ctx.graphics_queue.handle, 1, @ptrCast(&si), .null_handle);
        try device.queueWaitIdle(ctx.graphics_queue.handle);
    }

    pub fn copy_to_host(self: *@This(), ctx: *Engine.VulkanContext, pool: vk.CommandPool, copy_extent: vk.Extent3D) ![]align(8) u8 {
        const unit_size: usize = switch (self.format) {
            .r16g16b16a16_sfloat => 4 * 2,
            .r8g8b8a8_srgb => 4,
            else => return error.UnknownImageFormat,
        };
        const size = copy_extent.width * copy_extent.height * unit_size;
        const staging_buffer = try ctx.device.createBuffer(&.{
            .size = size,
            .usage = .{ .transfer_dst_bit = true },
            .sharing_mode = .exclusive,
        }, null);
        defer ctx.device.destroyBuffer(staging_buffer, null);
        const staging_mem_reqs = ctx.device.getBufferMemoryRequirements(staging_buffer);
        const staging_memory = try ctx.allocate(staging_mem_reqs, .{ .host_visible_bit = true, .host_coherent_bit = true });
        defer ctx.device.freeMemory(staging_memory, null);
        try ctx.device.bindBufferMemory(staging_buffer, staging_memory, 0);

        try copyImageToBuffer(ctx, &ctx.device, pool, self.image, self.io.image_layout, staging_buffer, copy_extent);

        const cpu_memory = try allocator.allocWithOptions(u8, size, 8, null);
        errdefer allocator.free(cpu_memory);

        {
            const data = try ctx.device.mapMemory(staging_memory, 0, vk.WHOLE_SIZE, .{});
            defer ctx.device.unmapMemory(staging_memory);

            const mapped_memory: [*]u8 = @ptrCast(@alignCast(data));
            @memcpy(cpu_memory, mapped_memory[0..cpu_memory.len]);
        }

        return cpu_memory;
    }

    pub fn deinit(self: *@This(), device: *Device) void {
        device.destroyImageView(self.view, null);
        device.freeMemory(self.memory, null);
        device.destroyImage(self.image, null);
        device.destroySampler(self.sampler, null);
    }

    pub fn buffer(self: *@This(), device: *Device, v: struct {
        usage: vk.BufferUsageFlags = .{},
    }) !BufferView {
        const buf = try device.createBuffer(&.{
            .size = self.size,
            .usage = (vk.BufferUsageFlags{
                .storage_buffer_bit = true,
            }).merge(v.usage),
            .sharing_mode = .exclusive,
        }, null);
        errdefer device.destroyBuffer(buf, null);

        try device.bindBufferMemory(buf, self.memory, 0);

        return .{ .buf = .{
            .buffer = buf,
            .memory = undefined,
            .dbi = .{
                .buffer = buf,
                .offset = 0,
                .range = vk.WHOLE_SIZE,
            },
        } };
    }

    pub fn layout_binding(_: *@This(), index: u32) vk.DescriptorSetLayoutBinding {
        return .{
            .binding = index,
            .descriptor_type = .storage_image,
            .descriptor_count = 1,
            .stage_flags = .{
                .vertex_bit = true,
                .fragment_bit = true,
                .compute_bit = true,
            },
        };
    }

    pub fn write_desc_set(self: *@This(), binding: u32, desc_set: vk.DescriptorSet) vk.WriteDescriptorSet {
        return .{
            .dst_set = desc_set,
            .dst_binding = binding,
            .dst_array_element = 0,
            .descriptor_count = 1,
            .descriptor_type = .storage_image,
            .p_image_info = @ptrCast(&self.io),
            // OOF: ??
            .p_buffer_info = undefined,
            .p_texel_buffer_view = undefined,
        };
    }
};

pub const UniformBuffer = struct {
    // not owned
    uniform_buffer: []u8,
    buffer: vk.Buffer,
    memory: vk.DeviceMemory,
    dbi: vk.DescriptorBufferInfo,

    pub fn new(uniforms: []u8, ctx: *Engine.VulkanContext) !@This() {
        const device = &ctx.device;

        const buffer = try device.createBuffer(&.{
            .size = uniforms.len,
            .usage = .{
                .uniform_buffer_bit = true,
            },
            .sharing_mode = .exclusive,
        }, null);
        errdefer device.destroyBuffer(buffer, null);
        const mem_req = device.getBufferMemoryRequirements(buffer);
        const memory = try ctx.allocate(mem_req, .{
            .host_visible_bit = true,
            .host_coherent_bit = true,
        });
        errdefer device.freeMemory(memory, null);
        try device.bindBufferMemory(buffer, memory, 0);

        return .{
            .uniform_buffer = uniforms,
            .buffer = buffer,
            .memory = memory,
            .dbi = .{
                .buffer = buffer,
                .offset = 0,
                .range = vk.WHOLE_SIZE,
            },
        };
    }

    pub fn deinit(self: *@This(), device: *Device) void {
        device.destroyBuffer(self.buffer, null);
        device.freeMemory(self.memory, null);

        // not owned
        // allocator.free(self.uniform_buffer);
    }

    pub fn upload(self: *@This(), device: *Device) !void {
        const maybe_mapped = try device.mapMemory(self.memory, 0, self.uniform_buffer.len, .{});
        const mapped = maybe_mapped orelse return error.MappingMemoryFailed;
        defer device.unmapMemory(self.memory);

        @memcpy(@as([*]u8, @ptrCast(mapped)), self.uniform_buffer);
    }

    pub fn layout_binding(_: *@This(), index: u32) vk.DescriptorSetLayoutBinding {
        return .{
            .binding = index,
            .descriptor_type = .uniform_buffer,
            .descriptor_count = 1,
            .stage_flags = .{
                .vertex_bit = true,
                .fragment_bit = true,
                .compute_bit = true,
            },
        };
    }

    pub fn write_desc_set(self: *@This(), binding: u32, desc_set: vk.DescriptorSet) vk.WriteDescriptorSet {
        return .{
            .dst_set = desc_set,
            .dst_binding = binding,
            .dst_array_element = 0,
            .descriptor_count = 1,
            .descriptor_type = .uniform_buffer,
            .p_buffer_info = @ptrCast(&self.dbi),
            // OOF: ??
            .p_image_info = undefined,
            .p_texel_buffer_view = undefined,
        };
    }
};

pub const Buffer = struct {
    buffer: vk.Buffer,
    memory: vk.DeviceMemory,
    dbi: vk.DescriptorBufferInfo,

    const Args = struct {
        size: u64,
        usage: vk.BufferUsageFlags = .{},
    };

    pub fn new_initialized(ctx: *Engine.VulkanContext, v: Args, val: anytype, pool: vk.CommandPool) !@This() {
        const this = try @This().new(
            ctx,
            .{ .size = v.size, .usage = v.usage.merge(.{
                .transfer_dst_bit = true,
            }) },
        );

        const staging_buffer = try ctx.device.createBuffer(&.{
            .size = v.size,
            .usage = .{ .transfer_src_bit = true },
            .sharing_mode = .exclusive,
        }, null);
        defer ctx.device.destroyBuffer(staging_buffer, null);
        const staging_mem_reqs = ctx.device.getBufferMemoryRequirements(staging_buffer);
        const staging_memory = try ctx.allocate(staging_mem_reqs, .{ .host_visible_bit = true, .host_coherent_bit = true });
        defer ctx.device.freeMemory(staging_memory, null);
        try ctx.device.bindBufferMemory(staging_buffer, staging_memory, 0);

        {
            const data = try ctx.device.mapMemory(staging_memory, 0, vk.WHOLE_SIZE, .{});
            defer ctx.device.unmapMemory(staging_memory);

            const gpu_vertices: [*]@TypeOf(val) = @ptrCast(@alignCast(data));
            @memset(gpu_vertices[0 .. v.size / @as(u64, @sizeOf(@TypeOf(val)))], val);
        }

        try copyBuffer(ctx, &ctx.device, pool, this.buffer, staging_buffer, v.size);

        return this;
    }

    pub fn new(ctx: *Engine.VulkanContext, v: Args) !@This() {
        const device = &ctx.device;

        const buffer = try device.createBuffer(&.{
            .size = v.size,
            .usage = (vk.BufferUsageFlags{
                .storage_buffer_bit = true,
            }).merge(v.usage),
            .sharing_mode = .exclusive,
        }, null);
        errdefer device.destroyBuffer(buffer, null);

        const mem_reqs = device.getBufferMemoryRequirements(buffer);
        const memory = try ctx.allocate(mem_reqs, .{ .device_local_bit = true });
        errdefer device.freeMemory(memory, null);
        try device.bindBufferMemory(buffer, memory, 0);

        return .{
            .buffer = buffer,
            .memory = memory,
            .dbi = .{
                .buffer = buffer,
                .offset = 0,
                .range = vk.WHOLE_SIZE,
            },
        };
    }

    pub fn deinit(self: *@This(), device: *Device) void {
        device.destroyBuffer(self.buffer, null);
        device.freeMemory(self.memory, null);
    }

    pub fn layout_binding(_: *@This(), index: u32) vk.DescriptorSetLayoutBinding {
        return .{
            .binding = index,
            .descriptor_type = .storage_buffer,
            .descriptor_count = 1,
            .stage_flags = .{
                .vertex_bit = true,
                .fragment_bit = true,
                .compute_bit = true,
            },
        };
    }

    pub fn write_desc_set(self: *@This(), binding: u32, desc_set: vk.DescriptorSet) vk.WriteDescriptorSet {
        return .{
            .dst_set = desc_set,
            .dst_binding = binding,
            .dst_array_element = 0,
            .descriptor_count = 1,
            .descriptor_type = .storage_buffer,
            .p_buffer_info = @ptrCast(&self.dbi),
            // OOF: ??
            .p_image_info = undefined,
            .p_texel_buffer_view = undefined,
        };
    }
};

pub const CmdBuffer = struct {
    bufs: []vk.CommandBuffer,

    // borrowed
    pool: vk.CommandPool,

    pub const Args = struct {
        pool: vk.CommandPool,
        size: usize,
    };

    pub fn init(device: *Device, v: Args) !@This() {
        const cmdbufs = try allocator.alloc(vk.CommandBuffer, v.size);
        errdefer allocator.free(cmdbufs);

        try device.allocateCommandBuffers(&.{
            .command_pool = v.pool,
            .level = .primary,
            .command_buffer_count = @intCast(cmdbufs.len),
        }, cmdbufs.ptr);
        errdefer device.freeCommandBuffers(v.pool, @intCast(cmdbufs.len), cmdbufs.ptr);

        return .{ .bufs = cmdbufs, .pool = v.pool };
    }

    pub fn deinit(self: *@This(), device: *Device) void {
        device.freeCommandBuffers(self.pool, @truncate(self.bufs.len), self.bufs.ptr);
        allocator.free(self.bufs);
    }

    pub fn begin(self: *@This(), device: *Device) !void {
        for (self.bufs) |cmdbuf| {
            try device.beginCommandBuffer(cmdbuf, &.{});
        }
    }

    pub fn end(self: *@This(), device: *Device) !void {
        for (self.bufs) |cmdbuf| {
            try device.endCommandBuffer(cmdbuf);
        }
    }

    pub fn memBarrier(self: *@This(), device: *Device, v: struct {
        src: vk.PipelineStageFlags = .{
            .compute_shader_bit = true,
        },
        dst: vk.PipelineStageFlags = .{
            .compute_shader_bit = true,
        },
    }) void {
        for (self.bufs) |cmdbuf| {
            device.cmdPipelineBarrier(cmdbuf, v.src, v.dst, .{}, 1, &[_]vk.MemoryBarrier{.{
                .src_access_mask = .{
                    .shader_read_bit = true,
                    .shader_write_bit = true,
                },
                .dst_access_mask = .{
                    .shader_read_bit = true,
                    .shader_write_bit = true,
                },
            }}, 0, null, 0, null);
        }

        // device.cmdPipelineBarrier(cmdbuf, .{
        //     .compute_shader_bit = true,
        // }, .{
        //     .vertex_input_bit = true,
        //     .vertex_shader_bit = true,
        // }, .{}, 1, &[_]vk.MemoryBarrier{.{
        //     .src_access_mask = .{
        //         .shader_write_bit = true,
        //     },
        //     .dst_access_mask = .{
        //         .shader_read_bit = true,
        //         .vertex_attribute_read_bit = true,
        //     },
        // }}, 1, (&[_]vk.BufferMemoryBarrier{.{
        //     .src_queue_family_index = ctx.graphics_queue.family,
        //     .dst_queue_family_index = ctx.graphics_queue.family,
        //     .buffer = vertex_buffer.buffer,
        //     .offset = 0,
        //     .size = vk.WHOLE_SIZE,
        //     .src_access_mask = .{
        //         .shader_write_bit = true,
        //     },
        //     .dst_access_mask = .{
        //         .vertex_attribute_read_bit = true,
        //         .shader_read_bit = true,
        //     },
        // }}), 0, null);
    }

    pub fn renderPass(self: *@This(), device: *Device, v: struct {
        extent: vk.Extent2D,
        pipeline: GraphicsPipeline,
        desc_set: vk.DescriptorSet,
        pass: vk.RenderPass,
        clear_color: [4]f32,
        framebuffers: []vk.Framebuffer,
    }) void {
        // - [Depth buffering - Vulkan Tutorial](https://vulkan-tutorial.com/Depth_buffering)
        // - [Setting up depth buffer - Vulkan Guide](https://vkguide.dev/docs/chapter-3/depth_buffer/)
        const clear = [_]vk.ClearValue{
            .{
                .color = .{
                    .float_32 = v.clear_color,
                },
            },
            .{
                .depth_stencil = .{
                    .depth = 1.0,
                    .stencil = 0,
                },
            },
        };

        for (self.bufs, v.framebuffers) |cmdbuf, framebuffer| {
            device.cmdSetViewport(cmdbuf, 0, 1, @ptrCast(&vk.Viewport{
                .x = 0,
                .y = 0,
                .width = @floatFromInt(v.extent.width),
                .height = @floatFromInt(v.extent.height),
                .min_depth = 0,
                .max_depth = 1,
            }));
            device.cmdSetScissor(cmdbuf, 0, 1, @ptrCast(&vk.Rect2D{
                .offset = .{ .x = 0, .y = 0 },
                .extent = v.extent,
            }));

            device.cmdBeginRenderPass(cmdbuf, &.{
                .render_pass = v.pass,
                .framebuffer = framebuffer,
                .render_area = .{
                    .offset = .{ .x = 0, .y = 0 },
                    .extent = v.extent,
                },
                .clear_value_count = clear.len,
                .p_clear_values = &clear,
            }, .@"inline");

            device.cmdBindPipeline(cmdbuf, .graphics, v.pipeline.pipeline);
            device.cmdBindDescriptorSets(cmdbuf, .graphics, v.pipeline.layout, 0, 1, @ptrCast(&v.desc_set), 0, null);
            device.cmdDraw(cmdbuf, 6, 1, 0, 0);

            device.cmdEndRenderPass(cmdbuf);
        }
    }

    pub fn bindCompute(self: *@This(), device: *Device, v: struct {
        pipeline: ComputePipeline,
        desc_set: vk.DescriptorSet,
    }) void {
        for (self.bufs) |cmdbuf| {
            device.cmdBindPipeline(cmdbuf, .compute, v.pipeline.pipeline);
            device.cmdBindDescriptorSets(cmdbuf, .compute, v.pipeline.layout, 0, 1, @ptrCast(&v.desc_set), 0, null);
        }
    }

    pub fn transitionImg(self: *@This(), device: *Device, v: struct {
        image: vk.Image,
        layout: vk.ImageLayout,
        new_layout: vk.ImageLayout,
        queue_family_index: u32,
    }) void {
        for (self.bufs) |cmdbuf| {
            transitionImage(cmdbuf, device, v.image, v.layout, v.new_layout, v.queue_family_index);
        }
    }

    pub fn transitionSwapchain(self: *@This(), device: *Device, v: struct {
        layout: vk.ImageLayout,
        new_layout: vk.ImageLayout,
        queue_family_index: u32,
        swapchain: *const Swapchain,
    }) void {
        for (self.bufs, v.swapchain.swap_images) |cmdbuf, img| {
            transitionImage(cmdbuf, device, img.image, v.layout, v.new_layout, v.queue_family_index);
        }
    }

    pub fn blitIntoImage(self: *@This(), device: *Device, v: struct {
        typ: vk.ImageAspectFlags = .{ .color_bit = true },
        image: vk.Image,
        size: vk.Extent2D,
        target_image: vk.Image,
        target_size: vk.Extent2D,
    }) void {
        const blit = [_]vk.ImageBlit2{.{
            .src_subresource = .{
                .aspect_mask = v.typ,
                .mip_level = 0,
                .base_array_layer = 0,
                .layer_count = 1,
            },
            .src_offsets = .{
                .{ .x = 0, .y = 0, .z = 0 },
                .{ .x = @intCast(v.size.width), .y = @intCast(v.size.height), .z = 1 },
            },
            .dst_subresource = .{
                .aspect_mask = v.typ,
                .mip_level = 0,
                .base_array_layer = 0,
                .layer_count = 1,
            },
            .dst_offsets = .{
                .{ .x = 0, .y = 0, .z = 0 },
                .{ .x = @intCast(v.target_size.width), .y = @intCast(v.target_size.height), .z = 1 },
            },
        }};

        const blitinfo = vk.BlitImageInfo2{
            .src_image = v.image,
            .src_image_layout = .transfer_src_optimal,
            .dst_image = v.target_image,
            .dst_image_layout = .transfer_dst_optimal,
            .region_count = blit.len,
            .p_regions = &blit,
            .filter = .linear,
        };

        for (self.bufs) |cmdbuf| {
            device.cmdBlitImage2(cmdbuf, &blitinfo);
        }
    }

    pub fn blitIntoSwapchain(self: *@This(), device: *Device, v: struct {
        typ: vk.ImageAspectFlags = .{ .color_bit = true },
        image: vk.Image,
        size: vk.Extent2D,
        swapchain: *const Swapchain,
    }) void {
        const blit = [_]vk.ImageBlit2{.{
            .src_subresource = .{
                .aspect_mask = v.typ,
                .mip_level = 0,
                .base_array_layer = 0,
                .layer_count = 1,
            },
            .src_offsets = .{
                .{ .x = 0, .y = 0, .z = 0 },
                .{ .x = @intCast(v.size.width), .y = @intCast(v.size.height), .z = 1 },
            },
            .dst_subresource = .{
                .aspect_mask = v.typ,
                .mip_level = 0,
                .base_array_layer = 0,
                .layer_count = 1,
            },
            .dst_offsets = .{
                .{ .x = 0, .y = 0, .z = 0 },
                .{ .x = @intCast(v.swapchain.extent.width), .y = @intCast(v.swapchain.extent.height), .z = 1 },
            },
        }};

        for (self.bufs, v.swapchain.swap_images) |cmdbuf, img| {
            const blitinfo = vk.BlitImageInfo2{
                .src_image = v.image,
                .src_image_layout = .transfer_src_optimal,
                .dst_image = img.image,
                .dst_image_layout = .transfer_dst_optimal,
                .region_count = blit.len,
                .p_regions = &blit,
                .filter = .linear,
            };
            device.cmdBlitImage2(cmdbuf, &blitinfo);
        }
    }

    pub fn drawIntoSwapchain(self: *@This(), device: *Device, v: struct {
        typ: vk.ImageAspectFlags = .{ .color_bit = true },
        image: vk.Image,
        image_layout: vk.ImageLayout,
        size: vk.Extent2D,
        swapchain: *const Swapchain,
        queue_family: u32,
    }) void {
        self.transitionImg(device, .{
            .image = v.image,
            .layout = v.image_layout,
            .new_layout = .transfer_src_optimal,
            .queue_family_index = v.queue_family,
        });

        self.transitionSwapchain(device, .{
            .layout = .undefined,
            .new_layout = .transfer_dst_optimal,
            .queue_family_index = v.queue_family,
            .swapchain = v.swapchain,
        });
        self.blitIntoSwapchain(device, .{
            .typ = v.typ,
            .image = v.image,
            .size = v.swapchain.extent,
            .swapchain = v.swapchain,
        });
        self.transitionSwapchain(device, .{
            .layout = .transfer_dst_optimal,
            .new_layout = .color_attachment_optimal,
            .queue_family_index = v.queue_family,
            .swapchain = v.swapchain,
        });

        self.transitionImg(device, .{
            .image = v.image,
            .layout = .transfer_src_optimal,
            .new_layout = v.image_layout,
            .queue_family_index = v.queue_family,
        });
    }

    pub fn drawIntoImage(self: *@This(), device: *Device, v: struct {
        typ: vk.ImageAspectFlags = .{ .color_bit = true },
        image: vk.Image,
        image_layout: vk.ImageLayout,
        size: vk.Extent2D,
        target_image: vk.Image,
        target_image_layout: vk.ImageLayout,
        target_size: vk.Extent2D,
        queue_family: u32,
    }) void {
        self.transitionImg(device, .{
            .image = v.image,
            .layout = v.image_layout,
            .new_layout = .transfer_src_optimal,
            .queue_family_index = v.queue_family,
        });

        self.transitionImg(device, .{
            .image = v.target_image,
            .layout = v.target_image_layout,
            .new_layout = .transfer_dst_optimal,
            .queue_family_index = v.queue_family,
        });
        self.blitIntoImage(device, .{
            .typ = v.typ,
            .image = v.image,
            .size = v.size,
            .target_image = v.target_image,
            .target_size = v.target_size,
        });
        self.transitionImg(device, .{
            .image = v.target_image,
            .layout = .transfer_dst_optimal,
            .new_layout = v.target_image_layout,
            .queue_family_index = v.queue_family,
        });

        self.transitionImg(device, .{
            .image = v.image,
            .layout = .transfer_src_optimal,
            .new_layout = v.image_layout,
            .queue_family_index = v.queue_family,
        });
    }

    pub fn dispatch(self: *@This(), device: *Device, v: struct { x: u32, y: u32 = 1, z: u32 = 1 }) void {
        for (self.bufs) |cmdbuf| {
            device.cmdDispatch(cmdbuf, v.x, v.y, v.z);
        }
    }
};

pub const Framebuffer = struct {
    bufs: []vk.Framebuffer,

    pub const Args = struct {
        pass: vk.RenderPass,
        extent: vk.Extent2D,
        swap_images: []Swapchain.SwapImage,
        depth: vk.ImageView,
    };

    pub fn init(device: *Device, v: Args) !@This() {
        const framebuffers = try allocator.alloc(vk.Framebuffer, v.swap_images.len);
        errdefer allocator.free(framebuffers);

        var i: usize = 0;
        errdefer for (framebuffers[0..i]) |fb| device.destroyFramebuffer(fb, null);
        for (framebuffers) |*fb| {
            const attachments = [_]vk.ImageView{ v.swap_images[i].view, v.depth };
            fb.* = try device.createFramebuffer(&.{
                .render_pass = v.pass,
                .attachment_count = attachments.len,
                .p_attachments = &attachments,
                .width = v.extent.width,
                .height = v.extent.height,
                .layers = 1,
            }, null);
            i += 1;
        }

        return .{ .bufs = framebuffers };
    }

    pub fn deinit(self: *@This(), device: *Device) void {
        for (self.bufs) |fb| device.destroyFramebuffer(fb, null);
        allocator.free(self.bufs);
    }
};

pub const Swapchain = struct {
    surface_format: vk.SurfaceFormatKHR,
    present_mode: vk.PresentModeKHR,
    extent: vk.Extent2D,
    handle: vk.SwapchainKHR,

    swap_images: []SwapImage,
    image_index: u32,
    next_image_acquired: vk.Semaphore,

    pub const PresentState = enum {
        optimal,
        suboptimal,
    };

    pub const Args = struct {
        prefer_present_mode: ?vk.PresentModeKHR = null,
    };

    pub fn init(ctx: *Engine.VulkanContext, extent: vk.Extent2D, args: Args) !Swapchain {
        return try initRecycle(ctx, .null_handle, extent, args);
    }

    pub fn initRecycle(ctx: *Engine.VulkanContext, old_handle: vk.SwapchainKHR, extent: vk.Extent2D, args: Args) !Swapchain {
        const caps = try ctx.instance.getPhysicalDeviceSurfaceCapabilitiesKHR(ctx.pdev, ctx.surface);
        const actual_extent = blk: {
            if (caps.current_extent.width != 0xFFFF_FFFF) {
                break :blk caps.current_extent;
            } else {
                break :blk vk.Extent2D{
                    .width = std.math.clamp(
                        extent.width,
                        caps.min_image_extent.width,
                        caps.max_image_extent.width,
                    ),
                    .height = std.math.clamp(
                        extent.height,
                        caps.min_image_extent.height,
                        caps.max_image_extent.height,
                    ),
                };
            }
        };
        if (actual_extent.width == 0 or actual_extent.height == 0) {
            return error.InvalidSurfaceDimensions;
        }

        const surface_format = blk: {
            const preferred = vk.SurfaceFormatKHR{
                // we use non-gamma corrected buffer (non-srgb)
                // so that dumping image using imagemagick and displaying to screen does not have weird gamma issues
                // i dug for about an hour and still can't figure out why this works.
                // - ig copying to srgb image applies gamma encoding?
                // - can't figure out if dumping and loading images using imagemagick also does gamma encoding/decoding
                // - do i load random images in unorm images?
                //   - does that do gamma decoding?
                // - do different image viewrs do some gamma encode/decode when loading from png?
                .format = .b8g8r8a8_unorm,
                .color_space = .srgb_nonlinear_khr,
            };

            const surface_formats = try ctx.instance.getPhysicalDeviceSurfaceFormatsAllocKHR(ctx.pdev, ctx.surface, allocator);
            defer allocator.free(surface_formats);

            for (surface_formats) |sfmt| {
                if (std.meta.eql(sfmt, preferred)) {
                    break :blk preferred;
                }
            }

            break :blk surface_formats[0]; // There must always be at least one supported surface format
        };
        const present_mode = blk: {
            const present_modes = try ctx.instance.getPhysicalDeviceSurfacePresentModesAllocKHR(ctx.pdev, ctx.surface, allocator);
            defer allocator.free(present_modes);

            if (args.prefer_present_mode) |mode| {
                if (std.mem.indexOfScalar(vk.PresentModeKHR, present_modes, mode) != null) {
                    break :blk mode;
                }
            }

            const preferred = [_]vk.PresentModeKHR{
                .mailbox_khr,
                .immediate_khr,
            };

            for (preferred) |mode| {
                if (std.mem.indexOfScalar(vk.PresentModeKHR, present_modes, mode) != null) {
                    break :blk mode;
                }
            }

            break :blk .fifo_khr;
        };

        var image_count = caps.min_image_count + 1;
        if (caps.max_image_count > 0) {
            image_count = @min(image_count, caps.max_image_count);
        }

        const qfi = [_]u32{ ctx.graphics_queue.family, ctx.present_queue.family };
        const sharing_mode: vk.SharingMode = if (ctx.graphics_queue.family != ctx.present_queue.family)
            .concurrent
        else
            .exclusive;

        const handle = try ctx.device.createSwapchainKHR(&.{
            .surface = ctx.surface,
            .min_image_count = image_count,
            .image_format = surface_format.format,
            .image_color_space = surface_format.color_space,
            .image_extent = actual_extent,
            .image_array_layers = 1,
            .image_usage = .{ .color_attachment_bit = true, .transfer_dst_bit = true },
            .image_sharing_mode = sharing_mode,
            .queue_family_index_count = qfi.len,
            .p_queue_family_indices = &qfi,
            .pre_transform = caps.current_transform,
            .composite_alpha = .{ .opaque_bit_khr = true },
            .present_mode = present_mode,
            .clipped = vk.TRUE,
            .old_swapchain = old_handle,
        }, null);
        errdefer ctx.device.destroySwapchainKHR(handle, null);

        if (old_handle != .null_handle) {
            // Apparently, the old swapchain handle still needs to be destroyed after recreating.
            ctx.device.destroySwapchainKHR(old_handle, null);
        }

        const swap_images = blk: {
            const images = try ctx.device.getSwapchainImagesAllocKHR(handle, allocator);
            defer allocator.free(images);

            const swap_images = try allocator.alloc(SwapImage, images.len);
            errdefer allocator.free(swap_images);

            var i: usize = 0;
            errdefer for (swap_images[0..i]) |si| si.deinit(&ctx.device);
            for (images) |image| {
                swap_images[i] = try SwapImage.init(&ctx.device, image, surface_format.format);
                i += 1;
            }

            break :blk swap_images;
        };
        errdefer {
            for (swap_images) |si| si.deinit(&ctx.device);
            allocator.free(swap_images);
        }

        var next_image_acquired = try ctx.device.createSemaphore(&.{}, null);
        errdefer ctx.device.destroySemaphore(next_image_acquired, null);

        const result = try ctx.device.acquireNextImageKHR(handle, std.math.maxInt(u64), next_image_acquired, .null_handle);
        if (result.result != .success) {
            return error.ImageAcquireFailed;
        }

        std.mem.swap(vk.Semaphore, &swap_images[result.image_index].image_acquired, &next_image_acquired);
        return Swapchain{
            .surface_format = surface_format,
            .present_mode = present_mode,
            .extent = actual_extent,
            .handle = handle,
            .swap_images = swap_images,
            .image_index = result.image_index,
            .next_image_acquired = next_image_acquired,
        };
    }

    fn deinitExceptSwapchain(self: Swapchain, device: *Engine.VulkanContext.Api.Device) void {
        for (self.swap_images) |si| si.deinit(device);
        allocator.free(self.swap_images);
        device.destroySemaphore(self.next_image_acquired, null);
    }

    pub fn waitForAllFences(self: Swapchain, device: *Engine.VulkanContext.Api.Device) !void {
        for (self.swap_images) |si| si.waitForFence(device) catch {};
    }

    pub fn deinit(self: Swapchain, device: *Engine.VulkanContext.Api.Device) void {
        self.deinitExceptSwapchain(device);
        device.destroySwapchainKHR(self.handle, null);
    }

    pub fn recreate(self: *Swapchain, ctx: *Engine.VulkanContext, new_extent: vk.Extent2D, args: Args) !void {
        const old_handle = self.handle;
        self.deinitExceptSwapchain();
        self.* = try initRecycle(ctx, old_handle, new_extent, .{
            .prefer_present_mode = args.prefer_present_mode orelse self.present_mode,
        });
    }

    pub fn currentImage(self: Swapchain) vk.Image {
        return self.swap_images[self.image_index].image;
    }

    pub fn currentSwapImage(self: Swapchain) *const SwapImage {
        return &self.swap_images[self.image_index];
    }

    pub fn present(self: *Swapchain, cmdbufs: []const vk.CommandBuffer, ctx: *Engine.VulkanContext, uniforms: anytype) !PresentState {
        // Simple method:
        // 1) Acquire next image
        // 2) Wait for and reset fence of the acquired image
        // 3) Submit command buffer with fence of acquired image,
        //    dependendent on the semaphore signalled by the first step.
        // 4) Present current frame, dependent on semaphore signalled by previous step
        // Problem: This way we can't reference the current image while rendering.
        // Better method: Shuffle the steps around such that acquire next image is the last step,
        // leaving the swapchain in a state with the current image.
        // 1) Wait for and reset fence of current image
        // 2) Submit command buffer, signalling fence of current image and dependent on
        //    the semaphore signalled by step 4.
        // 3) Present current frame, dependent on semaphore signalled by the submit
        // 4) Acquire next image, signalling its semaphore
        // One problem that arises is that we can't know beforehand which semaphore to signal,
        // so we keep an extra auxilery semaphore that is swapped around

        // Step 1: Make sure the current frame has finished rendering
        const current = self.currentSwapImage();
        try current.waitForFence(&ctx.device);
        try ctx.device.resetFences(1, @ptrCast(&current.frame_fence));

        try uniforms.upload(&ctx.device);

        // Step 2: Submit the command buffer
        const wait_stage = [_]vk.PipelineStageFlags{.{ .top_of_pipe_bit = true }};
        try ctx.device.queueSubmit(ctx.graphics_queue.handle, 1, &[_]vk.SubmitInfo{.{
            .wait_semaphore_count = 1,
            .p_wait_semaphores = @ptrCast(&current.image_acquired),
            .p_wait_dst_stage_mask = &wait_stage,
            .command_buffer_count = @intCast(cmdbufs.len),
            .p_command_buffers = cmdbufs.ptr,
            .signal_semaphore_count = 1,
            .p_signal_semaphores = @ptrCast(&current.render_finished),
        }}, current.frame_fence);

        // Step 3: Present the current frame
        _ = try ctx.device.queuePresentKHR(ctx.present_queue.handle, &.{
            .wait_semaphore_count = 1,
            .p_wait_semaphores = @ptrCast(&current.render_finished),
            .swapchain_count = 1,
            .p_swapchains = @ptrCast(&self.handle),
            .p_image_indices = @ptrCast(&self.image_index),
        });

        // Step 4: Acquire next frame
        const result = try ctx.device.acquireNextImageKHR(
            self.handle,
            std.math.maxInt(u64),
            self.next_image_acquired,
            .null_handle,
        );

        std.mem.swap(vk.Semaphore, &self.swap_images[result.image_index].image_acquired, &self.next_image_acquired);
        self.image_index = result.image_index;

        return switch (result.result) {
            .success => .optimal,
            .suboptimal_khr => .suboptimal,
            else => unreachable,
        };
    }

    const SwapImage = struct {
        image: vk.Image,
        view: vk.ImageView,
        image_acquired: vk.Semaphore,
        render_finished: vk.Semaphore,
        frame_fence: vk.Fence,

        fn init(device: *Engine.VulkanContext.Api.Device, image: vk.Image, format: vk.Format) !SwapImage {
            const view = try device.createImageView(&.{
                .image = image,
                .view_type = .@"2d",
                .format = format,
                .components = .{ .r = .identity, .g = .identity, .b = .identity, .a = .identity },
                .subresource_range = .{
                    .aspect_mask = .{ .color_bit = true },
                    .base_mip_level = 0,
                    .level_count = 1,
                    .base_array_layer = 0,
                    .layer_count = 1,
                },
            }, null);
            errdefer device.destroyImageView(view, null);

            const image_acquired = try device.createSemaphore(&.{}, null);
            errdefer device.destroySemaphore(image_acquired, null);

            const render_finished = try device.createSemaphore(&.{}, null);
            errdefer device.destroySemaphore(render_finished, null);

            const frame_fence = try device.createFence(&.{ .flags = .{ .signaled_bit = true } }, null);
            errdefer device.destroyFence(frame_fence, null);

            return SwapImage{
                .image = image,
                .view = view,
                .image_acquired = image_acquired,
                .render_finished = render_finished,
                .frame_fence = frame_fence,
            };
        }

        fn deinit(self: SwapImage, device: *Engine.VulkanContext.Api.Device) void {
            self.waitForFence(device) catch return;
            device.destroyImageView(self.view, null);
            device.destroySemaphore(self.image_acquired, null);
            device.destroySemaphore(self.render_finished, null);
            device.destroyFence(self.frame_fence, null);
        }

        fn waitForFence(self: SwapImage, device: *Engine.VulkanContext.Api.Device) !void {
            _ = try device.waitForFences(1, @ptrCast(&self.frame_fence), vk.TRUE, std.math.maxInt(u64));
        }
    };
};

pub fn transitionImage(
    cmdbuf: vk.CommandBuffer,
    device: *Device,
    image: vk.Image,
    layout: vk.ImageLayout,
    new_layout: vk.ImageLayout,
    queue_family_index: u32,
) void {
    const image_barrier = [_]vk.ImageMemoryBarrier2{.{
        .src_stage_mask = .{ .all_commands_bit = true },
        .src_access_mask = .{ .memory_write_bit = false },
        .dst_stage_mask = .{ .all_commands_bit = true },
        .dst_access_mask = .{
            .memory_write_bit = false,
            .memory_read_bit = false,
        },
        .old_layout = layout,
        .new_layout = new_layout,
        .subresource_range = .{
            .aspect_mask = .{ .color_bit = true },
            .base_mip_level = 0,
            .level_count = 1,
            .base_array_layer = 0,
            .layer_count = 1,
        },
        .image = image,
        .src_queue_family_index = queue_family_index,
        .dst_queue_family_index = queue_family_index,
    }};

    device.cmdPipelineBarrier2(cmdbuf, &.{
        .image_memory_barrier_count = image_barrier.len,
        .p_image_memory_barriers = &image_barrier,
    });
}

pub fn copyBuffer(
    ctx: *Engine.VulkanContext,
    device: *Device,
    pool: vk.CommandPool,
    dst: vk.Buffer,
    src: vk.Buffer,
    size: vk.DeviceSize,
) !void {
    var cmdbuf_handle: vk.CommandBuffer = undefined;
    try device.allocateCommandBuffers(&.{
        .command_pool = pool,
        .level = .primary,
        .command_buffer_count = 1,
    }, @ptrCast(&cmdbuf_handle));
    defer device.freeCommandBuffers(pool, 1, @ptrCast(&cmdbuf_handle));

    const cmdbuf = Engine.VulkanContext.Api.CommandBuffer.init(cmdbuf_handle, device.wrapper);

    try cmdbuf.beginCommandBuffer(&.{
        .flags = .{ .one_time_submit_bit = true },
    });

    const region = vk.BufferCopy{
        .src_offset = 0,
        .dst_offset = 0,
        .size = size,
    };
    cmdbuf.copyBuffer(src, dst, 1, @ptrCast(&region));

    try cmdbuf.endCommandBuffer();

    const si = vk.SubmitInfo{
        .command_buffer_count = 1,
        .p_command_buffers = (&cmdbuf.handle)[0..1],
        .p_wait_dst_stage_mask = undefined,
    };
    try device.queueSubmit(ctx.graphics_queue.handle, 1, @ptrCast(&si), .null_handle);
    try device.queueWaitIdle(ctx.graphics_queue.handle);
}

pub fn copyImageToBuffer(
    ctx: *Engine.VulkanContext,
    device: *Device,
    pool: vk.CommandPool,
    src: vk.Image,
    src_layout: vk.ImageLayout,
    dst: vk.Buffer,
    extent: vk.Extent3D,
) !void {
    var cmdbuf_handle: vk.CommandBuffer = undefined;
    try device.allocateCommandBuffers(&.{
        .command_pool = pool,
        .level = .primary,
        .command_buffer_count = 1,
    }, @ptrCast(&cmdbuf_handle));
    defer device.freeCommandBuffers(pool, 1, @ptrCast(&cmdbuf_handle));

    const cmdbuf = Engine.VulkanContext.Api.CommandBuffer.init(cmdbuf_handle, device.wrapper);

    try cmdbuf.beginCommandBuffer(&.{
        .flags = .{ .one_time_submit_bit = true },
    });

    const region = vk.BufferImageCopy{
        .buffer_offset = 0,
        .buffer_row_length = extent.width,
        .buffer_image_height = extent.height,
        .image_subresource = .{
            .aspect_mask = .{ .color_bit = true },
            .mip_level = 0,
            .base_array_layer = 0,
            .layer_count = 1,
        },
        .image_offset = .{ .x = 0, .y = 0, .z = 0 },
        .image_extent = extent,
    };
    cmdbuf.copyImageToBuffer(src, src_layout, dst, 1, @ptrCast(&region));

    try cmdbuf.endCommandBuffer();

    const si = vk.SubmitInfo{
        .command_buffer_count = 1,
        .p_command_buffers = (&cmdbuf.handle)[0..1],
        .p_wait_dst_stage_mask = undefined,
    };
    try device.queueSubmit(ctx.graphics_queue.handle, 1, @ptrCast(&si), .null_handle);
    try device.queueWaitIdle(ctx.graphics_queue.handle);
}

pub fn dump_image_to_file(
    image: *Image,
    ctx: *Engine.VulkanContext,
    pool: vk.CommandPool,
    copy_extent: vk.Extent2D,
    path: []const u8,
) !void {
    const buf = try image.copy_to_host(ctx, pool, .{
        .width = copy_extent.width,
        .height = copy_extent.height,
        .depth = 1,
    });
    defer allocator.free(buf);

    const pixels = try allocator.alloc(f32, copy_extent.width * copy_extent.height * 4);
    defer allocator.free(pixels);

    switch (image.format) {
        .r16g16b16a16_sfloat => {
            const src = std.mem.bytesAsSlice(f16, buf);
            for (src[0..pixels.len], 0..) |f, i| {
                pixels[i] = @floatCast(f);
            }
        },
        .r8g8b8a8_srgb => {
            const src = std.mem.bytesAsSlice(u8, buf);
            for (src[0..pixels.len], 0..) |f, i| {
                pixels[i] = @as(f32, @floatFromInt(f)) / 255.0;
            }
        },
        else => return error.UnknownImageFormat,
    }

    const blob = try utils.ImageMagick.encode_rgba_image(pixels, copy_extent.width, copy_extent.height);
    defer allocator.free(blob);

    const ts = std.time.timestamp();
    const filename = try std.fmt.allocPrint(allocator, "{d}.png", .{ts});
    defer allocator.free(filename);

    try std.fs.cwd().makePath(path);

    const joined_path = try std.fs.path.join(allocator, &[_][]const u8{ path, filename });
    defer allocator.free(joined_path);

    const file = try std.fs.cwd().createFile(joined_path, .{});
    defer file.close();
    try file.writeAll(blob);
}
