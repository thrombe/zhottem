#version 460

#include <common.glsl>
#include <uniforms.glsl>

// TODO: prevent desync of this set
//   by defining a fixed number of sets that are always bound
//   even if not used by pipelines.
//   prob prevents some optimization from vulkan's side, but it should be fine :P
layout(set = 0, binding = _bind_camera) uniform Ubo {
    Uniforms ubo;
};

void set_seed(int id) {
    seed = int(ubo.frame) ^ id ^ floatBitsToInt(ubo.time);
}

layout(push_constant) uniform PushConstantsUniform {
    PushConstants push;
};

// apprently restrict + readonly / writeonly helps shader optimizer.
// here it won't do anything here either way (vert shaders can't write)
layout(set = 0, binding = _bind_vertices) readonly restrict buffer VertexBuffer {
    Vertex vertices[];
};
layout(set = 0, binding = _bind_indices) readonly restrict buffer IndexBuffer {
    uint indices[];
};
layout(set = 0, binding = _bind_instances) readonly restrict buffer InstanceBuffer {
    Instance instances[];
};
layout(set = 0, binding = _bind_bones) readonly restrict buffer BoneBuffer {
    mat4 bones[];
};
layout(set = 0, binding = _bind_call_ctxts) readonly restrict buffer DrawCtxBuffer {
    DrawCtx draw_ctxts[];
};
layout(set = 0, binding = _bind_line_vertex_buffer) readonly restrict buffer LineVertices {
    LineVertex line_vertices[];
};

#ifdef MODEL_VERT_PASS
    layout(location = 0) out vec3 opos;
    layout(location = 1) out vec3 onormal;
    layout(location = 2) out vec2 ouv;
    layout(location = 3) out float light;
    void main() {
        DrawCtx ctx = draw_ctxts[gl_DrawID + push.first_draw_ctx];
        Instance inst = instances[gl_InstanceIndex + ctx.first_instance];
        Vertex v = vertices[indices[gl_VertexIndex + ctx.first_index] + ctx.first_vertex];

        vec4 pos = vec4(v.pos, 1.0);
        mat4 itransform = mat4(0.0);
        for (int i=0;i<4;i++) {
            itransform += bones[inst.bone_offset + v.bone_ids[i]] * v.bone_weights[i];
        }
        vec4 normal = itransform * vec4(v.normal, 0.0);
        normal = normalize(normal);
        vec4 light_dir = vec4(1.0, 1.0, 1.0, 0.0);
        light_dir = normalize(light_dir);

        pos = ubo.world_to_screen * itransform * pos;
        opos = pos.xyz;
        onormal = normal.xyz;
        ouv = v.uv.xy;
        gl_Position = pos;
        light = dot(normal.xyz, light_dir.xyz);
    }
#endif // MODEL_VERT_PASS

#ifdef MODEL_FRAG_PASS
    layout(set = 0, binding = _bind_texture) uniform sampler2D tex;

    layout(location = 0) in vec3 vpos;
    layout(location = 1) in vec3 vnormal;
    layout(location = 2) in vec2 uv;
    layout(location = 3) in float light;
    layout(location = 0) out vec4 fcolor;
    void main() {
        vec2 res = vec2(ubo.width, ubo.height);
        
        vec3 color = texture(tex, uv).xyz;
        fcolor = vec4(color * max(pow(light, 0.2), 0.2), 1.0);

        vec2 screen_uv = gl_FragCoord.xy/res.xy;
        vec2 center = step(-abs(screen_uv * 2.0 - 1.0), vec2(-0.004) * res.yx/min(res.x, res.y));
        float cursor = 1.0 - max(center.x, center.y);
        fcolor = vec4(mix(color, vec3(0.0, 1.0, 1.0), cursor), 1.0);
    }
#endif // MODEL_FRAG_PASS

#ifdef BG_VERT_PASS
    void main() {
        DrawCtx ctx = draw_ctxts[gl_DrawID + push.first_draw_ctx];
        Instance inst = instances[gl_InstanceIndex + ctx.first_instance];
        Vertex v = vertices[indices[gl_VertexIndex + ctx.first_index] + ctx.first_vertex];

        vec4 pos = vec4(v.pos, 1.0);
        pos.z = 1.0 - 0.000001;

        gl_Position = pos;
    }
#endif // BG_VERT_PASS

#ifdef BG_FRAG_PASS
    layout(location = 0) out vec4 fcolor;
    void main() {
        float y = gl_FragCoord.y/float(ubo.height) - 0.5;
        y -= ubo.camera.fwd.y;

        vec3 color = mix(vec3(1.0, 0.6, 0.6), vec3(0.2, 0.2, 0.3), y * 0.5 + 0.5);
        fcolor = vec4(color, 1.0);
    }
#endif // BG_FRAG_PASS

#ifdef DBG_VERT_PASS
    layout(location = 0) out vec4 vcolor;
    void main() {
        LineVertex v = line_vertices[gl_VertexIndex];

        vec4 pos = vec4(v.pos, 1.0);
        pos = ubo.world_to_screen * pos;

        gl_Position = pos;
        vcolor = v.color;
    }
#endif // DBG_VERT_PASS

#ifdef DBG_FRAG_PASS
    layout(location = 0) in vec4 vcolor;
    layout(location = 0) out vec4 fcolor;
    void main() {
        vec2 res = vec2(ubo.width, ubo.height);

        vec2 screen_uv = gl_FragCoord.xy/res.xy;
        vec2 center = step(-abs(screen_uv * 2.0 - 1.0), vec2(-0.004) * res.yx/min(res.x, res.y));
        float cursor = 1.0 - max(center.x, center.y);
        fcolor = vec4(mix(vcolor.xyz, vec3(0.0, 1.0, 1.0), cursor), 1.0);
    }
#endif // DBG_FRAG_PASS
