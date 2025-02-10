#version 460

#include <common.glsl>
#include <uniforms.glsl>

layout(set = 0, binding = 0) uniform Ubo {
    Uniforms ubo;
};

void set_seed(int id) {
    seed = int(ubo.frame) ^ id ^ floatBitsToInt(ubo.time);
}

#ifdef VERT_PASS
    layout(set = 1, binding = 0) uniform UboModel {
        mat4 model_mat;
    };
    layout(set = 1, binding = 2) readonly buffer BoneBuffer {
        mat4 bones[];
    };

    layout(location = _bind_vertex_position) in vec3 vpos;
    layout(location = _bind_normal) in vec3 vnormal;
    layout(location = _bind_uv) in vec2 uv;
    layout(location = _bind_bone_ids) in uvec4 bone_ids;
    layout(location = _bind_bone_weights) in vec4 bone_weights;
    layout(location = _bind_instance_bone_offset) in uint bone_offset;

    layout(location = 0) out vec3 opos;
    layout(location = 1) out vec3 onormal;
    layout(location = 2) out vec2 ouv;
    layout(location = 3) out float light;
    void main() {
        vec4 pos = vec4(vpos, 1.0);
        mat4 itransform = mat4(0.0);
        for (int i=0;i<4;i++) {
            itransform += bones[bone_offset + bone_ids[i]] * bone_weights[i];
        }
        vec4 normal = itransform * vec4(vnormal, 0.0);
        normal = normalize(normal);
        vec4 light_dir = vec4(1.0, 1.0, 1.0, 0.0);
        light_dir = normalize(light_dir);

        pos = ubo.world_to_screen * itransform * pos;
        opos = pos.xyz;
        onormal = normal.xyz;
        ouv = uv;
        gl_Position = pos;
        light = dot(normal.xyz, light_dir.xyz);
    }
#endif // VERT_PASS

#ifdef FRAG_PASS
    layout(set = 1, binding = 1) uniform sampler2D tex;

    layout(location = 0) in vec3 vpos;
    layout(location = 1) in vec3 vnormal;
    layout(location = 2) in vec2 uv;
    layout(location = 3) in float light;
    layout(location = 0) out vec4 fcolor;
    void main() {
        vec2 res = vec2(ubo.width, ubo.height);
        
        vec3 color = texture(tex, uv).xyz;
        fcolor = vec4(color * max(pow(light, 0.2), 0.2), 1.0);
    }
#endif // FRAG_PASS

#ifdef BG_VERT_PASS
    void main() {
        float z = 1.0 - 0.000001;
        vec3 positions[6] = vec3[6](
            vec3(1.0, 1.0, z),
            vec3(-1.0, 1.0, z),
            vec3(1.0, -1.0, z),
            vec3(1.0, -1.0, z),
            vec3(-1.0, 1.0, z),
            vec3(-1.0, -1.0, z)
        );

        vec3 pos = positions[gl_VertexIndex];

        gl_Position = vec4(pos, 1.0);
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
