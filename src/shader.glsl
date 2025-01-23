#version 460

#include <common.glsl>
#include <uniforms.glsl>

layout(set = 0, binding = 0) uniform Ubo {
    Uniforms ubo;
};
layout(set = 1, binding = 0) uniform UboModel {
    mat4 model_mat;
};

void set_seed(int id) {
    seed = int(ubo.frame) ^ id ^ floatBitsToInt(ubo.time);
}

#ifdef VERT_PASS
    layout(location = 0) in vec3 vpos; // vertex pos
    layout(location = 1) in vec3 ipos; // instance pos
    layout(location = 0) out vec3 opos;
    void main() {
        vec4 pos = vec4(vpos + ipos * 3, 1.0);
        pos = ubo.world_to_screen * model_mat * pos;
        opos = pos.xyz;
        gl_Position = pos;
    }
#endif // VERT_PASS

#ifdef FRAG_PASS
    layout(location = 0) in vec3 vpos;
    layout(location = 0) out vec4 fcolor;
    void main() {
        fcolor = vec4(gl_FragCoord.xy/1000.0, 0.0, 1.0);
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
