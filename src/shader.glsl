#version 460

#include <common.glsl>
#include <uniforms.glsl>

layout(set = 0, binding = 0) uniform Ubo {
    Uniforms ubo;
};
layout(set = 0, binding = 1) uniform UboModel {
    mat4 model_mat;
};

void set_seed(int id) {
    seed = int(ubo.frame) ^ id ^ floatBitsToInt(ubo.time);
}

#ifdef VERT_PASS
    layout(location = 0) in vec3 ipos;
    layout(location = 0) out vec3 opos;
    void main() {
        vec4 pos = vec4(ipos, 1.0);
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
