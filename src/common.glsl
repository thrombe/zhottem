
// #define global_id int(gl_LocalInvocationID.x +\
//         gl_LocalInvocationID.y * gl_WorkGroupSize.x +\
//         gl_WorkGroupID.x * gl_WorkGroupSize.x * gl_WorkGroupSize.y)

#define global_id int(gl_GlobalInvocationID.x +\
        gl_GlobalInvocationID.y * gl_NumWorkGroups.x * gl_WorkGroupSize.x +\
        gl_GlobalInvocationID.z * gl_NumWorkGroups.x * gl_NumWorkGroups.y * gl_WorkGroupSize.x * gl_WorkGroupSize.y)

#define u32 uint
#define i32 int
#define f32 float

uint rand_xorshift(uint state) {
    state ^= (state << 13);
    state ^= (state >> 17);
    state ^= (state << 5);
    return state;
}

uint hash(uint i) {
    i *= 0xB5297A4Du;
    i ^= i >> 8;
    i += 0x68E31DA4u;
    i ^= i << 8;
    i *= 0x1B56C4E9u;
    i ^= i >> 8;
    return i;
}

float fhash(uint i) {
    return float(hash(i))/4294967295.0;
}

uint seed = 0;
float random() {
    return fhash(seed++);
}

uint randuint() {
    return hash(seed++);
}

vec3 random_normal() {
    vec2 r = vec2(6.28318530718 * random(), acos(2.0 * random() - 1.0));
    vec2 c = cos(r), s = sin(r);
    return vec3(s.y * s.x, s.y * c.x, c.y);
}

int to1D(ivec3 pos, int size) {
    return pos.x + pos.y * size + pos.z * size * size;
}

int to1D(ivec2 pos, int size) {
    return pos.x + pos.y * size;
}

ivec2 to2D(int id, int side) {
    ivec2 pos = ivec2(id % side, id / side);
    return pos;
}

ivec3 to3D(int id, int side) {
    ivec3 pos = ivec3(id % side, (id / side)%side, (id / (side * side))%side);
    return pos;
}

int divide_roof(int a, int b) {
    return a / b + int(mod(a, b) > 0);
}

// - [hsv rgb conversion glsl shader · GitHub](https://gist.github.com/983/e170a24ae8eba2cd174f)
vec3 rgb2hsv(vec3 c) {
    vec4 k = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4 p = mix(vec4(c.bg, k.wz), vec4(c.gb, k.xy), step(c.b, c.g));
    vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

vec3 hsv2rgb(vec3 c) {
    vec4 k = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + k.xyz) * 6.0 - k.www);
    return c.z * mix(k.xxx, clamp(p - k.xxx, 0.0, 1.0), c.y);
}

vec3 reinhard_tonemap(vec3 x) {
    return x / (x + vec3(1.0));
}

vec3 tanh_tonemap(vec3 x) {
    x = clamp(x, -40.0, 40.0);
    return (exp(x) - exp(-x)) / (exp(x) + exp(-x));
}

vec3 aces_tonemap(vec3 x) {
    return (x * (2.51 * x + 0.03)) / (x * (2.43 * x + 0.59) + 0.14);
}

vec3 linear_tonemap(vec3 x, float exposure) {
    return x * exposure;
}

// prepare for display
vec3 gamma_encode(vec3 x, float gamma) {
    return vec3(
        pow(x.x, 1.0/gamma),
        pow(x.y, 1.0/gamma),
        pow(x.z, 1.0/gamma)
    );
}

// restore to linear
vec3 gamma_decode(vec3 x, float gamma) {
    return vec3(
        pow(x.x, gamma),
        pow(x.y, gamma),
        pow(x.z, gamma)
    );
}
