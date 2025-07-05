
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

// - [Mini: OkLab - by Xor - GM Shaders](https://mini.gmshaders.com/p/oklab)
vec3 oklab_from_linear(vec3 linear) {
    const mat3 im1 = mat3(0.4121656120, 0.2118591070, 0.0883097947,
                          0.5362752080, 0.6807189584, 0.2818474174,
                          0.0514575653, 0.1074065790, 0.6302613616);

    const mat3 im2 = mat3(+0.2104542553, +1.9779984951, +0.0259040371,
                          +0.7936177850, -2.4285922050, +0.7827717662,
                          -0.0040720468, +0.4505937099, -0.8086757660);

    vec3 lms = im1 * linear;

    return im2 * (sign(lms) * pow(abs(lms), vec3(1.0/3.0)));
}

vec3 linear_from_oklab(vec3 oklab) {
    const mat3 m1 = mat3(+1.000000000, +1.000000000, +1.000000000,
                         +0.396337777, -0.105561346, -0.089484178,
                         +0.215803757, -0.063854173, -1.291485548);

    const mat3 m2 = mat3(+4.076724529, -1.268143773, -0.004111989,
                         -3.307216883, +2.609332323, -0.703476310,
                         +0.230759054, -0.341134429, +1.706862569);

    vec3 lms = m1 * oklab;

    return m2 * (lms * lms * lms);
}

vec3 oklab_mix(vec3 lin1, vec3 lin2, float a) {
    // https://bottosson.github.io/posts/oklab
    const mat3 kCONEtoLMS = mat3(
         0.4121656120,  0.2118591070,  0.0883097947,
         0.5362752080,  0.6807189584,  0.2818474174,
         0.0514575653,  0.1074065790,  0.6302613616);
    const mat3 kLMStoCONE = mat3(
         4.0767245293, -1.2681437731, -0.0041119885,
        -3.3072168827,  2.6093323231, -0.7034763098,
         0.2307590544, -0.3411344290,  1.7068625689);

    // rgb to cone (arg of pow can't be negative)
    vec3 lms1 = pow( kCONEtoLMS*lin1, vec3(1.0/3.0) );
    vec3 lms2 = pow( kCONEtoLMS*lin2, vec3(1.0/3.0) );
    // lerp
    vec3 lms = mix( lms1, lms2, a );
    // gain in the middle (no oklab anymore, but looks better?)
    lms *= 1.0+0.2*a*(1.0-a);
    // cone to rgb
    return kLMStoCONE*(lms*lms*lms);
}

vec3 oklab_mix2(vec3 lin1, vec3 lin2, f32 t) {
    return linear_from_oklab(mix(oklab_from_linear(lin1), oklab_from_linear(lin2), t));
}

uint rgba_encode_u32(vec4 color) {
    uint r = uint(color.r * 255.0);
    uint g = uint(color.g * 255.0);
    uint b = uint(color.b * 255.0);
    uint a = uint(color.a * 255.0);
    return (a << 24) | (b << 16) | (g << 8) | r;
}

vec4 rgba_decode_u32(uint color) {
    uint r = color & 0xFF;
    uint g = (color >> 8) & 0xFF;
    uint b = (color >> 16) & 0xFF;
    uint a = (color >> 24) & 0xFF;
    return vec4(float(r), float(g), float(b), float(a)) / 255.0;
}

// https://mini.gmshaders.com/p/tonemaps
vec3 uncharted2_tonemap(vec3 x) {
    x *= 16.0;
    const float A = 0.15;
    const float B = 0.50;
    const float C = 0.10;
    const float D = 0.20;
    const float E = 0.02;
    const float F = 0.30;

    return ((x*(A*x+C*B)+D*E)/(x*(A*x+B)+D*F))-E/F;
}

vec3 unreal_tonemap(vec3 x) {
    // Unreal 3, Documentation: "Color Grading"
    // Adapted to be close to Tonemap_ACES, with similar range
    // Gamma 2.2 correction is baked in, don't use with sRGB conversion!
    return x / (x + 0.155) * 1.019;
}

// float tanh(float x) {
//     float exp_neg_2x = exp(-2.0 * x);
//     return -1.0 + 2.0 / (1.0 + exp_neg_2x);
// }

vec3 reinhard_tonemap(vec3 x) {
    return x / (x + vec3(1.0));
}

vec3 tanh_tonemap(vec3 x) {
    x = clamp(x, -40.0, 40.0);
    return (exp(x) - exp(-x)) / (exp(x) + exp(-x));
}

vec3 tanh_tonemap2(vec3 x) {
    x = clamp(x, -40.0, 40.0);
    vec3 exp_neg_2x = exp(-2.0 * x);
    return -1.0 + 2.0 / (1.0 + exp_neg_2x);
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
