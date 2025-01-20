#version 460

#include <common.glsl>
#include <mandlebulb_uniforms.glsl>

layout(set = 0, binding = 0) uniform Ubo {
    Uniforms ubo;
};
layout(set = 0, binding = 1) buffer VoxelBuffer {
    vec4 voxels[];
};
layout(set = 0, binding = 2) buffer GBuffer {
    vec4 gbuffer[];
};

layout(set = 0, binding = 3, rgba16) writeonly uniform image2D screen;

void set_seed(int id) {
    seed = int(ubo.frame) ^ id ^ floatBitsToInt(ubo.time);
}

bool inGrid(ivec3 pos) {
    if (any(lessThan(pos, ivec3(0)))) {
        return false;
    }
    if (any(greaterThanEqual(pos, ivec3(ubo.voxel_grid_side)))) {
        return false;
    }

    return true;
}

bool inGrid(vec3 pos) {
    if (any(lessThan(pos, vec3(0)))) {
        return false;
    }
    if (any(greaterThanEqual(pos, vec3(1.0)))) {
        return false;
    }

    return true;
}

vec4 voxel_fetch(ivec3 v) {
    if (!inGrid(v)) {
        return vec4(0.0);
    }

    int side = ubo.voxel_grid_side;
    return voxels[to1D(v, side)];
}

vec4 voxel_fetch(vec3 p) {
    p += 0.5;

    if (!inGrid(p)) {
        return vec4(0.0);
    }

    int side = ubo.voxel_grid_side;
    ivec3 pos = ivec3(p * vec3(side));
    return voxels[to1D(pos, side)];
}

vec4 trilinear_voxel_fetch(vec3 p) {
    int side = ubo.voxel_grid_side;
    p = (p + 0.5) * vec3(side);

    ivec3 v = ivec3(p + 0.0001);
    
    if (!inGrid(v)) {
        return vec4(0.0);
    }

    vec4 xmymzm = voxel_fetch(v + ivec3(0, 0, 0));
    vec4 xpymzm = voxel_fetch(v + ivec3(1, 0, 0));
    vec4 xmypzm = voxel_fetch(v + ivec3(0, 1, 0));
    vec4 xpypzm = voxel_fetch(v + ivec3(1, 1, 0));
    vec4 xmymzp = voxel_fetch(v + ivec3(0, 0, 1));
    vec4 xpymzp = voxel_fetch(v + ivec3(1, 0, 1));
    vec4 xmypzp = voxel_fetch(v + ivec3(0, 1, 1));
    vec4 xpypzp = voxel_fetch(v + ivec3(1, 1, 1));
    
    vec4 ymzm = mix(xmymzm, xpymzm, fract(p.x));
    vec4 ypzm = mix(xmypzm, xpypzm, fract(p.x));
    vec4 ymzp = mix(xmymzp, xpymzp, fract(p.x));
    vec4 ypzp = mix(xmypzp, xpypzp, fract(p.x));
    
    vec4 zm = mix(ymzm, ypzm, fract(p.y));
    vec4 zp = mix(ymzp, ypzp, fract(p.y));
    
    return mix(zm, zp, fract(p.z));
}

vec3 background(vec3 rd) {
    return mix(ubo.background_color1.xyz, ubo.background_color2.xyz, rd.y * 0.5 + 0.5);
}

struct BulbResult {
    float potential;
    float r;
    vec3 trap;
    float density;
    vec3 col;
    vec3 emission;
};

BulbResult bulb(vec3 pos) {
    vec3 w0 = pos / ubo.fractal_scale;
    vec3 w = w0;
    float dr = 1.0;
    float r2 = dot(w, w);
    vec3 trap = vec3(1.0);
    
    for (int i = 0; i < ubo.fractal_iterations && r2 < ubo.escape_r * ubo.escape_r; i++) {
        // -[](https://www.shadertoy.com/view/stc3Ws)
        // float p = 8.0;
        // vec3 w2 = w * w;
        // vec3 w4 = w2 * w2;
        // float k1 = w2.x * w2.z;
        // float k2 = w2.x + w2.z;
        // float k3 = w4.x + w4.z + w2.y * (w2.y - 6.0 * k2) + 2.0 * k1;
        // float k4 = k2 * k2 * k2;
        // float k5 = k3 * inversesqrt(k4 * k4 * k2);
        // float k6 = w.y * (k2 - w2.y);
        // w.x = pos.x + 64.0 * k6 * k5 * w.x * w.z * (w2.x - w2.z) * (w4.x - 6.0 * k1 + w4.z);
        // w.y = pos.y - 16.0 * k6 * k6 * k2 + k3 * k3;
        // w.z = pos.z - 8.0 * k6 * k5 * (w4.x * (w4.x - 28.0 * k1 + 70.0 * w4.z) + w4.z * (w4.z - 28.0 * k1));

        // - [Inigo Quilez mandlebulb](https://iquilezles.org/articles/mandelbulb/)
        // // extract polar coordinates
        // float p = mod(ubo.time/5.0, 10.0);
        float p = ubo.bulb_z_pow;
        float wr = sqrt(dot(w, w));
        float wo = acos(w.y / wr);
        float wi = atan(w.x, w.z);
        // scale and rotate the point
        wr = pow(wr, p);
        wo = wo * p;
        wi = wi * p;
        // convert back to cartesian coordinates
        // MOUS: apparently it's much faster when you don't call sin() twice :/
        float k1 = sin(wo);
        w.x = w0.x + wr * k1 * sin(wi);
        w.y = w0.y + wr * cos(wo);
        w.z = w0.z + wr * k1 * cos(wi);

        dr = dr * pow(r2, p/2.0) * p + 1.0;
        r2 = dot(w, w);
        trap = min(abs(w), trap);
    }
    
    BulbResult res;
    res.r = sqrt(r2);
    // distance estimation (through the Hubbard-Douady potential)
    res.potential = ubo.fractal_scale * 0.25 * log(r2) * res.r / dr;
    res.trap = trap * 1.5;
    res.col = mix(ubo.trap_color1.xyz, ubo.trap_color2.xyz, res.trap);
    res.emission = mix(ubo.emission_color1.xyz, ubo.emission_color2.xyz, res.trap.z) * step(res.trap.y * res.trap.x * res.trap.z, 0.00001);
    res.density = step(r2, ubo.escape_r * ubo.escape_r);
    return res;
}

float march(vec3 ro, vec3 rd) {
    const float stepSize = ubo.march_step_factor / ubo.fractal_density;
    float t = random() * stepSize;

    // allows this many units of penetration 'into' the fractal
    float penetration_depth = random() / max(ubo.fractal_density * ubo.ray_penetration_factor, 1.0);

    for (int i = 0; i < ubo.march_iterations && t < ubo.t_max && penetration_depth > 0.0; i++) {
        vec3 pos = ro + t * rd;
        BulbResult v = bulb(pos);
        float dt = v.potential;
        penetration_depth -= stepSize * v.density;
        t += max(dt, stepSize);
    }
    return mix(-1.0, t, step(penetration_depth, 0.0));
}

vec4 gather(vec3 ro, vec3 rd) {
    // avoid div by 0 (assign 0.00001 to any component of rd that are 0)
    rd += (1.0 - abs(sign(rd))) * 0.00001; 

    float t = 0.0;
    float transparency = 1.0;
    float depth = 1000.0;
    vec3 col = vec3(0.0);

    if (ubo.voxel_debug_view != 0) {
        int side = ubo.voxel_grid_side;
        vec3 cubeSize = 1.0 / vec3(side);
        for (int i = 0; i < side * 3 && transparency > ubo.min_background_color_contribution && t < ubo.t_max; i++) {
            vec3 p = ro + t * rd;
            vec3 l = (cubeSize * floor(p / cubeSize + sign(rd) * 0.50001 + 0.5) - p) / rd;
            float stepSize = min(min(l.x, l.y), l.z) + 0.00001;
        
            vec4 v = voxel_fetch(p);
            if(v.a > 0.0) {
                depth = min(depth, t);

                float sample_transparency = exp(-stepSize * ubo.fractal_density);
                col += transparency * (1.0 - sample_transparency) * v.rgb;
                transparency *= sample_transparency;
            }
        
            t += stepSize;
        }
    } else {
        // - [Volume Rendering](https://www.scratchapixel.com/lessons/3d-basic-rendering/volume-rendering-for-developers/intro-volume-rendering.html)
        // step size is fun to play with in combination with aliasing factor. this gives more control at lower values
        const float stepSize = pow(ubo.gather_step_factor/2.5, 2.5) / ubo.fractal_density;

        // a random phase to offset steps by - for preventing aliasing
        float phase = pow(random(), exp(ubo.stylistic_aliasing_factor));
        for (int i = 0; i < ubo.gather_iterations && transparency > ubo.min_background_color_contribution && t < ubo.t_max; i++) {
            vec3 p = ro + rd * t;
        
            BulbResult v = bulb(p);
        
            if (v.density > 0.0) {
                depth = min(depth, t);

                float sample_transparency = exp(-stepSize * ubo.fractal_density);
                vec3 voxel_color = trilinear_voxel_fetch(p).rgb;
                vec3 sample_color = voxel_color * v.col  + v.emission;
                col += transparency * (1.0 - sample_transparency) * sample_color;
                transparency *= sample_transparency;
            }
        
            t += max(v.potential, 0.0);
        
            // i don't understand this 1- fract(t/stepsize).
            // other things in randomish range (0, 1) works too, but this behaves better when we change step factor
            t += (1.0 - fract(t / stepSize + phase) - 0.000001) * stepSize;
        }
    }
    return vec4(transparency * background(rd) + col, depth);
}

#ifdef EYEFACE_VOLUME
    layout (local_size_x = 8, local_size_y = 8) in;
    void main() {
        int id = global_id;
        set_seed(id);

        if (ubo.frame > ubo.gi_samples) {
            return;
        }
        int side = ubo.voxel_grid_side;
        if (id >= side * side * side) {
            return;
        }

        if (ubo.frame == 0) {
            voxels[id] = vec4(0.0);
        }

        ivec3 ipos = to3D(id, side);
        vec3 pos = vec3(ipos)/float(side) - 0.5;

        vec3 p = pos + vec3(random(), random(), random())/float(side);
        vec3 col = step(march(p, normalize(ubo.light_dir.xyz)), 0.0) * vec3(2.0);

        if (ubo.frame > 0) {
            // infinite bounce GI
            vec3 rd = random_normal();
            float t = march(p, rd);
            vec3 p = p + t * rd;
            BulbResult v = bulb(p);
            vec3 voxel_color = trilinear_voxel_fetch(p).rgb;
            col += mix(background(rd), voxel_color * v.col + v.emission, step(0.0, t));
        }

        voxels[id] = mix(
            voxels[id],
            vec4(col, bulb(p).density),
            1.0/float(ubo.frame + 1)
        );
    }
#endif // EYEFACE_VOLUME

#ifdef EYEFACE_RENDER
    layout (local_size_x = 8, local_size_y = 8) in;
    void main() {
        int id = global_id;
        set_seed(id);

        if (id >= ubo.width * ubo.height) {
            return;
        }

        if (ubo.frame == 0) {
            gbuffer[id] = vec4(0.0, 0.0, 0.0, 1000.0);
        }

        ivec2 ipos = to2D(id, ubo.width);
        ivec2 ires = ivec2(ubo.width, ubo.height);
        vec2 pos = vec2(ipos);
        vec2 res = vec2(ires);

        vec3 ro = ubo.camera.eye.xyz;
        vec3 rd = ubo.camera.fwd.xyz;
        rd += ubo.camera.right.xyz * (pos.x - res.x/2.0)/res.y;
        rd += ubo.camera.up.xyz * (pos.y - res.y/2.0)/res.y;
        rd = normalize(rd);

        vec4 f_color = gather(ro, rd);
        if (ubo.camera.meta.did_change == 0) {
            gbuffer[id] = vec4(
                mix(f_color.xyz, gbuffer[id].xyz, ubo.temporal_blend_factor),
                min(f_color.w, gbuffer[id].w)
            );
        } else {
            gbuffer[id] = f_color;
        }
    }
#endif // EYEFACE_RENDER

#ifdef EYEFACE_DRAW
    layout (local_size_x = 8, local_size_y = 8) in;
    void main() {
        int id = global_id;
        set_seed(id);

        if (ubo.width * ubo.height <= id) {
            return;
        }
        ivec2 ipos = to2D(id, ubo.width);

        vec3 f_color = gbuffer[id].xyz;

        f_color = linear_tonemap(f_color, ubo.exposure);
        f_color = tanh_tonemap(f_color);
        f_color = gamma_decode(f_color, ubo.gamma);
        imageStore(screen, ipos, vec4(f_color, 1.0));
    }
#endif // EYEFACE_DRAW
