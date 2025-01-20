const std = @import("std");

pub const Rng = struct {
    rng: std.Random,
    constraints: Constraints = .{},

    pub const Constraints = struct {
        min: f32 = -1,
        max: f32 = 1,
        flip_sign: bool = false,
    };

    pub fn init(rng: std.Random) @This() {
        return .{ .rng = rng };
    }

    pub fn next(self: *const @This()) f32 {
        var rn = self.rng.float(f32);
        rn = self.constraints.min + rn * (self.constraints.max - self.constraints.min);

        if (self.constraints.flip_sign) {
            if (self.rng.boolean()) {
                rn *= -1;
            }
        }

        return rn;
    }

    pub fn with(self: *const @This(), c: struct {
        min: ?f32 = null,
        max: ?f32 = null,
        flip_sign: ?bool = null,
    }) @This() {
        var this = self.*;
        if (c.min) |min| {
            this.constraints.min = min;
        }
        if (c.max) |max| {
            this.constraints.max = max;
        }
        if (c.flip_sign) |flip| {
            this.constraints.flip_sign = flip;
        }
        return this;
    }

    pub fn with2(self: *const @This(), c: Constraints) @This() {
        var this = self.*;
        this.constraints = c;
        return this;
    }
};

pub const Vec4 = extern struct {
    x: f32 = 0,
    y: f32 = 0,
    z: f32 = 0,
    w: f32 = 0,

    pub fn dot(self: *const @This(), other: @This()) f32 {
        return self.x * other.x +
            self.y * other.y +
            self.z * other.z +
            self.w * other.w;
    }

    pub fn cross(self: *const @This(), other: @This()) @This() {
        return .{
            .x = self.y * other.z - self.z * other.y,
            .y = self.z * other.x - self.x * other.z,
            .z = self.x * other.y - self.y * other.x,
            .w = 0,
        };
    }

    pub fn mul(self: *const @This(), other: @This()) @This() {
        return .{
            .x = self.x * other.x,
            .y = self.y * other.y,
            .z = self.z * other.z,
            .w = self.w * other.w,
        };
    }

    pub fn add(self: *const @This(), other: @This()) @This() {
        return .{
            .x = self.x + other.x,
            .y = self.y + other.y,
            .z = self.z + other.z,
            .w = self.w + other.w,
        };
    }

    pub fn sub(self: *const @This(), other: @This()) @This() {
        return .{
            .x = self.x - other.x,
            .y = self.y - other.y,
            .z = self.z - other.z,
            .w = self.w - other.w,
        };
    }

    pub fn scale(self: *const @This(), s: f32) @This() {
        return .{ .x = self.x * s, .y = self.y * s, .z = self.z * s, .w = self.w * s };
    }

    pub fn mix(self: *const @This(), other: @This(), t: f32) @This() {
        return .{
            .x = std.math.lerp(self.x, other.x, t),
            .y = std.math.lerp(self.y, other.y, t),
            .z = std.math.lerp(self.z, other.z, t),
            .w = std.math.lerp(self.w, other.w, t),
        };
    }

    pub fn splat3(t: f32) @This() {
        return .{ .x = t, .y = t, .z = t };
    }

    pub fn splat4(t: f32) @This() {
        return .{ .x = t, .y = t, .z = t, .w = t };
    }

    pub fn normalize3D(self: *const @This()) @This() {
        var this = self.*;
        this.w = 0;

        const size = @sqrt(this.dot(this));
        return .{
            .x = self.x / size,
            .y = self.y / size,
            .z = self.z / size,
            .w = self.w,
        };
    }

    pub fn normalize4D(self: *const @This()) @This() {
        const size = @sqrt(self.dot(self.*));
        return .{
            .x = self.x / size,
            .y = self.y / size,
            .z = self.z / size,
            .w = self.w / size,
        };
    }

    pub fn quat_identity_rot() @This() {
        return .{ .w = 1 };
    }

    pub fn quat_euler_angles(pitch: f32, yaw: f32) @This() {
        // No roll is used, only pitch and yaw
        const half_pitch = pitch * 0.5;
        const half_yaw = yaw * 0.5;

        const cos_pitch = @cos(half_pitch);
        const sin_pitch = @sin(half_pitch);
        const cos_yaw = @cos(half_yaw);
        const sin_yaw = @sin(half_yaw);

        return .{
            .w = cos_pitch * cos_yaw,
            .x = sin_pitch * cos_yaw,
            .y = cos_pitch * sin_yaw,
            .z = -sin_pitch * sin_yaw, // Negative for correct rotation direction
        };
    }

    pub fn quat_angle_axis(angle: f32, axis: Vec4) @This() {
        // - [Visualizing quaternions, an explorable video series](https://eater.net/quaternions)
        const s = @sin(angle / 2.0);
        var q = axis.normalize3D().scale(s);
        q.w = @cos(angle / 2.0);
        return q;
    }

    // mult from the right means applying that rotation first.
    pub fn quat_mul(self: *const @This(), other: @This()) @This() {
        return .{
            .w = self.w * other.w - self.x * other.x - self.y * other.y - self.z * other.z,
            .x = self.w * other.x + self.x * other.w + self.y * other.z - self.z * other.y,
            .y = self.w * other.y + self.y * other.w + self.z * other.x - self.x * other.z,
            .z = self.w * other.z + self.z * other.w + self.x * other.y - self.y * other.x,
        };
    }

    // - [How to Use Quaternions - YouTube](https://www.youtube.com/watch?v=bKd2lPjl92c)
    // use this for rotation relative to world axes
    pub fn quat_global_rot(self: *const @This(), other: @This()) @This() {
        return other.quat_mul(self.*);
    }

    // use this for rotations relative to player's fwd, right, up as the axes
    pub fn quat_local_rot(self: *const @This(), other: @This()) @This() {
        return self.quat_mul(other);
    }

    pub fn quat_conjugate(self: *const @This()) @This() {
        return .{ .w = self.w, .x = -self.x, .y = -self.y, .z = -self.z };
    }

    pub fn rotate_vector(self: *const @This(), v: Vec4) Vec4 {
        const qv = .{ .w = 0, .x = v.x, .y = v.y, .z = v.z };
        const q_conjugate = self.quat_conjugate();
        const q_result = self.quat_mul(qv).quat_mul(q_conjugate);
        return Vec4{ .x = q_result.x, .y = q_result.y, .z = q_result.z };
    }

    pub fn to_buf(self: *const @This()) [4]f32 {
        return .{ self.x, self.y, self.z, self.w };
    }

    pub fn gamma_correct_inv(self: *const @This()) @This() {
        // const p: f32 = 1.0 / 2.2;
        const p: f32 = 2.2;
        return .{
            .x = std.math.pow(f32, self.x, p),
            .y = std.math.pow(f32, self.y, p),
            .z = std.math.pow(f32, self.z, p),
            .w = std.math.pow(f32, self.w, p),
        };
    }

    pub const random = struct {
        pub fn vec3(rng: *const Rng) Vec4 {
            return .{
                .x = rng.next(),
                .y = rng.next(),
                .z = rng.next(),
            };
        }

        pub fn vec4(rng: *const Rng) Vec4 {
            return .{
                .x = rng.next(),
                .y = rng.next(),
                .z = rng.next(),
                .w = rng.next(),
            };
        }
    };
};

// - [Matrix storage](https://github.com/hexops/machengine.org/blob/0aab00137dc3d1098e5237e2bee124e0ef9fbc17/content/docs/math/matrix-storage.md)
// vulkan wants | V1, V2, V3, V4 | (columns contiguous in memory).
// so we need to store matrix in transposed form
//
// all computation below is performed assuming right associative multiplication
// and uses column vectors (even though it is stored as row vectors in the struct)
// self.data[0] is 1 vector
//
pub const Mat4x4 = extern struct {
    data: [4]Vec4 = std.mem.zeroes([4]Vec4),

    pub fn mul_vec4(self: *const @This(), v: Vec4) Vec4 {
        const this = self.transpose();
        return .{
            .x = this.data[0].dot(v),
            .y = this.data[1].dot(v),
            .z = this.data[2].dot(v),
            .w = this.data[3].dot(v),
        };
    }

    pub fn mul_mat(self: *const @This(), o: @This()) @This() {
        const this = self.transpose();
        return .{ .data = .{
            .{
                .x = this.data[0].dot(o.data[0]),
                .y = this.data[1].dot(o.data[0]),
                .z = this.data[2].dot(o.data[0]),
                .w = this.data[3].dot(o.data[0]),
            },
            .{
                .x = this.data[0].dot(o.data[1]),
                .y = this.data[1].dot(o.data[1]),
                .z = this.data[2].dot(o.data[1]),
                .w = this.data[3].dot(o.data[1]),
            },
            .{
                .x = this.data[0].dot(o.data[2]),
                .y = this.data[1].dot(o.data[2]),
                .z = this.data[2].dot(o.data[2]),
                .w = this.data[3].dot(o.data[2]),
            },
            .{
                .x = this.data[0].dot(o.data[3]),
                .y = this.data[1].dot(o.data[3]),
                .z = this.data[2].dot(o.data[3]),
                .w = this.data[3].dot(o.data[3]),
            },
        } };
    }

    pub fn transpose(self: *const @This()) @This() {
        return .{ .data = .{
            .{ .x = self.data[0].x, .y = self.data[1].x, .z = self.data[2].x, .w = self.data[3].x },
            .{ .x = self.data[0].y, .y = self.data[1].y, .z = self.data[2].y, .w = self.data[3].y },
            .{ .x = self.data[0].z, .y = self.data[1].z, .z = self.data[2].z, .w = self.data[3].z },
            .{ .x = self.data[0].w, .y = self.data[1].w, .z = self.data[2].w, .w = self.data[3].w },
        } };
    }

    pub fn mix(self: *const @This(), other: *const @This(), t: f32) @This() {
        var this = std.mem.zeroes(@This());
        inline for (0..self.data.len) |i| {
            this.data[i] = self.data[i].mix(other.data[i], t);
        }
        return this;
    }

    pub fn identity() @This() {
        return .{ .data = .{
            .{ .x = 1, .y = 0, .z = 0, .w = 0 },
            .{ .x = 0, .y = 1, .z = 0, .w = 0 },
            .{ .x = 0, .y = 0, .z = 1, .w = 0 },
            .{ .x = 0, .y = 0, .z = 0, .w = 1 },
        } };
    }

    pub fn perspective_projection(height: u32, width: u32, near: f32, far: f32, fov: f32) @This() {
        // - [Perspective Projection](https://www.youtube.com/watch?v=U0_ONQQ5ZNM)
        var self = @This(){};

        const a = @as(f32, @floatFromInt(height)) / @as(f32, @floatFromInt(width));
        const f = 1.0 / @tan(fov / 2);
        const l = far / (far - near);

        self.data[0].x = a * f;
        self.data[1].y = f;
        self.data[2].z = l;
        self.data[2].w = 1;
        self.data[3].z = -l * near;

        return self;
    }

    pub fn view(eye: Vec4, at: Vec4, _up: Vec4) @This() {
        // - [» Deriving the View Matrix](https://twodee.org/blog/17560)

        const front = at.normalize3D();
        const up = _up.normalize3D();
        const right = front.cross(up);

        const translate_inv = Mat4x4{ .data = .{
            .{ .x = 1, .y = 0, .z = 0, .w = -eye.x },
            .{ .x = 0, .y = 1, .z = 0, .w = -eye.y },
            .{ .x = 0, .y = 0, .z = 1, .w = -eye.z },
            .{ .x = 0, .y = 0, .z = 0, .w = 1 },
        } };

        return (Mat4x4{ .data = .{
            right,
            up,
            front,
            .{ .w = 1 },
        } }).transpose().mul_mat(translate_inv.transpose());
    }

    pub fn scaling_mat(vec3: Vec4) @This() {
        return .{ .data = .{
            .{ .x = vec3.x },
            .{ .y = vec3.y },
            .{ .z = vec3.z },
            .{ .w = 1 },
        } };
    }

    pub fn translation_mat(vec3: Vec4) @This() {
        return .{ .data = .{
            .{ .x = 1 },
            .{ .y = 1 },
            .{ .z = 1 },
            .{ .x = vec3.x, .y = vec3.y, .z = vec3.z, .w = 1 },
        } };
    }

    pub fn rot_mat_euler_angles(vec3: Vec4) @This() {
        const rotz = (@This(){ .data = .{
            .{ .x = @cos(vec3.z), .y = -@sin(vec3.z), .z = 0 },
            .{ .x = @sin(vec3.z), .y = @cos(vec3.z), .z = 0 },
            .{ .x = 0, .y = 0, .z = 1 },
            .{ .w = 1 },
        } }).transpose();
        const roty = (@This(){ .data = .{
            .{ .x = @cos(vec3.y), .y = 0, .z = @sin(vec3.y) },
            .{ .x = 0, .y = 1, .z = 0 },
            .{ .x = -@sin(vec3.y), .y = 0, .z = @cos(vec3.y) },
            .{ .w = 1 },
        } }).transpose();
        const rotx = (@This(){ .data = .{
            .{ .x = 1, .y = 0, .z = 0 },
            .{ .x = 0, .y = @cos(vec3.x), .z = -@sin(vec3.x) },
            .{ .x = 0, .y = @sin(vec3.x), .z = @cos(vec3.x) },
            .{ .w = 1 },
        } }).transpose();
        return roty.mul_mat(rotx).mul_mat(rotz);
    }

    pub fn rot_mat_from_quat(rot: Vec4) @This() {
        const x = Vec4{ .x = 1 };
        const y = Vec4{ .y = 1 };
        const z = Vec4{ .z = 1 };

        return .{ .data = .{
            rot.rotate_vector(x),
            rot.rotate_vector(y),
            rot.rotate_vector(z),
            .{ .w = 1 },
        } };
    }

    // - [3D Shearing Transformation](https://www.geeksforgeeks.org/computer-graphics-3d-shearing-transformation/)
    pub fn shear_mat(
        x: struct { y: f32 = 0, z: f32 = 0 },
        y: struct { x: f32 = 0, z: f32 = 0 },
        z: struct { x: f32 = 0, y: f32 = 0 },
    ) @This() {
        return (@This(){ .data = .{
            .{ .x = 1, .y = x.y, .z = x.z },
            .{ .x = y.x, .y = 1, .z = y.z },
            .{ .x = z.x, .y = z.y, .z = 1 },
            .{ .w = 1 },
        } }).transpose();
    }

    pub const random = struct {
        // there's no point in having constrained random numbers for this
        pub fn rot(rng: *const Rng) Mat4x4 {
            var q = Vec4{
                .x = (rng.rng.float(f32) - 0.5),
                .y = (rng.rng.float(f32) - 0.5),
                .z = (rng.rng.float(f32) - 0.5),
                .w = (rng.rng.float(f32) - 0.5),
            };
            q = q.normalize4D();

            const r = Mat4x4.rot_mat_from_quat(q);
            return r;
        }

        pub fn translate(rng: *const Rng) Mat4x4 {
            return .{ .data = .{
                .{ .x = 1 }, .{ .y = 1 }, .{ .z = 1 }, .{
                    .x = rng.next(),
                    .y = rng.next(),
                    .z = rng.next(),
                    .w = 1,
                },
            } };
        }

        pub fn scale(rng: *const Rng) Mat4x4 {
            return .{
                .data = .{
                    .{ .x = rng.next() },
                    .{ .y = rng.next() },
                    .{ .z = rng.next() },
                    .{ .w = 1 },
                },
            };
        }

        pub fn shear(rng: *const Rng) Mat4x4 {
            return .{ .data = .{
                .{ .x = 1, .y = rng.next(), .z = rng.next() },
                .{ .x = rng.next(), .y = 1, .z = rng.next() },
                .{ .x = rng.next(), .y = rng.next(), .z = 1 },
                .{ .w = 1 },
            } };
        }
    };
};

pub const ColorParse = struct {
    pub fn hex_rgba(typ: type, comptime hex: []const u8) typ {
        if (hex.len != 9 or hex[0] != '#') {
            @compileError("invalid color");
        }

        return .{
            .r = @as(f32, @floatFromInt(parseHex(hex[1], hex[2]))) / 255.0,
            .g = @as(f32, @floatFromInt(parseHex(hex[3], hex[4]))) / 255.0,
            .b = @as(f32, @floatFromInt(parseHex(hex[5], hex[6]))) / 255.0,
            .a = @as(f32, @floatFromInt(parseHex(hex[7], hex[8]))) / 255.0,
        };
    }

    pub fn hex_xyzw(typ: type, comptime hex: []const u8) typ {
        if (hex.len != 9 or hex[0] != '#') {
            @compileError("invalid color");
        }

        return .{
            .x = @as(f32, @floatFromInt(parseHex(hex[1], hex[2]))) / 255.0,
            .y = @as(f32, @floatFromInt(parseHex(hex[3], hex[4]))) / 255.0,
            .z = @as(f32, @floatFromInt(parseHex(hex[5], hex[6]))) / 255.0,
            .w = @as(f32, @floatFromInt(parseHex(hex[7], hex[8]))) / 255.0,
        };
    }

    fn parseHex(comptime high: u8, comptime low: u8) u8 {
        return (hexDigitToInt(high) << 4) | hexDigitToInt(low);
    }

    fn hexDigitToInt(comptime digit: u8) u8 {
        if (digit >= '0' and digit <= '9') {
            return digit - '0';
        } else if (digit >= 'a' and digit <= 'f') {
            return digit - 'a' + 10;
        } else if (digit >= 'A' and digit <= 'F') {
            return digit - 'A' + 10;
        }
        @compileError("invalid hex digit");
    }
};

pub const Camera = struct {
    pos: Vec4,
    pitch: f32 = 0,
    yaw: f32 = 0,
    speed: f32 = 1.0,
    sensitivity: f32 = 1.0,
    sensitivity_scale: f32 = 0.003,
    basis: struct {
        fwd: Vec4,
        right: Vec4,
        up: Vec4,
    },

    pub const constants = struct {
        pub const pitch_min = -std.math.pi / 2.0 + 0.1;
        pub const pitch_max = std.math.pi / 2.0 - 0.1;
        pub const basis = struct {
            pub const vulkan = struct {
                pub const up = Vec4{ .y = -1 };
                pub const fwd = Vec4{ .z = 1 };
                pub const right = Vec4{ .x = 1 };
            };
            pub const opengl = struct {
                pub const up = Vec4{ .y = 1 };
                pub const fwd = Vec4{ .z = -1 };
                pub const right = Vec4{ .x = 1 };
            };
        };
    };

    pub fn init(pos: Vec4, basis: anytype) @This() {
        return .{
            .pos = pos,
            .basis = .{
                .fwd = basis.fwd,
                .right = basis.right,
                .up = basis.up,
            },
        };
    }

    pub fn tick(
        self: *@This(),
        delta: f32,
        dp: struct { dx: i32, dy: i32 },
        pressed: struct { w: bool, a: bool, s: bool, d: bool, shift: bool, ctrl: bool },
    ) void {
        // rotation should not be multiplied by deltatime. if mouse moves by 3cm, it should always rotate the same amount.
        self.yaw += @as(f32, @floatFromInt(dp.dx)) * self.sensitivity_scale * self.sensitivity;
        self.pitch -= @as(f32, @floatFromInt(dp.dy)) * self.sensitivity_scale * self.sensitivity;
        self.pitch = std.math.clamp(self.pitch, constants.pitch_min, constants.pitch_max);

        const rot = self.rot_quat();
        const fwd = rot.rotate_vector(self.basis.fwd);
        const right = rot.rotate_vector(self.basis.right);

        var speed = self.speed;
        if (pressed.shift) {
            speed *= 2.0;
        }
        if (pressed.ctrl) {
            speed *= 0.1;
        }

        if (pressed.w) {
            self.pos = self.pos.add(fwd.scale(delta * speed));
        }
        if (pressed.a) {
            self.pos = self.pos.sub(right.scale(delta * speed));
        }
        if (pressed.s) {
            self.pos = self.pos.sub(fwd.scale(delta * speed));
        }
        if (pressed.d) {
            self.pos = self.pos.add(right.scale(delta * speed));
        }
    }

    pub fn world_to_screen_mat(self: *const @This(), width: u32, height: u32) Mat4x4 {
        const rot = self.rot_quat();
        const up = rot.rotate_vector(self.basis.up);
        const fwd = rot.rotate_vector(self.basis.fwd);

        const projection_matrix = Mat4x4.perspective_projection(height, width, 0.01, 100.0, std.math.pi / 3.0);
        const view_matrix = Mat4x4.view(self.pos, fwd, up);
        const world_to_screen = projection_matrix.mul_mat(view_matrix);

        return world_to_screen;
    }

    pub fn rot_quat(self: *const @This()) Vec4 {
        var rot = Vec4.quat_identity_rot();
        rot = rot.quat_mul(Vec4.quat_angle_axis(self.pitch, self.basis.right));
        rot = rot.quat_mul(Vec4.quat_angle_axis(self.yaw, self.basis.up));
        rot = rot.quat_conjugate();
        return rot;
    }
};
