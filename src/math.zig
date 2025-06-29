const std = @import("std");

pub fn divide_roof(x: anytype, r: @TypeOf(x)) @TypeOf(x) {
    switch (@typeInfo(@TypeOf(x))) {
        .int => {
            return @divFloor(x, r) + @as(@TypeOf(x), @intFromBool(@mod(x, r) > 0));
        },
        else => {
            return x / r + @as(@TypeOf(x), @intFromBool(x % r > 0));
        },
    }
}

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

pub fn Vec2T(T: type) type {
    return extern struct {
        x: T = 0,
        y: T = 0,
    };
}

pub fn Vec3T(T: type) type {
    return extern struct {
        x: T = 0,
        y: T = 0,
        z: T = 0,
        __: T = 0,
    };
}

pub fn Vec4T(T: type) type {
    return extern struct {
        x: T = 0,
        y: T = 0,
        z: T = 0,
        w: T = 0,
    };
}

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

    // right handed cross product
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

    pub fn clamp(self: *const @This(), low: f32, high: f32) @This() {
        return .{
            .x = std.math.clamp(self.x, low, high),
            .y = std.math.clamp(self.y, low, high),
            .z = std.math.clamp(self.z, low, high),
            .w = std.math.clamp(self.w, low, high),
        };
    }

    pub fn splat3(t: f32) @This() {
        return .{ .x = t, .y = t, .z = t };
    }

    pub fn splat(t: f32) @This() {
        return .{ .x = t, .y = t, .z = t, .w = t };
    }

    pub fn xyz(self: *const @This()) Vec3 {
        return .{ .x = self.x, .y = self.y, .z = self.z };
    }

    pub fn max3(self: *const @This()) f32 {
        return @max(self.x, @max(self.y, self.z));
    }

    pub fn max(self: *const @This()) f32 {
        return @max(@max(self.x, self.y), @max(self.z, self.w));
    }

    pub fn length(self: *const @This()) f32 {
        return @sqrt(self.dot(self.*));
    }

    pub fn length_sq(self: *const @This()) f32 {
        return self.dot(self.*);
    }

    pub fn abs(self: *const @This()) @This() {
        return .{
            .x = @abs(self.x),
            .y = @abs(self.y),
            .z = @abs(self.z),
            .w = @abs(self.w),
        };
    }

    pub fn sign(self: *const @This()) @This() {
        return .{
            .x = std.math.sign(self.x),
            .y = std.math.sign(self.y),
            .z = std.math.sign(self.z),
            .w = std.math.sign(self.w),
        };
    }

    pub fn pow(self: *const @This(), p: f32) @This() {
        return .{
            .x = std.math.pow(f32, self.x, p),
            .y = std.math.pow(f32, self.y, p),
            .z = std.math.pow(f32, self.z, p),
            .w = std.math.pow(f32, self.w, p),
        };
    }

    pub fn normalize3D(self: *const @This()) @This() {
        var this = self.*;
        this.w = 0;

        const size = @sqrt(this.dot(this));
        return .{
            .x = self.x / size,
            .y = self.y / size,
            .z = self.z / size,
            .w = 0,
        };
    }

    pub fn normalize(self: *const @This()) @This() {
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

    pub fn quat_angle_axis(angle: f32, axis: Vec3) @This() {
        // - [Visualizing quaternions, an explorable video series](https://eater.net/quaternions)
        const s = @sin(angle / 2.0);
        var q = axis.normalize().scale(s);
        const c = @cos(angle / 2.0);
        return q.withw(c);
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

    // rotate by the angle diff between a and b
    // and the axis orthogonal to the plane formed by these vecs
    pub fn quat_from_diff(a: Vec3, b: Vec3) @This() {
        const da = a.normalize();
        const db = b.normalize();
        const axis = da.cross(db);
        const d = da.dot(db);
        if (d > 0.9999) return .quat_identity_rot();
        if (d < -0.9999) return @This().quat_identity_rot().scale(-1);
        const theta = std.math.acos(d);
        return @This().quat_angle_axis(theta, axis);
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

    pub fn rotate_vector(self: *const @This(), v: Vec3) Vec3 {
        const qv = Vec4{ .w = 0, .x = v.x, .y = v.y, .z = v.z };
        const q_conjugate = self.quat_conjugate();
        const q_result = self.quat_mul(qv).quat_mul(q_conjugate);
        return Vec3{ .x = q_result.x, .y = q_result.y, .z = q_result.z };
    }

    pub fn inverse_rotate_vector(self: *const @This(), v: Vec3) Vec3 {
        const qv = Vec4{ .w = 0, .x = v.x, .y = v.y, .z = v.z };
        const q_conjugate = self.quat_conjugate();
        const q_result = q_conjugate.quat_mul(qv).quat_mul(self.*);
        return Vec3{ .x = q_result.x, .y = q_result.y, .z = q_result.z };
    }

    pub fn as_buf(self: *@This()) []f32 {
        const bytes = std.mem.asBytes(self);
        return std.mem.bytesAsSlice(f32, bytes);
    }

    pub fn to_buf(self: *const @This()) [4]f32 {
        return .{ self.x, self.y, self.z, self.w };
    }

    pub fn from_buf(buf: anytype) @This() {
        if (comptime buf.len > 4) @compileError("buffer is too long for Vec4");
        return .{ .x = buf[0], .y = buf[1], .z = buf[2], .w = buf[3] };
    }

    pub fn random(rng: *const Rng) @This() {
        return .{
            .x = rng.next(),
            .y = rng.next(),
            .z = rng.next(),
            .w = rng.next(),
        };
    }
};

pub const Vec3 = extern struct {
    x: f32 = 0,
    y: f32 = 0,
    z: f32 = 0,
    __: f32 = 0,

    pub fn dot(self: *const @This(), other: @This()) f32 {
        return self.x * other.x +
            self.y * other.y +
            self.z * other.z;
    }

    // right handed cross product
    pub fn cross(self: *const @This(), other: @This()) @This() {
        return .{
            .x = self.y * other.z - self.z * other.y,
            .y = self.z * other.x - self.x * other.z,
            .z = self.x * other.y - self.y * other.x,
        };
    }

    pub fn mul(self: *const @This(), other: @This()) @This() {
        return .{
            .x = self.x * other.x,
            .y = self.y * other.y,
            .z = self.z * other.z,
        };
    }

    pub fn add(self: *const @This(), other: @This()) @This() {
        return .{
            .x = self.x + other.x,
            .y = self.y + other.y,
            .z = self.z + other.z,
        };
    }

    pub fn sub(self: *const @This(), other: @This()) @This() {
        return .{
            .x = self.x - other.x,
            .y = self.y - other.y,
            .z = self.z - other.z,
        };
    }

    pub fn scale(self: *const @This(), s: f32) @This() {
        return .{ .x = self.x * s, .y = self.y * s, .z = self.z * s };
    }

    pub fn mix(self: *const @This(), other: @This(), t: f32) @This() {
        return .{
            .x = std.math.lerp(self.x, other.x, t),
            .y = std.math.lerp(self.y, other.y, t),
            .z = std.math.lerp(self.z, other.z, t),
        };
    }

    pub fn clamp(self: *const @This(), low: f32, high: f32) @This() {
        return .{
            .x = std.math.clamp(self.x, low, high),
            .y = std.math.clamp(self.y, low, high),
            .z = std.math.clamp(self.z, low, high),
        };
    }

    pub fn splat(t: f32) @This() {
        return .{ .x = t, .y = t, .z = t };
    }

    pub fn max(self: *const @This()) f32 {
        return @max(self.x, @max(self.y, self.z));
    }

    pub fn length(self: *const @This()) f32 {
        return @sqrt(self.dot(self.*));
    }

    pub fn length_sq(self: *const @This()) f32 {
        return self.dot(self.*);
    }

    pub fn abs(self: *const @This()) @This() {
        return .{
            .x = @abs(self.x),
            .y = @abs(self.y),
            .z = @abs(self.z),
        };
    }

    pub fn sign(self: *const @This()) @This() {
        return .{
            .x = std.math.sign(self.x),
            .y = std.math.sign(self.y),
            .z = std.math.sign(self.z),
        };
    }

    pub fn pow(self: *const @This(), p: f32) @This() {
        return .{
            .x = std.math.pow(f32, self.x, p),
            .y = std.math.pow(f32, self.y, p),
            .z = std.math.pow(f32, self.z, p),
        };
    }

    pub fn normalize(self: *const @This()) @This() {
        var this = self.*;

        const size = @sqrt(this.dot(this));
        return .{
            .x = self.x / size,
            .y = self.y / size,
            .z = self.z / size,
        };
    }

    pub fn to_buf(self: *const @This()) [3]f32 {
        return .{ self.x, self.y, self.z };
    }

    pub fn from_buf(buf: anytype) @This() {
        if (comptime buf.len > 3) @compileError("buffer is too long for Vec3");
        return .{ .x = buf[0], .y = buf[1], .z = buf[2] };
    }

    pub fn random(rng: *const Rng) @This() {
        return .{
            .x = rng.next(),
            .y = rng.next(),
            .z = rng.next(),
        };
    }

    pub fn withw(self: *const @This(), w: f32) Vec4 {
        return .{ .x = self.x, .y = self.y, .z = self.z, .w = w };
    }
};

pub const Vec2 = extern struct {
    x: f32,
    y: f32,
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

    pub const Axis = enum { x, y, z };

    pub fn mul_vec4(self: *const @This(), v: Vec4) Vec4 {
        const this = self.transpose();
        return .{
            .x = this.data[0].dot(v),
            .y = this.data[1].dot(v),
            .z = this.data[2].dot(v),
            .w = this.data[3].dot(v),
        };
    }

    pub fn mul_scalar(self: *const @This(), t: f32) @This() {
        const this = self.*;
        this.data[0] = this.data[0].scale(t);
        this.data[1] = this.data[1].scale(t);
        this.data[2] = this.data[2].scale(t);
        this.data[3] = this.data[3].scale(t);
        return this;
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

    pub fn determinant3x3(self: *@This()) f32 {
        const a = self.data[0].x;
        const b = self.data[1].x;
        const c = self.data[2].x;
        const d = self.data[0].y;
        const e = self.data[1].y;
        const f = self.data[2].y;
        const g = self.data[0].z;
        const h = self.data[1].z;
        const i = self.data[2].z;
        return a * e * i + d * h * c + g * b * f - a * h * f - g * e * c - d * b * i;
    }

    pub fn inverse3x3(self: *const @This()) ?@This() {
        const det = self.determinant3x3();
        if (det == 0.0) return null;

        const a = self.data[0].x;
        const b = self.data[1].x;
        const c = self.data[2].x;
        const d = self.data[0].y;
        const e = self.data[1].y;
        const f = self.data[2].y;
        const g = self.data[0].z;
        const h = self.data[1].z;
        const i = self.data[2].z;

        return (@This(){
            .data = .{
                .{ .x = e * i - f * h, .y = c * h - b * i, .z = b * f - c * e },
                .{ .x = f * g - d * i, .y = a * i - c * g, .z = c * d - a * f },
                .{ .x = d * h - e * g, .y = b * g - a * h, .z = a * e - b * d },
                .{},
            },
        }).transpose().mul_scalar(1.0 / det);
    }

    pub fn change_basis3x3(self: *const @This(), basis: *const @This()) ?@This() {
        return basis.mul_mat(self).mul_mat(&basis.inverse3x3() orelse return null);
    }

    pub fn axis(self: *const @This(), a: Axis) Vec4 {
        // so axes would be stored as a column (as we have column vectors)
        // but we store everything transposed on cpu side.
        return switch (a) {
            .x => self.data[0],
            .y => self.data[1],
            .z => self.data[2],
        };
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

    pub fn view(eye: Vec3, fwd: Vec3, left: Vec3, up: Vec3) @This() {
        // - [» Deriving the View Matrix](https://twodee.org/blog/17560)
        //   - this seems to be left handed

        const translate_inv = Mat4x4{ .data = .{
            .{ .x = 1, .y = 0, .z = 0, .w = -eye.x },
            .{ .x = 0, .y = 1, .z = 0, .w = -eye.y },
            .{ .x = 0, .y = 0, .z = 1, .w = -eye.z },
            .{ .x = 0, .y = 0, .z = 0, .w = 1 },
        } };

        return (Mat4x4{ .data = .{
            left.normalize().withw(0),
            up.normalize().withw(0),
            fwd.normalize().withw(0),
            .{ .w = 1 },
        } }).transpose().mul_mat(translate_inv.transpose());
    }

    pub fn scaling_mat(vec: Vec3) @This() {
        return .{ .data = .{
            .{ .x = vec.x },
            .{ .y = vec.y },
            .{ .z = vec.z },
            .{ .w = 1 },
        } };
    }

    pub fn translation_mat(vec: Vec3) @This() {
        return .{ .data = .{
            .{ .x = 1 },
            .{ .y = 1 },
            .{ .z = 1 },
            .{ .x = vec.x, .y = vec.y, .z = vec.z, .w = 1 },
        } };
    }

    pub fn rot_mat_euler_angles(vec: Vec3) @This() {
        const rotz = (@This(){ .data = .{
            .{ .x = @cos(vec.z), .y = -@sin(vec.z), .z = 0 },
            .{ .x = @sin(vec.z), .y = @cos(vec.z), .z = 0 },
            .{ .x = 0, .y = 0, .z = 1 },
            .{ .w = 1 },
        } }).transpose();
        const roty = (@This(){ .data = .{
            .{ .x = @cos(vec.y), .y = 0, .z = @sin(vec.y) },
            .{ .x = 0, .y = 1, .z = 0 },
            .{ .x = -@sin(vec.y), .y = 0, .z = @cos(vec.y) },
            .{ .w = 1 },
        } }).transpose();
        const rotx = (@This(){ .data = .{
            .{ .x = 1, .y = 0, .z = 0 },
            .{ .x = 0, .y = @cos(vec.x), .z = -@sin(vec.x) },
            .{ .x = 0, .y = @sin(vec.x), .z = @cos(vec.x) },
            .{ .w = 1 },
        } }).transpose();
        return roty.mul_mat(rotx).mul_mat(rotz);
    }

    pub fn rot_mat_from_quat(rot: Vec4) @This() {
        const x = Vec3{ .x = 1 };
        const y = Vec3{ .y = 1 };
        const z = Vec3{ .z = 1 };

        return .{ .data = .{
            rot.rotate_vector(x).withw(0),
            rot.rotate_vector(y).withw(0),
            rot.rotate_vector(z).withw(0),
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

    pub fn decompose_rot_trans(self: *const @This()) struct { translation: Vec3, rotation: Vec4 } {
        const translate = self.data[3].xyz();

        var rot = Vec4.quat_identity_rot();
        var t: f32 = 0;
        const m00 = self.data[0].x;
        const m01 = self.data[0].y;
        const m10 = self.data[1].x;
        const m11 = self.data[1].y;
        const m02 = self.data[0].z;
        const m20 = self.data[2].x;
        const m12 = self.data[1].z;
        const m21 = self.data[2].y;
        const m22 = self.data[2].z;
        if (m22 < 0) {
            if (m00 > m11) {
                t = 1 + m00 - m11 - m22;
                rot = .{ .x = t, .y = m01 + m10, .z = m20 + m02, .w = m12 - m21 };
            } else {
                t = 1 - m00 + m11 - m22;
                rot = .{ .x = m01 + m10, .y = t, .z = m12 + m21, .w = m20 - m02 };
            }
        } else {
            if (m00 < -m11) {
                t = 1 - m00 - m11 + m22;
                rot = .{ .x = m20 + m02, .y = m12 + m21, .z = t, .w = m01 - m10 };
            } else {
                t = 1 + m00 + m11 + m22;
                rot = .{ .x = m12 - m21, .y = m20 - m02, .z = m01 - m10, .w = t };
            }
        }
        rot = rot.scale(0.5 / @sqrt(t));

        return .{
            .translation = translate,
            .rotation = rot,
        };
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
            q = q.normalize();

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
    renderer_basis: struct {
        fwd: Vec3,
        right: Vec3,
        up: Vec3,
    },
    world_basis: struct {
        fwd: Vec3,
        right: Vec3,
        up: Vec3,
    },

    pub const constants = struct {
        pub const pitch_min = -std.math.pi / 2.0 + 0.1;
        pub const pitch_max = std.math.pi / 2.0 - 0.1;
        pub const basis = struct {
            // right handed -y up
            pub const vulkan = struct {
                pub const up = Vec3{ .y = -1 };
                pub const fwd = Vec3{ .z = 1 };
                pub const right = Vec3{ .x = 1 };
            };
            // right handed y up
            pub const opengl = struct {
                pub const up = Vec3{ .y = 1 };
                pub const fwd = Vec3{ .z = 1 };
                pub const right = Vec3{ .x = -1 };
            };
        };
    };

    // right handed basis only (this file is right handed)
    pub fn init(renderer_basis: anytype, world_basis: anytype) @This() {
        return .{
            .renderer_basis = .{
                .fwd = renderer_basis.fwd,
                .right = renderer_basis.right,
                .up = renderer_basis.up,
            },
            .world_basis = .{
                .fwd = world_basis.fwd,
                .right = world_basis.right,
                .up = world_basis.up,
            },
        };
    }

    pub fn world_to_screen_mat(self: *const @This(), v: struct {
        width: u32,
        height: u32,
        pos: Vec3,
        pitch: f32,
        yaw: f32,
        near: f32 = 0.1,
        far: f32 = 10000.0,
        fov: f32 = std.math.pi / 3.0,
    }) Mat4x4 {
        const rot = self.rot_quat(v.pitch, v.yaw);
        const up = rot.rotate_vector(self.renderer_basis.up);
        const fwd = rot.rotate_vector(self.renderer_basis.fwd);
        const left = rot.rotate_vector(self.renderer_basis.right.scale(-1));

        const projection_matrix = Mat4x4.perspective_projection(v.height, v.width, v.near, v.far, v.fov);
        const view_matrix = Mat4x4.view(v.pos, fwd, left, up);
        const world_to_screen = projection_matrix.mul_mat(view_matrix);

        return world_to_screen;
    }

    pub fn rot_quat(self: *const @This(), pitch: f32, yaw: f32) Vec4 {
        var rot = Vec4.quat_identity_rot();
        rot = rot.quat_mul(Vec4.quat_angle_axis(pitch, self.world_basis.right));
        rot = rot.quat_mul(Vec4.quat_angle_axis(yaw, self.world_basis.up));
        rot = rot.quat_conjugate();
        return rot;
    }

    pub fn dirs(self: *const @This(), pitch: f32, yaw: f32) struct {
        rot: Vec4,
        fwd: Vec3,
        up: Vec3,
        right: Vec3,
    } {
        const rot = self.rot_quat(pitch, yaw);
        const up = rot.rotate_vector(self.world_basis.up);
        const fwd = rot.rotate_vector(self.world_basis.fwd);
        const right = rot.rotate_vector(self.world_basis.right);

        return .{
            .rot = rot,
            .up = up,
            .fwd = fwd,
            .right = right,
        };
    }
};
