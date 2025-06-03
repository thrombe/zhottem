const std = @import("std");

const math = @import("math.zig");

const main = @import("main.zig");
const allocator = main.allocator;

const utils = @import("utils.zig");

// - [OpenGL Skeletal Animation Tutorial #1 - YouTube](https://www.youtube.com/watch?v=f3Cr8Yx3GGA)
pub const Armature = struct {
    bones: []Bone,
    animations: []Animation,
};

pub const Animation = struct {
    name: []const u8,
    bones: []BoneAnimation,
};

pub const BoneAnimation = struct {
    translation_keyframes: std.ArrayList(Keyframe),
    rotation_keyframes: std.ArrayList(Keyframe),
    scale_keyframes: std.ArrayList(Keyframe),
};

pub const Keyframe = struct {
    value: math.Vec4,
    time: f32,
};

pub const BoneId = u32; // index into Model.bones

// - [glTF-Tutorials/gltfTutorial/gltfTutorial_020_Skins.md](https://github.com/KhronosGroup/glTF-Tutorials/blob/main/gltfTutorial/gltfTutorial_020_Skins.md)
//   - joint transform = global joint transform * inverse bind matrix
// - [glTF™ 2.0 Specification](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#transformations)
//   - you get global transforms by traversing the node hierarchy. (and also animations i'd guess)
// for updating it to the gpu buffer, start with a list of matrices
// initialize all transforms to the inverse bind matrices for the joints
// this will transform all vertices into the "bone space"
// then apply the transform as described below (bone = calculated * bone)
// start from the parent bone, apply bone transform to self, self.children
// loop through the children, repeat same for each child in depth first order.
pub const Bone = struct {
    inverse_bind_matrix: math.Mat4x4,
    local_transform: Transform,
    children: []BoneId,
    parent: ?BoneId,
};

pub const Transform = struct {
    translation: math.Vec3 = math.Vec3{},
    rotation: math.Vec4 = math.Vec4.quat_identity_rot(),
    scale: math.Vec3 = math.Vec3.splat(1.0),

    pub fn apply_global(self: *const @This(), transform: @This()) @This() {
        return .{
            .translation = transform.translation.add(transform.rotation.rotate_vector(self.translation)),
            .rotation = transform.rotation.quat_local_rot(self.rotation),
            .scale = transform.scale.mul(self.scale),
        };
    }

    // NOTE: returns matrix with column vectors (vulkan => opengl.transpose())
    pub fn transform_mat(self: *const @This()) math.Mat4x4 {
        const rot = self.rotation orelse [4]f32{ 0, 0, 0, 1 };
        const scale = self.scale orelse [3]f32{ 1, 1, 1 };
        const translate = self.translation orelse [3]f32{ 0, 0, 0 };

        const rotation_mat = math.Mat4x4.rot_mat_from_quat(.{
            .x = rot[0],
            .y = rot[1],
            .z = rot[2],
            .w = rot[3],
        });
        const scale_mat = math.Mat4x4.scaling_mat(.{
            .x = scale[0],
            .y = scale[1],
            .z = scale[2],
        });
        const translation_mat = math.Mat4x4.translation_mat(.{
            .x = translate[0],
            .y = translate[1],
            .z = translate[2],
        });

        // - [glTF™ 2.0 Specification](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#transformations)
        // To compose the local transformation matrix, TRS properties MUST be converted to matrices and postmultiplied in the T * R * S order.
        // first the scale is applied to the vertices, then the rotation, and then the translation
        return translation_mat.mul_mat(rotation_mat).mul_mat(scale_mat);
    }
};

pub const VertexId = u32; // index into mesh.{.vertices, .normals, .uvs, .bones}

pub const VertexBone = struct {
    bone: BoneId,
    weight: f32,
};

pub const Mesh = struct {
    name: []const u8,
    vertices: [][3]f32,
    normals: [][3]f32,
    uvs: [][2]f32,
    faces: [][3]VertexId,

    // position = bones.map(bone => bone.transform * pos * bone.weight).sum()
    // these bones are not in the model skeleton hierarchy.
    // we just upload all bones to the gpu after transforming all bones considering the skeleton hierarchy.
    // pretty common to cap bones per vertex to 4
    bones: [][]VertexBone,
    alloc: std.mem.Allocator,

    pub fn cube(alloc: std.mem.Allocator) !@This() {
        var vertices = std.ArrayList([3]f32).init(alloc);
        errdefer vertices.deinit();
        var normals = std.ArrayList([3]f32).init(alloc);
        errdefer normals.deinit();
        var uvs = std.ArrayList([2]f32).init(alloc);
        errdefer uvs.deinit();
        var bones = std.ArrayList([]VertexBone).init(alloc);
        errdefer bones.deinit();
        var faces = std.ArrayList([3]u32).init(alloc);
        errdefer faces.deinit();

        try vertices.appendSlice(&[_][3]f32{
            .{ 1.0, -1.0, -1.0 },
            .{ 1.0, -1.0, 1.0 },
            .{ -1.0, -1.0, 1.0 },
            .{ -1.0, -1.0, -1.0 },
            .{ 1.0, 1.0, -1.0 },
            .{ 1.0, 1.0, 1.0 },
            .{ -1.0, 1.0, 1.0 },
            .{ -1.0, 1.0, -1.0 },
        });
        try normals.appendSlice(&[_][3]f32{
            .{ 0.0, 0.0, 1.0 },
            .{ 0.0, 0.0, 1.0 },
            .{ 0.0, 0.0, 1.0 },
            .{ 0.0, 0.0, 1.0 },
            .{ 0.0, 0.0, -1.0 },
            .{ 0.0, 0.0, -1.0 },
            .{ 0.0, 0.0, -1.0 },
            .{ 0.0, 0.0, -1.0 },
        });
        try uvs.appendSlice(&[_][2]f32{
            .{ 1.0, 0.0 },
            .{ 1.0, 1.0 },
            .{ 0.0, 1.0 },
            .{ 0.0, 0.0 },
            .{ 1.0, 0.0 },
            .{ 1.0, 1.0 },
            .{ 0.0, 1.0 },
            .{ 0.0, 0.0 },
        });
        for (0..vertices.items.len) |_| {
            const vbone = try alloc.alloc(VertexBone, 1);
            vbone[0].bone = 0;
            vbone[0].weight = 1;
            try bones.append(vbone);
        }
        try faces.appendSlice(&[_][3]u32{
            .{ 0, 1, 2 },
            .{ 0, 2, 3 },
            .{ 4, 5, 6 },
            .{ 4, 6, 7 },
            .{ 0, 4, 7 },
            .{ 0, 7, 3 },
            .{ 1, 5, 6 },
            .{ 1, 6, 2 },
            .{ 0, 1, 5 },
            .{ 0, 5, 4 },
            .{ 3, 2, 6 },
            .{ 3, 6, 7 },
        });

        return .{
            .alloc = alloc,
            .name = try alloc.dupe(u8, "cube"),
            .vertices = try vertices.toOwnedSlice(),
            .normals = try normals.toOwnedSlice(),
            .uvs = try uvs.toOwnedSlice(),
            .bones = try bones.toOwnedSlice(),
            .faces = try faces.toOwnedSlice(),
        };
    }

    pub fn plane(alloc: std.mem.Allocator) !@This() {
        var vertices = std.ArrayList([3]f32).init(alloc);
        errdefer vertices.deinit();
        var normals = std.ArrayList([3]f32).init(alloc);
        errdefer normals.deinit();
        var uvs = std.ArrayList([2]f32).init(alloc);
        errdefer uvs.deinit();
        var bones = std.ArrayList([]VertexBone).init(alloc);
        errdefer bones.deinit();
        var faces = std.ArrayList([3]u32).init(alloc);
        errdefer faces.deinit();

        try vertices.appendSlice(&[_][3]f32{
            .{ -1.0, 0.0, -1.0 },
            .{ 1.0, 0.0, -1.0 },
            .{ 1.0, 0.0, 1.0 },
            .{ -1.0, 0.0, 1.0 },
        });
        try normals.appendSlice(&[_][3]f32{
            .{ 0.0, 1.0, 0.0 },
            .{ 0.0, 1.0, 0.0 },
            .{ 0.0, 1.0, 0.0 },
            .{ 0.0, 1.0, 0.0 },
        });
        try uvs.appendSlice(&[_][2]f32{
            .{ 0.0, 0.0 },
            .{ 1.0, 0.0 },
            .{ 1.0, 1.0 },
            .{ 0.0, 1.0 },
        });
        for (0..vertices.items.len) |_| {
            const vbone = try alloc.alloc(VertexBone, 1);
            vbone[0].bone = 0;
            vbone[0].weight = 1;
            try bones.append(vbone);
        }
        try faces.appendSlice(&[_][3]u32{
            .{ 0, 1, 2 },
            .{ 0, 2, 3 },
        });

        return .{
            .alloc = alloc,
            .name = try alloc.dupe(u8, "plane"),
            .vertices = try vertices.toOwnedSlice(),
            .normals = try normals.toOwnedSlice(),
            .uvs = try uvs.toOwnedSlice(),
            .bones = try bones.toOwnedSlice(),
            .faces = try faces.toOwnedSlice(),
        };
    }

    pub fn boneless(self: *const @This(), alloc: std.mem.Allocator) !@This() {
        const bones = try alloc.dupe([]VertexBone, self.bones);

        for (0..self.vertices.len) |i| {
            const vbone = try alloc.alloc(VertexBone, 1);
            vbone[0].bone = 0;
            vbone[0].weight = 1;
            bones[i] = vbone;
        }

        const new = @This(){
            .alloc = alloc,
            .name = try alloc.dupe(u8, self.name),
            .bones = bones,
            .uvs = try alloc.dupe([2]f32, self.uvs),
            .faces = try alloc.dupe([3]u32, self.faces),
            .normals = try alloc.dupe([3]f32, self.normals),
            .vertices = try alloc.dupe([3]f32, self.vertices),
        };

        return new;
    }

    pub fn deinit(self: *@This()) void {
        self.alloc.free(self.name);
        self.alloc.free(self.vertices);
        self.alloc.free(self.normals);
        self.alloc.free(self.faces);
        for (self.bones) |bone| {
            self.alloc.free(bone);
        }
        self.alloc.free(self.bones);
        self.alloc.free(self.uvs);
    }
};

pub const Gltf = struct {
    // - [glTF™ 2.0 Specification](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html)
    //   - all data is little endian
    //
    // NOTE: we don't handle big endian as it practically does not exist :} (i am lazy)

    header: Header,
    info: std.json.Parsed(Info),
    bin_chunk: Chunk,
    arena: *std.heap.ArenaAllocator,
    alloc: std.mem.Allocator,

    buf: []const u8,

    pub fn parse_glb(path: []const u8) !@This() {
        var arena = try allocator.create(std.heap.ArenaAllocator);
        errdefer allocator.destroy(arena);
        arena.* = std.heap.ArenaAllocator.init(allocator.*);
        errdefer arena.deinit();
        const alloc = arena.allocator();

        var reader = try Reader.load(alloc, path);
        errdefer reader.deinit();

        const header = try reader.read(Header);
        if (!std.mem.eql(u8, &header.magic, "glTF")) {
            return error.InvalidGltfHeader;
        }
        if (@as(usize, @intCast(header.length)) != reader.buf.len) {
            return error.CurruptedFile;
        }

        var json_chunk = try reader.chunk();
        if (!std.mem.eql(u8, &json_chunk.typ, "JSON")) {
            return error.InvalidJsonChunk;
        }

        const bin_chunk = try reader.chunk();
        if (!std.mem.eql(u8, &bin_chunk.typ, "BIN\x00")) {
            return error.InvalidBinChunk;
        }

        var info = try json_chunk.read_json(Info);
        errdefer info.deinit();

        return .{
            .header = header,
            .buf = reader.buf,
            .bin_chunk = bin_chunk,
            .info = info,
            .arena = arena,
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.info.deinit();
        self.alloc.free(self.buf);
        self.arena.deinit();
        allocator.destroy(self.arena);
    }

    pub fn parse_mesh(self: *@This(), mesh: *Info.MeshInfo) !Mesh {
        var vertices = std.ArrayList([3]f32).init(self.alloc);
        errdefer vertices.deinit();
        var normals = std.ArrayList([3]f32).init(self.alloc);
        errdefer normals.deinit();
        var uvs = std.ArrayList([2]f32).init(self.alloc);
        errdefer uvs.deinit();
        var weights = std.ArrayList([4]f32).init(self.alloc);
        defer weights.deinit();
        var joints = std.ArrayList([4]u32).init(self.alloc);
        defer joints.deinit();
        var faces = std.ArrayList([3]u32).init(self.alloc);
        errdefer faces.deinit();

        for (mesh.primitives) |prim| {
            const base: u32 = @intCast(vertices.items.len);

            for (prim.attributes.items) |attr| {
                switch (attr.typ) {
                    .position => {
                        const slice = try self.get_slice(attr.acc, [3]f32);
                        try vertices.appendSlice(slice);
                    },
                    .normal => {
                        const slice = try self.get_slice(attr.acc, [3]f32);
                        try normals.appendSlice(slice);
                    },
                    .texcoord => |set| {
                        if (set.set != 0) {
                            continue;
                        }

                        const slice = try self.get_slice(attr.acc, [2]f32);
                        try uvs.appendSlice(slice);
                    },
                    .weight => |set| {
                        if (set.set != 0) {
                            continue;
                        }

                        if (self.accessor(attr.acc).matches_typ([4]f32)) {
                            const slice = try self.get_slice(attr.acc, [4]f32);
                            try weights.appendSlice(slice);
                        } else if (self.accessor(attr.acc).matches_typ([4]u8)) {
                            const slice = try self.get_slice(attr.acc, [4]u8);
                            for (slice) |w| {
                                try weights.append([_]f32{
                                    @as(f32, @floatFromInt(w[0])) / 255.0,
                                    @as(f32, @floatFromInt(w[1])) / 255.0,
                                    @as(f32, @floatFromInt(w[2])) / 255.0,
                                    @as(f32, @floatFromInt(w[3])) / 255.0,
                                });
                            }
                        } else if (self.accessor(attr.acc).matches_typ([4]u16)) {
                            const slice = try self.get_slice(attr.acc, [4]u16);
                            for (slice) |w| {
                                try weights.append([_]f32{
                                    @as(f32, @floatFromInt(w[0])) / 255.0,
                                    @as(f32, @floatFromInt(w[1])) / 255.0,
                                    @as(f32, @floatFromInt(w[2])) / 255.0,
                                    @as(f32, @floatFromInt(w[3])) / 255.0,
                                });
                            }
                        } else {
                            continue;
                        }
                    },
                    .joint => |set| {
                        if (set.set != 0) {
                            continue;
                        }

                        if (self.accessor(attr.acc).matches_typ([4]u8)) {
                            const slice = try self.get_slice(attr.acc, [4]u8);
                            for (slice) |b| {
                                try joints.append([_]u32{
                                    b[0],
                                    b[1],
                                    b[2],
                                    b[3],
                                });
                            }
                        } else if (self.accessor(attr.acc).matches_typ([4]u16)) {
                            const slice = try self.get_slice(attr.acc, [4]u16);
                            for (slice) |b| {
                                try joints.append([_]u32{
                                    b[0],
                                    b[1],
                                    b[2],
                                    b[3],
                                });
                            }
                        } else {
                            continue;
                        }
                    },
                    else => {},
                }
            }

            const indices = prim.indices.?;
            if (self.accessor(indices).matches_typ(u16)) {
                const slice = try self.get_slice(indices, [3]u16);
                const duped = try self.alloc.alloc([3]u32, slice.len);
                defer self.alloc.free(duped);
                for (0..duped.len) |i| {
                    duped[i][0] = slice[i][0] + base;
                    duped[i][1] = slice[i][1] + base;
                    duped[i][2] = slice[i][2] + base;
                }
                try faces.appendSlice(duped);
            } else if (self.accessor(indices).matches_typ(u32)) {
                const slice = try self.get_slice(indices, [3]u32);
                try faces.appendSlice(slice);
            } else {
                return error.BadIndexType;
            }
        }
        var bones = std.ArrayList([]VertexBone).init(self.alloc);
        errdefer bones.deinit();

        for (joints.items, weights.items) |b, w| {
            const bone = try self.alloc.alloc(VertexBone, 4);
            errdefer self.alloc.free(bone);

            for (0..4) |i| {
                bone[i].bone = b[i];
                bone[i].weight = w[i];
            }

            try bones.append(bone);
        }

        return .{
            .alloc = self.alloc,
            .name = try self.alloc.dupe(u8, mesh.name),
            .vertices = try vertices.toOwnedSlice(),
            .normals = try normals.toOwnedSlice(),
            .uvs = try uvs.toOwnedSlice(),
            .bones = try bones.toOwnedSlice(),
            .faces = try faces.toOwnedSlice(),
        };
    }

    pub fn parse_armature(self: *@This(), skini: *Info.SkinInfo) !Armature {
        var bones = std.ArrayList(Bone).init(self.alloc);

        if (skini.inverseBindMatrices) |ibm| {
            const matrices = try self.get_slice(ibm, [16]f32);
            for (matrices) |m| {
                // https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#data-alignment
                // apparently matrices are already column major
                try bones.append(.{
                    .inverse_bind_matrix = std.mem.bytesAsValue(math.Mat4x4, std.mem.sliceAsBytes(&m)).*,
                    .local_transform = .{},
                    .children = undefined,
                    .parent = null,
                });
            }
        }

        // create a node to bone mapping
        var joint_to_bone = std.AutoHashMap(Info.NodeIndex, BoneId).init(self.alloc);
        for (skini.joints, 0..) |j, i| {
            try joint_to_bone.put(j, @intCast(i));
        }

        // assign local transforms and correct chidren bones
        for (skini.joints, 0..) |j, i| {
            const node = &self.info.value.nodes[j];
            bones.items[i].local_transform = node.transform();

            var children = std.ArrayList(BoneId).init(self.alloc);
            for (node.children) |chi| {
                try children.append(joint_to_bone.get(@intCast(chi)) orelse continue);
            }
            bones.items[i].children = try children.toOwnedSlice();
        }

        // find parents
        for (0..bones.items.len) |i| {
            for (bones.items[i].children) |j| {
                bones.items[j].parent = @intCast(i);
            }
        }

        const animations = try self.parse_skin_animations(&joint_to_bone);

        return .{
            .bones = try bones.toOwnedSlice(),
            .animations = animations,
        };
    }

    fn parse_skin_animations(self: *@This(), joint_to_bone: *std.AutoHashMap(Info.NodeIndex, BoneId)) ![]Animation {
        var animations = std.ArrayList(Animation).init(self.alloc);

        const info = &self.info.value;
        const animations_info = info.animations;
        for (animations_info) |anim| {
            const name = anim.name;
            const bone_keyframes = try self.alloc.alloc(BoneAnimation, joint_to_bone.count());
            @memset(bone_keyframes, BoneAnimation{
                .translation_keyframes = .init(self.alloc),
                .rotation_keyframes = .init(self.alloc),
                .scale_keyframes = .init(self.alloc),
            });

            for (anim.channels) |channel| {
                const sampler = &anim.samplers[channel.sampler];
                const times = try self.get_slice(sampler.input, f32);
                const bonei = joint_to_bone.get(channel.target.node) orelse continue;
                const keyframes = &bone_keyframes[bonei];

                switch (channel.target.path) {
                    .rotation => {
                        const values = try self.get_slice(sampler.output, [4]f32);
                        for (times, values) |t, v| {
                            try keyframes.rotation_keyframes.append(.{
                                .value = .{
                                    .x = v[0],
                                    .y = v[1],
                                    .z = v[2],
                                    .w = v[3],
                                },
                                .time = t,
                            });
                        }
                    },
                    .scale => {
                        const values = try self.get_slice(sampler.output, [3]f32);
                        for (times, values) |t, v| {
                            try keyframes.scale_keyframes.append(.{
                                .value = .{
                                    .x = v[0],
                                    .y = v[1],
                                    .z = v[2],
                                },
                                .time = t,
                            });
                        }
                    },
                    .translation => {
                        const values = try self.get_slice(sampler.output, [3]f32);
                        for (times, values) |t, v| {
                            try keyframes.translation_keyframes.append(.{
                                .value = .{
                                    .x = v[0],
                                    .y = v[1],
                                    .z = v[2],
                                },
                                .time = t,
                            });
                        }
                    },
                    else => continue,
                }
            }

            if (bone_keyframes[0].translation_keyframes.items.len == 0) {
                continue;
            }
            try animations.append(.{
                .name = try self.alloc.dupe(u8, name),
                .bones = bone_keyframes,
            });
        }

        return try animations.toOwnedSlice();
    }

    fn find_mesh(self: *@This(), name: []const u8) ?*Info.MeshInfo {
        const info = &self.info.value;

        for (0..info.meshes.len) |meshi| {
            const m = &info.meshes[meshi];
            std.debug.print("mesh: {s}\n", .{m.name});
            if (std.mem.eql(u8, m.name, name)) {
                return m;
            }
        }

        return null;
    }

    fn find_skin(self: *@This(), name: []const u8) ?*Info.SkinInfo {
        const info = &self.info.value;
        const skins = info.skins;

        for (0..skins.len) |skini| {
            const s = &skins[skini];
            const sname = s.name;
            std.debug.print("skin: {s}\n", .{sname});
            if (std.mem.eql(u8, sname, name)) {
                return s;
            }
        }

        return null;
    }

    fn accessor(self: *@This(), ai: Info.AccessorIndex) *Info.Accessor {
        return &self.info.value.accessors[ai];
    }

    fn get_slice(self: *@This(), ai: Info.AccessorIndex, typ: type) ![]const typ {
        const acc = &self.info.value.accessors[ai];

        const Ti = @typeInfo(typ);
        const T = switch (Ti) {
            .array => |child| child.child,
            else => typ,
        };

        // array length not checked deleberately here. sometimes we want [3]u32, but the defined type is u32 implicitly in multiples of 3
        if (!acc.componentType.typ(T)) {
            return error.BadAccessorTyp;
        }

        const view = &self.info.value.bufferViews[acc.bufferView];
        if (view.buffer != 0 or self.info.value.buffers[view.buffer].uri != null) {
            return error.ExternalBufferNotSupported;
        }

        return self.bin_chunk.get_slice(acc, view, typ);
    }

    fn get_bytes(self: *@This(), ai: Info.AccessorIndex) ![]const u8 {
        const acc = &self.info.value.accessors[ai];
        const view = &self.info.value.bufferViews[acc.bufferView];
        if (view.buffer != 0 or self.info.value.buffers[view.buffer].uri != null) {
            return error.ExternalBufferNotSupported;
        }

        const raw = self.bin_chunk.get_bytes(acc, view);
        return raw;
    }

    const Header = extern struct {
        magic: [4]u8,
        version: u32,
        length: u32,
    };

    pub const Info = struct {
        scene: SceneIndex,
        scenes: []SceneInfo,
        nodes: []Node,
        meshes: []MeshInfo = &.{},
        materials: []MaterialInfo = &.{},
        skins: []SkinInfo = &.{},
        images: []ImageInfo = &.{},
        textures: []TextureInfo = &.{},
        samplers: []SamplerInfo = &.{},
        animations: []AnimationInfo = &.{},
        accessors: []Accessor,
        bufferViews: []BufferView,
        buffers: []struct {
            byteLength: usize,
            uri: ?[]const u8 = null,
        },

        pub const AccessorIndex = usize;
        pub const SceneIndex = usize;
        pub const NodeIndex = usize;
        pub const SamplerIndex = usize;
        pub const SkinIndex = usize;
        pub const MeshIndex = usize;
        pub const ImageIndex = usize;
        pub const BufferIndex = usize;
        pub const BufferViewIndex = usize;
        pub const MaterialIndex = usize;
        // pub const CameraIndex = usize;

        pub const ZhottExtras = struct {
            zhott_components: []Component,

            pub const Component = struct {
                component_name: []const u8,
                value: std.json.Value,
            };
        };

        pub const SceneInfo = struct {
            name: []const u8 = &.{},
            nodes: []NodeIndex = &.{},
        };
        pub const Node = struct {
            name: []const u8 = &.{},

            mesh: ?MeshIndex = null,
            // camera: ?CameraIndex = null,
            skin: ?SkinIndex = null,

            rotation: ?[4]f32 = null,
            scale: ?[3]f32 = null,
            translation: ?[3]f32 = null,

            matrix: ?[16]f32 = null,

            weights: ?[]f32 = null, // only with morph targets

            children: []NodeIndex = &.{},

            extras: ?ZhottExtras = null,

            pub fn transform(self: *const @This()) Transform {
                if (self.matrix) |m| {
                    const mat = std.mem.bytesToValue(math.Mat4x4, std.mem.asBytes(&m));
                    // OOF?: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#data-alignment
                    // matrices though accessors are already in column major, but no idea about the json ones.
                    const decom = mat.transpose().decompose_rot_trans();
                    std.debug.print("broken code is run. scale not handled\n", .{});
                    return .{
                        .translation = decom.translation,
                        .rotation = decom.rotation,
                    };
                }

                const rot = self.rotation orelse math.Vec4.quat_identity_rot().to_buf();
                const scale = self.scale orelse [3]f32{ 1, 1, 1 };
                const translate = self.translation orelse [3]f32{ 0, 0, 0 };

                return .{
                    .translation = .{
                        .x = translate[0],
                        .y = translate[1],
                        .z = translate[2],
                    },
                    .rotation = .{
                        .x = rot[0],
                        .y = rot[1],
                        .z = rot[2],
                        .w = rot[3],
                    },
                    .scale = .{
                        .x = scale[0],
                        .y = scale[1],
                        .z = scale[2],
                    },
                };
            }
        };
        pub const MeshInfo = struct {
            name: []const u8 = &.{},
            primitives: []struct {
                attributes: Attributes,
                indices: ?AccessorIndex = null,
                material: ?MaterialIndex = null,
                mode: enum(u32) {
                    points = 0,
                    lines = 1,
                    line_loop = 2,
                    line_strip = 3,
                    triangles = 4,
                    triangle_strip = 5,
                    triangle_fan = 6,
                } = .triangles,
                // targets: []struct {}, // not supported
            },
        };
        pub const MaterialInfo = struct {
            name: []const u8 = &.{},
        };
        pub const SkinInfo = struct {
            name: []const u8 = &.{},
            inverseBindMatrices: ?AccessorIndex = null,
            skeleton: ?NodeIndex = null,
            joints: []NodeIndex,
        };
        pub const ImageInfo = struct {
            name: []const u8 = &.{},

            // uri: ?[]const u8 = null, // not supported
            mimeType: []const u8,
            bufferView: BufferViewIndex,
        };
        pub const TextureInfo = struct {
            name: []const u8 = &.{},
            sampler: ?SamplerIndex = null,
            source: ImageIndex,
        };
        pub const SamplerInfo = struct {
            name: []const u8 = &.{},
            wrapT: Wrap = .repeat,
            wrapS: Wrap = .repeat,
            magFilter: ?enum(u32) {
                nearest = 9728,
                linear = 9729,
            } = null,
            minFilter: ?enum(u32) {
                nearest = 9728,
                linear = 9729,
                nearest_mipmap_nearest = 9984,
                linear_mipmap_nearest = 9985,
                nearest_mipmap_linear = 9986,
                linear_mipmap_linear = 9987,
            } = null,

            pub const Wrap = enum(u32) {
                clamp_to_edge = 33071,
                mirror_repeat = 33648,
                repeat = 10497,
            };
        };
        pub const AnimationInfo = struct {
            name: []const u8 = &.{},
            channels: []struct {
                sampler: SamplerIndex,

                // this tells us what property of what node is to be animated by the given sampler
                target: struct {
                    node: NodeIndex,
                    path: enum {
                        rotation,
                        scale,
                        translation,
                        weights, // only with morph targets
                    },
                },
            },
            samplers: []struct {
                // this are the input times
                // relative to start of this animation.
                input: AccessorIndex,

                // we want to translate *to* these values at the corresponding time steps.
                output: AccessorIndex,
                interpolation: enum {
                    LINEAR,
                    STEP,
                    CUBICSPLINE,
                } = .LINEAR,
            },
        };
        pub const PrimitiveAttribute = struct {
            typ: PrimitiveAttributeType,
            acc: AccessorIndex,

            fn parse_from_int_and_field_name(num: i64, fieldname: []const u8) !@This() {
                @setEvalBranchQuota(2000);
                inline for (@typeInfo(PrimitiveAttributeType).@"union".fields) |field| {
                    if (std.ascii.startsWithIgnoreCase(fieldname, field.name)) {
                        const typ = @unionInit(
                            PrimitiveAttributeType,
                            field.name,
                            switch (comptime std.meta.stringToEnum(std.meta.Tag(PrimitiveAttributeType), field.name).?) {
                                .texcoord, .color, .joint, .weight => blk: {
                                    var s = std.mem.splitSequence(u8, fieldname, "_");
                                    _ = s.next();
                                    const unparsed = s.next() orelse return error.MissingField;
                                    const parsed = std.fmt.parseInt(u32, unparsed, 10) catch return error.MissingField;
                                    break :blk .{ .set = parsed };
                                },
                                else => {},
                            },
                        );
                        return .{ .typ = typ, .acc = @intCast(num) };
                    }
                }

                return error.MissingField;
            }
        };
        pub const PrimitiveAttributeType = union(enum) {
            position,
            normal,
            tangent,

            texcoord: struct { set: u32 },
            color: struct { set: u32 },
            joint: struct { set: u32 },
            weight: struct { set: u32 },

            pub fn components(self: @This()) usize {
                return switch (self) {
                    .position => 3,
                    .normal => 3,
                    .tangent => 4,
                    .texcoord => 2,
                    .color => 3, // or 4 :/

                    // - [JOINTS_0 pointing to what exactly?](https://github.com/KhronosGroup/glTF/issues/2141)
                    // these are indices into skin.joints which point to nodes[i]
                    .joint => 4,
                    .weight => 4,
                };
            }
        };
        pub const Attributes = struct {
            items: []PrimitiveAttribute,

            pub fn jsonParse(alloc: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) !@This() {
                const value = try std.json.innerParse(std.json.Value, alloc, source, options);
                if (std.meta.activeTag(value) != .object) {
                    return error.UnexpectedToken;
                }
                const obj = value.object;

                var items = std.ArrayList(PrimitiveAttribute).init(alloc);
                errdefer items.deinit();

                var it = obj.iterator();
                while (it.next()) |e| {
                    switch (e.value_ptr.*) {
                        .integer => |num| {
                            const attr = try PrimitiveAttribute.parse_from_int_and_field_name(num, e.key_ptr.*);

                            try items.append(attr);
                        },
                        else => return error.MissingField,
                    }
                }
                return .{
                    .items = try items.toOwnedSlice(),
                };
            }
        };
        pub const Accessor = struct {
            bufferView: BufferViewIndex,
            byteOffset: u32 = 0,
            componentType: enum(u32) {
                // https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#accessor-data-types
                byte = 5120,
                unsigned_byte = 5121,
                short = 5122,
                unsigned_short = 5123,
                unsigned_integer = 5125,
                float = 5126,

                pub fn typ(self: @This(), t: type) bool {
                    return switch (self) {
                        .byte => i8 == t,
                        .unsigned_byte => u8 == t,
                        .short => i16 == t,
                        .unsigned_short => u16 == t,
                        .unsigned_integer => u32 == t,
                        .float => f32 == t,
                    };
                }

                pub fn stride(self: @This()) usize {
                    return switch (self) {
                        .byte => 1,
                        .unsigned_byte => 1,
                        .short => 2,
                        .unsigned_short => 2,
                        .unsigned_integer => 4,
                        .float => 4,
                    };
                }
            },
            count: usize,
            max: ?[]f32 = null,
            min: ?[]f32 = null,
            type: enum {
                SCALAR,
                VEC2,
                VEC3,
                VEC4,
                MAT2,
                MAT3,
                MAT4,

                pub fn components(self: @This()) usize {
                    return switch (self) {
                        .SCALAR => 1,
                        .VEC2 => 2,
                        .VEC3 => 3,
                        .VEC4 => 4,
                        .MAT2 => 4,
                        .MAT3 => 9,
                        .MAT4 => 16,
                    };
                }
            },

            pub fn len(self: *@This()) usize {
                return self.count * self.type.components() * self.componentType.stride();
            }

            pub fn matches_typ(self: *@This(), typ: type) bool {
                const Ti = @typeInfo(typ);
                const T = switch (Ti) {
                    .array => |child| child.child,
                    else => typ,
                };

                const length = switch (Ti) {
                    .array => |child| child.len,
                    else => 1,
                };

                return self.componentType.typ(T) and self.type.components() == length;
            }
        };
        pub const BufferView = struct {
            name: []const u8 = &.{},
            buffer: BufferIndex,
            byteOffset: usize = 0,
            byteLength: usize,
            target: ?enum(u32) {
                ARRAY_BUFFER = 34962,
                ELEMENT_ARRAY_BUFFER = 34963,
            } = null,
        };
    };

    const Chunk = struct {
        typ: [4]u8,
        buf: []const u8,
        alloc: std.mem.Allocator,

        fn read_json(self: *@This(), typ: type) !std.json.Parsed(typ) {
            return try std.json.parseFromSlice(typ, self.alloc, self.buf, .{
                .ignore_unknown_fields = true,
                .allocate = .alloc_always,
            });
        }

        fn get_bytes(self: *@This(), acc: *Info.Accessor, view: *Info.BufferView) []const u8 {
            return self.buf[view.byteOffset..][0..view.byteLength][acc.byteOffset..][0..acc.len()];
        }

        fn get_slice(self: *@This(), acc: *Info.Accessor, view: *Info.BufferView, typ: type) []const typ {
            const buf = self.get_bytes(acc, view);
            return std.mem.bytesAsSlice(typ, @as([]align(4) const u8, @alignCast(buf)));
        }
    };

    const Reader = struct {
        buf: []const u8,
        head: usize = 0,
        alloc: std.mem.Allocator,

        fn load(alloc: std.mem.Allocator, _path: []const u8) !@This() {
            const path = try utils.fspath.cwd_join(allocator.*, _path);
            defer allocator.free(path);
            var file = try std.fs.openFileAbsolute(path, .{});
            defer file.close();
            const buf = try file.readToEndAllocOptions(
                alloc,
                100 * 1000 * 1000,
                null,
                8,
                null,
            );
            errdefer alloc.free(buf);

            return .{
                .alloc = alloc,
                .buf = buf,
            };
        }

        fn deinit(self: *@This()) void {
            self.alloc.free(self.buf);
        }

        fn read(self: *@This(), typ: type) !typ {
            if (self.buf.len < self.head + @sizeOf(typ)) {
                return error.CouldNotParseHeader;
            }

            defer self.head += @sizeOf(typ);
            return @as(*const typ, @ptrCast(@alignCast(self.buf[self.head..].ptr))).*;
        }

        fn chunk(self: *@This()) !Chunk {
            const len: u32 = std.mem.bytesAsValue(u32, &try self.read([4]u8)).*;
            const typ = try self.read([4]u8);
            defer self.head += len;

            return .{
                .typ = typ,
                .buf = self.buf[self.head..][0..len],
                .alloc = self.alloc,
            };
        }
    };
};

pub const Wav = struct {
    header: Header,
    data: [][2]f32,
    duration: u64,

    const Header = extern struct {
        riff: [4]u8,
        chunkSize: u32,
        wave: [4]u8,
        fmt: [4]u8,
        subchunk1Size: u32, // Size of the "fmt " chunk
        audioFormat: u16, // Audio format (1 for PCM)
        numChannels: u16, // Number of channels (1 for mono, 2 for stereo)
        sampleRate: u32,
        byteRate: u32, // SampleRate * NumChannels * BitsPerSample / 8
        blockAlign: u16, // NumChannels * BitsPerSample / 8
        bitsPerSample: u16,
        data: [4]u8,
        subchunk2Size: u32,
    };

    pub fn duration_sec(self: *const @This()) f32 {
        return @floatCast(@as(f64, @floatFromInt(self.duration)) / std.time.ns_per_s);
    }

    pub fn deinit(self: *@This()) void {
        allocator.free(self.data);
    }

    pub fn parse_wav(path: []const u8) !@This() {
        var reader = try Reader.load(path);
        defer reader.deinit();

        var header = try reader.read(Header);

        if (!std.mem.eql(u8, &header.riff, "RIFF") or !std.mem.eql(u8, &header.wave, "WAVE") or !std.mem.eql(u8, &header.fmt, "fmt ")) {
            return error.InvalidWAVFile;
        }
        // only PCM supported for now.
        if (header.audioFormat != 1) {
            return error.UnsupportedFormat;
        }
        if (std.mem.eql(u8, &header.data, "LIST")) {
            reader.head += header.subchunk2Size;
            header.data = try reader.read([4]u8);
            header.subchunk2Size = try reader.read(u32);
        }

        if (!std.mem.eql(u8, &header.data, "data")) {
            return error.InvalidWAVFile;
        }

        // there's no real reason to restrict to these other than for convenience.
        // i just want all audio to be same for the game to play.
        // any assets that don't match these can be converted to support this using ffmpeg.
        // maybe in future we can just convert to match these values at runtime.
        if (header.numChannels != 2) {
            return error.UnsupportedNumChannels;
        }
        if (header.sampleRate != 48000) {
            return error.UnsupportedSampleRate;
        }
        if (header.bitsPerSample != 16) {
            return error.UnsupportedBitsPerSample;
        }

        const raw = std.mem.bytesAsSlice([2]i16, reader.buf[reader.head..][0..header.subchunk2Size]);
        const floats = try allocator.alloc([2]f32, raw.len);
        errdefer allocator.free(floats);

        for (raw, floats) |r, *f| {
            f[0] = @as(f32, @floatFromInt(r[0])) / 32768.0;
            f[1] = @as(f32, @floatFromInt(r[1])) / 32768.0;
        }

        return .{
            .header = header,
            .data = floats,
            .duration = floats.len * std.time.ns_per_s / header.sampleRate,
        };
    }

    const Reader = struct {
        buf: []const u8,
        head: usize = 0,

        fn load(_path: []const u8) !@This() {
            const path = try utils.fspath.cwd_join(allocator.*, _path);
            defer allocator.free(path);
            var file = try std.fs.openFileAbsolute(path, .{});
            defer file.close();
            const buf = try file.readToEndAllocOptions(
                allocator.*,
                100 * 1000 * 1000,
                null,
                8,
                null,
            );
            errdefer allocator.free(buf);

            return .{
                .buf = buf,
            };
        }

        fn deinit(self: *@This()) void {
            allocator.free(self.buf);
        }

        fn read(self: *@This(), typ: type) !typ {
            if (self.buf.len < self.head + @sizeOf(typ)) {
                return error.CouldNotParseHeader;
            }

            defer self.head += @sizeOf(typ);
            return @as(*align(1) const typ, @ptrCast(self.buf[self.head..].ptr)).*;
        }
    };
};

pub const TypeSchemaGenerator = struct {
    buf: std.ArrayList(u8),
    had_first_component: bool = false,

    const Writer = std.ArrayList(u8).Writer;
    const tab = 2;

    pub fn init(comptime special: struct {
        transform: ?type = null,
        entity: ?type = null,
        name: ?type = null,
    }) !@This() {
        var self = @This(){ .buf = .init(allocator.*) };
        const w = self.buf.writer();

        try w.print("{{\n{[pad]s: >[indent]}\"special\": {{", .{
            .pad = "",
            .indent = tab,
        });

        inline for (@typeInfo(@TypeOf(special)).@"struct".fields, 0..) |field, i| {
            if (i > 0) {
                try w.print(",", .{});
            }
            try w.print("\n{[pad]s: >[indent]}\"{[name]s}\": ", .{
                .pad = "",
                .indent = tab * 2,
                .name = field.name,
            });

            if (@field(special, field.name)) |typ| {
                try w.print("\"{s}\"", .{@typeName(typ)});
            } else {
                try w.print("null", .{});
            }
        }

        try w.print("\n{[pad]s: >[indent]}}},\n{[pad]s: >[indent]}\"components\": {{", .{
            .pad = "",
            .indent = tab,
        });

        return self;
    }

    pub fn deinit(self: *@This()) void {
        self.buf.deinit();
    }

    pub fn write_to_file(self: *@This(), _path: []const u8) !void {
        const path = try utils.fspath.cwd_join(allocator.*, _path);
        defer allocator.free(path);
        var file = try std.fs.createFileAbsolute(path, .{});
        defer file.close();

        const w = file.writer();
        try w.print("{[json]s}\n{[pad]s: >[indent]}}}\n}}", .{
            .pad = "",
            .indent = tab,
            .json = self.buf.items,
        });
    }

    pub fn write_schema(self: *@This(), comptime typ: type, comptime default: ?typ) !void {
        const w = self.buf.writer();
        try w.print("{[comma]s}\n{[pad]s: >[indent]}\"{[name]s}\": ", .{
            .comma = if (self.had_first_component) "," else "",
            .pad = "",
            .indent = tab * 2,
            .name = @typeName(typ),
        });
        self.had_first_component = true;
        try self.write_type(typ, default, tab + 1);
    }

    fn write_type(self: *@This(), comptime typ: type, comptime default: ?typ, comptime indent: u32) !void {
        const w = self.buf.writer();
        try w.print("{{\n", .{});
        try w.print("{[pad]s: >[indent]}\"type\": \"{[name]s}\",\n", .{
            .pad = "",
            .indent = indent * tab,
            .name = self.get_type_type(typ),
        });

        switch (typ) {
            u32, []const u8, bool, f32, math.Vec3, math.Vec4 => {
                try w.print("{[pad]s: >[indent]}\"default\": ", .{ .pad = "", .indent = indent * tab });
                try self.write_default_value(typ, default);
                try w.print("\n{[pad]s: >[indent]}}}", .{ .pad = "", .indent = (indent - 1) * tab });
                return;
            },
            else => {},
        }

        const T = @typeInfo(typ);
        switch (T) {
            .@"struct" => {
                try w.print("{[pad]s: >[indent]}\"properties\": ", .{ .pad = "", .indent = indent * tab });
                try self.write_props(typ, default, indent + 1);
            },
            .@"union" => {
                try w.print("{[pad]s: >[indent]}\"default_tag\": ", .{ .pad = "", .indent = indent * tab });
                try self.write_default_value(typ, default);

                try w.print(",\n{[pad]s: >[indent]}\"variants\": ", .{ .pad = "", .indent = indent * tab });
                try self.write_props(typ, default, indent + 1);
            },
            .@"enum" => {
                try w.print("{[pad]s: >[indent]}\"default\": ", .{ .pad = "", .indent = indent * tab });
                try self.write_default_value(typ, default);

                try w.print(",\n{[pad]s: >[indent]}\"variants\": ", .{ .pad = "", .indent = indent * tab });
                try self.write_props(typ, default, indent + 1);
            },
            else => @compileError("can't handle: " ++ @typeName(typ)),
        }

        try w.print("\n{[pad]s: >[indent]}}}", .{ .pad = "", .indent = (indent - 1) * tab });
    }

    fn write_default_value(self: *@This(), comptime typ: type, comptime default: ?typ) !void {
        const w = self.buf.writer();

        switch (typ) {
            []const u8 => {
                try w.print("\"{s}\"", .{default orelse ""});
                return;
            },
            u32, f32 => {
                try w.print("{}", .{default orelse 0});
                return;
            },
            bool => {
                try w.print("{}", .{default orelse false});
                return;
            },
            math.Vec3 => {
                const def = default orelse math.Vec3{};
                try w.print("[{}, {}, {}]", .{ def.x, def.y, def.z });
                return;
            },
            math.Vec4 => {
                const def = default orelse math.Vec4{};
                try w.print("[{}, {}, {}, {}]", .{ def.x, def.y, def.z, def.w });
                return;
            },
            else => {},
        }

        const T = @typeInfo(typ);
        switch (T) {
            .@"enum" => |t| {
                const def = if (default) |def| @tagName(def) else t.fields[0].name;
                try w.print("\"{s}\"", .{def});
            },
            .@"union" => |t| {
                const def = if (default) |def| @tagName(std.meta.activeTag(def)) else @as([]const u8, t.fields[0].name);
                try w.print("\"{s}\"", .{def});
            },
            else => @compileError("can't handle: " ++ @typeName(typ)),
        }
    }

    fn get_type_type(_: *@This(), comptime typ: type) []const u8 {
        switch (typ) {
            u32 => return "int",
            []const u8 => return "string",
            bool => return "bool",
            f32 => return "float",
            math.Vec3 => return "vec3",
            math.Vec4 => return "vec4",
            else => {},
        }
        return switch (@typeInfo(typ)) {
            .@"struct" => "struct",
            .@"union" => "union",
            .@"enum" => "enum",
            else => @compileError("can't handle this type here: " ++ @typeName(typ)),
        };
    }

    fn write_props(self: *@This(), comptime typ: type, comptime default: ?typ, comptime indent: u32) !void {
        const w = self.buf.writer();
        const T = @typeInfo(typ);
        switch (T) {
            .@"struct" => |t| {
                try w.print("{{\n", .{});

                inline for (t.fields, 0..) |field, i| {
                    if (i > 0) {
                        try w.print(",\n", .{});
                    }
                    try w.print("{[pad]s: >[indent]}\"{[str]s}\": ", .{ .pad = "", .indent = indent * tab, .str = field.name });
                    const def = if (default) |def| @field(def, field.name) else field.defaultValue();
                    try self.write_type(field.type, def, indent + 1);
                }

                try w.print("\n{[pad]s: >[indent]}}}", .{ .pad = "", .indent = (indent - 1) * tab });
            },
            .@"union" => |t| {
                try w.print("{{\n", .{});

                inline for (t.fields, 0..) |field, i| {
                    if (i > 0) {
                        try w.print(",\n", .{});
                    }
                    try w.print("{[pad]s: >[indent]}\"{[str]s}\": ", .{ .pad = "", .indent = indent * tab, .str = field.name });

                    const field_tag = comptime std.meta.stringToEnum(t.tag_type.?, field.name).?;
                    const def = if (default) |def| if (comptime std.meta.activeTag(def) == field_tag) @field(def, field.name) else null;
                    try self.write_type(field.type, def, indent + 1);
                }

                try w.print("\n{[pad]s: >[indent]}}}", .{ .pad = "", .indent = (indent - 1) * tab });
            },
            .@"enum" => |t| {
                try w.print("[ ", .{});
                inline for (t.fields, 0..) |field, i| {
                    if (i > 0) {
                        try w.print(", ", .{});
                    }
                    try w.print("\"{s}\"", .{field.name});
                }
                try w.print("]", .{});
            },
            else => @compileError("can't handle: " ++ @typeName(typ)),
        }
    }
};
