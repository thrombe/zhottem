 // This file is generated from code. DO NOT EDIT.

 struct Camera3DMeta {
     uint did_change;
     uint did_move;
     uint did_rotate;
 };

 struct Camera3D {
     vec3 eye;
     vec3 fwd;
     vec3 right;
     vec3 up;
     Camera3DMeta meta;
 };

 struct Mouse {
     int x;
     int y;
     uint left;
     uint right;
 };

 struct Frame {
     uint frame;
     float time;
     float deltatime;
     int width;
     int height;
     int monitor_width;
     int monitor_height;
 };

 struct Uniforms {
     Camera3D camera;
     Mouse mouse;
     mat4 world_to_screen;
     Frame frame;
 };

 struct Vertex {
     vec3 pos;
     vec3 normal;
     vec4 uv;
     uint bone_ids[4];
     float bone_weights[4];
 };

 struct Instance {
     uint bone_offset;
 };

 struct DrawCtx {
     uint first_vertex;
     uint first_index;
     uint first_instance;
 };

 struct PushConstants {
     uint first_draw_ctx;
 };

 struct LineVertex {
     vec3 pos;
     vec4 color;
 };

 const int _bind_camera = 0;
 const int _bind_vertices = 1;
 const int _bind_indices = 2;
 const int _bind_instances = 3;
 const int _bind_bones = 4;
 const int _bind_call_ctxts = 5;
 const int _bind_texture = 6;
 const int _bind_line_vertex_buffer = 7;

