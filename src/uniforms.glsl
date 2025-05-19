 // This file is generated from code. DO NOT EDIT.

 struct CameraMeta {
     uint did_change;
     uint did_move;
     uint did_rotate;
     uint pad;
 };

 struct Camera {
     vec3 eye;
     vec3 fwd;
     vec3 right;
     vec3 up;
     CameraMeta meta;
 };

 struct Mouse {
     int x;
     int y;
     uint left;
     uint right;
 };

 struct Uniforms {
     Camera camera;
     Mouse mouse;
     mat4 world_to_screen;
     uint frame;
     float time;
     float deltatime;
     int width;
     int height;
     int monitor_width;
     int monitor_height;
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

 const int _bind_camera = 0;
 const int _bind_vertices = 1;
 const int _bind_indices = 2;
 const int _bind_instances = 3;
 const int _bind_bones = 4;
 const int _bind_texture = 5;

