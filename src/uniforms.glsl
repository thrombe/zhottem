 // This file is generated from code. DO NOT EDIT.

 struct CameraMeta {
     uint did_change;
     uint did_move;
     uint did_rotate;
     uint pad;
 };

 struct Camera {
     vec4 eye;
     vec4 fwd;
     vec4 right;
     vec4 up;
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

 const int _bind_camera = 0;
 const int _bind_instanced = 1;

 const int _bind_vertex = 0;
 const int _bind_instance = 1;

 const int _bind_instance_transform = 0;
 const int _bind_vertex_position = 4;
 const int _bind_normal = 5;
 const int _bind_uv = 6;

