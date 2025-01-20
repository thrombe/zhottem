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

