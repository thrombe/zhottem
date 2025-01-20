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
     uint frame;
     float time;
     float deltatime;
     int width;
     int height;
     int monitor_width;
     int monitor_height;
     float exposure;
     float gamma;
     int voxel_grid_side;
     int voxel_debug_view;
     uint pad1;
     vec4 background_color1;
     vec4 background_color2;
     vec4 trap_color1;
     vec4 trap_color2;
     vec4 emission_color1;
     vec4 emission_color2;
     vec4 light_dir;
     int fractal_iterations;
     int march_iterations;
     int gather_iterations;
     float gather_step_factor;
     float march_step_factor;
     float escape_r;
     float fractal_scale;
     float fractal_density;
     float ray_penetration_factor;
     uint gi_samples;
     float temporal_blend_factor;
     float min_background_color_contribution;
     float stylistic_aliasing_factor;
     float t_max;
     float bulb_z_pow;
 };

