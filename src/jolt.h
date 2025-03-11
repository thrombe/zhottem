
typedef unsigned int u32;
typedef int i32;
typedef float f32;
typedef double f64;

typedef struct {
  f32 x;
  f32 y;
  f32 z;
  f32 w;
} vec4;
typedef struct {
  f32 x;
  f32 y;
  f32 z;
} vec3;

typedef enum {
  SHAPE_SPHERE = 0,
  SHAPE_BOX,
} ShapeType;

typedef enum {
  MOTION_STATIC = 0,
  MOTION_KINEMATIC,
  MOTION_DYNAMIC,
} MotionType;

typedef enum {
  MOTION_DISCRETE = 0,
  MOTION_LINEAR_CAST,
} MotionQuality;

typedef union {
  struct {
    f32 radius;
  } sphere;

  struct {
    vec3 size;
  } box;
} ZShapeSettings;

typedef struct {
  ShapeType shape_type;
  ZShapeSettings shape;

  MotionType motion_type;
  MotionQuality motion_quality;
  vec3 pos;
  vec3 velocity;
  vec3 angular_velocity;
} ZBodySettings;

typedef struct {
  vec4 rot;
  vec3 pos;
} ZTransform;

typedef void *ZPhysics;

ZPhysics physics_create();
void physics_start(ZPhysics p);
void physics_delete(ZPhysics p);
void physics_optimize(ZPhysics p);
void physics_update(ZPhysics p, float sim_time, u32 steps);
u32 physics_add_body(ZPhysics p, ZBodySettings bsettings);
ZTransform physics_get_transform(ZPhysics p, u32 bid);
void physics_add_force(ZPhysics p, u32 _bid, vec3 force);
void physics_set_rotation(ZPhysics p, u32 _bid, vec4 rot);

void testfn();
