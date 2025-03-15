#include<stdlib.h>

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
  SHAPE_CAPSULE,
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

  struct {
    f32 half_height;
    f32 radius;
  } capsule;
} ZShapeSettings;

typedef struct {
  ShapeType shape_type;
  ZShapeSettings shape;

  MotionType motion_type;
  MotionQuality motion_quality;
  vec3 pos;
  vec4 rotation;
  vec3 velocity;
  vec3 angular_velocity;
  f32 friction;
} ZBodySettings;

typedef struct {
  vec4 rot;
  vec3 pos;
} ZTransform;

typedef void *ZPhysics;

typedef void *(*Zallocate_function)(size_t in_size);
typedef void *(*Zreallocate_function)(void *in_block, size_t old_size, size_t new_size);
typedef void (*Zfree_function)(void *in_block);
typedef void *(*Zaligned_allocate_function)(size_t in_size, size_t in_alignment);
typedef void (*Zaligned_free_function)(void *in_block);

typedef struct {
  Zallocate_function allocfn;
  Zreallocate_function reallocfn;
  Zfree_function freefn;
  Zaligned_allocate_function aligned_allocfn;
  Zaligned_free_function aligned_freefn;
} ZAllocator;

ZPhysics physics_create(ZAllocator alloc);
void physics_post_reload(ZPhysics p, ZAllocator alloc);
void physics_start(ZPhysics p);
void physics_delete(ZPhysics p);
void physics_optimize(ZPhysics p);
void physics_update(ZPhysics p, float sim_time, u32 steps);
u32 physics_add_body(ZPhysics p, ZBodySettings bsettings);
ZTransform physics_get_transform(ZPhysics p, u32 bid);
void physics_add_force(ZPhysics p, u32 _bid, vec3 force);
void physics_set_rotation(ZPhysics p, u32 _bid, vec4 rot);

void testfn();
