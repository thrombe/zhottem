#include <cstdarg>
#include <iostream>
#include <stdio.h>

#include <Jolt/Jolt.h>

#include "Jolt/Core/Memory.h"
#include "Jolt/Physics/Body/MotionQuality.h"
#include "Jolt/Physics/Body/MotionType.h"
#include "Jolt/Physics/Character/Character.h"
#include "Jolt/Physics/Character/CharacterBase.h"
#include "Jolt/Physics/Character/CharacterVirtual.h"
#include "Jolt/Physics/Collision/Shape/CapsuleShape.h"
#include "Jolt/Physics/Collision/Shape/RotatedTranslatedShape.h"
#include "Jolt/Physics/Collision/Shape/Shape.h"
#include <Jolt/Core/Factory.h>
#include <Jolt/Core/JobSystemThreadPool.h>
#include <Jolt/Core/TempAllocator.h>
#include <Jolt/Physics/Body/BodyActivationListener.h>
#include <Jolt/Physics/Body/BodyCreationSettings.h>
#include <Jolt/Physics/Body/BodyInterface.h>
#include <Jolt/Physics/Collision/Shape/BoxShape.h>
#include <Jolt/Physics/Collision/Shape/SphereShape.h>
#include <Jolt/Physics/EActivation.h>
#include <Jolt/Physics/PhysicsSettings.h>
#include <Jolt/Physics/PhysicsSystem.h>
#include <Jolt/RegisterTypes.h>

JPH_SUPPRESS_WARNINGS

using namespace JPH;
using namespace JPH::literals;
using namespace std;

static void TraceImpl(const char *inFMT, ...) {
  va_list list;
  va_start(list, inFMT);
  char buffer[1024];
  vsnprintf(buffer, sizeof(buffer), inFMT, list);
  va_end(list);

  cout << buffer << endl;
}

#ifdef JPH_ENABLE_ASSERTS

static bool AssertFailedImpl(const char *inExpression, const char *inMessage, const char *inFile, uint inLine) {
  cout << inFile << ":" << inLine << ": (" << inExpression << ") " << (inMessage != nullptr ? inMessage : "") << endl;
  return true;
};

#endif // JPH_ENABLE_ASSERTS

namespace Layers {
static constexpr ObjectLayer NON_MOVING = 0;
static constexpr ObjectLayer MOVING = 1;
static constexpr ObjectLayer NUM_LAYERS = 2;
}; // namespace Layers

class ObjectLayerPairFilterImpl : public ObjectLayerPairFilter {
public:
  virtual bool ShouldCollide(ObjectLayer inObject1, ObjectLayer inObject2) const override {
    switch (inObject1) {
    case Layers::NON_MOVING:
      return inObject2 == Layers::MOVING;
    case Layers::MOVING:
      return true;
    default:
      JPH_ASSERT(false);
      return false;
    }
  }
};

// If you want to fine tune your broadphase layers define JPH_TRACK_BROADPHASE_STATS and look at the stats reported on the TTY.
namespace BroadPhaseLayers {
static constexpr BroadPhaseLayer NON_MOVING(0);
static constexpr BroadPhaseLayer MOVING(1);
static constexpr uint NUM_LAYERS(2);
}; // namespace BroadPhaseLayers

class BPLayerInterfaceImpl final : public BroadPhaseLayerInterface {
public:
  BPLayerInterfaceImpl() {
    // Create a mapping table from object to broad phase layer
    mObjectToBroadPhase[Layers::NON_MOVING] = BroadPhaseLayers::NON_MOVING;
    mObjectToBroadPhase[Layers::MOVING] = BroadPhaseLayers::MOVING;
  }

  virtual uint GetNumBroadPhaseLayers() const override { return BroadPhaseLayers::NUM_LAYERS; }

  virtual BroadPhaseLayer GetBroadPhaseLayer(ObjectLayer inLayer) const override {
    JPH_ASSERT(inLayer < Layers::NUM_LAYERS);
    return mObjectToBroadPhase[inLayer];
  }

#if defined(JPH_EXTERNAL_PROFILE) || defined(JPH_PROFILE_ENABLED)
  virtual const char *GetBroadPhaseLayerName(BroadPhaseLayer inLayer) const override {
    switch ((BroadPhaseLayer::Type)inLayer) {
    case (BroadPhaseLayer::Type)BroadPhaseLayers::NON_MOVING:
      return "NON_MOVING";
    case (BroadPhaseLayer::Type)BroadPhaseLayers::MOVING:
      return "MOVING";
    default:
      JPH_ASSERT(false);
      return "INVALID";
    }
  }
#endif // JPH_EXTERNAL_PROFILE || JPH_PROFILE_ENABLED

private:
  BroadPhaseLayer mObjectToBroadPhase[Layers::NUM_LAYERS];
};

class ObjectVsBroadPhaseLayerFilterImpl : public ObjectVsBroadPhaseLayerFilter {
public:
  virtual bool ShouldCollide(ObjectLayer inLayer1, BroadPhaseLayer inLayer2) const override {
    switch (inLayer1) {
    case Layers::NON_MOVING:
      return inLayer2 == BroadPhaseLayers::MOVING;
    case Layers::MOVING:
      return true;
    default:
      JPH_ASSERT(false);
      return false;
    }
  }
};

// WOOPS: thread safety
class MyContactListener : public ContactListener {
public:
  virtual ValidateResult OnContactValidate(const Body &inBody1, const Body &inBody2, RVec3Arg inBaseOffset,
                                           const CollideShapeResult &inCollisionResult) override {
    // Allows you to ignore a contact before it is created
    return ValidateResult::AcceptAllContactsForThisBodyPair;
  }

  virtual void OnContactAdded(const Body &inBody1, const Body &inBody2, const ContactManifold &inManifold,
                              ContactSettings &ioSettings) override {}

  virtual void OnContactPersisted(const Body &inBody1, const Body &inBody2, const ContactManifold &inManifold,
                                  ContactSettings &ioSettings) override {}

  virtual void OnContactRemoved(const SubShapeIDPair &inSubShapePair) override {}
};

// WOOPS: thread safety
class MyBodyActivationListener : public BodyActivationListener {
public:
  virtual void OnBodyActivated(const BodyID &inBodyID, uint64 inBodyUserData) override {}

  virtual void OnBodyDeactivated(const BodyID &inBodyID, uint64 inBodyUserData) override {}
};

extern "C" {
#include "jolt.h"
}

class Physics {
public:
  const uint cMaxBodies = 65536;
  const uint cNumBodyMutexes = 0;
  const uint cMaxBodyPairs = 65536;
  const uint cMaxContactConstraints = 10240;

  Factory *factory = nullptr;
  ZAllocator alloc;

  TempAllocatorImpl *temp_allocator = nullptr;
  JobSystemThreadPool *job_system = nullptr;
  PhysicsSystem physics_system;

  BPLayerInterfaceImpl broad_phase_layer_interface;
  ObjectVsBroadPhaseLayerFilterImpl object_vs_broadphase_layer_filter;
  ObjectLayerPairFilterImpl object_vs_object_layer_filter;
  MyBodyActivationListener body_activation_listener;
  MyContactListener contact_listener;

  CharacterVirtual *character = nullptr;
  Vec3 character_force = Vec3();

  static Physics *create_new(ZAllocator alloc) {
    Allocate = alloc.allocfn;
    Reallocate = alloc.reallocfn;
    Free = alloc.freefn;
    AlignedAllocate = alloc.aligned_allocfn;
    AlignedFree = alloc.aligned_freefn;

    Trace = TraceImpl;
    JPH_IF_ENABLE_ASSERTS(AssertFailed = AssertFailedImpl;)
    Factory::sInstance = new Factory(); // ser/deser

    RegisterTypes();
    Physics *p = new Physics();
    p->factory = Factory::sInstance;
    p->temp_allocator = new TempAllocatorImpl(10 * 1024 * 1024);
    p->job_system = new JobSystemThreadPool(cMaxPhysicsJobs, cMaxPhysicsBarriers, thread::hardware_concurrency() - 1);
    return p;
  }

  void delete_self() {
    UnregisterTypes();
    delete Factory::sInstance;
    Factory::sInstance = nullptr;

    delete temp_allocator;
    temp_allocator = nullptr;
    delete job_system;
    job_system = nullptr;

    delete this;
  }

  void post_reload(ZAllocator alloc) {
    this->alloc = alloc;
    Allocate = alloc.allocfn;
    Reallocate = alloc.reallocfn;
    Free = alloc.freefn;
    AlignedAllocate = alloc.aligned_allocfn;
    AlignedFree = alloc.aligned_freefn;

    Trace = TraceImpl;
    JPH_IF_ENABLE_ASSERTS(AssertFailed = AssertFailedImpl;)
    Factory::sInstance = factory;
  }

  void start() {
    physics_system.Init(cMaxBodies, cNumBodyMutexes, cMaxBodyPairs, cMaxContactConstraints, broad_phase_layer_interface,
                        object_vs_broadphase_layer_filter, object_vs_object_layer_filter);
    physics_system.SetBodyActivationListener(&body_activation_listener);
    physics_system.SetContactListener(&contact_listener);
  }

  void optimize() { physics_system.OptimizeBroadPhase(); }

  BodyInterface &bodyi() { return physics_system.GetBodyInterface(); }

  void update(float sim_time, int steps) {
    Vec3 velocity = character->GetLinearVelocity() + (physics_system.GetGravity() + character_force) * sim_time;
    character->SetLinearVelocity(velocity);
    character_force = Vec3();

    CharacterVirtual::ExtendedUpdateSettings update_settings;
    character->ExtendedUpdate(sim_time / steps, physics_system.GetGravity(), update_settings,
                              physics_system.GetDefaultBroadPhaseLayerFilter(Layers::MOVING),
                              physics_system.GetDefaultLayerFilter(Layers::MOVING), {}, {}, *temp_allocator);

    // auto bodyi = physics_system.GetBodyInterface();
    // bodyi.getbo

    physics_system.Update(sim_time, steps, temp_allocator, job_system);
    // if (character) {
    //   character->PostSimulation(0.01);
    // }
  }
};

Vec3 vec3_cast(vec3 vec) { return Vec3(vec.x, vec.y, vec.z); }

vec3 Vec3_cast(Vec3 vec) {
  vec3 v;
  v.x = vec.GetX();
  v.y = vec.GetY();
  v.z = vec.GetZ();
  return v;
}

Vec4 vec4_cast(vec4 vec) { return Vec4(vec.x, vec.y, vec.z, vec.w); }

vec4 Vec4_cast(Vec4 vec) {
  vec4 v;
  v.x = vec.GetX();
  v.y = vec.GetY();
  v.z = vec.GetZ();
  v.w = vec.GetW();
  return v;
}

vec4 Vec4_cast(Quat vec) {
  vec4 v;
  v.x = vec.GetX();
  v.y = vec.GetY();
  v.z = vec.GetZ();
  v.w = vec.GetW();
  return v;
}

extern "C" {

ZPhysics physics_create(ZAllocator alloc) { return Physics::create_new(alloc); }

void physics_post_reload(ZPhysics p, ZAllocator alloc) {
  auto physics = (Physics *)p;
  physics->post_reload(alloc);
}

void physics_delete(ZPhysics p) {
  auto physics = (Physics *)p;
  physics->delete_self();
}

void physics_start(ZPhysics p) {
  auto physics = (Physics *)p;
  physics->start();
}

void physics_optimize(ZPhysics p) {
  auto physics = (Physics *)p;
  physics->optimize();
}

void physics_update(ZPhysics p, float sim_time, u32 steps) {
  auto physics = (Physics *)p;
  physics->update(sim_time, steps);
}

u32 physics_add_body(ZPhysics p, ZBodySettings bsettings) {
  auto physics = (Physics *)p;

  BodyCreationSettings settings;
  settings.mPosition = vec3_cast(bsettings.pos);
  settings.mRotation = Quat(vec4_cast(bsettings.rotation).Normalized());
  settings.mLinearVelocity = vec3_cast(bsettings.velocity);
  settings.mAngularVelocity = vec3_cast(bsettings.angular_velocity);
  settings.mMotionType = static_cast<EMotionType>(bsettings.motion_type);
  settings.mMotionQuality = static_cast<EMotionQuality>(bsettings.motion_quality);
  settings.mObjectLayer = Layers::MOVING;
  settings.mFriction = bsettings.friction;

  switch (bsettings.shape_type) {
  case SHAPE_SPHERE: {
    Ref<SphereShapeSettings> sphere = new SphereShapeSettings();
    sphere->mRadius = bsettings.shape.sphere.radius;
    settings.SetShape(sphere->Create().Get());
    settings.SetShapeSettings(sphere);
  } break;
  case SHAPE_BOX: {
    Ref<BoxShapeSettings> box = new BoxShapeSettings();
    box->mHalfExtent = vec3_cast(bsettings.shape.box.size);
    settings.SetShape(box->Create().Get());
    settings.SetShapeSettings(box);
  } break;
  case SHAPE_CAPSULE: {
    Ref<CapsuleShapeSettings> capsule = new CapsuleShapeSettings();
    capsule->mHalfHeightOfCylinder = bsettings.shape.capsule.half_height;
    capsule->mRadius = bsettings.shape.capsule.radius;
    Ref<Shape> shape = capsule->Create().Get();
    settings.SetShape(shape);
    settings.SetShapeSettings(capsule);

    // BodyInterface &bi = physics->bodyi();
    // Body *b = bi.CreateBody(settings);
    // bi.AddBody(b->GetID(), EActivation::Activate);
    // return b->GetID().GetIndexAndSequenceNumber();

    Ref<CharacterVirtualSettings> char_settings = new CharacterVirtualSettings();
    char_settings->mShape =
        RotatedTranslatedShapeSettings(Vec3(0, -(capsule->mHalfHeightOfCylinder + capsule->mRadius), 0), Quat::sIdentity(), shape)
            .Create()
            .Get();
    char_settings->mInnerBodyShape = shape;
    char_settings->mInnerBodyLayer = Layers::MOVING;
    // settings->mSupportingVolume = Plane(Vec3::sAxisY(), -cCharacterRadiusStanding);
    physics->character = new CharacterVirtual(char_settings, settings.mPosition, settings.mRotation, 0, &physics->physics_system);

    return physics->character->GetInnerBodyID().GetIndexAndSequenceNumber();
  } break;
  default:
    return 0;
  }

  BodyInterface &bi = physics->bodyi();
  Body *b = bi.CreateBody(settings);
  bi.AddBody(b->GetID(), EActivation::Activate);

  return b->GetID().GetIndexAndSequenceNumber();
}

ZTransform physics_get_transform(ZPhysics p, u32 _bid) {
  auto physics = (Physics *)p;
  BodyID bid = BodyID(_bid);

  BodyInterface &bi = physics->bodyi();
  auto pos = bi.GetPosition(bid);
  auto rot = bi.GetRotation(bid);

  ZTransform zt;
  zt.pos = Vec3_cast(pos);
  zt.rot = Vec4_cast(rot);
  return zt;
}

void physics_set_rotation(ZPhysics p, u32 _bid, vec4 rot) {
  auto physics = (Physics *)p;
  BodyID bid = BodyID(_bid);

  BodyInterface &bi = physics->bodyi();
  bi.SetRotation(bid, Quat(vec4_cast(rot).Normalized()), EActivation::Activate);
}

void physics_add_force(ZPhysics p, u32 _bid, vec3 force) {
  auto physics = (Physics *)p;
  BodyID bid = BodyID(_bid);
  BodyInterface &bi = physics->bodyi();
  if (bid == physics->character->GetInnerBodyID()) {
    physics->character_force = vec3_cast(force);
  } else {
    bi.AddForce(bid, vec3_cast(force) * 100);
  }
}

void testfn() {}
}
