import bpy
import json
import os
from typing import Dict
from bpy.props import (
    StringProperty,
    BoolProperty,
    IntProperty,
    FloatProperty,
    FloatVectorProperty,
    EnumProperty,
    CollectionProperty,
    PointerProperty,
)
from bpy.types import PropertyGroup, Panel, Operator, Menu


class Component:
    def __init__(self, clas, props: dict, defn: dict):
        self.name = clas.__name__
        self.clas = clas
        self.props = props
        self.defn = defn


class ComponentRegistry:
    schema: dict
    components: Dict[str, Component]

    def __init__(self):
        self.components: Dict[str, Component] = {}

        with open(os.path.join(os.path.dirname(__file__), "../components.json")) as f:
            self.schema = json.loads(f.read())

        for name, typ in self.schema.items():
            self.parse_type(name, typ)

    # TODO: namespaced subtypes Rigidbody.constraints etc
    def parse_type(self, name: str, typ: dict):
        if name in self.components:
            return self.components[name]

        class_props = {"bl_label": name, "bl_idname": "zhottem." + name}
        if typ["type"] == "struct":
            __annotations__ = {}
            for tname, ttyp in typ["properties"].items():
                comp = self.parse_type(tname, ttyp)
                __annotations__[tname] = comp.clas(**comp.props)

            class_props["__annotations__"] = __annotations__

            value_props = {}
            self.components[name] = Component(
                PointerProperty,
                value_props | {"type": type(name, (PropertyGroup,), class_props)},
                typ,
            )
            return self.components[name]
        elif typ["type"] == "union":
            __annotations__ = {}

            variant_tags = list(typ["variants"].keys())
            enum = EnumProperty(
                items=[(k, k, "") for k in variant_tags], default=typ["default_tag"]
            )
            __annotations__["enum"] = enum

            for tname, variant in typ["variants"].items():
                comp = self.parse_type(tname, variant)
                __annotations__[tname] = comp.clas(**comp.props)

            class_props["__annotations__"] = __annotations__
            self.components[name] = Component(
                PointerProperty,
                {"type": type(name, (PropertyGroup,), class_props)},
                typ,
            )
            return self.components[name]
        elif typ["type"] == "enum":
            value_props = {
                "items": [(k, k, "") for k in typ["variants"]],
                "default": typ["default"],
            }
            self.components[name] = Component(EnumProperty, value_props, typ)
            return self.components[name]
        elif typ["type"] == "int":
            value_props = {"default": typ.get("default", 0)}
            return Component(IntProperty, value_props, typ)
        elif typ["type"] == "float":
            value_props = {"default": typ.get("default", 0)}
            return Component(FloatProperty, value_props, typ)
        elif typ["type"] == "bool":
            value_props = {"default": typ.get("default", False)}
            return Component(BoolProperty, value_props, typ)
        elif typ["type"] == "string":
            value_props = {"default": typ.get("default", "")}
            return Component(StringProperty, value_props, typ)
        elif typ["type"] == "vec3":
            value_props = {"default": typ.get("default", [0, 0, 0]), "size": 3}
            return Component(FloatVectorProperty, value_props, typ)
        elif typ["type"] == "vec4":
            value_props = {"default": typ.get("default", [0, 0, 0, 0]), "size": 4}
            return Component(FloatVectorProperty, value_props, typ)
        else:
            pass


class OBJECT_OT_add_game_component(Operator):
    bl_idname = "object.add_game_component"
    bl_label = "Add Game Component"
    bl_property = "component_type"

    def component_enum_variants(self, context):
        reg = bpy.context.window_manager.component_registry
        return [(k, k, "") for k in reg.schema.keys()]

    component_type: EnumProperty(items=component_enum_variants)

    def execute(self, context):
        # reg: ComponentRegistry = bpy.context.window_manager.component_registry
        obj = context.object
        components = obj.game_components

        if any(c.component_type == self.component_type for c in components):
            return {"CANCELLED"}

        # comp = reg.components[self.component_type]
        # setattr(obj, "zhott_" + self.component_type, comp.clas(**comp.props))

        comp = components.add()
        comp.component_type = self.component_type

        return {"FINISHED"}


class OBJECT_PT_game_components(Panel):
    bl_label = "Game Components"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"

    def draw(self, context):
        layout = self.layout
        obj = context.object
        components = obj.game_components
        reg = bpy.context.window_manager.component_registry

        row = layout.row()
        row.menu("COMPONENT_MT_add", text="Add Component")

        for i, name in enumerate(components):
            name = name.component_type
            comp = reg.components[name]

            box = layout.box()
            header = box.row()
            header.label(text=name)
            header.operator("object.remove_game_component", text="", icon="X").index = i
            self.draw_type(obj, "zhott_" + name, box, name, comp.defn)

    def draw_type(self, obj, propname, layout, name: str, defn: dict):
        if defn["type"] == "struct":
            for pname, ptype in defn["properties"].items():
                box = layout
                if ptype["type"] in ["struct", "union"]:
                    box = layout.box()
                    box.label(text=pname)
                self.draw_type(getattr(obj, propname), pname, box, pname, ptype)
        elif defn["type"] == "union":
            layout.prop(getattr(obj, propname), "enum", text="type")
            for vname, vtype in defn["variants"].items():
                if vname != getattr(getattr(obj, propname), "enum"):
                    continue
                box = layout
                if vtype["type"] in ["struct", "union"]:
                    box = layout.box()
                    box.label(text=vname)
                self.draw_type(getattr(obj, propname), vname, box, vname, vtype)
        else:
            row = layout.row()
            row.prop(obj, propname, text=name)


class GLTF2ExportUserExtension:
    def gather_node_hook(self, gltf2_object, blender_object, export_settings):
        if blender_object and blender_object.game_components:
            gltf2_object.extras = gltf2_object.extras or {}
            components_data = []

            for comp in blender_object.game_components:
                comp_data = {
                    "type": comp.component_type,
                    "properties": self._serialize_properties(comp.properties),
                }
                components_data.append(comp_data)

            gltf2_object.extras["components"] = components_data

    def _serialize_properties(self, props):
        output = {}
        for prop in props:
            parts = prop.path.split(".")
            current = output
            for part in parts[:-1]:
                current = current.setdefault(part, {})

            value = getattr(prop, f"value_{prop.type}")
            if prop.type == "vec3":
                value = list(value)
            current[parts[-1]] = value
        return output


class OBJECT_OT_remove_game_component(Operator):
    bl_idname = "object.remove_game_component"
    bl_label = "Remove Game Component"

    index: IntProperty()

    def execute(self, context):
        obj = context.object
        obj.game_components.remove(self.index)
        return {"FINISHED"}


# Add this menu class
class COMPONENT_MT_add(Menu):
    bl_label = "Add Component"
    bl_idname = "COMPONENT_MT_add"

    def draw(self, context):
        reg = bpy.context.window_manager.component_registry
        layout = self.layout
        for comp_type in reg.schema:
            props = layout.operator(
                OBJECT_OT_add_game_component.bl_idname, text=comp_type
            )
            props.component_type = comp_type


class ComponentType(PropertyGroup):
    component_type: StringProperty()


classes = (
    OBJECT_OT_remove_game_component,
    OBJECT_OT_add_game_component,
    OBJECT_PT_game_components,
    COMPONENT_MT_add,
    ComponentType,
)


def register():
    bpy.types.WindowManager.component_registry = ComponentRegistry()

    for cls in classes:
        bpy.utils.register_class(cls)

    reg = bpy.context.window_manager.component_registry
    for name, comp in reg.components.items():
        if "type" in comp.props:
            bpy.utils.register_class(comp.props["type"])

    for name in reg.schema.keys():
        comp = reg.components[name]
        setattr(bpy.types.Object, "zhott_" + name, comp.clas(**comp.props))

    bpy.types.Object.game_components = CollectionProperty(type=ComponentType)


def unregister():
    for cls in reversed(classes):
        bpy.utils.unregister_class(cls)

    reg = bpy.context.window_manager.component_registry
    for name, comp in reg.components.items():
        if "type" in comp.props:
            bpy.utils.unregister_class(comp.props["type"])

    for name in reg.schema.keys():
        delattr(bpy.types.Object, "zhott_" + name)

    del bpy.types.Object.game_components
    del bpy.types.WindowManager.component_registry
