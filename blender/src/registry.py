import bpy
import json
import os
from bpy.props import (
    StringProperty,
    BoolProperty,
    IntProperty,
    FloatProperty,
    FloatVectorProperty,
    EnumProperty,
    CollectionProperty,
)
from bpy.types import PropertyGroup, Panel, Operator, Menu


class ComponentRegistry:
    schema: dict
    enums: dict

    def __init__(self):
        with open(os.path.join(os.path.dirname(__file__), "../components.json")) as f:
            self.schema = json.loads(f.read())

        self.enums = {}
        for tname, t in self.schema.items():
            if t["type"] == "enum":
                self.enums[tname] = t["variants"]
            elif t["type"] == "union":
                self.enums[tname] = list(t["variants"].keys())


class ComponentProperty(PropertyGroup):
    """Base property type that supports nested paths"""

    name: StringProperty()
    path: StringProperty()  # Dot-separated path for nested properties
    type: StringProperty()

    # Value storage
    value_int: IntProperty()
    value_float: FloatProperty()
    value_bool: BoolProperty()
    value_string: StringProperty()
    value_vec3: FloatVectorProperty(size=3)
    value_vec4: FloatVectorProperty(size=4)

    def enum_variants(self, context):
        reg = bpy.context.window_manager.component_registry
        if self.enum_type in reg.enums:
            return [(k, k, "") for k in reg.enums[self.enum_type]]
        else:
            return [
                ("undefined", "undefined", ""),
            ]

    enum_type: StringProperty()
    value_enum: EnumProperty(items=enum_variants)


class GameComponent(PropertyGroup):
    component_type: StringProperty()
    properties: CollectionProperty(type=ComponentProperty)


class OBJECT_OT_add_game_component(Operator):
    bl_idname = "object.add_game_component"
    bl_label = "Add Game Component"
    bl_property = "component_type"

    def component_enum_variants(self, context):
        reg = bpy.context.window_manager.component_registry
        return [(k, k, "") for k in reg.schema.keys()]

    component_type: EnumProperty(items=component_enum_variants)

    def execute(self, context):
        obj = context.object
        components = obj.game_components

        if any(c.component_type == self.component_type for c in components):
            self.report({"WARNING"}, f"Component {self.component_type} already exists")
            return {"CANCELLED"}

        new_comp = components.add()
        new_comp.component_type = self.component_type

        reg = bpy.context.window_manager.component_registry
        comp_def = reg.schema[self.component_type]
        self._add_properties(new_comp.properties, comp_def)
        return {"FINISHED"}

    def _add_properties(self, props, defn, path=""):
        if defn["type"] == "struct":
            for name, prop_def in defn["properties"].items():
                new_path = f"{path}.{name}" if path else name
                if prop_def["type"] == "struct":
                    self._add_properties(props, prop_def, new_path)
                else:
                    prop = props.add()
                    prop.name = name
                    prop.path = new_path
                    prop.type = prop_def["type"]
                    self._set_default(prop, prop_def)
        elif defn["type"] == "union":
            prop = props.add()
            prop.name = defn["tag"]
            prop.path = path
            prop.type = "enum"
            prop.enum_type = self.component_type
            prop.value_enum = defn["default_tag"]

            for variant, variant_def in defn["variants"].items():
                new_path = f"{path}.{variant}" if path else variant
                self._add_properties(props, variant_def, new_path)
        elif defn["type"] == "enum":
            prop = props.add()
            prop.type = defn["type"]
            prop.enum_type = self.component_type
            prop.value_enum = defn["default"]
        else:
            prop = props.add()
            prop.type = defn["type"]
            self._set_default(prop, defn)

    def _set_default(self, prop, defn):
        if prop.type == "int":
            prop.value_int = defn.get("default", 0)
        elif prop.type == "float":
            prop.value_float = defn.get("default", 0.0)
        elif prop.type == "bool":
            prop.value_bool = defn.get("default", False)
        elif prop.type == "string":
            prop.value_string = defn.get("default", "")
        elif prop.type == "enum":
            prop.value_enum = defn["default"]
        elif prop.type == "vec3":
            prop.value_vec3 = defn.get("default", (0, 0, 0))
        elif prop.type == "vec4":
            prop.value_vec3 = defn.get("default", (0, 0, 0, 0))


class OBJECT_PT_game_components(Panel):
    bl_label = "Game Components"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"

    def draw(self, context):
        layout = self.layout
        obj = context.object

        # Add component dropdown
        row = layout.row()
        row.menu("COMPONENT_MT_add", text="Add Component")

        # Draw components
        for idx, comp in enumerate(obj.game_components):
            box = layout.box()
            header = box.row()
            header.label(text=comp.component_type)
            header.operator(
                "object.remove_game_component", text="", icon="X"
            ).index = idx

            self.draw_properties(box, comp)

    def draw_properties(self, layout, comp):
        reg = bpy.context.window_manager.component_registry
        comp_def = reg.schema.get(comp.component_type, {})
        properties_by_path = {prop.path: prop for prop in comp.properties}
        self._draw_property_tree(layout, comp_def, properties_by_path)

    def _draw_property_tree(self, layout, defn, properties, path=""):
        if defn["type"] == "struct":
            for name, prop_def in defn["properties"].items():
                current_path = f"{path}.{name}" if path else name
                if prop_def["type"] == "struct":
                    self._draw_struct(layout, name, prop_def, properties, current_path)
                else:
                    self._draw_property(layout, properties[current_path])
        elif defn["type"] == "union":
            tag_prop = properties[path]
            layout.prop(tag_prop, "value_enum", text=tag_prop.name)

            active_variant = tag_prop.value_enum
            variant_path = f"{path}.{active_variant}" if path else active_variant
            if active_variant in defn["variants"]:
                variant_def = defn["variants"][active_variant]
                self._draw_property_tree(layout, variant_def, properties, variant_path)
        else:
            self._draw_property(layout, properties[path])

    def _draw_struct(self, layout, name, defn, properties, path):
        box = layout.box()
        box.label(text=name)
        self._draw_property_tree(box, defn, properties, path)

    def _draw_property(self, layout, prop):
        row = layout.row()
        if prop.type == "int":
            row.prop(prop, "value_int", text=prop.name)
        elif prop.type == "float":
            row.prop(prop, "value_float", text=prop.name)
        elif prop.type == "bool":
            row.prop(prop, "value_bool", text=prop.name)
        elif prop.type == "string":
            row.prop(prop, "value_string", text=prop.name)
        elif prop.type == "enum":
            row.prop(prop, "value_enum", text=prop.name)
        elif prop.type == "vec3":
            row.prop(prop, "value_vec3", text=prop.name)
        elif prop.type == "vec4":
            row.prop(prop, "value_vec4", text=prop.name)


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
        components = obj.game_components
        if 0 <= self.index < len(components):
            components.remove(self.index)
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


classes = (
    ComponentProperty,
    GameComponent,
    OBJECT_OT_remove_game_component,
    OBJECT_OT_add_game_component,
    OBJECT_PT_game_components,
    COMPONENT_MT_add,
)


def register():
    bpy.types.WindowManager.component_registry = ComponentRegistry()

    for cls in classes:
        bpy.utils.register_class(cls)

    bpy.types.Object.game_components = CollectionProperty(type=GameComponent)


def unregister():
    for cls in reversed(classes):
        bpy.utils.unregister_class(cls)

    del bpy.types.Object.game_components
    del bpy.types.WindowManager.component_registry
