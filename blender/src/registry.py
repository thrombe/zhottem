import bpy
import json
import os
from typing import Dict, Any, Optional
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
from bpy.types import (
    DriverVariable,
    FCurve,
    PropertyGroup,
    Panel,
    Operator,
    Menu,
    Object,
)


class ComponentsStatus(str):
    UNINIT = "uninit"
    INIT = "init"


class ComponentsStatusProp(PropertyGroup):
    status: EnumProperty(
        items=[(k, k, "") for k in [ComponentsStatus.UNINIT, ComponentsStatus.INIT]],
        default=ComponentsStatus.UNINIT,
    )  # type:ignore


class Component:
    def __init__(self, clas, props: dict, defn: dict, name: Optional[str] = None):
        self.name = name if name else clas.__name__
        self.clas = clas
        self.props = props
        self.defn = defn


class ComponentRegistry:
    name_prefix: str = "zhott_"
    path_prefix: str = "zhottem."
    schema: dict
    components: Dict[str, Component]
    classes: list

    # the class of top level propgroup
    top_prop: Component

    def __init__(self):
        self.components: Dict[str, Component] = {}
        self.classes = [
            ComponentsStatusProp,
        ]

        with open(os.path.join(os.path.dirname(__file__), "../components.json")) as f:
            self.schema = json.loads(f.read())

        for name, typ in self.schema.items():
            comp = self.parse_type(name, typ)
            self.components[name] = comp

        self.top_prop = self.prepare_top_prop()

    def prepare_top_prop(self) -> Component:
        name = "TopProp"
        class_props: dict = {"bl_label": name, "bl_idname": self.path_prefix + name}
        __annotations__: dict = {}

        for name in self.schema.keys():
            comp = self.components[name]
            __annotations__[name] = comp.clas(**comp.props)

        comp = self.prepare_component_entry()
        __annotations__["active_components"] = CollectionProperty(
            type=comp.props["type"]
        )
        __annotations__["components_status"] = PointerProperty(
            type=ComponentsStatusProp
        )

        class_props["__annotations__"] = __annotations__
        t = type(name, (PropertyGroup,), class_props)
        self.classes.append(t)
        return Component(
            PointerProperty,
            {"type": t},
            self.schema,
            name,
        )

    def prepare_component_entry(self) -> Component:
        name = "ComponentEntry"
        class_props: dict = {"bl_label": name, "bl_idname": self.path_prefix + name}
        __annotations__: dict = {}

        __annotations__["type"] = EnumProperty(
            items=[(k, k, "") for k in self.schema.keys()]
        )

        class_props["__annotations__"] = __annotations__
        t = type(name, (PropertyGroup,), class_props)
        self.classes.append(t)
        return Component(
            PointerProperty,
            {"type": t},
            {
                "type": "struct",
                "properties": {
                    "type": {  # this "type" is the attr name
                        "type": "enum",
                        "variants": list(self.schema.keys()),
                    }
                },
            },
            name,
        )

    # https://projects.blender.org/blender/blender/issues/86719#issuecomment-232525
    def parse_type(self, name: str, typ: dict) -> Component:
        class_props: dict = {"bl_label": name, "bl_idname": self.path_prefix + name}
        if typ["type"] == "struct":
            __annotations__: dict = {}
            for tname, ttyp in typ["properties"].items():
                comp = self.parse_type(tname, ttyp)
                __annotations__[tname] = comp.clas(**comp.props)

            class_props["__annotations__"] = __annotations__

            t = type(name, (PropertyGroup,), class_props)
            self.classes.append(t)
            return Component(
                PointerProperty,
                {"type": t},
                typ,
                name,
            )
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
            t = type(name, (PropertyGroup,), class_props)
            self.classes.append(t)
            return Component(
                PointerProperty,
                {"type": t},
                typ,
                name,
            )
        elif typ["type"] == "enum":
            value_props = {
                "items": [(k, k, "") for k in typ["variants"]],
                "default": typ["default"],
            }
            return Component(EnumProperty, value_props, typ)
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
            raise Exception("unknown component type")


class OBJECT_OT_add_game_component(Operator):
    bl_idname = "object.add_game_component"
    bl_label = "Add Game Component"
    bl_property = "component_type"

    def component_enum_variants(self, context):
        reg: ComponentRegistry = bpy.context.window_manager.component_registry  # type: ignore
        return [(k, k, "") for k in reg.schema.keys()]

    component_type: EnumProperty(items=component_enum_variants)  # type: ignore

    def execute(self, context):
        reg: ComponentRegistry = bpy.context.window_manager.component_registry  # type:ignore
        obj: Object = context.object  # type:ignore
        top_prop = getattr(obj, reg.name_prefix + "components")  # type:ignore
        status: ComponentsStatusProp = top_prop.components_status  # type:ignore
        components = top_prop.active_components  # type:ignore

        # TODO: can insert objects like collision shape and add a driver to it controlled by our component
        if status.status == ComponentsStatus.UNINIT:
            status.status = ComponentsStatus.INIT  # type:ignore

            # - [Easily Create Driver Variables with Blender Python - YouTube](https://www.youtube.com/watch?v=m-OFyHHY4KI)
            transform: PropertyGroup = getattr(top_prop, "Transform")
            drivers: list[FCurve] = transform.driver_add("position")  # type:ignore
            for driver, x in zip(drivers, ["x", "y", "z"]):
                driver.driver.expression = "vec_" + x  # type:ignore
                var: DriverVariable = driver.driver.variables.new()  # type:ignore
                var.name = "vec_" + x
                var.targets[0].id = obj
                var.targets[0].data_path = "location." + x

        if any(c == self.component_type for c in components):
            return {"CANCELLED"}

        comp = components.add()
        comp.type = self.component_type

        return {"FINISHED"}


class OBJECT_PT_game_components(Panel):
    bl_label = "Game Components"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"

    def draw(self, context):
        reg: ComponentRegistry = bpy.context.window_manager.component_registry  # type:ignore
        layout: bpy.types.UILayout = self.layout  # type:ignore
        obj: Object = context.object  # type:ignore
        top_prop = getattr(obj, reg.name_prefix + "components")  # type:ignore
        components = top_prop.active_components  # type:ignore

        row = layout.row()
        row.menu("COMPONENT_MT_add", text="Add Component")

        for i, name in enumerate(components):
            name = name.type
            comp = reg.components[name]

            box = layout.box()
            header = box.row()
            header.label(text=name)
            rgc: OBJECT_OT_remove_game_component = header.operator(
                "object.remove_game_component", text="", icon="X"
            )  # type:ignore
            rgc.index = i
            self.draw_type(top_prop, box, name, comp.defn)

    def draw_type(
        self,
        obj,
        layout: bpy.types.UILayout,
        name: str,
        defn: dict,
    ):
        if defn["type"] == "struct":
            for pname, ptype in defn["properties"].items():
                box = layout
                if ptype["type"] in ["struct", "union"]:
                    box = layout.box()
                    box.label(text=pname)
                self.draw_type(getattr(obj, name), box, pname, ptype)
        elif defn["type"] == "union":
            layout.prop(getattr(obj, name), "enum", text="type")
            for vname, vtype in defn["variants"].items():
                if vname != getattr(getattr(obj, name), "enum"):
                    continue
                box = layout
                if vtype["type"] in ["struct", "union"]:
                    box = layout.box()
                    box.label(text=vname)
                self.draw_type(getattr(obj, name), box, vname, vtype)
        else:
            row = layout.row()
            row.prop(obj, name, text=name)


class OBJECT_OT_remove_game_component(Operator):
    bl_idname = "object.remove_game_component"
    bl_label = "Remove Game Component"

    index: IntProperty()  # type:ignore

    def execute(self, context):
        reg: ComponentRegistry = bpy.context.window_manager.component_registry  # type:ignore
        obj: Object = context.object  # type:ignore
        top_prop = getattr(obj, reg.name_prefix + "components")  # type:ignore
        top_prop.active_components.remove(self.index)  # type:ignore
        return {"FINISHED"}


# Add this menu class
class COMPONENT_MT_add(Menu):
    bl_label = "Add Component"
    bl_idname = "COMPONENT_MT_add"

    def draw(self, context):
        reg: ComponentRegistry = bpy.context.window_manager.component_registry  # type:ignore
        layout: bpy.types.UILayout = self.layout  # type:ignore
        for comp_type in reg.schema:
            props: OBJECT_OT_add_game_component = layout.operator(
                OBJECT_OT_add_game_component.bl_idname, text=comp_type
            )  # type:ignore
            props.component_type = comp_type


classes = (
    OBJECT_OT_remove_game_component,
    OBJECT_OT_add_game_component,
    OBJECT_PT_game_components,
    COMPONENT_MT_add,
)


class glTF2ExportUserExtension:
    def __init__(self):
        from io_scene_gltf2.io.com.gltf2_io_extensions import Extension  # type:ignore

        self.Extension = Extension

    def glTF2_pre_export_callback(export_settings):
        print("This will be called before exporting the glTF file.")

    def glTF2_post_export_callback(export_settings):
        print("This will be called after exporting the glTF file.")

    def gather_node_hook(
        self,
        gltf2_object: Any,
        blender_object: Object,
        export_settings: Dict[str, Any],
    ) -> None:
        # Note: If you are using Collection Exporters, you may want to restrict the export for some collections
        # You can access the collection like this: export_settings['gltf_collection']
        # So you can check if you want to use this hook for this collection or not, using
        # if export_settings['gltf_collection'] != "Coll":
        #     return

        wm = bpy.context.window_manager
        reg: Optional[ComponentRegistry] = getattr(wm, "component_registry", None)
        if not reg:
            return

        if not blender_object:  # type:ignore
            return

        top_prop = getattr(blender_object, reg.name_prefix + "components", None)  # type:ignore
        if not top_prop:
            return

        components_data = []
        for comp_entry in top_prop.active_components:  # type:ignore
            comp = reg.components.get(comp_entry.type)
            if not comp:
                continue

            prop_group = getattr(top_prop, comp.name, None)
            if not prop_group:
                continue

            comp_data = {
                "component_name": comp.name,
                "value": self.serialize_component(prop_group, comp.defn),
            }
            components_data.append(comp_data)

        gltf2_object.extras = {reg.name_prefix + "components": components_data}

    def serialize_component(
        self,
        prop_group,
        defn: Dict[str, Any],
    ):
        if defn["type"] == "struct":
            return {
                pname: self.serialize_component(getattr(prop_group, pname), pdef)
                for pname, pdef in defn["properties"].items()
            }
        elif defn["type"] == "union":
            active_variant = prop_group.enum
            variant_def = defn["variants"][active_variant]
            return {
                str(getattr(prop_group, "enum")): self.serialize_component(
                    getattr(prop_group, active_variant),
                    variant_def,
                )
            }
        elif defn["type"] == "int":
            return int(prop_group)
        elif defn["type"] == "float":
            return float(prop_group)
        elif defn["type"] == "bool":
            return bool(prop_group)
        elif defn["type"] == "string":
            return str(prop_group)
        elif defn["type"] in ("vec3", "vec4"):
            return [float(v) for v in prop_group]
        elif defn["type"] == "enum":
            return str(prop_group)
        else:
            return None


def draw_export(context, layout):
    # Note: If you are using Collection Exporter, you may want to restrict UI for some collections
    # You can access the collection like this: context.collection
    # So you can check if you want to show the UI for this collection or not, using
    # if context.collection.name != "Coll":
    #     return

    header, body = layout.panel("GLTF_addon_example_exporter", default_closed=False)
    header.use_property_split = False

    header.label(text="lmao")


def register():
    bpy.types.WindowManager.component_registry = ComponentRegistry()  # type:ignore

    for cls in classes:
        bpy.utils.register_class(cls)

    reg: ComponentRegistry = bpy.context.window_manager.component_registry  # type:ignore
    for cls in reg.classes:
        bpy.utils.register_class(cls)

    comp = reg.top_prop
    setattr(bpy.types.Object, reg.name_prefix + "components", comp.clas(**comp.props))

    from io_scene_gltf2 import exporter_extension_layout_draw  # type:ignore

    exporter_extension_layout_draw["Example glTF Extension"] = draw_export


def unregister():
    for cls in reversed(classes):
        bpy.utils.unregister_class(cls)

    reg: ComponentRegistry = bpy.context.window_manager.component_registry  # type:ignore
    for cls in reg.classes:
        bpy.utils.unregister_class(cls)

    delattr(bpy.types.Object, reg.name_prefix + "components")

    del bpy.types.WindowManager.component_registry  # type:ignore

    from io_scene_gltf2 import exporter_extension_layout_draw  # type:ignore

    del exporter_extension_layout_draw["Example glTF Extension"]
