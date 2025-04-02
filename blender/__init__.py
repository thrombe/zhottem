import bpy
import os
import socket
import threading
import select
from bpy.app.handlers import persistent

bl_info = {
    "name": "zhottem-blender",
    "blender": (4, 2, 0),
}


class Reload(bpy.types.Operator):
    """reloads add-ons"""

    bl_idname = "zhottem.reload"
    bl_label = bl_idname
    bl_options = {"REGISTER"}

    def execute(self, context):
        bpy.ops.script.reload()
        return {"FINISHED"}


SOCKET_PATH = "/tmp/zhottem/blender.sock"
server = None
clients = set()
running = False
thread = None
lock = threading.Lock()


def start_server():
    global server, running, thread

    # Remove existing socket if it exists
    if os.path.exists(SOCKET_PATH):
        os.unlink(SOCKET_PATH)

    # Create Unix socket
    server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    server.bind(SOCKET_PATH)
    server.listen(5)
    running = True

    print(f"Server started at {SOCKET_PATH}")

    def server_loop():
        while running:
            try:
                # Use select to handle multiple connections
                readable, _, _ = select.select([server] + list(clients), [], [], 1.0)

                for sock in readable:
                    if sock is server:
                        # New client connection
                        client, _ = server.accept()
                        with lock:
                            clients.add(client)
                        print(f"New client connected. Total clients: {len(clients)}")
                    else:
                        # Handle client message
                        try:
                            data = sock.recv(1024)
                            if data:
                                message = data.decode("utf-8").strip()
                                print(f"Received from client: {message}")
                                # Here you could add custom handling of client messages
                            else:
                                # Client disconnected
                                with lock:
                                    clients.remove(sock)
                                sock.close()
                                print(
                                    f"Client disconnected. Total clients: {len(clients)}"
                                )
                        except:
                            with lock:
                                if sock in clients:
                                    clients.remove(sock)
                            sock.close()
            except Exception as e:
                print(f"Server error: {e}")
                break

        # Cleanup
        with lock:
            for client in clients:
                client.close()
            clients.clear()
        server.close()
        if os.path.exists(SOCKET_PATH):
            os.unlink(SOCKET_PATH)

    thread = threading.Thread(target=server_loop)
    thread.daemon = True
    thread.start()


def notify_clients(message):
    """Send message to all connected clients"""
    if not running or not clients:
        return

    with lock:
        disconnected = set()
        for client in clients:
            try:
                client.send((message + "\n").encode("utf-8"))
            except:
                disconnected.add(client)

        # Clean up disconnected clients
        for client in disconnected:
            clients.remove(client)
            client.close()


@persistent
def depsgraph_update(scene: bpy.types.Scene):
    """Handler for dependency graph updates"""
    notify_clients("DEPSGRAPH_UPDATE")


@persistent
def frame_change(scene):
    """Handler for frame changes"""
    notify_clients(f"FRAME_CHANGE:{scene.frame_current}")


class SocketServerPanel(bpy.types.Panel):
    bl_label = "Socket Server"
    bl_idname = "VIEW3D_PT_socket_server"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"
    bl_category = "Socket"

    def draw(self, context):
        layout = self.layout
        row = layout.row()
        row.label(text=f"Connected clients: {len(clients)}")

        if not running:
            row = layout.row()
            row.operator("zhottem.socketserver_start")
        else:
            row = layout.row()
            row.operator("zhottem.socketserver_stop")


class SOCKETSERVER_OT_start(bpy.types.Operator):
    bl_idname = "zhottem.socketserver_start"
    bl_label = "Start Server"

    def execute(self, context):
        global running
        if not running:
            start_server()
            # Register handlers
            bpy.app.handlers.depsgraph_update_post.append(depsgraph_update)
            bpy.app.handlers.frame_change_post.append(frame_change)
        return {"FINISHED"}


class SOCKETSERVER_OT_stop(bpy.types.Operator):
    bl_idname = "zhottem.socketserver_stop"
    bl_label = "Stop Server"

    def execute(self, context):
        global running, server, thread
        if running:
            running = False
            if server:
                server.close()
            if thread:
                thread.join(timeout=1.0)
            # Unregister handlers
            if depsgraph_update in bpy.app.handlers.depsgraph_update_post:
                bpy.app.handlers.depsgraph_update_post.remove(depsgraph_update)
            if frame_change in bpy.app.handlers.frame_change_post:
                bpy.app.handlers.frame_change_post.remove(frame_change)
        return {"FINISHED"}


# class ComponentRegistery(bpy.types.PropertyGroup):
#     """defines components that can be added to anything"""

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
        with open(os.path.join(os.path.dirname(__file__), "components.json")) as f:
            self.schema = json.loads(f.read())

        self.enums = {}
        for tname, t in self.schema.items():
            if t["type"] == "enum":
                self.enums[tname] = t["variants"]
            elif t["type"] == "union":
                self.enums[tname] = list(t["variants"].keys())

        print(self.enums)


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


def register_1():
    bpy.utils.register_class(Reload)
    bpy.utils.register_class(SocketServerPanel)
    bpy.utils.register_class(SOCKETSERVER_OT_start)
    bpy.utils.register_class(SOCKETSERVER_OT_stop)

    # Register glTF export handler
    try:
        from io_scene_gltf2.io.com.gltf2_io_extensions import Extension
        from io_scene_gltf2.blender.exp import gltf2_blender_export

        gltf2_blender_export.GLTF2ExportUserExtension = GLTF2ExportUserExtension
    except ImportError:
        print("GLTF2 Export extension not available")


def register_2():
    bpy.types.WindowManager.component_registry = ComponentRegistry()

    for cls in classes:
        bpy.utils.register_class(cls)

    bpy.types.Object.game_components = CollectionProperty(type=GameComponent)


def unregister_1():
    bpy.utils.unregister_class(Reload)

    global running
    if running:
        bpy.ops.socketserver.stop()

    bpy.utils.unregister_class(SocketServerPanel)
    bpy.utils.unregister_class(SOCKETSERVER_OT_start)
    bpy.utils.unregister_class(SOCKETSERVER_OT_stop)


def unregister_2():
    for cls in reversed(classes):
        bpy.utils.unregister_class(cls)

    del bpy.types.Object.game_components
    del bpy.types.WindowManager.component_registry


class Component:
    def __init__(self, name: str, typ: str, val: PropertyGroup):
        self.name = name
        self.type = typ
        self.val = val


class ComponentRegistry2:
    def __init__(self):
        self.base_types = {
            "f32": (FloatProperty, {}),
            "u32": (IntProperty, {}),
            "vec3": (FloatVectorProperty, {"size": 3}),
            "vec4": (FloatVectorProperty, {"size": 4}),
        }
        self.classes = []

        import json

        with open(os.path.join(os.path.dirname(__file__), "components.json")) as f:
            schema = json.loads(f.read())

        for name, t in schema.items():
            if t["type"] == "struct":
                __annotations__ = {}
                for prop, typ in t["properties"].items():
                    __annotations__[prop] = FloatProperty(name=prop, default=0)
                cls = type(name, (PropertyGroup,), {"__annotations__": __annotations__})
                self.classes.append(cls)


r = ComponentRegistry2()


def get_vals(_, _2):
    return [(c.__name__, c.__name__, "") for c in r.classes]


class OBJECT_OT_add_game_component2(Operator):
    bl_idname = "object.add_game_component2"
    bl_label = "Add Game Component"
    bl_property = "component_type"

    component_type: EnumProperty(items=get_vals)

    def execute(self, context):
        obj = context.object
        # components = obj.game_components2
        components = obj.game_components3

        # new_comp = components.add()
        # new_comp.component_type = self.component_type

        components[self.component_type] = 12
        print(components)

        return {"FINISHED"}


class OBJECT_OT_remove_game_component(Operator):
    bl_idname = "object.remove_game_component2"
    bl_label = "Remove Game Component"

    index: IntProperty()

    def execute(self, context):
        obj = context.object
        components = obj.game_components2
        if 0 <= self.index < len(components):
            components.remove(self.index)
        return {"FINISHED"}


# Add this menu class
class COMPONENT_MT_add2(Menu):
    bl_label = "Add Component"
    bl_idname = "COMPONENT_MT_add2"

    def draw(self, context):
        layout = self.layout
        for comp_type in r.classes:
            props = layout.operator(
                OBJECT_OT_add_game_component2.bl_idname, text=comp_type.__name__
            )
            props.component_type = comp_type.__name__


class OBJECT_PT_game_components2(Panel):
    bl_label = "Game Components2"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"

    def draw(self, context):
        layout = self.layout
        obj = context.object

        # Add component dropdown
        row = layout.row()
        row.menu("COMPONENT_MT_add2", text="Add Component")

        # Draw components
        for idx, comp in enumerate(obj.game_components2):
            box = layout.box()
            header = box.row()
            header.label(text=comp.component_type2)
            header.operator("object.remove_game_component2", text="", icon="X")


def register_3():
    for clss in r.classes + [
        OBJECT_OT_add_game_component2,
        COMPONENT_MT_add2,
        OBJECT_PT_game_components2,
    ]:
        bpy.utils.register_class(clss)

    bpy.types.Object.game_components2 = CollectionProperty(type=PropertyGroup)
    bpy.types.Object.game_components3 = {}


def unregister_3():
    for clss in r.classes + [
        OBJECT_OT_add_game_component2,
        COMPONENT_MT_add2,
        OBJECT_PT_game_components2,
    ]:
        bpy.utils.unregister_class(clss)

    del bpy.types.Object.game_components2
    del bpy.types.Object.game_components3


def register():
    register_1()
    register_2()
    register_3()


def unregister():
    unregister_1()
    unregister_2()
    unregister_3()
