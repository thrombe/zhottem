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
from bpy.props import (
    EnumProperty,
    StringProperty,
    BoolProperty,
    IntProperty,
    FloatProperty,
    FloatVectorProperty,
    CollectionProperty,
    PointerProperty,
)
from bpy.types import PropertyGroup, Panel, Operator, Menu

AVAILABLE_COMPONENTS = {
    "EntityId": {"type": "INT", "default": 0},
    "Collider": {
        "type": "COMPLEX",
        "defaults": {
            "collider_type": "BOX",
            "radius": 1.0,
            "height": 2.0,
            "size": (1.0, 1.0, 1.0),
        },
    },
    "AnimationType": {
        "type": "ENUM",
        "items": [
            ("IDLE", "Idle", "Idle animation"),
            ("WALK", "Walk", "Walk animation"),
            ("RUN", "Run", "Run animation"),
        ],
        "default": "IDLE",
    },
    "IsStatic": {"type": "BOOL", "default": False},
    "Mass": {"type": "FLOAT", "default": 1.0},
}


class ColliderProperties(PropertyGroup):
    collider_type: EnumProperty(
        name="Type",
        items=[
            ("BOX", "Box", "Box collider"),
            ("SPHERE", "Sphere", "Sphere collider"),
            ("CAPSULE", "Capsule", "Capsule collider"),
            ("MESH", "Mesh", "Mesh collider"),
        ],
        default="BOX",
        description="Type of collider shape",
    )
    radius: FloatProperty(
        name="Radius",
        default=1.0,
        min=0.01,
        soft_max=10.0,
        step=0.1,
        subtype="DISTANCE",
        description="Radius for sphere or capsule collider",
    )
    height: FloatProperty(
        name="Height",
        default=2.0,
        min=0.01,
        soft_max=10.0,
        step=0.1,
        subtype="DISTANCE",
        description="Height for capsule collider",
    )
    size: FloatVectorProperty(
        name="Size",
        default=(1.0, 1.0, 1.0),
        min=0.01,
        soft_max=10.0,
        step=0.1,
        subtype="XYZ_LENGTH",
        description="Dimensions for box collider",
    )


class GameComponent(PropertyGroup):
    component_type: StringProperty(name="Type")

    # Simple value storage
    int_value: IntProperty(name="Value", default=0)
    enum_value: EnumProperty(
        name="Value",
        items=[
            ("IDLE", "Idle", "Idle animation"),
            ("WALK", "Walk", "Walk animation"),
            ("RUN", "Run", "Run animation"),
        ],
        default="IDLE",
    )
    bool_value: BoolProperty(name="Enabled", default=False)
    float_value: FloatProperty(name="Value", default=0.0)

    # Complex components
    collider: PointerProperty(type=ColliderProperties)


class OBJECT_OT_add_game_component(Operator):
    bl_idname = "object.add_game_component"
    bl_label = "Add Game Component"

    component_type: StringProperty()

    def execute(self, context):
        obj = context.object
        components = obj.game_components

        if any(c.component_type == self.component_type for c in components):
            self.report({"WARNING"}, f"Component {self.component_type} already exists")
            return {"CANCELLED"}

        new_comp = components.add()
        new_comp.component_type = self.component_type

        # Set defaults
        comp_data = AVAILABLE_COMPONENTS[self.component_type]
        if comp_data["type"] == "INT":
            new_comp.int_value = comp_data["default"]
        elif comp_data["type"] == "ENUM":
            new_comp.enum_value = comp_data["default"]
        elif comp_data["type"] == "BOOL":
            new_comp.bool_value = comp_data["default"]
        elif comp_data["type"] == "FLOAT":
            new_comp.float_value = comp_data["default"]
        elif self.component_type == "Collider":
            collider = new_comp.collider
            defaults = comp_data["defaults"]
            collider.collider_type = defaults["collider_type"]
            collider.radius = defaults["radius"]
            collider.height = defaults["height"]
            collider.size = defaults["size"]

        return {"FINISHED"}


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


class COMPONENT_MT_add(Menu):
    bl_label = "Add Component"
    bl_idname = "COMPONENT_MT_add"

    def draw(self, context):
        layout = self.layout
        for comp_type in AVAILABLE_COMPONENTS:
            props = layout.operator(
                OBJECT_OT_add_game_component.bl_idname, text=comp_type
            )
            props.component_type = comp_type


class OBJECT_PT_game_components(Panel):
    bl_label = "Game Components"
    bl_idname = "OBJECT_PT_game_components"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"

    def draw(self, context):
        layout = self.layout
        obj = context.object

        # Add component dropdown
        row = layout.row()
        row.menu("COMPONENT_MT_add", text="Add Component")

        # List components
        for idx, comp in enumerate(obj.game_components):
            box = layout.box()
            header = box.row()
            header.label(text=comp.component_type)
            header.operator(
                OBJECT_OT_remove_game_component.bl_idname, text="", icon="X"
            ).index = idx

            # Draw component properties
            if comp.component_type == "Collider":
                collider = comp.collider
                box.prop(collider, "collider_type")

                if collider.collider_type in {"SPHERE", "CAPSULE"}:
                    box.prop(collider, "radius")
                if collider.collider_type == "CAPSULE":
                    box.prop(collider, "height")
                if collider.collider_type == "BOX":
                    box.prop(collider, "size")
            else:
                comp_data = AVAILABLE_COMPONENTS.get(comp.component_type, {})
                if comp_data.get("type") == "ENUM":
                    box.prop(comp, "enum_value", text="")
                elif comp_data.get("type") == "INT":
                    box.prop(comp, "int_value")
                elif comp_data.get("type") == "BOOL":
                    box.prop(comp, "bool_value")
                elif comp_data.get("type") == "FLOAT":
                    box.prop(comp, "float_value")


class GLTF2ExportUserExtension:
    def __init__(self):
        self.properties = bpy.context.scene.get("gltf2ExportUserExtensions", {})

    def gather_node_hook(self, gltf2_object, blender_object, export_settings):
        if blender_object and blender_object.game_components:
            if not hasattr(gltf2_object, "extras"):
                gltf2_object.extras = {}

            components_data = []
            for comp in blender_object.game_components:
                comp_data = {"type": comp.component_type, "values": {}}

                if comp.component_type == "Collider":
                    collider = comp.collider
                    comp_data["values"]["type"] = collider.collider_type
                    if collider.collider_type in {"SPHERE", "CAPSULE"}:
                        comp_data["values"]["radius"] = collider.radius
                    if collider.collider_type == "CAPSULE":
                        comp_data["values"]["height"] = collider.height
                    if collider.collider_type == "BOX":
                        comp_data["values"]["size"] = list(collider.size)
                else:
                    value_type = AVAILABLE_COMPONENTS[comp.component_type][
                        "type"
                    ].lower()
                    comp_data["values"]["value"] = getattr(comp, f"{value_type}_value")

                components_data.append(comp_data)

            if components_data:
                gltf2_object.extras["components"] = components_data


def register():
    bpy.utils.register_class(Reload)
    bpy.utils.register_class(SocketServerPanel)
    bpy.utils.register_class(SOCKETSERVER_OT_start)
    bpy.utils.register_class(SOCKETSERVER_OT_stop)

    bpy.utils.register_class(ColliderProperties)
    bpy.utils.register_class(GameComponent)
    bpy.utils.register_class(OBJECT_OT_add_game_component)
    bpy.utils.register_class(OBJECT_OT_remove_game_component)
    bpy.utils.register_class(COMPONENT_MT_add)
    bpy.utils.register_class(OBJECT_PT_game_components)

    bpy.types.Object.game_components = CollectionProperty(type=GameComponent)

    # Register glTF export handler
    try:
        from io_scene_gltf2.io.com.gltf2_io_extensions import Extension
        from io_scene_gltf2.blender.exp import gltf2_blender_export

        gltf2_blender_export.GLTF2ExportUserExtension = GLTF2ExportUserExtension
    except ImportError:
        print("GLTF2 Export extension not available")


def unregister():
    bpy.utils.unregister_class(Reload)

    global running
    if running:
        bpy.ops.socketserver.stop()

    bpy.utils.unregister_class(SocketServerPanel)
    bpy.utils.unregister_class(SOCKETSERVER_OT_start)
    bpy.utils.unregister_class(SOCKETSERVER_OT_stop)

    bpy.utils.unregister_class(ColliderProperties)
    bpy.utils.unregister_class(GameComponent)
    bpy.utils.unregister_class(OBJECT_OT_add_game_component)
    bpy.utils.unregister_class(OBJECT_OT_remove_game_component)
    bpy.utils.unregister_class(COMPONENT_MT_add)
    bpy.utils.unregister_class(OBJECT_PT_game_components)

    del bpy.types.Object.game_components
