import bpy

from .src import registry
from .src import server
from .src import utils

bl_info = {
    "name": "zhottem-blender",
    "blender": (4, 2, 0),
}

# https://github.com/KhronosGroup/glTF-Blender-IO/tree/main/example-addons/example_gltf_exporter_extension
glTF2ExportUserExtension = registry.glTF2ExportUserExtension


def register():
    # bpy.utils.register_class(utils.Reload)

    # server.register()
    registry.register()


def unregister():
    # bpy.utils.unregister_class(utils.Reload)

    # server.unregister()
    registry.unregister()
