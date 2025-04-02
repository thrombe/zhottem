import bpy

from .src import registry
from .src import server
from .src import utils

bl_info = {
    "name": "zhottem-blender",
    "blender": (4, 2, 0),
}


def register():
    bpy.utils.register_class(utils.Reload)

    # Register glTF export handler
    try:
        from io_scene_gltf2.io.com.gltf2_io_extensions import Extension
        from io_scene_gltf2.blender.exp import gltf2_blender_export

        gltf2_blender_export.GLTF2ExportUserExtension = utils.GLTF2ExportUserExtension
    except ImportError:
        print("GLTF2 Export extension not available")

    server.register()
    registry.register()


def unregister():
    bpy.utils.unregister_class(utils.Reload)

    server.unregister()
    registry.unregister()
