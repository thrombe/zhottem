import bpy

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


def register():
    bpy.utils.register_class(Reload)


def unregister():
    bpy.utils.unregister_class(Reload)
