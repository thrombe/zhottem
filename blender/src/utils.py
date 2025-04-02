import bpy


class Reload(bpy.types.Operator):
    """reloads add-ons"""

    bl_idname = "zhottem.reload"
    bl_label = bl_idname
    bl_options = {"REGISTER"}

    def execute(self, context):
        bpy.ops.script.reload()
        return {"FINISHED"}
