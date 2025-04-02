import bpy
import os
import socket
import threading
import select
from bpy.app.handlers import persistent


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


def register():
    bpy.utils.register_class(SocketServerPanel)
    bpy.utils.register_class(SOCKETSERVER_OT_start)
    bpy.utils.register_class(SOCKETSERVER_OT_stop)


def unregister_1():
    global running
    if running:
        bpy.ops.socketserver.stop()

    bpy.utils.unregister_class(SocketServerPanel)
    bpy.utils.unregister_class(SOCKETSERVER_OT_start)
    bpy.utils.unregister_class(SOCKETSERVER_OT_stop)
