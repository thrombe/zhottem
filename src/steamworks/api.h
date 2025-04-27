#pragma once

#include <stdbool.h>
#include <stdint.h>

#ifndef CALLCONV_C
#define CALLCONV_C(typ) typ
#endif

CALLCONV_C(void *) zphysicsAlloc(uint64_t sz);
CALLCONV_C(void) zphysicsFree(void *ptr);

#define zalloc(sze) zphysicsAlloc(sze)
#define zfree(p) zphysicsFree(p)

typedef struct {
  const uint8_t *data;
  uint32_t len;

  // connection handle
  uint32_t conn;

  uint64_t user_steam_id;

  // unique incrementing number in it's lane
  int64_t message_number;
} NetworkMessage;

typedef struct {
  const uint8_t *data;
  uint32_t len;

  struct {
    bool reliable;
    bool force_flush;

    // drop if not sent in a very short amount of time
    bool no_delay;
    bool restart_broken_session;
  } flags;
} OutgoingMessage;

typedef struct {
  void *ctx;
  void (*msg_recv)(void *ctx, NetworkMessage msg);
} ClientCallbacks;

typedef struct {
  void *ctx;
  void (*msg_recv)(void *ctx, NetworkMessage msg);
} ServerCallbacks;

typedef void *ZhottSteamCtx;
CALLCONV_C(ZhottSteamCtx) steam_init();
CALLCONV_C(void) steam_deinit(ZhottSteamCtx);
CALLCONV_C(bool) server_init(ZhottSteamCtx, ServerCallbacks);
CALLCONV_C(void) server_deinit(ZhottSteamCtx);
CALLCONV_C(void) server_tick(ZhottSteamCtx);
CALLCONV_C(void) server_pre_reload(ZhottSteamCtx);
CALLCONV_C(void) server_post_reload(ZhottSteamCtx, ServerCallbacks);
CALLCONV_C(void) server_msg_send(ZhottSteamCtx, uint32_t, uint32_t, OutgoingMessage);
CALLCONV_C(void) client_init(ZhottSteamCtx, ClientCallbacks);
CALLCONV_C(void) client_deinit(ZhottSteamCtx);
CALLCONV_C(void) client_tick(ZhottSteamCtx);
CALLCONV_C(void) client_pre_reload(ZhottSteamCtx);
CALLCONV_C(void) client_post_reload(ZhottSteamCtx, ClientCallbacks);
CALLCONV_C(void) client_msg_send(ZhottSteamCtx, OutgoingMessage);
CALLCONV_C(bool) client_is_connected(ZhottSteamCtx _ctx);
