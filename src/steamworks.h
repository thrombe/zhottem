
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#ifndef CALLCONV_C
#define CALLCONV_C(typ) typ
#endif

// CALLCONV_C(void*) zphysicsAlloc(uint64_t sz);
// CALLCONV_C(void) zphysicsFree(void *ptr);

#define zalloc(sze) malloc(sze)
#define zfree(p) free(p)

typedef void *ZhottSteamCtx;
CALLCONV_C(ZhottSteamCtx) steam_init();
CALLCONV_C(void) steam_deinit(ZhottSteamCtx);
CALLCONV_C(void) server_init(ZhottSteamCtx);
CALLCONV_C(void) server_deinit(ZhottSteamCtx);
CALLCONV_C(void) server_tick(ZhottSteamCtx);
CALLCONV_C(void) client_init(ZhottSteamCtx);
CALLCONV_C(void) client_deinit(ZhottSteamCtx);
CALLCONV_C(void) client_tick(ZhottSteamCtx);
CALLCONV_C(bool) client_connect_to_server(ZhottSteamCtx);
