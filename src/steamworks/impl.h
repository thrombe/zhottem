#pragma once

#include <stdint.h>

#include <steam_api.h>
#include <steam_gameserver.h>

extern "C" {
#define CALLCONV_C(typ) extern "C" typ __cdecl
#include "steamworks/api.h"
}

typedef struct {
  const char *version;
  const char *moddir;
  const char *product;
  const char *description;
  uint16_t port;
  uint16_t updater_port;

  bool initialized;
  HSteamListenSocket listen_socket;
  HSteamNetPollGroup poll_group;
} ZhottServer;

typedef struct {
  const char *gamedir;
  const char *lobby_password;

  bool initialized;
  SteamAPICall_t lobby_request;
  SteamAPICall_t lobby_created;
  CSteamID lobby_id;
  HSteamNetConnection server_conn;
} ZhottClient;

typedef struct {
  ZhottServer server;
  ZhottClient client;
} ZhottSteamContext;
