#pragma once

#include <stdint.h>

#include <steam/steam_api_flat.h>
#include <steam/steam_gameserver.h>

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
  ServerCallbacks callbacks;
} ZhottServer;

typedef struct {
  const char *gamedir;
  const char *lobby_password;

  bool initialized;
  bool connected;
  SteamAPICall_t lobby_request;
  SteamAPICall_t lobby_created;
  uint64_steamid lobby_id;
  HSteamNetConnection server_conn;
  ClientCallbacks callbacks;
} ZhottClient;

typedef struct {
  ZhottServer server;
  ZhottClient client;
} ZhottSteamContext;

// server needs to send client's info in addition to the data sent by the clients
typedef struct {
  uint32_t conn;
  uint64_t user_steam_id;
  int64_t message_number;
} ClientMessageHeader;
