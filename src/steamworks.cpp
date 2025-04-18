#include <stdio.h>
#include <unistd.h>

#include <steam_api.h>
#include <steam_gameserver.h>

#include "isteamgameserver.h"
#include "isteammatchmaking.h"
#include "isteamnetworkingsockets.h"
#include "steamclientpublic.h"
#include "steamnetworkingtypes.h"
#include "steamtypes.h"

extern "C" {
#define CALLCONV_C(typ) extern "C" typ __cdecl
#include "steamworks.h"
}

CALLCONV_C(void) steam_api_dbg_hook(int nSeverity, const char *pchDebugText) {
  // if you're running in the debugger, only warnings (nSeverity >= 1) will be
  // sent if you add -debug_steamapi to the command-line, a lot of extra
  // informational messages will also be sent
  printf("%s\n", pchDebugText);

  if (nSeverity >= 1) {
    // place to set a breakpoint for catching API errors
    int x = 3;
    (void)x;
  }
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

CALLCONV_C(void *) steam_init() {
  if (SteamAPI_RestartAppIfNecessary(k_uAppIdInvalid)) {
    printf("relaunching via steam\n");
    return NULL;
  }

  SteamErrMsg errMsg = {0};
  if (SteamAPI_InitEx(&errMsg) != k_ESteamAPIInitResult_OK) {
    printf("SteamAPI_Init() failed: %s\n", errMsg);
    return NULL;
  }
  SteamAPI_ManualDispatch_Init();

  SteamClient()->SetWarningMessageHook(&steam_api_dbg_hook);

  if (!SteamUser()->BLoggedOn()) {
    printf("Steam user is not logged in\n");
    return NULL;
  }

  // TODO: parse cli args
  // +connect ipaddress:port
  //  - steam passes this when user joins a server using the server browser
  //  thing
  // +connect_lobby lobbyid
  //  - steam passes this when user joins a friend invite
  // the use of SteamApps()->GetLaunchCommandLine() requires some special steam
  // settings for the app

  auto ctx = (ZhottSteamContext *)zalloc(sizeof(ZhottSteamContext));
  *ctx = {0};
  return ctx;
}

CALLCONV_C(void) steam_deinit(void *ctx) {
  SteamAPI_Shutdown();
  zfree(ctx);
  return;
}

CALLCONV_C(void) server_init(void *_ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;
  ctx->server = {
      .version = "1.0.0.0",
      .moddir = "zhottem",
      .product = "zhottem",
      .description = "zhottem test game",
      .port = 27015,
      .updater_port = 27016,
  };
  auto server = &ctx->server;

  SteamErrMsg errMsg = {0};
  if (SteamGameServer_InitEx(0, server->port, server->updater_port,
                             eServerModeAuthentication, server->version,
                             &errMsg) != k_ESteamAPIInitResult_OK) {
    printf("SteamGameServer_Init call failed: %s\n", errMsg);
    return;
  }
  server->initialized = true;

  SteamGameServer()->SetModDir(server->moddir);
  SteamGameServer()->SetProduct(server->product);
  SteamGameServer()->SetGameDescription(server->description);

  SteamGameServer()->SetPasswordProtected(false);

  // SteamGameServer()->LogOn("token?");
  SteamGameServer()->LogOnAnonymous();

  SteamNetworkingUtils()->InitRelayNetworkAccess();

  // SteamGameServer()->SetAdvertiseServerActive(true);

  server->listen_socket =
      SteamGameServerNetworkingSockets()->CreateListenSocketP2P(0, 0, nullptr);
  server->poll_group = SteamGameServerNetworkingSockets()->CreatePollGroup();
}

CALLCONV_C(void) server_deinit(void *_ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;
  ctx->server.initialized = false;

  SteamGameServerNetworkingSockets()->CloseListenSocket(
      ctx->server.listen_socket);
  SteamGameServerNetworkingSockets()->DestroyPollGroup(ctx->server.poll_group);
  ctx->server.listen_socket = 0;
  ctx->server.poll_group = 0;

  SteamGameServer()->LogOff();

  SteamGameServer_Shutdown();
}

static void server_callback_tick(ZhottSteamContext *ctx) {
  // TODO: server gotta listen to SteamNetConnectionStatusChangedCallback_t
  // for accepting or rejecting connections
  // EResult res = SteamGameServerNetworkingSockets()->AcceptConnection( hConn
  // ); if ( res != k_EResultOK )
  // SteamGameServerNetworkingSockets()->CloseConnection( hConn,
  // k_ESteamNetConnectionEnd_AppException_Generic, "Failed to accept
  // connection", false );
  // SteamGameServerNetworkingSockets()->SetConnectionPollGroup(hConn,
  // m_hNetPollGroup);

  HSteamPipe hSteamPipe = SteamGameServer_GetHSteamPipe();
  SteamAPI_ManualDispatch_RunFrame(hSteamPipe);
  CallbackMsg_t callback;
  while (SteamAPI_ManualDispatch_GetNextCallback(hSteamPipe, &callback)) {
    switch (callback.m_iCallback) {
    case SteamNetConnectionStatusChangedCallback_t::k_iCallback: {
      auto event =
          (SteamNetConnectionStatusChangedCallback_t *)callback.m_pubParam;
    } break;
    default: {
    } break;
    }

    SteamAPI_ManualDispatch_FreeLastCallback(hSteamPipe);
  }
}

static void server_socket_tick(ZhottSteamContext *ctx) {
  SteamNetworkingMessage_t *msgs[128];
  int numMessages =
      SteamGameServerNetworkingSockets()->ReceiveMessagesOnPollGroup(
          ctx->server.poll_group, msgs, 128);
  for (int idxMsg = 0; idxMsg < numMessages; idxMsg++) {
    SteamNetworkingMessage_t *message = msgs[idxMsg];

    message->Release();
    message = nullptr;
  }

  if (numMessages > 0) {
    printf("recved %d messages\n", numMessages);
  }
}

CALLCONV_C(void) server_tick(void *_ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;

  // SteamGameServer_RunCallbacks();
  server_callback_tick(ctx);
  server_socket_tick(ctx);
}

CALLCONV_C(void) client_init(void *_ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;
  ctx->client = {
      .gamedir = "zhottem",
      .lobby_password = "zhottpass",
      .lobby_request = k_uAPICallInvalid,
      .lobby_created = k_uAPICallInvalid,
      .lobby_id = CSteamID(),
      .server_conn = k_HSteamNetConnection_Invalid,
  };
  SteamMatchmaking()->AddRequestLobbyListStringFilter(
      "zhott_password", ctx->client.lobby_password, k_ELobbyComparisonEqual);
  ctx->client.lobby_request = SteamMatchmaking()->RequestLobbyList();

  ctx->client.initialized = true;

  SteamNetworkingUtils()->InitRelayNetworkAccess();
}

CALLCONV_C(void) client_deinit(void *_ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;
  ctx->client.initialized = false;
}

static void client_callback_tick(ZhottSteamContext *ctx) {
  // TODO: client gotta listen to SteamNetConnectionStatusChangedCallback_t
  // to know if server accepted or rejected the connection
  // SteamNetConnectionStatusChangedCallback_t t;

  HSteamPipe hSteamPipe = SteamAPI_GetHSteamPipe();
  SteamAPI_ManualDispatch_RunFrame(hSteamPipe);
  CallbackMsg_t callback;
  while (SteamAPI_ManualDispatch_GetNextCallback(hSteamPipe, &callback)) {
    switch (callback.m_iCallback) {
    case LobbyMatchList_t::k_iCallback: {
      auto event = (LobbyMatchList_t *)callback.m_pubParam;

      bool found = false;
      for (int i = 0; i < event->m_nLobbiesMatching; i++) {
        auto lobby = SteamMatchmaking()->GetLobbyByIndex(i);
        SteamMatchmaking()->JoinLobby(lobby);
        found = true;
        break;
      }

      if (!found) {
        ctx->client.lobby_created =
            SteamMatchmaking()->CreateLobby(k_ELobbyTypePublic, 4);
      }
    } break;
    case LobbyCreated_t::k_iCallback: {
      auto event = (LobbyCreated_t *)callback.m_pubParam;
      ctx->client.lobby_id = event->m_ulSteamIDLobby;
      printf("lobby created: %llu\n", event->m_ulSteamIDLobby);

      SteamMatchmaking()->SetLobbyGameServer(ctx->client.lobby_id, 0, 0,
                                             SteamGameServer()->GetSteamID());
      SteamMatchmaking()->SetLobbyData(ctx->client.lobby_id, "name",
                                       "zhottem lobby lesgoo");
      SteamMatchmaking()->SetLobbyData(ctx->client.lobby_id, "zhott_password",
                                       ctx->client.lobby_password);
    } break;
    case LobbyEnter_t::k_iCallback: {
      auto event = (LobbyEnter_t *)callback.m_pubParam;
      if (event->m_EChatRoomEnterResponse == k_EChatRoomEnterResponseSuccess) {
        printf("lobby joined successfully\n");

        CSteamID server_id = CSteamID();
        if (SteamMatchmaking()->GetLobbyGameServer(event->m_ulSteamIDLobby,
                                                   NULL, NULL, &server_id)) {
          SteamNetworkingIdentity identity;
          identity.SetSteamID(server_id);
          ctx->client.server_conn =
              SteamNetworkingSockets()->ConnectP2P(identity, 0, 0, nullptr);
        }

        auto num =
            SteamMatchmaking()->GetNumLobbyMembers(event->m_ulSteamIDLobby);
        for (int i = 0; i < num; i++) {
          printf("lobby member %d %llu\n", i,
                 SteamMatchmaking()
                     ->GetLobbyMemberByIndex(event->m_ulSteamIDLobby, i)
                     .ConvertToUint64());
        }
      } else {
        printf("can't join lobby %d\n", event->m_EChatRoomEnterResponse);
      }
    } break;
    case LobbyDataUpdate_t::k_iCallback: {
      auto event = (LobbyDataUpdate_t *)callback.m_pubParam;
      auto lobby = event->m_ulSteamIDLobby;
      auto num = SteamMatchmaking()->GetLobbyDataCount(lobby);
      for (int i = 0; i < num; i++) {
        char key[256];
        char val[256];
        if (SteamMatchmaking()->GetLobbyDataByIndex(lobby, i, key, 256, val,
                                                    256)) {
          printf("lobby %llu data %d %s: %s\n", lobby, i, key, val);
        }
      }
    } break;
    case SteamAPICallCompleted_t::k_iCallback: {
      // auto pCallCompleted = (SteamAPICallCompleted_t *)callback.m_pubParam;
      // void *pTmpCallResult = zalloc(callback.m_cubParam);
      // bool bFailed;
      // if (SteamAPI_ManualDispatch_GetAPICallResult(
      //         hSteamPipe, pCallCompleted->m_hAsyncCall, pTmpCallResult,
      //         callback.m_cubParam, callback.m_iCallback, &bFailed)) {
      //   // Dispatch the call result to the registered handler(s) for the
      //   // call identified by pCallCompleted->m_hAsyncCall
      //   printf("manual dispatch %d %llu\n", pCallCompleted->m_iCallback,
      //          pCallCompleted->m_hAsyncCall);
      // }
      // zfree(pTmpCallResult);
    } break;
    default: {
    } break;
    }

    SteamAPI_ManualDispatch_FreeLastCallback(hSteamPipe);
  }
}

static void client_socket_tick(ZhottSteamContext *ctx) {
  if (!SteamNetworkingSockets())
    return;
  if (ctx->client.server_conn == k_HSteamNetConnection_Invalid)
    return;

  SteamNetworkingMessage_t *msgs[32];
  int res = SteamNetworkingSockets()->ReceiveMessagesOnConnection(
      ctx->client.server_conn, msgs, 32);
  for (int i = 0; i < res; i++) {
    SteamNetworkingMessage_t *message = msgs[i];

    message->Release();
    message = nullptr;
  }
}

CALLCONV_C(void) client_tick(void *_ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;

  // SteamAPI_RunCallbacks();
  client_callback_tick(ctx);
  client_socket_tick(ctx);
}
