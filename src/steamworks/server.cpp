
#include "steamworks/impl.h"

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
