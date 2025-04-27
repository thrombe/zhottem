
#include <cstring>

#include "steamworks/impl.h"

CALLCONV_C(bool) server_init(ZhottSteamCtx _ctx, ServerCallbacks callbacks) {
  auto ctx = (ZhottSteamContext *)_ctx;
  ctx->server = {
      .version = "1.0.0.0",
      .moddir = "zhottem",
      .product = "zhottem",
      .description = "zhottem test game",
      .port = 27015,
      .updater_port = 27016,
      .callbacks = callbacks,
  };
  auto server = &ctx->server;

  SteamErrMsg errMsg = {0};
  if (SteamGameServer_InitEx(0, server->port, server->updater_port,
                             eServerModeAuthentication, server->version,
                             &errMsg) != k_ESteamAPIInitResult_OK) {
    printf("SteamGameServer_Init call failed: %s\n", errMsg);
    return false;
  }
  server->initialized = true;

  auto game_server = SteamAPI_SteamGameServer();
  SteamAPI_ISteamGameServer_SetModDir(game_server, server->moddir);
  SteamAPI_ISteamGameServer_SetProduct(game_server, server->product);
  SteamAPI_ISteamGameServer_SetGameDescription(game_server,
                                               server->description);

  SteamAPI_ISteamGameServer_SetPasswordProtected(game_server, false);

  // SteamAPI_ISteamGameServer_LogOn(game_server, "token?");
  SteamAPI_ISteamGameServer_LogOnAnonymous(game_server);

  auto net_utils = SteamAPI_SteamNetworkingUtils_SteamAPI();
  SteamAPI_ISteamNetworkingUtils_InitRelayNetworkAccess(net_utils);

  // SteamAPI_ISteamGameServer_SetAdvertiseServerActive(game_server, true);

  auto net_sockets = SteamAPI_SteamGameServerNetworkingSockets_SteamAPI();
  server->listen_socket =
      SteamAPI_ISteamNetworkingSockets_CreateListenSocketP2P(net_sockets, 0, 0,
                                                             nullptr);
  server->poll_group =
      SteamAPI_ISteamNetworkingSockets_CreatePollGroup(net_sockets);
  return true;
}

CALLCONV_C(void) server_deinit(ZhottSteamCtx _ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;
  ctx->server.initialized = false;

  auto net_sockets = SteamAPI_SteamGameServerNetworkingSockets_SteamAPI();
  auto _ = SteamAPI_ISteamNetworkingSockets_CloseListenSocket(
      net_sockets, ctx->server.listen_socket);
  _ = SteamAPI_ISteamNetworkingSockets_DestroyPollGroup(net_sockets,
                                                        ctx->server.poll_group);
  ctx->server.listen_socket = 0;
  ctx->server.poll_group = 0;

  auto game_server = SteamAPI_SteamGameServer();
  SteamAPI_ISteamGameServer_LogOff(game_server);

  SteamGameServer_Shutdown();
}

static void server_callback_tick(ZhottSteamContext *ctx) {
  auto net_sockets = SteamAPI_SteamGameServerNetworkingSockets_SteamAPI();

  auto hSteamPipe = SteamGameServer_GetHSteamPipe();
  SteamAPI_ManualDispatch_RunFrame(hSteamPipe);
  CallbackMsg_t callback;
  while (SteamAPI_ManualDispatch_GetNextCallback(hSteamPipe, &callback)) {
    printf("server callback event %d\n", callback.m_iCallback);
    switch (callback.m_iCallback) {
    case SteamNetAuthenticationStatus_t::k_iCallback: {
      auto event = (SteamNetAuthenticationStatus_t *)callback.m_pubParam;
      printf("server steam auth debug stat: %s %d\n", event->m_debugMsg,
             event->m_eAvail);

      // if (event->m_eAvail == k_ESteamNetworkingAvailability_Current) {
      //   SteamGameServer()->SetMaxPlayerCount(4);
      //   SteamGameServer()->SetPasswordProtected(false);
      //   SteamGameServer()->SetServerName("zhottem server");
      //   SteamGameServer()->SetBotPlayerCount(0); // optional, default 0
      //   SteamGameServer()->SetMapName("testmap");
      // }
    } break;
    case SteamRelayNetworkStatus_t::k_iCallback: {
      auto event = (SteamRelayNetworkStatus_t *)callback.m_pubParam;
      printf("server steam relay debug stat: %s %d\n", event->m_debugMsg,
             event->m_eAvail);
    } break;
    case SteamNetConnectionStatusChangedCallback_t::k_iCallback: {
      printf("server connection callback\n");
      auto event =
          (SteamNetConnectionStatusChangedCallback_t *)callback.m_pubParam;
      if (event->m_info.m_hListenSocket &&
          event->m_eOldState == k_ESteamNetworkingConnectionState_None &&
          event->m_info.m_eState ==
              k_ESteamNetworkingConnectionState_Connecting) {
        // received new connection
        printf("received connection\n");
        auto res = SteamAPI_ISteamNetworkingSockets_AcceptConnection(
            net_sockets, event->m_hConn);
        if (res == k_EResultOK) {
          if (!SteamAPI_ISteamNetworkingSockets_SetConnectionPollGroup(
                  net_sockets, event->m_hConn, ctx->server.poll_group)) {
            printf("!!!connection added to invalid poll group\n");
          }
        } else {
          auto _ = SteamAPI_ISteamNetworkingSockets_CloseConnection(
              net_sockets, event->m_hConn,
              k_ESteamNetConnectionEnd_AppException_Generic, NULL, false);
        }
      } else if ((event->m_eOldState ==
                      k_ESteamNetworkingConnectionState_Connecting ||
                  event->m_eOldState ==
                      k_ESteamNetworkingConnectionState_Connected) &&
                 event->m_info.m_eState ==
                     k_ESteamNetworkingConnectionState_ProblemDetectedLocally) {
        // closed by localhost
        printf("closed connection by localhost\n");
        auto _ = SteamAPI_ISteamNetworkingSockets_CloseConnection(
            net_sockets, event->m_hConn,
            k_ESteamNetConnectionEnd_AppException_Generic, NULL, false);
      }
    } break;
    case GSPolicyResponse_t::k_iCallback: {
      auto event = (GSPolicyResponse_t *)callback.m_pubParam;
      printf("server policy resp: %d\n", event->m_bSecure);
    } break;
    case GSClientDeny_t::k_iCallback: {
      auto event = (GSClientDeny_t *)callback.m_pubParam;
      printf("denied client connect: %s %d\n", event->m_rgchOptionalText,
             event->m_eDenyReason);
    } break;
    case GSClientApprove_t::k_iCallback: {
      auto event = (GSClientApprove_t *)callback.m_pubParam;
      printf("approved client connect\n");
    } break;
    case SteamAPICallCompleted_t::k_iCallback: {
      auto pCallCompleted = (SteamAPICallCompleted_t *)callback.m_pubParam;
      void *pTmpCallResult = zalloc(callback.m_cubParam);
      bool bFailed;
      if (SteamAPI_ManualDispatch_GetAPICallResult(
              hSteamPipe, pCallCompleted->m_hAsyncCall, pTmpCallResult,
              callback.m_cubParam, callback.m_iCallback, &bFailed)) {
        // Dispatch the call result to the registered handler(s) for the
        // call identified by pCallCompleted->m_hAsyncCall
        printf("manual dispatch %d %llu\n", pCallCompleted->m_iCallback,
               pCallCompleted->m_hAsyncCall);
      }
      zfree(pTmpCallResult);
    } break;
    default: {
    } break;
    }

    SteamAPI_ManualDispatch_FreeLastCallback(hSteamPipe);
  }
}

static void server_socket_tick(ZhottSteamContext *ctx) {
  auto net_sockets = SteamAPI_SteamGameServerNetworkingSockets_SteamAPI();
  SteamNetworkingMessage_t *msgs[128];
  int numMessages = SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnPollGroup(
      net_sockets, ctx->server.poll_group, msgs, 128);
  for (int idxMsg = 0; idxMsg < numMessages; idxMsg++) {
    SteamNetworkingMessage_t *message = msgs[idxMsg];

    auto steam_id =
        SteamAPI_SteamNetworkingIdentity_GetSteamID(&message->m_identityPeer);
    auto callbacks = ctx->server.callbacks;
    callbacks.msg_recv(callbacks.ctx,
                       NetworkMessage{
                           .data = (uint8_t *)message->m_pData,
                           .len = (uint32_t)message->m_cbSize,
                           .conn = message->m_conn,
                           .user_steam_id = steam_id,
                           .message_number = message->m_nMessageNumber,
                       });

    SteamAPI_SteamNetworkingMessage_t_Release(message);
    message = nullptr;
  }
}

CALLCONV_C(void) server_tick(ZhottSteamCtx _ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;

  // SteamGameServer_RunCallbacks();
  server_callback_tick(ctx);
  server_socket_tick(ctx);
}

CALLCONV_C(void)
server_msg_send(ZhottSteamCtx _ctx, uint32_t conn, uint32_t from_conn,
                OutgoingMessage msg) {
  auto ctx = (ZhottSteamContext *)_ctx;

  uint32_t flags = 0;
  if (msg.flags.reliable) {
    flags = k_nSteamNetworkingSend_Reliable;
  } else {
    flags = k_nSteamNetworkingSend_Unreliable;
  }
  if (msg.flags.force_flush) {
    flags |= k_nSteamNetworkingSend_NoNagle;
  }
  if (msg.flags.no_delay) {
    flags |= k_nSteamNetworkingSend_NoDelay;
  }
  auto buf = (uint8_t *)zalloc(sizeof(ClientMessageHeader) + msg.len);
  *((ClientMessageHeader *)buf) = ClientMessageHeader{
      .conn = from_conn,
      .user_steam_id = 0,
      .message_number = 0,
  };
  memcpy(buf + sizeof(ClientMessageHeader), msg.data, msg.len);

  auto net_sockets = SteamAPI_SteamGameServerNetworkingSockets_SteamAPI();
  auto _ = SteamAPI_ISteamNetworkingSockets_SendMessageToConnection(
      net_sockets, conn, buf, sizeof(ClientMessageHeader) + msg.len, flags,
      NULL);

  zfree(buf);
}

CALLCONV_C(void) server_pre_reload(ZhottSteamCtx _ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;
  ctx->server.callbacks = {};
}

CALLCONV_C(void)
server_post_reload(ZhottSteamCtx _ctx, ServerCallbacks callbacks) {
  auto ctx = (ZhottSteamContext *)_ctx;
  ctx->server.callbacks = callbacks;
}
