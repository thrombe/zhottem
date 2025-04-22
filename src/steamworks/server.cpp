
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
                             eServerModeAuthenticationAndSecure,
                             server->version,
                             &errMsg) != k_ESteamAPIInitResult_OK) {
    printf("SteamGameServer_Init call failed: %s\n", errMsg);
    return false;
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
  return true;
}

CALLCONV_C(void) server_deinit(ZhottSteamCtx _ctx) {
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
  HSteamPipe hSteamPipe = SteamGameServer_GetHSteamPipe();
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
        auto res = SteamGameServerNetworkingSockets()->AcceptConnection(
            event->m_hConn);
        if (res == k_EResultOK) {
          if (!SteamGameServerNetworkingSockets()->SetConnectionPollGroup(
                  event->m_hConn, ctx->server.poll_group)) {
            printf("!!!connection added to invalid poll group\n");
          }
        } else {
          auto _ = SteamGameServerNetworkingSockets()->CloseConnection(
              event->m_hConn, k_ESteamNetConnectionEnd_AppException_Generic,
              NULL, false);
        }
      } else if ((event->m_eOldState ==
                      k_ESteamNetworkingConnectionState_Connecting ||
                  event->m_eOldState ==
                      k_ESteamNetworkingConnectionState_Connected) &&
                 event->m_info.m_eState ==
                     k_ESteamNetworkingConnectionState_ProblemDetectedLocally) {
        // closed by localhost
        printf("closed connection by localhost\n");
        auto _ = SteamGameServerNetworkingSockets()->CloseConnection(
            event->m_hConn, k_ESteamNetConnectionEnd_AppException_Generic, NULL,
            false);
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
      printf("approved client connect: %llu\n",
             event->m_SteamID.ConvertToUint64());
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

    auto callbacks = ctx->server.callbacks;
    callbacks.msg_recv(
        callbacks.ctx,
        NetworkMessage{
            .data = (uint8_t *)message->GetData(),
            .len = message->GetSize(),
            .conn = message->m_conn,
            .user_steam_id =
                message->m_identityPeer.GetSteamID().ConvertToUint64(),
            .message_number = message->m_nMessageNumber,
        });

    message->Release();
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
server_msg_send(ZhottSteamCtx _ctx, uint32_t conn, OutgoingMessage msg) {
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
  auto buf = (uint8_t*)zalloc(sizeof(ClientMessageHeader) + msg.len);
  *((ClientMessageHeader*)buf) = ClientMessageHeader{
      .conn = conn,
      .user_steam_id = 0,
      .message_number = 0,
  };
  memcpy(buf + sizeof(ClientMessageHeader), msg.data, msg.len);

  auto _ = SteamGameServerNetworkingSockets()->SendMessageToConnection(
      conn, buf, sizeof(ClientMessageHeader) + msg.len, flags, NULL);

  zfree(buf);
}
