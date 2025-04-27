
#include "steamworks/impl.h"

CALLCONV_C(void) client_init(ZhottSteamCtx _ctx, ClientCallbacks callbacks) {
  auto ctx = (ZhottSteamContext *)_ctx;
  ctx->client = {
      .gamedir = "zhottem",
      .lobby_password = "zhottpass",
      .lobby_request = k_uAPICallInvalid,
      .lobby_created = k_uAPICallInvalid,
      .lobby_id = 0,
      .server_conn = k_HSteamNetConnection_Invalid,
      .callbacks = callbacks,
  };

  auto matchmaking = SteamAPI_SteamMatchmaking();
  SteamAPI_ISteamMatchmaking_AddRequestLobbyListStringFilter(
      matchmaking, "zhott_password", ctx->client.lobby_password,
      k_ELobbyComparisonEqual);

  ctx->client.initialized = true;

  auto net_utils = SteamAPI_SteamNetworkingUtils_SteamAPI();
  SteamAPI_ISteamNetworkingUtils_InitRelayNetworkAccess(net_utils);
}

CALLCONV_C(void) client_deinit(ZhottSteamCtx _ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;
  ctx->client.initialized = false;

  if (ctx->client.lobby_id) {
    auto matchmaking = SteamAPI_SteamMatchmaking();
    SteamAPI_ISteamMatchmaking_LeaveLobby(matchmaking, ctx->client.lobby_id);
  }

  if (ctx->client.server_conn) {
    auto net_sockets = SteamAPI_SteamNetworkingSockets_SteamAPI();
    auto _ = SteamAPI_ISteamNetworkingSockets_CloseConnection(
        net_sockets, ctx->client.server_conn,
        k_ESteamNetConnectionEnd_AppException_Generic, NULL, false);
  }
}

static void client_callback_tick(ZhottSteamContext *ctx) {
  auto matchmaking = SteamAPI_SteamMatchmaking();
  auto gameserver = SteamAPI_SteamGameServer();
  auto net_sockets = SteamAPI_SteamNetworkingSockets_SteamAPI();

  auto hSteamPipe = SteamAPI_GetHSteamPipe();
  SteamAPI_ManualDispatch_RunFrame(hSteamPipe);

  CallbackMsg_t callback;
  while (SteamAPI_ManualDispatch_GetNextCallback(hSteamPipe, &callback)) {
    switch (callback.m_iCallback) {
    case LobbyMatchList_t::k_iCallback: {
      auto event = (LobbyMatchList_t *)callback.m_pubParam;

      bool found = false;
      for (int i = 0; i < event->m_nLobbiesMatching; i++) {
        auto lobby = SteamAPI_ISteamMatchmaking_GetLobbyByIndex(matchmaking, i);
        auto _ = SteamAPI_ISteamMatchmaking_JoinLobby(matchmaking, lobby);
        found = true;
        break;
      }

      if (!found) {
        ctx->client.lobby_created = SteamAPI_ISteamMatchmaking_CreateLobby(
            matchmaking, k_ELobbyTypePublic, 4);
      }
    } break;
    case LobbyCreated_t::k_iCallback: {
      auto event = (LobbyCreated_t *)callback.m_pubParam;
      ctx->client.lobby_id = event->m_ulSteamIDLobby;
      printf("lobby created: %llu\n", event->m_ulSteamIDLobby);

      auto server_id = SteamAPI_ISteamGameServer_GetSteamID(gameserver);
      SteamAPI_ISteamMatchmaking_SetLobbyGameServer(
          matchmaking, ctx->client.lobby_id, 0, 0, server_id);
      auto _ = SteamAPI_ISteamMatchmaking_SetLobbyData(
          matchmaking, ctx->client.lobby_id, "name", "zhottem lobby lesgoo");
      _ = SteamAPI_ISteamMatchmaking_SetLobbyData(
          matchmaking, ctx->client.lobby_id, "zhott_password",
          ctx->client.lobby_password);
    } break;
    case LobbyEnter_t::k_iCallback: {
      auto event = (LobbyEnter_t *)callback.m_pubParam;
      if (event->m_EChatRoomEnterResponse == k_EChatRoomEnterResponseSuccess) {
        printf("lobby joined successfully\n");

        uint64_steamid server_id = 0;
        if (SteamAPI_ISteamMatchmaking_GetLobbyGameServer(
                matchmaking, event->m_ulSteamIDLobby, NULL, NULL,
                (CSteamID *)&server_id)) {
          SteamNetworkingIdentity identity = {};
          SteamAPI_SteamNetworkingIdentity_Clear(&identity);
          SteamAPI_SteamNetworkingIdentity_SetSteamID(&identity, server_id);

          printf("trying to connect to server\n");
          ctx->client.server_conn = SteamAPI_ISteamNetworkingSockets_ConnectP2P(
              net_sockets, identity, 0, 0, NULL);
        }

        auto num = SteamAPI_ISteamMatchmaking_GetNumLobbyMembers(
            matchmaking, event->m_ulSteamIDLobby);
        for (int i = 0; i < num; i++) {
          auto lobby = SteamAPI_ISteamMatchmaking_GetLobbyMemberByIndex(
              matchmaking, event->m_ulSteamIDLobby, i);
          printf("lobby member %d %llu\n", i, lobby);
        }
      } else {
        printf("can't join lobby %d\n", event->m_EChatRoomEnterResponse);
      }
    } break;
    case LobbyDataUpdate_t::k_iCallback: {
      auto event = (LobbyDataUpdate_t *)callback.m_pubParam;
      auto lobby = event->m_ulSteamIDLobby;
      auto num =
          SteamAPI_ISteamMatchmaking_GetLobbyDataCount(matchmaking, lobby);

      for (int i = 0; i < num; i++) {
        char key[256];
        char val[256];
        if (SteamAPI_ISteamMatchmaking_GetLobbyDataByIndex(
                matchmaking, lobby, i, key, 256, val, 256)) {
          printf("lobby %llu data %d %s: %s\n", lobby, i, key, val);
        }
      }
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
    case SteamNetAuthenticationStatus_t::k_iCallback: {
      auto event = (SteamNetAuthenticationStatus_t *)callback.m_pubParam;
      printf("client steam auth debug stat: %s %d\n", event->m_debugMsg,
             event->m_eAvail);
    } break;
    case SteamRelayNetworkStatus_t::k_iCallback: {
      auto event = (SteamRelayNetworkStatus_t *)callback.m_pubParam;
      printf("client steam relay debug stat: %s %d\n", event->m_debugMsg,
             event->m_eAvail);
      if (event->m_eAvail == k_ESteamNetworkingAvailability_Current) {
        printf("lobby request made from client\n");
        ctx->client.lobby_request =
            SteamAPI_ISteamMatchmaking_RequestLobbyList(matchmaking);
      }
    } break;
    case SteamNetConnectionStatusChangedCallback_t::k_iCallback: {
      auto event =
          (SteamNetConnectionStatusChangedCallback_t *)callback.m_pubParam;
      printf("client connection callback old: %d new: %d\n", event->m_eOldState,
             event->m_info.m_eState);
      if ((event->m_eOldState == k_ESteamNetworkingConnectionState_Connecting ||
           event->m_eOldState ==
               k_ESteamNetworkingConnectionState_FindingRoute) &&
          event->m_info.m_eState ==
              k_ESteamNetworkingConnectionState_Connected) {
        // connected successfully to remote
        printf("successfully connected to remote\n");

        ctx->client.connected = true;
      } else if (event->m_eOldState ==
                     k_ESteamNetworkingConnectionState_Connected &&
                 event->m_info.m_eState ==
                     k_ESteamNetworkingConnectionState_ClosedByPeer) {
        // rejected/closed by remote
        printf("rejected/closed connection by remote\n");
        auto _ = SteamAPI_ISteamNetworkingSockets_CloseConnection(
            net_sockets, event->m_hConn,
            k_ESteamNetConnectionEnd_AppException_Generic, NULL, false);
      } else if ((event->m_eOldState ==
                      k_ESteamNetworkingConnectionState_Connecting ||
                  event->m_eOldState ==
                      k_ESteamNetworkingConnectionState_Connected) &&
                 event->m_info.m_eState ==
                     k_ESteamNetworkingConnectionState_ProblemDetectedLocally) {
        // closed by localhost
        printf("rejected/closed connection by localhost\n");
        auto _ = SteamAPI_ISteamNetworkingSockets_CloseConnection(
            net_sockets, event->m_hConn,
            k_ESteamNetConnectionEnd_AppException_Generic, NULL, false);

        uint64_steamid server_id = 0;
        _ = SteamAPI_ISteamMatchmaking_GetLobbyGameServer(
            matchmaking, ctx->client.lobby_id, NULL, NULL,
            (CSteamID *)&server_id);

        auto gameserver_steamid =
            SteamAPI_ISteamGameServer_GetSteamID(gameserver);
        printf("server ids %llu %llu\n", server_id, gameserver_steamid);

        SteamNetworkingIdentity identity;
        SteamAPI_SteamNetworkingIdentity_Clear(&identity);
        SteamAPI_SteamNetworkingIdentity_SetSteamID(&identity, server_id);

        printf("trying to connect to server\n");
        ctx->client.server_conn = SteamAPI_ISteamNetworkingSockets_ConnectP2P(
            net_sockets, identity, 0, 0, NULL);
      }
    } break;
    default: {
    } break;
    }

    SteamAPI_ManualDispatch_FreeLastCallback(hSteamPipe);
  }
}

static void client_socket_tick(ZhottSteamContext *ctx) {
  auto net_sockets = SteamAPI_SteamNetworkingSockets_SteamAPI();
  if (!net_sockets)
    return;
  if (ctx->client.server_conn == k_HSteamNetConnection_Invalid)
    return;

  SteamNetworkingMessage_t *msgs[32];
  int res = SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnConnection(
      net_sockets, ctx->client.server_conn, msgs, 32);
  for (int i = 0; i < res; i++) {
    SteamNetworkingMessage_t *message = msgs[i];

    uint8_t *buf = (uint8_t *)message->m_pData;
    auto header = (ClientMessageHeader *)buf;

    auto callbacks = ctx->client.callbacks;
    callbacks.msg_recv(callbacks.ctx,
                       NetworkMessage{
                           .data = buf + sizeof(ClientMessageHeader),
                           .len = (uint32_t)message->m_cbSize -
                                  (uint32_t)sizeof(ClientMessageHeader),
                           .conn = header->conn,
                           .user_steam_id = header->user_steam_id,
                           .message_number = header->message_number,
                       });

    SteamAPI_SteamNetworkingMessage_t_Release(message);
    message = nullptr;
  }
}

CALLCONV_C(void) client_tick(ZhottSteamCtx _ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;

  // SteamAPI_RunCallbacks();
  client_callback_tick(ctx);
  client_socket_tick(ctx);
}

CALLCONV_C(void) client_msg_send(ZhottSteamCtx _ctx, OutgoingMessage msg) {
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
  auto net_sockets = SteamAPI_SteamNetworkingSockets_SteamAPI();
  auto _ = SteamAPI_ISteamNetworkingSockets_SendMessageToConnection(
      net_sockets, ctx->client.server_conn, msg.data, msg.len, flags, NULL);
}

CALLCONV_C(bool) client_is_connected(ZhottSteamCtx _ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;
  return ctx->client.connected;
}

CALLCONV_C(void) client_pre_reload(ZhottSteamCtx _ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;
  ctx->client.callbacks = {};
}

CALLCONV_C(void)
client_post_reload(ZhottSteamCtx _ctx, ClientCallbacks callbacks) {
  auto ctx = (ZhottSteamContext *)_ctx;
  ctx->client.callbacks = callbacks;
}
