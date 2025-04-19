
#include "steamworks/impl.h"

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

  ctx->client.initialized = true;

  SteamNetworkingUtils()->InitRelayNetworkAccess();
}

CALLCONV_C(void) client_deinit(void *_ctx) {
  auto ctx = (ZhottSteamContext *)_ctx;
  ctx->client.initialized = false;
}

static void client_callback_tick(ZhottSteamContext *ctx) {
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
          printf("trying to connect to server\n");
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
        ctx->client.lobby_request = SteamMatchmaking()->RequestLobbyList();
      }
    } break;
    case SteamNetConnectionStatusChangedCallback_t::k_iCallback: {
      auto event =
          (SteamNetConnectionStatusChangedCallback_t *)callback.m_pubParam;
      printf("client connection callback old: %d new: %d\n", event->m_eOldState,
             event->m_info.m_eState);
      if (event->m_eOldState == k_ESteamNetworkingConnectionState_Connecting &&
          event->m_info.m_eState ==
              k_ESteamNetworkingConnectionState_Connected) {
        // connected successfully to remote
        printf("successfully connected to remote\n");
      } else if (event->m_eOldState ==
                     k_ESteamNetworkingConnectionState_Connected &&
                 event->m_info.m_eState ==
                     k_ESteamNetworkingConnectionState_ClosedByPeer) {
        // rejected/closed by remote
        printf("rejected/closed connection by remote\n");
        auto _ = SteamNetworkingSockets()->CloseConnection(
            event->m_hConn, k_ESteamNetConnectionEnd_AppException_Generic, NULL,
            false);
      } else if ((event->m_eOldState ==
                      k_ESteamNetworkingConnectionState_Connecting ||
                  event->m_eOldState ==
                      k_ESteamNetworkingConnectionState_Connected) &&
                 event->m_info.m_eState ==
                     k_ESteamNetworkingConnectionState_ProblemDetectedLocally) {
        // closed by localhost
        printf("rejected/closed connection by localhost\n");
        auto _ = SteamNetworkingSockets()->CloseConnection(
            event->m_hConn, k_ESteamNetConnectionEnd_AppException_Generic, NULL,
            false);

        auto server_id = CSteamID();
        SteamMatchmaking()->GetLobbyGameServer(ctx->client.lobby_id, NULL, NULL,
                                               &server_id);
        printf("server ids %llu %llu\n", server_id.ConvertToUint64(),
               SteamGameServer()->GetSteamID().ConvertToUint64());
        SteamNetworkingIdentity identity;
        identity.SetSteamID(server_id);
        printf("trying to connect to server\n");
        ctx->client.server_conn =
            SteamNetworkingSockets()->ConnectP2P(identity, 0, 0, nullptr);
      }
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

    printf("client recved: %.*s\n", message->GetSize(),
           (char *)message->GetData());
    auto pong = "pong";
    SteamNetworkingSockets()->SendMessageToConnection(
        ctx->client.server_conn, pong, strlen(pong),
        k_nSteamNetworkingSend_Reliable, NULL);

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
