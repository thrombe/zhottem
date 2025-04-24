
#include "steamworks/impl.h"

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

  auto client = SteamClient();
  SteamAPI_ISteamClient_SetWarningMessageHook(client, &steam_api_dbg_hook);

  auto user = SteamAPI_SteamUser();
  if (!SteamAPI_ISteamUser_BLoggedOn(user)) {
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
  *ctx = {};
  return ctx;
}

CALLCONV_C(void) steam_deinit(void *ctx) {
  SteamAPI_Shutdown();
  zfree(ctx);
  return;
}
