-module(bet_hub_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    error_logger:info_msg("Starting ~p application", [?MODULE]),
    bet_hub_sup:start_link().

stop(_State) ->
    ok.
