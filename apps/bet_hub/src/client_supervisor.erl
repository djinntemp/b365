%% @author  Jakub Kubiak <king@browarnicki.pl>
%% @copyright 2015  Jakub Kubiak.
%% @doc simple_one_for_one supervisor for TCP/UDP clients 

-module(client_supervisor).

-behaviour(supervisor).

-export([
         start_link/0,
         start_client/0
       ]).
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
start_client() ->
    error_logger:info_msg("~p: Starting client", [?MODULE]),
    supervisor:start_child(?MODULE, []).

    
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init(_Args) ->
    SupervisorFlags = {simple_one_for_one,
                       0,
                       1
                      },
    ChildSpecs = [{client_handler,       %id 
                    {client_handler, start_link, []}, %start
                    temporary,              %restart
                    brutal_kill,            %shutdown
                    worker,                 %type
                    [client_handler]    %modules
                  }],
    {ok, {SupervisorFlags, ChildSpecs}}.
