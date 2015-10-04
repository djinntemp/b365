%% @author  Jakub Kubiak <king@browarnicki.pl>
%% @copyright 2015  Jakub Kubiak.
%% @doc 

-module(network_listener).
-behaviour(gen_server).

-author('Jakub Kubiak <king@browarnicki.pl>').


%% ===================================================================
%% Public API
%% ===================================================================
-export([start_link/1]).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================
-export([init/1,
         stop/1,
         stop/0,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

%% ===================================================================
%% Public API
%% ===================================================================
%% @spec start_link({Type::atom(), Port::integer()}) -> {ok, Pid}
%% @doc Starts network listener. Type indicates protocol: 'tcp' or 'udp'.
start_link({Type, Port}) ->
    gen_server:start_link(?MODULE, {Type, Port}, []).


    
%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init({udp, Port}) ->
    error_logger:info_msg("Starting ~p module. Protocol: udp, Port: ~p",
                          [?MODULE, Port]),
                          
    {ok, UdpSocket} = gen_udp:open(Port, [binary, {active,true}]),
    {ok, UdpSocket};

init({tcp, Port}) ->
    error_logger:info_msg("Starting ~p module. Protocol: tcp, Port: ~p",
                          [?MODULE, Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active,once}, {packet,line}, {reuseaddr, true}]),
    gen_server:cast(self(), accept),
    {ok, ListenSocket}.

stop(_Pid) ->
    stop().
stop() ->
    gen_server:cast(self(), stop).


    
handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_cast(accept, ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    error_logger:info_msg("New Client has connected: ~p", [ClientSocket]),
    {ok, Pid} = tcp_client_supervisor:start_client(),
    gen_tcp:controlling_process(ClientSocket, Pid),
    error_logger:info_msg("New Client transfered to tcp_client_handler", [ClientSocket]),
    tcp_client_handler:set_socket(Pid, ClientSocket),
    gen_server:cast(self(), accept),
    {noreply, ListenSocket};
    
handle_cast(stop, State) ->
    {stop, normal, shutdown_ok, State};
handle_cast(Message, State) ->
    error_logger:info_msg("Cast message received: ~p", [Message]),
    {noreply, State}.
    
handle_info({udp, _Socket, _Ip, _Port, Data} = Message, State) ->
    error_logger:info_msg("UDP: incomming data: ~p", [Data]),
    udp_client_handler:handle_message(Message),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.



%% ===================================================================
%% private functions
%% ===================================================================

