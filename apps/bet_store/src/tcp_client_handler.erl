%% @author  Jakub Kubiak <king@browarnicki.pl>
%% @copyright 2015  Jakub Kubiak.
%% @doc Handler for TCP protocol 

-module(tcp_client_handler).

-behaviour(gen_fsm).

-include_lib("tcp_client.hrl").

%% ===================================================================
%% Public API
%% ===================================================================
-export([start_link/0, set_socket/2]).


%% ===================================================================
%% gen_fsm callbacks
%% ===================================================================
-export([
         init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4
]).

%% ===================================================================
%% fsm states
%% ===================================================================
-export([
    state_socket_wait/2,
    state_data_wait/2
]).


-define(TIMEOUT, 120000).


%% ===================================================================
%% Public API
%% ===================================================================
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).
 
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).
    
    
%% ===================================================================
%% fsm states 
%% ===================================================================
   
%% process is waiting for socket
state_socket_wait({socket_ready, Socket}, State) when is_port(Socket) ->
    inet:setopts(Socket, [{active, once}, raw, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {ok, Data} = network_protocol_parser:parse(<<"help">>),
    ok = gen_tcp:send(Socket, Data),
    {next_state, state_data_wait, State#tcp_handler_state{socket=Socket, address=IP}, ?TIMEOUT};
state_socket_wait(Other, State) ->
    error_logger:error_msg("State: state_socket_wait. Unknown message: ~p", [Other]),
    {next_state, state_socket_wait, State}.
 
%% events coming from client
state_data_wait({data, Data}, #tcp_handler_state{socket=ClientSocket} = State) ->
    error_logger:info_msg("TCP: incomming data: (~p)", [Data]),
    {ok, Response} = network_protocol_parser:parse(Data),
    ok = gen_tcp:send(ClientSocket, Response),
    inet:setopts(ClientSocket, [{active, once}]),
    {next_state, state_data_wait, State, ?TIMEOUT};
 
state_data_wait(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout - closing.", [self()]),
    {stop, normal, State};
 
state_data_wait(Data, State) ->
    error_logger:info_msg("~p Ignoring data: ~p", [self(), Data]),
    {next_state, state_data_wait, State, ?TIMEOUT}.
    
    
    
    
%% ===================================================================
%% Callback functions 
%% ===================================================================
init([]) ->
    process_flag(trap_exit, true),
    {ok, state_socket_wait, #tcp_handler_state{}}.
    
%% @private 
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
    
    
%% @private   
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
    
    
%% @private    
handle_info({tcp, Socket, Bin}, StateName, #tcp_handler_state{socket=Socket} = StateData) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);
 
handle_info({tcp_closed, Socket}, _StateName,
            #tcp_handler_state{socket=Socket, address=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};
 
handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.
    
%% @private
terminate(_Reason, _StateName, #tcp_handler_state{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.

%% @private    
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.