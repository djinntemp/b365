%% @author  Jakub Kubiak <king@browarnicki.pl>
%% @copyright 2015  Jakub Kubiak.
%% @doc Key value storage. 

-module(data_storage).
-behaviour(gen_server).

-author('Jakub Kubiak <king@browarnicki.pl>').

%% ===================================================================
%% Public API
%% ===================================================================
-export([
         start_link/0,
         insert/2,
         delete/1,
         update/2,
         select/1
]).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         stop/0,
         stop/1
]).


%% ===================================================================
%% Public API
%% ===================================================================
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
  
  
%% @spec insert(Key, Value) -> ok | conflict
%% @doc Inserts the Value object associated with given Key.
%% Returns 'ok' when succeeds.
%% If Key is already present 'conflict' atom will be returned. 
insert(Key, Value) ->
    gen_server:call(?MODULE, {insert, {Key, Value}}).
    

  
%% @spec delete(Key) -> ok | noval
%% @doc Deletes the Value object associated with given Key.
%% Returns 'ok' when succeeds.
%% If Key is not present 'noval' atom will be returned. 
delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

    
%% @spec update(Key, Value) -> ok | created
%% @doc Updates the Value object associated with given Key.
%% Returns 'ok' when succeeds.
%% If Key is not present new Value object will be inserted and 'created' atom will be returned. 
update(Key, Value) ->
    gen_server:call(?MODULE, {update, {Key, Value}}).
    
    
%% @spec select(Key) -> ok | noval
%% @doc Retrieves the Value object associated with given Key.
%% Returns {ok, Value} tuple when succeeds.
%% If Key is not present 'noval' atom will be returned. 
select(Key) ->
    gen_server:call(?MODULE, {select, Key}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([]) ->
    error_logger:info_msg("Starting ~p server", [?MODULE]),
    Options = [
        set,
        private
    ],
    State = ets:new(data_storage_table, Options),
    {ok, State}.

stop(_Pid) ->
    stop().

stop() ->
    gen_server:call(?MODULE, stop).

handle_call({insert, {Key, Value}}, _From, State) ->
    Response = tab_insert(State, {Key, Value}),
    {reply, Response, State};
    
handle_call({delete, Key}, _From, State) ->
    Response = tab_delete(State, Key),
    {reply, Response, State};
    
handle_call({update, {Key, Value}}, _From, State) ->
    Response = tab_update(State, {Key, Value}),
    {reply, Response, State};
    
handle_call({select, Key}, _From, State) ->
    Response = tab_select(State, Key),
    {reply, Response, State};

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};
    
handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.



%% ===================================================================
%% private functions
%% ===================================================================
tab_insert(Table, {Key, Value}) ->
    Result = ets:insert_new(Table, [{Key, Value}]),
    if 
        (Result =:= true) ->
            ok;
        true ->
            conflict
    end.
        
    
tab_delete(Table, Key) ->
    IsStored = ets:member(Table, Key),
    if 
        (IsStored =:= false) ->
            noval;
        true ->
            true = ets:delete(Table, Key),
            ok
    end.
    
tab_update(Table, {Key, Value}) ->
    IsStored = ets:member(Table, Key),
    true = ets:insert(Table, [{Key, Value}]),
    if 
        (IsStored =:= false) ->
            created;
        true ->
            ok
    end.
    
tab_select(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            noval;
        [{Key, Value}] ->
            {ok, Value}
    end.
