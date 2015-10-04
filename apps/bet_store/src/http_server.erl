%% @author  Jakub Kubiak <king@browarnicki.pl>
%% @copyright 2015  Jakub Kubiak.
%% @doc 

-module(http_server).
-behaviour(gen_server).

-author('Jakub Kubiak <king@browarnicki.pl>').

-include_lib("http_server.hrl").


%% ===================================================================
%% Public API
%% ===================================================================
-export([start_link/1]).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         stop/0,
         stop/1]).

%% ===================================================================
%% Public API
%% ===================================================================
start_link(Config) ->
    {ok, Pid} = init_httpd_server(Config),
	error_logger:info_msg("HTTPD pid: ~p.", [Pid]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{httpd, Pid}], []).


    
%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([{httpd, Pid}]) ->
    State = {httpd, Pid},
    {ok, State}.

stop(_Pid) ->
    stop().

stop() ->
    gen_server:call(?MODULE, stop).

handle_call(stop, _From, State) ->
    {httpd, Pid} = State,
    ok = stop_httpd_server(Pid),
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
init_httpd_server(Config) ->
    error_logger:info_msg("Starting httpd with config: ~p", [Config]),
    
	{ok, Pid} = inets:start(httpd, [
		{port, Config#http_server_config.port}, 
		{ipfamily, inet},
		{modules, [mod_log, http_store_filter, http_rest_handler]},
		{server_name, Config#http_server_config.name},
		{server_root, Config#http_server_config.root_dir},
		{document_root, Config#http_server_config.document_dir},
		{log_format, combined},
		{error_log, Config#http_server_config.log_error},
		{transfer_log, Config#http_server_config.log_transfer},
		{security_log, Config#http_server_config.log_security},
		{server_tokens, {private, Config#http_server_config.name}},
		{keep_alive, false},
		{script_nocache, false}
	]),
	{ok, Pid}.
	
	
	
stop_httpd_server(Pid) ->
    inets:stop(httpd, Pid).