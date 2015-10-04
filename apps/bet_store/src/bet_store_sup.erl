-module(bet_store_sup).
-behaviour(supervisor).

-include_lib("http_server.hrl").

%% ===================================================================
%% Public API
%% ===================================================================
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macros for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Vars), {I, {I, start_link, Vars}, permanent, 5000, Type, [I]}).
-define(CHILD(Name, I, Type, Vars), {Name, {I, start_link, Vars}, permanent, 5000, Type, [I]}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    error_logger:info_msg("Starting ~p supervisor", [?MODULE]),
    HTTPConfig = get_http_server_config(),
    {ok, TCP_SERVER_PORT} = application:get_env(tcp_listener_port),
    {ok, UDP_SERVER_PORT} = application:get_env(udp_listener_port),
    
    DataStorage =   ?CHILD(data_storage, worker),
    TcpClientSup =  ?CHILD(tcp_client_supervisor, supervisor),
    TcpServer =     ?CHILD(tcp_server, network_listener, worker, [{tcp, TCP_SERVER_PORT}]),
    UdpServer =     ?CHILD(udp_server, network_listener, worker, [{udp, UDP_SERVER_PORT}]),
    HTTPServer =    ?CHILD(http_server, worker, [HTTPConfig]),
    {ok, { {one_for_one, 5, 10}, [
                                    DataStorage,
                                    TcpClientSup,
                                    TcpServer,
                                    UdpServer,
                                    HTTPServer
    ]} }.

    
%% ===================================================================
%% private functions
%% ===================================================================    
get_http_server_config() ->
    {ok, HTTP_SERVER_PORT} = application:get_env(http_server_port),
	{ok, HTTP_SERVER_NAME} = application:get_env(http_server_name),
	{ok, ApplicationVersion} = application:get_key(vsn),
	OTPVersion = erlang:system_info(otp_release),
	HTTP_SERVER_FULL_NAME = HTTP_SERVER_NAME ++ "/" ++ ApplicationVersion ++ " (unix/linux " ++ lists:flatten(io_lib:format("~p", [os:version()])) ++ ") OTP/" ++ OTPVersion,
	error_logger:info_msg("Getting env variables"),
	{ok, HTTP_SERVER_ROOT_DIR} = application:get_env(http_server_root_dir),
	{ok, HTTP_SERVER_DOCUMENT_DIR} = application:get_env(http_server_document_dir),
	{ok, HTTP_SERVER_LOG_ERROR} = application:get_env(http_server_log_error),
	{ok, HTTP_SERVER_LOG_TRANSFER} = application:get_env(http_server_log_transfer),
	{ok, HTTP_SERVER_LOG_SECURITY} = application:get_env(http_server_log_security),
	
	#http_server_config{
	                     port = HTTP_SERVER_PORT,
	                     name = HTTP_SERVER_FULL_NAME,
	                     root_dir = HTTP_SERVER_ROOT_DIR,
                         document_dir = HTTP_SERVER_DOCUMENT_DIR,
                         log_error = HTTP_SERVER_LOG_ERROR,
                         log_transfer = HTTP_SERVER_LOG_TRANSFER,
                         log_security = HTTP_SERVER_LOG_SECURITY
    }.
