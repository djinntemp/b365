-module(http_server_tests).
 
-include_lib("eunit/include/eunit.hrl").
-include_lib("http_server.hrl").
 
http_server_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun server_is_alive/1,
        fun callbacks_test/1
    ]}.
 
setup() ->
    ?debugMsg("setup"),
    ok = inets:start(),
    Config = create_server_config(),
    {ok, Pid} = http_server:start_link(Config),
    Pid.

cleanup(Pid) ->
    ?debugMsg("cleanup"),
    http_server:stop(Pid),
    ?assertEqual(false, is_process_alive(Pid)),
    ok = inets:stop().
    
server_is_alive(Pid) ->
    fun() ->
        ?assertEqual(true, is_process_alive(Pid))
    end.
 
callbacks_test(Pid) ->
    fun() ->
        gen_server:cast(Pid, cast),
        Pid ! info,
        ?assertEqual(error, gen_server:call(Pid, unknown))
    end.
    
    
    
create_server_config() ->
    #http_server_config{
	                     port = 8080,
	                     name = "test_server",
	                     root_dir = "./",
                         document_dir = "./",
                         log_error = "test.log",
                         log_transfer = "test.log",
                         log_security = "test.log"
    }.