%% @author  Jakub Kubiak <king@browarnicki.pl>
%% @copyright 2015  Jakub Kubiak.
%% @doc RESTful API handler for inets:httpd module   

-module(http_rest_handler).

-export([do/1]).

-include_lib("inets/include/httpd.hrl"). 
-include_lib("http_server.hrl").


%% ===================================================================
%% Public API
%% ===================================================================

%% httpd Module:do callbacks

%% @doc Handles GET request. Retrieves value from store
do(Req) when (Req#mod.method == ?HTTP_METHOD_GET) ->
    {ok, Response} = process_get(Req#mod.data),
    {proceed, Response};

%% @doc Handles POST request. Inserts value into store
do(Req) when (Req#mod.method == ?HTTP_METHOD_POST) ->
    {ok, Response} = process_post({Req#mod.data, Req#mod.entity_body}),
    {proceed, Response};

%% @doc Handles PUT request. Updated value into store. 
%%      If value was not present it inserts new one. 
do(Req) when (Req#mod.method == ?HTTP_METHOD_PUT) ->
    {ok, Response} = process_put({Req#mod.data, Req#mod.entity_body}),
    {proceed, Response};

%% @doc Handles DELETE request. Purges value from store. 
do(Req) when (Req#mod.method == ?HTTP_METHOD_DELETE) ->
    {ok, Response} = process_delete(Req#mod.data),
    {proceed, Response};

%% @doc Handles HEAD request. Retrieves value metadata (headers).
do(Req) when (Req#mod.method == ?HTTP_METHOD_HEAD) ->
    {ok, Response} = process_head(Req#mod.data),
    {proceed, Response};
        
do(Req) ->
    error_logger:error_msg("Unsupported method: ~p.~n", [Req#mod.method]),
    {ok, Response} = http_helpers:http_response_not_allowed(),          
    {proceed, Response}.
	

    
%% ===================================================================
%% private functions
%% ===================================================================
%% @private
process_get([]) ->
    http_helpers:http_response_not_found();
process_get(Key) ->
    case data_storage:select(Key) of
        noval ->
            http_helpers:http_response_not_found();
            
        {ok, Value} ->
            http_helpers:http_response_ok("application/x-www-form-urlencoded", [Value])
    end.
    
%% @private
process_post({[], _Value}) ->
    http_helpers:http_response_bad_request(wrong_key);
process_post({_Key, []}) ->
    http_helpers:http_response_bad_request(wrong_value);
process_post({Key, Value}) ->
    case data_storage:insert(Key, Value) of
        conflict ->
            http_helpers:http_response_conflict();
            
        ok ->
            http_helpers:http_response_created()
    end.
    
%% @private    
process_put({[], _Value}) ->
    http_helpers:http_response_bad_request(wrong_key);    
process_put({_Key, []}) ->
    http_helpers:http_response_bad_request(wrong_value); 
process_put({Key, Value}) ->
    case data_storage:update(Key, Value) of
        ok ->
            http_helpers:http_response_ok();
            
        created ->
            http_helpers:http_response_created()
    end.
    
%% @private
process_delete([]) ->
    http_helpers:http_response_bad_request(wrong_key);
process_delete(Key) ->
    case data_storage:delete(Key) of
        ok ->
            http_helpers:http_response_ok();
            
        noval ->
            http_helpers:http_response_not_found()
    end.

%% @private
process_head([]) ->
    http_helpers:http_response_not_found();
process_head(Key) ->
    case data_storage:select(Key) of
        noval ->
            http_helpers:http_response_not_found();
            
        {ok, _Value} ->
            http_helpers:http_response_ok_head("application/x-www-form-urlencoded")
    end.