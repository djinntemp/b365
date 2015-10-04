-module(http_store_filter).

-export([do/1]).

-include_lib("inets/include/httpd.hrl"). 
-include_lib("http_server.hrl").


%% ===================================================================
%% Public API
%% ===================================================================

%% httpd Module:do callback
%% @doc Performs all checks to validate request. Only /store/[key] values
%%      are accepted.
do(Req) ->
    RequestPath = string:tokens(Req#mod.request_uri, "/"),
    try
        ok = check_request_length(RequestPath),
        ok = check_is_store_root(RequestPath),
        {ok, Key} = get_store_key(RequestPath),
        {proceed, Key}
    catch
        error:{badmatch, request_length} -> break_and_return(request_length);
        error:{badmatch, incorrect_root} -> break_and_return(incorrect_root);
        error:function_clause -> break_and_return(incorrect_root)
    end.
    
    

    
%% ===================================================================
%% private functions
%% ===================================================================
    
break_and_return(Reason) ->
    error_logger:error_msg("Bad request. Reason: ~p", [Reason]),
    {ok, Response} = http_helpers:http_response_bad_request(atom_to_list(Reason)),
    {break, Response}.
            
check_request_length(RequestPath) ->
    if
        (length(RequestPath) >= 1) and (length(RequestPath) < 3) ->
            ok;
        
        true ->
            request_length
    end.
            
check_is_store_root(RequestPath) ->
    Root = lists:nth(1, RequestPath),
    if 
        (Root =:= ?HTTP_REQUEST_STORE) ->
            ok;
            
        true ->
            incorrect_root
    end.

get_store_key(RequestPath) ->
    if 
        (length(RequestPath) >= 2) ->
            {ok, lists:nth(2, RequestPath)};
            
        true ->
            {ok, []}
    end.