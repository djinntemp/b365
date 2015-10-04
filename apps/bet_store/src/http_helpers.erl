%% @author  Jakub Kubiak <king@browarnicki.pl>
%% @copyright 2015  Jakub Kubiak.
%% @doc Helper methods for HTTP protocol 

-module(http_helpers).

-export([generate_http_response/1,
	 http_response_ok/2,
	 http_response_ok_head/1,
	 http_response_ok/0,
	 http_response_created/0,
	 http_response_bad_request/1,
	 http_response_not_found/0,
	 http_response_not_allowed/0,
	 http_response_conflict/0
 ]).

-include_lib("http_server.hrl").

http_response_ok(ContentType, Content) ->
    Response = #http_response{
                            code = 200,
                            content_type = ContentType,
                            content = Content
    },
    generate_http_response(Response).

http_response_ok_head(ContentType) ->
    {ok, [{response, {response, [
		{code, 200}, 
		{content_type, ContentType}, 
		{cache_control, "private"}
	], nobody}}]}.

http_response_ok() ->
    {ok, [{response, {200, "<html><title>200 - OK</title><body><h1>Success</h1></body></html>"}}]}.
    
http_response_created() ->
    {ok, [{response, {201, "<html><title>201 - Created</title><body><h1>Successfully created</h1></body></html>"}}]}.

http_response_bad_request(Reason) ->
    {ok, [{response, {400, lists:flatten(io_lib:format("<html><title>400 - Bad Request</title><body><h1>Bad Request. Reason: ~p</h1></body></html>", [Reason]))}}]}.
    
http_response_not_found() ->
    {ok, [{response, {404, "<html><title>404 - Not Found</title><body><h1>Not Found</h1></body></html>"}}]}.

http_response_not_allowed() ->
    {ok, [{response, {405, "<html><title>405 - Method not allowed</title><body><h1>Method not allowed</h1></body></html>"}}]}.
    
http_response_conflict() ->
    {ok, [{response, {409, "<html><title>409 - Conflict</title><body><h1>Conflict. Resource already exists</h1></body></html>"}}]}.

    
generate_http_response(Response) when is_record(Response, http_response) ->
	Code = Response#http_response.code,
	ContentType = Response#http_response.content_type,
	Content = Response#http_response.content,
	if 
		Response#http_response.content_length =:= null ->
			ContentLength = integer_to_list(length(lists:flatten(Content)));
		true->
			ContentLength = Response#http_response.content_length
	end,
	{ok, [{response, {response, [
		{code, Code}, 
		{content_type, ContentType}, 
		{content_length, ContentLength},
		{cache_control, "private"}
	], Content}}]}.
