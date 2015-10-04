-module(http_store_filter_tests).
 
-include_lib("eunit/include/eunit.hrl").
-include_lib("inets/include/httpd.hrl"). 
 
http_store_filter_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun correct_request/1,
        fun incorrect_request/1
    ]}.
 
setup() ->
    ?debugMsg("setup"),
    state.

cleanup(_State) ->
    ?debugMsg("cleanup"),
    ok.
    
correct_request(_State) ->
    fun() ->
        ?assertEqual({proceed, []}, http_store_filter:do(#mod{request_uri = "/store"})),
        ?assertEqual({proceed, []}, http_store_filter:do(#mod{request_uri = "/store/"})),
        ?assertEqual({proceed, "123"}, http_store_filter:do(#mod{request_uri = "/store/123"})),
        ?assertEqual({proceed, "123"}, http_store_filter:do(#mod{request_uri = "/store/123/"}))
    end.
 
incorrect_request(_State) ->
    fun() ->
        ?assertMatch({break, _}, http_store_filter:do(#mod{request_uri = "/else"})),
        ?assertMatch({break, _}, http_store_filter:do(#mod{request_uri = "/else/"})),
        ?assertMatch({break, _}, http_store_filter:do(#mod{request_uri = "/else/123"})),
        ?assertMatch({break, _}, http_store_filter:do(#mod{request_uri = "/else/123/"})),
        ?assertMatch({break, _}, http_store_filter:do(#mod{request_uri = "/else/1234"})),
        ?assertMatch({break, _}, http_store_filter:do(#mod{request_uri = "/store/1234/123"})),
        ?assertMatch({break, _}, http_store_filter:do(#mod{request_uri = "/"})),
        ?assertMatch({break, _}, http_store_filter:do(#mod{request_uri = ""}))
    end.
    
    
