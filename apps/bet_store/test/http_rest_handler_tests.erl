-module(http_rest_handler_tests).
 
-include_lib("eunit/include/eunit.hrl").
-include_lib("inets/include/httpd.hrl"). 
-include_lib("http_server.hrl").
 
http_rest_handler_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun get_tests/1,
        fun post_tests/1,
        fun put_tests/1,
        fun delete_tests/1,
        fun head_tests/1,
        fun non_existing_method_tests/1
    ]}.
 
setup() ->
    ?debugMsg("setup"),
    {ok, Pid} = data_storage:start_link(),
    Pid.

cleanup(Pid) ->
    ?debugMsg("cleanup"),
    data_storage:stop(Pid),
    receive
    after 10 ->
        ok
    end,
    ?assertEqual(false, is_process_alive(Pid)).
    
get_tests(_State) ->
    fun() ->
        % checking non-existing keys
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = []})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "123"})),
        
        % inserting "123" key
        data_storage:insert("123", "1"),
        
        % searching for "123" and other still non-existing keys
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "122"})),
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "123"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "124"})),
        
        data_storage:insert("124", "1"),
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "124"}))
        
    end.
 
post_tests(_State) ->
    fun() ->
        % checking non-existing keys
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "123"})),
        
        % checking whether post works 
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CREATED, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "123", entity_body = "1"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CONFLICT, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "123", entity_body = "1"})),
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "123"})),
        
        % checking non-existing keys
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "124"})),
        
        % checking whether post works after deleting existing key
        data_storage:delete("123"),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "123"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CREATED, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "123", entity_body = "1"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CONFLICT, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "123", entity_body = "1"})),
        
        % checking incorrect requests
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_BAD_REQUEST, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "", entity_body = ""})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_BAD_REQUEST, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = [], entity_body = ""})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_BAD_REQUEST, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "", entity_body = []})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_BAD_REQUEST, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = [], entity_body = []})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_BAD_REQUEST, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "125", entity_body = ""})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_BAD_REQUEST, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "126", entity_body = []}))
    end.
    
    
put_tests(_State) ->
    fun() ->
        % checking non-existing keys
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "123"})),
        
        % checking whether put works 
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CREATED, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_PUT, data = "123", entity_body = "1"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_OK, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_PUT, data = "123", entity_body = "2"})),
        
        % checking whether post works after deleting existing key
        data_storage:delete("123"),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "123"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CREATED, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_PUT, data = "123", entity_body = "1"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CREATED, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_PUT, data = "124", entity_body = "2"})),
        
        
        % checking incorrect requests
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_BAD_REQUEST, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_PUT, data = "", entity_body = ""})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_BAD_REQUEST, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_PUT, data = [], entity_body = ""})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_BAD_REQUEST, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_PUT, data = "", entity_body = []})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_BAD_REQUEST, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_PUT, data = [], entity_body = []})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_BAD_REQUEST, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_PUT, data = "125", entity_body = ""})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_BAD_REQUEST, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_PUT, data = "126", entity_body = []}))
        
    end.
    
delete_tests(_State) ->
    fun() ->
        %populate store with data
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CREATED, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "123", entity_body = "1"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CREATED, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "124", entity_body = "2"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CREATED, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "125", entity_body = "3"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CREATED, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "126", entity_body = "4"})),
        
        %check whether populated
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "123"})),
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "124"})),
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "125"})),
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "126"})),
        
        %delete second key and check remaining three
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_OK, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_DELETE, data = "123"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "123"})),
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "124"})),
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "125"})),
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "126"})),
        
        %delete non-existing data
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_DELETE, data = "123"})),
        
        %delete remaining data
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_OK, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_DELETE, data = "124"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_OK, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_DELETE, data = "125"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_OK, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_DELETE, data = "126"})),
        
        %check whether all data were removed
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "123"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "124"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "125"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_GET, data = "126"})),
        
        % checking incorrect requests
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_BAD_REQUEST, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_DELETE, data = ""}))
        
    end.

head_tests(_State) ->
    fun() ->
        %populate store with data
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CREATED, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "123", entity_body = "1"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CREATED, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "124", entity_body = "2"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CREATED, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "125", entity_body = "3"})),
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_CREATED, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_POST, data = "126", entity_body = "4"})),
        
        %check whether HEAD is returning data
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_HEAD, data = "123"})),
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_HEAD, data = "124"})),
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_HEAD, data = "125"})),
        ?assertMatch({proceed, [{response, {response, [{code, ?HTTP_STATUS_OK} | _], _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_HEAD, data = "126"})),
        
        %check non-existing data with HEAD
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_HEAD, data = "128"})),
        
        % checking incorrect requests
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_FOUND, _}}]}, http_rest_handler:do(#mod{method = ?HTTP_METHOD_HEAD, data = ""}))
        
    end.
    
    
non_existing_method_tests(_State) ->
    fun() ->
        ?assertMatch({proceed, [{response, {?HTTP_STATUS_NOT_ALLOWED, _}}]}, http_rest_handler:do(#mod{method = "OPTIONS", data = "123"}))
    end.
