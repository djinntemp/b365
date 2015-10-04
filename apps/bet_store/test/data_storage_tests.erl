-module(data_storage_tests).
 
-include_lib("eunit/include/eunit.hrl").
 
data_storage_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun server_is_alive/1,
        fun insert_test/1,
        fun insert_collission_test/1,
        fun select_test/1,
        fun delete_test/1,
        fun update_test/1,
        fun update_non_existing_test/1,
        fun callbacks_test/1
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
    
server_is_alive(Pid) ->
    fun() ->
        ?assertEqual(true, is_process_alive(Pid))
    end.
 
insert_test(_Pid) ->
    fun() ->
        ?assertEqual(ok, data_storage:insert(1, 11)),
        ?assertEqual(ok, data_storage:insert(2, 22)),
        ?assertEqual(ok, data_storage:insert(3, 33)),
        ?assertEqual(ok, data_storage:insert(4, 44)),
        ?assertEqual(ok, data_storage:insert(5, 55)),
        ?assertEqual(ok, data_storage:insert(6, 66)),
        ?assertEqual(ok, data_storage:insert(7, 77))
    end.
    
    
insert_collission_test(_Pid) ->
    fun() ->
        ?assertEqual(ok, data_storage:insert(1, 11)),
        ?assertEqual(ok, data_storage:insert(2, 22)),
        ?assertEqual(ok, data_storage:insert(3, 33)),
        ?assertEqual(conflict, data_storage:insert(1, 44)),
        ?assertEqual(conflict, data_storage:insert(2, 55)),
        ?assertEqual(conflict, data_storage:insert(3, 66)),
        ?assertEqual(ok, data_storage:insert(4, 77)),
        
        % objects 1,2,3 shall not be modified
        ?assertEqual({ok, 11}, data_storage:select(1)),
        ?assertEqual({ok, 22}, data_storage:select(2)),
        ?assertEqual({ok, 33}, data_storage:select(3)),
        ?assertEqual({ok, 77}, data_storage:select(4))
    end.
    
    
select_test(_Pid) ->
    fun() ->
        ?assertEqual(ok, data_storage:insert(1, 11)),
        ?assertEqual(ok, data_storage:insert(2, 22)),
        ?assertEqual(ok, data_storage:insert(3, 33)),
        ?assertEqual(ok, data_storage:insert(4, 44)),
        ?assertEqual(ok, data_storage:insert(5, 55)),
        ?assertEqual(ok, data_storage:insert(6, 66)),
        ?assertEqual(ok, data_storage:insert(7, 77)),
        
        % object with key '8' was never inserted. noval shall be returned
        ?assertEqual(noval, data_storage:select(8)),
        
        ?assertEqual({ok, 77}, data_storage:select(7)),
        ?assertEqual({ok, 66}, data_storage:select(6)),
        ?assertEqual({ok, 55}, data_storage:select(5)),
        ?assertEqual({ok, 44}, data_storage:select(4)),
        ?assertEqual({ok, 33}, data_storage:select(3)),
        ?assertEqual({ok, 22}, data_storage:select(2)),
        ?assertEqual({ok, 11}, data_storage:select(1))
    end.
    
    
delete_test(_Pid) ->
    fun() ->
        ?assertEqual(ok, data_storage:insert(1, 11)),
        ?assertEqual(ok, data_storage:insert(2, 22)),
        ?assertEqual(ok, data_storage:insert(3, 33)),
        ?assertEqual(ok, data_storage:insert(4, 44)),
        ?assertEqual(ok, data_storage:insert(5, 55)),
        ?assertEqual(ok, data_storage:insert(6, 66)),
        ?assertEqual(ok, data_storage:insert(7, 77)),
        
        % object with key '3' shall be successfully deleted
        ?assertEqual(ok, data_storage:delete(3)),
        
        % object with key '8' was never inserted. noval shall be returned
        ?assertEqual(noval, data_storage:delete(8)),
        
        % object with key '3' was deleted earlier
        ?assertEqual(noval, data_storage:delete(3)),
        
        %remaining objects shall maintain intact
        ?assertEqual({ok, 77}, data_storage:select(7)),
        ?assertEqual({ok, 66}, data_storage:select(6)),
        ?assertEqual({ok, 55}, data_storage:select(5)),
        ?assertEqual({ok, 44}, data_storage:select(4)),
        ?assertEqual({ok, 22}, data_storage:select(2)),
        ?assertEqual({ok, 11}, data_storage:select(1))
    end.
    
    
update_test(_Pid) ->
    fun() ->
        ?assertEqual(ok, data_storage:insert(1, 11)),
        ?assertEqual(ok, data_storage:insert(2, 22)),
        ?assertEqual(ok, data_storage:insert(3, 33)),
        ?assertEqual(ok, data_storage:insert(4, 44)),
        ?assertEqual(ok, data_storage:insert(5, 55)),
        ?assertEqual(ok, data_storage:insert(6, 66)),
        ?assertEqual(ok, data_storage:insert(7, 77)),
        
        ?assertEqual({ok, 77}, data_storage:select(7)),
        ?assertEqual({ok, 66}, data_storage:select(6)),
        ?assertEqual({ok, 55}, data_storage:select(5)),
        ?assertEqual({ok, 44}, data_storage:select(4)),
        ?assertEqual({ok, 33}, data_storage:select(3)),
        ?assertEqual({ok, 22}, data_storage:select(2)),
        ?assertEqual({ok, 11}, data_storage:select(1)),
        
        % updating value of 3
        ?assertEqual(ok, data_storage:update(3, 00)),
        
        % new value of 3 shall be 00        
        ?assertEqual({ok, 77}, data_storage:select(7)),
        ?assertEqual({ok, 66}, data_storage:select(6)),
        ?assertEqual({ok, 55}, data_storage:select(5)),
        ?assertEqual({ok, 44}, data_storage:select(4)),
        ?assertEqual({ok, 00}, data_storage:select(3)),
        ?assertEqual({ok, 22}, data_storage:select(2)),
        ?assertEqual({ok, 11}, data_storage:select(1))
    end.
    
    
update_non_existing_test(_Pid) ->
    fun() ->
        ?assertEqual(ok, data_storage:insert(1, 11)),
        ?assertEqual(ok, data_storage:insert(2, 22)),
        ?assertEqual(ok, data_storage:insert(3, 33)),
        ?assertEqual(ok, data_storage:insert(4, 44)),
        ?assertEqual(ok, data_storage:insert(5, 55)),
        ?assertEqual(ok, data_storage:insert(6, 66)),
        ?assertEqual(ok, data_storage:insert(7, 77)),
        
        % there's no '8' yet
        ?assertEqual(noval, data_storage:select(8)),
        
        % updating non existing object
        ?assertEqual(created, data_storage:update(8, 88)),
        
        % checking consistency
        ?assertEqual({ok, 77}, data_storage:select(7)),
        ?assertEqual({ok, 66}, data_storage:select(6)),
        ?assertEqual({ok, 55}, data_storage:select(5)),
        ?assertEqual({ok, 44}, data_storage:select(4)),
        ?assertEqual({ok, 33}, data_storage:select(3)),
        ?assertEqual({ok, 22}, data_storage:select(2)),
        ?assertEqual({ok, 11}, data_storage:select(1)),
        
        % object with key '8' shall be found
        ?assertEqual({ok, 88}, data_storage:select(8))
    end.
    

callbacks_test(Pid) ->
    fun() ->
        gen_server:cast(Pid, cast),
        Pid ! info,
        ?assertEqual(error, gen_server:call(Pid, unknown))
    end.