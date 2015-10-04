-module(network_protocol_parser_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("network_protocol.hrl").


network_protocol_parser_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun insert_test/1,
        fun insert_collission_test/1,
        fun select_test/1,
        fun delete_test/1,
        fun update_test/1,
        fun update_non_existing_test/1,
        fun format_check/1
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
    
    
format_check(_Pid) ->
    fun() ->
        ?assertMatch({ok, [<<"Commands", _Rest/binary>> | _]}, network_protocol_parser:parse(<<"help">>)),
        ?assertMatch({ok, [<<"Commands", _Rest/binary>> | _]}, network_protocol_parser:parse(<<"nonexistingcommand:21431:231">>)),
        ?assertMatch({ok, [<<"Commands", _Rest/binary>> | _]}, network_protocol_parser:parse(<<"insert 211321">>)),
        
        ?assertEqual(?WRONGKEYVAL, network_protocol_parser:parse(<<"insert:123">>)),
        ?assertEqual(?WRONGKEYVAL, network_protocol_parser:parse(<<"update:123">>))
    end.
    
    
insert_test(_Pid) ->
    fun() ->
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:123:1">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:124:2">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:125:3">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:126:4">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:127:5">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:128:6">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:129:7">>))
    end.

insert_collission_test(_Pid) ->    
    fun() ->
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:123:11">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:124:22">>)),
        ?assertEqual(?CONFLICT, network_protocol_parser:parse(<<"insert:123:33">>)),
        ?assertEqual(?CONFLICT, network_protocol_parser:parse(<<"insert:124:44">>)),
        ?assertEqual(?CONFLICT, network_protocol_parser:parse(<<"insert:123:55">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:128:66">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:129:77">>)),
        
        % objects 123,124, 128,129 shall not be modified
        ?assertEqual({ok, "11"}, network_protocol_parser:parse(<<"select:123">>)),
        ?assertEqual({ok, "22"}, network_protocol_parser:parse(<<"select:124">>)),
        ?assertEqual({ok, "66"}, network_protocol_parser:parse(<<"select:128">>)),
        ?assertEqual({ok, "77"}, network_protocol_parser:parse(<<"select:129">>))
    end.
    
    
select_test(_Pid) ->
    fun() ->
        % populate store with data
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:123:11">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:124:22">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:125:33">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:126:44">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:127:55">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:128:66">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:129:77">>)),
        
        % object with key '8' was never inserted. noval shall be returned
        ?assertEqual(?NOVAL, network_protocol_parser:parse(<<"select:8">>)),
        
        ?assertEqual({ok, "11"}, network_protocol_parser:parse(<<"select:123">>)),
        ?assertEqual({ok, "22"}, network_protocol_parser:parse(<<"select:124">>)),
        ?assertEqual({ok, "33"}, network_protocol_parser:parse(<<"select:125">>)),
        ?assertEqual({ok, "44"}, network_protocol_parser:parse(<<"select:126">>)),
        ?assertEqual({ok, "55"}, network_protocol_parser:parse(<<"select:127">>)),
        ?assertEqual({ok, "66"}, network_protocol_parser:parse(<<"select:128">>)),
        ?assertEqual({ok, "77"}, network_protocol_parser:parse(<<"select:129">>))
    end.
    
    
delete_test(_Pid) ->
    fun() ->
        % populate store with data
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:123:11">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:124:22">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:125:33">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:126:44">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:127:55">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:128:66">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:129:77">>)),
        
        % object with key '125' shall be successfully deleted
        ?assertEqual(?OK, network_protocol_parser:parse(<<"delete:125">>)),
        
        % object with key '122' was never inserted. noval shall be returned
        ?assertEqual(?NOVAL, network_protocol_parser:parse(<<"delete:122">>)),
        
        % object with key '125' was deleted earlier
        ?assertEqual(?NOVAL, network_protocol_parser:parse(<<"delete:125">>)),
        
        %remaining objects shall maintain intact
        ?assertEqual({ok, "11"}, network_protocol_parser:parse(<<"select:123">>)),
        ?assertEqual({ok, "22"}, network_protocol_parser:parse(<<"select:124">>)),
        ?assertEqual({ok, "44"}, network_protocol_parser:parse(<<"select:126">>)),
        ?assertEqual({ok, "55"}, network_protocol_parser:parse(<<"select:127">>)),
        ?assertEqual({ok, "66"}, network_protocol_parser:parse(<<"select:128">>)),
        ?assertEqual({ok, "77"}, network_protocol_parser:parse(<<"select:129">>))
    end.
    
    
    
update_test(_Pid) ->
    fun() ->
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:123:11">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:124:22">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:125:33">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:126:44">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:127:55">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:128:66">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:129:77">>)),
        
        % updating value of 125
        ?assertEqual(?OK, network_protocol_parser:parse(<<"update:125:00">>)),
        
        % new value of 125 shall be 00, remaining items shall not be changed        
        ?assertEqual({ok, "11"}, network_protocol_parser:parse(<<"select:123">>)),
        ?assertEqual({ok, "22"}, network_protocol_parser:parse(<<"select:124">>)),
        ?assertEqual({ok, "00"}, network_protocol_parser:parse(<<"select:125">>)),
        ?assertEqual({ok, "44"}, network_protocol_parser:parse(<<"select:126">>)),
        ?assertEqual({ok, "55"}, network_protocol_parser:parse(<<"select:127">>)),
        ?assertEqual({ok, "66"}, network_protocol_parser:parse(<<"select:128">>)),
        ?assertEqual({ok, "77"}, network_protocol_parser:parse(<<"select:129">>))
    end.
    
    
update_non_existing_test(_Pid) ->
    fun() ->
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:123:11">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:124:22">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:126:44">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:127:55">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:128:66">>)),
        ?assertEqual(?OK, network_protocol_parser:parse(<<"insert:129:77">>)),
        
        % there's no '125' yet
        ?assertEqual(?NOVAL, network_protocol_parser:parse(<<"select:125">>)),
        
        % updating non existing object
        ?assertEqual(?CREATED, network_protocol_parser:parse(<<"update:125:88">>)),
        
        % checking consistency
        ?assertEqual({ok, "11"}, network_protocol_parser:parse(<<"select:123">>)),
        ?assertEqual({ok, "22"}, network_protocol_parser:parse(<<"select:124">>)),
        ?assertEqual({ok, "44"}, network_protocol_parser:parse(<<"select:126">>)),
        ?assertEqual({ok, "55"}, network_protocol_parser:parse(<<"select:127">>)),
        ?assertEqual({ok, "66"}, network_protocol_parser:parse(<<"select:128">>)),
        ?assertEqual({ok, "77"}, network_protocol_parser:parse(<<"select:129">>)),
        
        % object with key '128' shall be found
        ?assertEqual({ok, "88"}, network_protocol_parser:parse(<<"select:125">>))
        
    end.
    
    

