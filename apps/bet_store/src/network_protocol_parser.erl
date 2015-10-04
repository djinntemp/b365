-module(network_protocol_parser).

-export([parse/1]).

-include_lib("network_protocol.hrl").

parse(<<?HELP, _Rest/binary>>) ->
    help_response();

parse(<<?SELECT, Key/binary>>) ->
    process_select(Key);
parse(<<?INSERT, KeyVal/binary>>) ->
    process_insert(KeyVal);
parse(<<?UPDATE, KeyVal/binary>>) ->
    process_update(KeyVal);
parse(<<?DELETE, Key/binary>>) ->
    process_delete(Key);
parse(_Unknown) ->
    help_response().


help_response() ->
    {ok, [<<"Commands:\r\n">>,
          <<"help           - displays this message.\r\n">>,
          <<"                 If command is unrecognized help shall appear\r\n">>,
          <<"select:KEY     - retrieves object from storage. Returns VAL\r\n">>,
          <<"insert:KEY:VAL - inserts object to storage. Returns ok|conflict\r\n">>,
          <<"update:KEY:VAL - updates object in storage. Returns ok|created\r\n">>,
          <<"delete:KEY     - deletes object from storage. Returns ok|noval\r\n">>
    ]}.
    
    
process_select(Binary) ->
    case data_storage:select(extract_key(Binary)) of
        noval ->
            ?NOVAL;
            
        {ok, Value} ->
            {ok, Value}
    end.
    
    
process_insert(KeyVal) ->
    case extract_key_value(KeyVal) of 
        {Key, Value} ->
            process_insert(Key, Value);
        wrong_key_value ->
            ?WRONGKEYVAL
    end.
            
    
process_insert(Key, Value) ->
    case data_storage:insert(Key, Value) of
        conflict ->
            ?CONFLICT;
            
        ok ->
            ?OK
    end.
    
    
process_update(KeyVal) ->
    case extract_key_value(KeyVal) of
        {Key, Value} ->
            process_update(Key, Value);
        wrong_key_value ->
            ?WRONGKEYVAL
    end.
    
process_update(Key, Value) ->
    case data_storage:update(Key, Value) of
        ok ->
            ?OK;
            
        created ->
            ?CREATED
    end.
    
    
process_delete(Binary) ->
    case data_storage:delete(extract_key(Binary)) of
        ok ->
            ?OK;
            
        noval ->
            ?NOVAL
    end.
    
    
    
extract_key_value(KeyVal) ->
    List = binary:split(KeyVal, <<":">>),
    if
        length(List) =:= 2 ->
            [Key, Value] = List,
            {binary_to_list(Key), binary_to_list(Value)};
        true ->
            wrong_key_value
    end.
    
extract_key(Binary) ->
    NoR = binary:replace(Binary, <<"\r">>, <<"">>),
    NoN = binary:replace(NoR, <<"\n">>, <<"">>),
    binary_to_list(NoN).