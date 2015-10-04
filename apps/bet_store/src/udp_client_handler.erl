%% @author  Jakub Kubiak <king@browarnicki.pl>
%% @copyright 2015  Jakub Kubiak.
%% @doc Handler for UDP messages 

-module(udp_client_handler).

-export([handle_message/1]).


handle_message({udp, Socket, Ip, Port, Data}) ->
    {ok, Response} = network_protocol_parser:parse(Data),
    gen_udp:send(Socket, Ip, Port, Response).