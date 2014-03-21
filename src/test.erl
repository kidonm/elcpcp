-module(test).
-export([init/0, on_connect/1, on_message/2, on_disconnect/1]).
-compile(export_all).
-behavior(elcpcp_listener).

on_connect(Client) ->
    io:format("connect ~p~n", [Client]),
    noreply.

on_message(Client, Msg) ->
    io:format("msg from ~p: ~p~n", [Client, Msg]),
    noreply.

on_disconnect(Client) ->
    io:format("disconnect ~p~n", [Client]),
    noreply.


init() ->
    ok = application:start(elcpcp),
    {ok, _Pid} = elcpcp_listener:start_link(?MODULE, [], []),
    
    {ok, ClientSocket1} = gen_udp:open(10001, [binary, {active, true}]),
    {ok, ClientSocket2} = gen_udp:open(10002, [binary, {active, true}]),
    
    gen_udp:send(ClientSocket1, {127,0,0,1}, 4066, <<2#11000000, "C", "o", "o", "k", "i", "e">>), 
    gen_udp:send(ClientSocket2, {127,0,0,1}, 4066, <<2#11000000, "C", "o", "o", "k", "i", "e">>),

    elcpcp:send_message({{127,0,0,1}, 10001}, {datagram_ind, cornet_msg, {kbd_up_ind, 1}}).
