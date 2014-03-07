-module(network_layer).
-export([start_link/1, send/2, add_listener/1]).
-export([recv_loop/2, send_loop/1]).

start_link(_Opt) ->
    {ok, UdpSocket} = gen_udp:open(4066, [binary, {active, true}]),
    register(udp_recv_pid, spawn_link(?MODULE, recv_loop, [UdpSocket, []])),
    register(udp_send_pid, spawn_link(?MODULE, send_loop, [UdpSocket])).


recv_loop(UdpSocket, Listeners) ->
    receive 
        {add_listener, Pid} -> 
            recv_loop(UdpSocket, [Pid | Listeners]);
        {udp, UdpSocket, IP, Port, Packet} ->
            Msg = {on_message, {IP, Port}, lcp_msg:parse(Packet)},
            lists:map(fun(Pid) -> Pid ! Msg end, Listeners),
            recv_loop(UdpSocket, Listeners)
    end.

add_listener(Listener) ->
    udp_recv_pid ! {add_listener, Listener}.

send_loop(UdpSocket) ->
    send_loop(UdpSocket).

send(_Client, _Msg) ->
    ok.


