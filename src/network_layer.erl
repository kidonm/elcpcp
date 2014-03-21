-module(network_layer).
-export([start_link/1, send/2, add_listener/1]).
-export([recv_loop_init/0]).

start_link(_Opt) ->
    register(udp_recv_pid, spawn_link(?MODULE, recv_loop_init, [])),
    ok.

recv_loop_init() ->
    {ok, UdpSocket} = gen_udp:open(4066, [binary, {active, true}]),
    recv_loop(UdpSocket, []).

recv_loop(UdpSocket, Listeners) ->
    receive 
        {add_listener, Pid} ->
            recv_loop(UdpSocket, [Pid | Listeners]);
        {udp, UdpSocket, IP, Port, Packet} ->
            Msg = {on_message, {IP, Port}, lcp_msg:parse(Packet)},
            lists:map(fun(Pid) -> Pid ! Msg end, Listeners),
            recv_loop(UdpSocket, Listeners);
        {send_udp, IP, Port, Packet} ->
            gen_udp:send(UdpSocket, IP, Port, Packet);
        _ -> 
            recv_loop(UdpSocket, Listeners)
    end.

add_listener(Listener) ->
    udp_recv_pid ! {add_listener, Listener}.

send({Ip, Port}, Msg) ->
    udp_recv_pid ! {send_udp, Ip, Port, Msg}.

