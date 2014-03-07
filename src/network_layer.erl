-module(network_layer).
-export([start_link/1, is_started/0, send/2, add_listener/1]).
-export([recv_loop_init/0]).

start_link(_Opt) ->
    register(udp_recv_pid, spawn_link(?MODULE, recv_loop_init, [])),
    ok.

recv_loop_init() ->
    {ok, UdpSocket} = gen_udp:open(4066, [binary, {active, true}]),
    register(udp_send_socket, UdpSocket),
    recv_loop(UdpSocket, []).

recv_loop(UdpSocket, Listeners) ->
    receive 
        {add_listener, Pid} ->
            recv_loop(UdpSocket, [Pid | Listeners]);
        {udp, UdpSocket, IP, Port, Packet} ->
            Msg = {on_message, {IP, Port}, lcp_msg:parse(Packet)},
            lists:map(fun(Pid) -> Pid ! Msg end, Listeners),
            recv_loop(UdpSocket, Listeners);
        _ -> 
            recv_loop(UdpSocket, Listeners)
    end.

add_listener(Listener) ->
    udp_recv_pid ! {add_listener, Listener}.

send({Ip, Port}, Msg) ->
    gen_udp:send(udp_send_socket, Ip, Port, Msg).

is_started() ->
    whereis(udp_recv_pid).
