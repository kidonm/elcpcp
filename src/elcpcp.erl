-module(elcpcp).
-export([start/0, send_message/2]).

start() ->
    ok = application:start(elcpcp).

send_message({Ip, Port}, Msg) ->
    network_layer:send({Ip, Port}, lcp_msg:create(Msg)),
    ok.
