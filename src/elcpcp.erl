-module(elcpcp).
-export([start/0, send_msg/2]).

start() ->
    ok = application:start(elcp).

send_msg(_Client, _Message) ->
    ok.
