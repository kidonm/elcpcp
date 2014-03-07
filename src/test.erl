-module(test).
-export([init/1, on_connect/1, on_message/2, on_disconnect/1]).
-compile(export_all).

-behavior(elcpcp_listener).

init(_) ->
    ok.

on_connect(Client) ->
    io:format("connect ~p", [Client]),
    ok.

on_message(Client, Msg) ->
    io:format("~p: ~p", [Client, Msg]),
    ok.

on_disconnect(Client) ->
    io:format("disconnect ~p", [Client]),
    ok.

start() -> 
    ok = application:start(elcpcp).
    
