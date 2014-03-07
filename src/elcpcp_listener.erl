-module(elcpcp_listener).
-export([behaviour_info/1]).
-export([connect/2, message/3, disconnect/2, start_link/3, send_message/2]).
-export([loop/2]).

behaviour_info(callbacks) ->
    [{on_connect, 1}, {on_message, 2}, {on_disconnect, 1}];

behaviour_info(_) ->
    undefined.

loop(Module, Opts) ->
    Ret = receive 
        {on_message, User, Message} ->
            Module:on_message(User, Message);
        {on_connect, User} -> 
            Module:on_connect(User);
        {on_disconnect, User} -> 
            Module:on_disconnect(User)
    end, 
    case Ret of
        noreply -> 
            loop(Module, Opts);
        {reply, {Ip, Port}, Msg} -> 
            network_layer:send_message({Ip, Port}, Msg),
            loop(Module, Opts);
        {stop, Reason} -> 
            io:format("elcpcp_listener stopped, reason ~p", [Reason]);
        S -> 
            io:format("elcpcp_listener stopped, unknown return val ~p", [S])
    end.

send_message({Ip, Port}, Msg) ->
    network_layer:send_message({Ip, Port}, Msg).

start_link(Module, _Args, Opts) ->
    Pid = spawn_link(?MODULE, loop, [Module, Opts]),
    network_layer:add_listener(Pid),
    {ok, Pid}.

connect(Pid, User) ->
    Pid ! {connect, User}.

message(Pid, User, Message) ->
    Pid ! {message, User, Message}.

disconnect(Pid, User) ->
    Pid ! {disconnect, User}.
