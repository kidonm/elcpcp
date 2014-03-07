-module(elcpcp_listener).
-export([behaviour_info/1]).
-export([connect/2, message/3, disconnect/2, start_link/2]).


behaviour_info(callbacks) ->
    [{init, 1}, {on_connect, 1}, {on_message, 2}, {on_disconnect, 1}];

behaviour_info(_) ->
    undefined.

loop(Module, State) ->
    NewState = receive 
        {message, User, Message} -> Module:on_message(User, Message);
        {connect, User} -> Module:on_connect(User);
        {disconnect, User} -> Module:on_disconnect(User)
    end, %TODO: add checking of state
    loop(Module, NewState).

start_link(Module, State) ->
    Ret = spawn_link(?MODULE, loop, [Module, State]),
    case Ret of
        {ok, Pid} -> network_layer:add_listener(Pid);
        _ -> not_spawned
    end,
    Ret.

connect(Pid, User) ->
    Pid ! {connect, User}.

message(Pid, User, Message) ->
    Pid ! {message, User, Message}.

disconnect(Pid, User) ->
    Pid ! {disconnect, User}.
