-module(network_layer).
-export([start_link/0, send/2, add_listener/1]).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                 terminate/2, code_change/3]).

-record(state, {opts=[], listeners=[], udp_sock}).
-record(args, {udp_port=4066}).

%% private
parse_env() -> 
    Env = application:get_all_env(elcpcp),
    lists:foldr(
        fun(X, Acc) -> 
            case X of
                {udp_port, Port} ->
                    Acc#args{udp_port=Port};
                _ -> Acc
            end
        end,
        #args{},
        Env).

%% api
add_listener(Listener) ->
    gen_server:cast(udp_recv_pid, {add_listener, Listener}).

send({Ip, Port}, Msg) ->
    gen_server:cast(udp_recv_pid, {send_udp, Ip, Port, Msg}).

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    register(udp_recv_pid, Pid),
    {ok, Pid}.


%% gen_server
init(Opts) ->
    Env = parse_env(),
    {ok, UdpSocket} = gen_udp:open(Env#args.udp_port, [binary, {active, true}]),
    {ok, #state{opts=Opts, listeners=[], udp_sock=UdpSocket}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_msg, _from, State) -> {noreply, State}.
handle_info({udp, _UdpSocket, IP, Port, Packet}, State=#state{listeners=Listeners}) -> 
    Msg = {on_message, {IP, Port}, lcp_msg:parse(Packet)},
    lists:map(fun(Pid) -> Pid ! Msg end, Listeners),
    {noreply, State}.
handle_cast({send_udp, IP, Port, Packet}, State=#state{udp_sock=UdpSocket}) ->
    gen_udp:send(UdpSocket, IP, Port, Packet),
    {noreply, State};
handle_cast({add_listener, Pid}, State=#state{listeners=Listeners}) ->
    {noreply, State#state{listeners=[Pid | Listeners]}}.

terminate(_Reason, #state{udp_sock=UdpSocket}) -> 
    gen_udp:close(UdpSocket),
    unregister(udp_recv_pid),
    ok.
