
-module(elcpcp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    NetworkSpec = {network_layer,
                   {network_layer, start_link, []},
                   permanent, 1000, worker, [network_layer]},
    ListenerSpec = {elcpcp_listener_sup,
                    {elcpcp_listener_sup, start_link, []},
                    permanent, 1000, supervisor, [elcpcp_listener_sup]},
    {ok, { {one_for_one, 5, 10}, [NetworkSpec, ListenerSpec]} }.

