-module(elcpcp_listener_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_elcpcp_listener/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


add_elcpcp_listener(Module, Opts) ->
    Spec = {make_ref(),
            {elcpcp_listener, start_link, [Module, Opts]},
            transient, 1000, worker, [elcpcp_listener]},
    supervisor:start_child(?MODULE, Spec).
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
