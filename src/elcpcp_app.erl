-module(elcpcp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    network_layer:start_link(na),
    elcpcp_sup:start_link().

stop(_State) ->
    ok.

