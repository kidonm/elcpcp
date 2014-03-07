-module(elcpcp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, _StartArgs) ->
    network_layer:start_link([]),
    elcpcp_sup:start_link().

stop(_State) ->
    ok.

