-module(msg_factory).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{parse, 1}, {create, 1}];

behaviour_info(_) ->
    undefined.
