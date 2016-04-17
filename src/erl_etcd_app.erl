-module(erl_etcd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("---------~p ~p ~n", [_StartType, _StartArgs]),
    erl_etcd_sup:start_link().

stop(_State) ->
    ok.
