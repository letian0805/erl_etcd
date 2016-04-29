-module(etcd).

-export([start/0, stop/0]).

-export([]).

start()->
    inets:start(),
    httpc:set_options([{pipeline_timeout, 0}]),
    p1_yaml:start(),
    application:start(etcd),
    code:load_file(etcd_data),
    code:load_file(etcd_http),
    code:load_file(etcd_opt).

stop()->
    application:stop(etcd).
