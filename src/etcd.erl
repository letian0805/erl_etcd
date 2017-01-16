-module(etcd).

-export([start/0, stop/0]).

-export([]).

start()->
    ok = ssl:start(),
    ok = inets:start(),
    ok = httpc:set_options([{max_sessions, 200}, {pipeline_timeout, 0}], default),
    p1_yaml:start(),
    application:start(etcd),
    code:load_file(etcd_data),
    code:load_file(etcd_http),
    code:load_file(etcd_watcher),
    code:load_file(etcd_opt).

stop()->
    application:stop(etcd).
