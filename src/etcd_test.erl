-module(etcd_test).

-export([test/0]).

test()->
    test_put(),
    test_get(),
    test_listen(),
    ok.

test_put()->
    K = 'foo1',
    V = <<"bar">>,
    etcd_opt:save_config(K, V),
    {foo1, V} = etcd_opt:save_config(K, <<"bar1">>),
    ok.

test_get()->
    K = 'foo2',
    V = <<"bar2">>,
    etcd_opt:save_config(K, V),
    V1 = etcd_opt:load_config(K),
    {foo2, V} = V1,
    ok.

listener(Key, Config)->
    io:format("~p ~p~n", [Key, Config]).

test_listen()->
    etcd_opt:save_config(foo3, <<"bar">>),
    etcd_watcher:start(foo3, fun listener/2),
    timer:sleep(1000),
    etcd_opt:save_config(foo3, <<"bar3">>),
    ok.
