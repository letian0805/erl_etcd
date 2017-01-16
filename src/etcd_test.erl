-module(etcd_test).

-export([test/0]).

test()->
    test_watch(),
    test_put(),
    test_get(),
    ok.

test_put()->
    io:format("--------test_put start~n"),
    K = 'foo1',
    V = <<"bar">>,
    etcd_opt:save_config(K, V),
    {foo1, V} = etcd_opt:save_config(K, <<"bar1">>),
    ok.

test_get()->
    io:format("--------test_get start~n"),
    K = 'foo2',
    V = <<"bar2">>,
    etcd_opt:save_config(K, V),
    V1 = etcd_opt:load_config(K),
    {foo2, V} = V1,
    ok.

test_watch()->
    io:format("--------test_watch start~n"),
    catch ets:delete(etcd_watcher_test),
    ets:new(etcd_watcher_test, [named_table, public, {keypos, 1}]),
    etcd_opt:save_config(foo3, <<"bar">>),
    catch etcd_watcher:stop(foo3),
    timer:sleep(500),
    etcd_watcher:start(foo3, fun(K, C)->
                                     io:format("~p ~p~n", [K, C]),
                                     ets:insert(etcd_watcher_test, {K, C})
                             end),
    timer:sleep(5000),
    etcd_opt:save_config(foo3, <<"bar3">>),
    timer:sleep(5000),
    [{foo3, <<"bar3">>}] = ets:lookup(etcd_watcher_test, foo3),
    ok.
