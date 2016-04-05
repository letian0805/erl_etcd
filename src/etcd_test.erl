-module(etcd_test).

-export([test/0]).

test()->
    test_put(),
    test_get().

test_put()->
    im_etcd:put(<<"'test'">>, <<"'aaaa'">>),
    {test, <<"aaaa">>} = im_etcd:put(<<"'test'">>, <<"'bbbb'">>),
    K = <<"'test1'/'foo'">>,
    V = <<"[{'a':'b', '$c':'d', 'e':[{'$f':'g'}]}]">>,
    im_etcd:put(K, V),
    {foo, [[{a, <<"b">>}, {c, d}, {e, [[{f, g}]]}]]} = im_etcd:put(K, <<"'bar'">>),
    ok.

test_get()->
    D = <<"'test1'">>,
    K = <<D/binary, "/", "'foo'">>,
    V = <<"[{'a':'b', '$c':'d', 'e':[{'$\"f\"':'g'}]}]">>,
    im_etcd:put(K, V),
    {test1, [{foo, [[{a, <<"b">>}, {c, d}, {e, [[{<<"f">>, g}]]}]]}]} = im_etcd:get(D),
    ok.
