-module(etcd_json).

-export([erl_to_json/2, json_to_erl/2, parse_json_key/1]).

erl_to_json(Key, Value)->
    do_erl_to_json(Key, Value).

json_to_erl(Key, Value)->
    do_json_to_erl(Key, Value).

do_json_to_erl(Key, Value)->
    prepare_json_to_erl(jsx:decode(Key), jsx:decode(Value)).

parse_json_key(<<"\"", _/binary>> = Key)->
    jsx:decode(Key);
parse_json_key(Key)when is_binary(Key)->
    binary_to_atom(Key, latin1).

prepare_json_to_erl(<<"$", Key/binary>>, Value)->
    {parse_json_key(Key), binary_to_atom(Value, latin1)};
prepare_json_to_erl(Key, Value)->
    {parse_json_key(Key), prepare_json_to_erl_1(Value)}.

prepare_json_to_erl_1([H|_] = Value)when is_tuple(H)->
    lists:map(fun({K, V})->
                      prepare_json_to_erl(K, V)
              end, Value);
prepare_json_to_erl_1([H|_] = Value)when is_list(H)->
    lists:map(fun prepare_json_to_erl_1/1, Value);

prepare_json_to_erl_1(V)->
    prepare_json_to_erl_2(V).

prepare_json_to_erl_2([_|_]=Value)->
    lists:map(fun(V)->
                      prepare_json_to_erl_2(V)
              end, Value);

prepare_json_to_erl_2({<<"$", K/binary>>, V})->
    {parse_json_key(K), binary_to_atom(V, latin1)};
prepare_json_to_erl_2({K, [_|_]=V})->
    {parse_json_key(K), prepare_json_to_erl_2(V)};
prepare_json_to_erl_2({K, V})->
    {parse_json_key(K), V};
prepare_json_to_erl_2(V)->
    V.

do_erl_to_json(Key, Value)->
    prepare_erl_to_json(Key, Value).

prepare_erl_to_json(Key, Value)when is_atom(Value)->
    NewKey = prepare_erl_to_json_1(Key),
    {<<"$", NewKey/binary>>, atom_to_binary(Value, latin1)};
prepare_erl_to_json(Key, [H|_] = Value)when is_tuple(H)->
    {prepare_erl_to_json_1(Key), prepare_erl_to_json_1(Value)};
prepare_erl_to_json(Key, [_|_] = Value)->
    VL = lists:map(fun(V)->
                           prepare_erl_to_json_1(V)
                   end, Value),
    {prepare_erl_to_json_1(Key), VL};
prepare_erl_to_json(K, V)->
    {K, V}.

prepare_erl_to_json_1(V)when is_atom(V)->
    atom_to_binary(V, latin1);
prepare_erl_to_json_1([H|_]=V)when is_tuple(H)->
    lists:map(fun({K1, V1})->
                      prepare_erl_to_json(K1, V1)
              end, V);

prepare_erl_to_json_1(V)->
    iolist_to_binary(V).
