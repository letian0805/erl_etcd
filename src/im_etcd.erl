-module(im_etcd).
-export([load_config/1, save_config/2]).

load_config(K)when is_atom(K)->
    load_config(etcd_json:encode_key(K));
load_config(K)when is_binary(K)->
    C = handle_json_response(get, erl_etcd:get(K)),
    decode_config(C).

save_config(K, V)when is_atom(K)->
    save_config(etcd_json:encode_key(K), V);
save_config(K, V)when is_binary(K)->
    do_save_config(K, V).

do_save_config(K, [{_, _}|_] = V)->
    lists:foreach(fun({K1, V1})->
                          K2 = etcd_json:encode_key(K1),
                          K3 = <<K/binary, "/", K2/binary>>,
                          do_save_config(K3, V1)
                  end, V);
do_save_config(K, V)->
    V1 = etcd_json:encode_value(V),
    C = handle_json_response(put, erl_etcd:put(K, V1)),
    decode_config(C).

decode_config([{_, _}|_] = C)->
    lists:map(fun({K, V})->
                      {etcd_json:decode_key(K), decode_config(V)}
              end, C);
decode_config({K, V})->
    {etcd_json:decode_key(K), decode_config(V)};
decode_config(undefined)->
    undefined;
decode_config(V)->
    etcd_json:decode_value(V).


handle_json_response(API, Result) ->
    %% io:format("----------------------~n"),
    %% io:format("~p -> ~p~n", [API, Result]),
    %% io:format("----------------------~n"),
    handle_json_response1(API, Result).

handle_json_response1(get, #{<<"action">> := <<"get">>,
                             <<"node">> := Node})->
    process_node(Node);

handle_json_response1(put, #{<<"action">> := <<"set">>,
                             <<"node">> := _Node,
                             <<"prevNode">> := PrevNode
                            }) ->
    process_node(PrevNode);

handle_json_response1(put, #{<<"action">> := <<"set">>,
                             <<"node">> := _Node
                            }) ->
    undefined.

process_node(#{<<"createdIndex">> := _CreatedIndex,
               <<"key">> := Key,
               <<"modifiedIndex">> := _ModedifiedIndex,
               <<"value">> := Value
              })->
    {_, K} = get_key(Key),
    {K, Value};

process_node(#{<<"createdIndex">> := _CreatedIndex,
               <<"dir">> := true,
               <<"key">> := Key,
               <<"modifiedIndex">> := _ModedifiedIndex,
               <<"nodes">> := Nodes
              })->
    Value = process_node_1(Nodes),
    {_, K} = get_key(Key),
    {K, Value};

process_node(#{<<"createdIndex">> := _CreatedIndex,
               <<"dir">> := true,
               <<"key">> := Key,
               <<"modifiedIndex">> := _ModedifiedIndex
              })->
    <<"/", K/binary>> = Key,
    handle_json_response1(get, erl_etcd:get(K));

process_node(#{<<"dir">> := true,
               <<"nodes">> := Nodes
              })->
    process_node_1(Nodes).

process_node_1(Nodes)->
    lists:map(fun process_node/1, Nodes).

get_path([])->
    <<"">>;
get_path([_|_]=D)->
    LD = lists:last(D),
    lists:map(fun(D1)->
                      case D1 of
                          LD -> D1;
                          _ -> <<D1/binary, "/">>
                      end
              end, D).

get_key(Path)->
    Parts = binary:split(Path, <<"/">>, [global]) -- [<<>>],
    K = lists:last(Parts),
    D = Parts -- [K],
    ND = iolist_to_binary(get_path(D)),
    {ND, K}.

