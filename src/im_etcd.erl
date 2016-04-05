-module(im_etcd).
-export([get/1, put/2]).

get(Key) ->
    URL = erl_etcd:build_get_url(Key),
    handle_json_response(get, erl_etcd:http_request(get, URL)).

put(Key, Value)->
    K = real_key(Key),
    Dir = binary:part(Key, 0, byte_size(Key)-byte_size(K)),
    {K1, V1} = etcd_json:erl_to_json(K, Value),
    Dir1 = iolist_to_binary([Dir, "/"]),
    URL = erl_etcd:build_put_url(Dir1, K1),
    handle_json_response(put, erl_etcd:http_request(put, URL, erl_etcd:build_assignment(V1, value))).


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

real_key(Key)->
    Parts = binary:split(Key, <<"/">>, [trim, global]),
    lists:last(Parts).

process_nodes(Nodes)->
    lists:map(fun process_node/1, Nodes).

process_node(#{<<"createdIndex">> := _CreatedIndex,
               <<"key">> := Key,
               <<"modifiedIndex">> := _ModedifiedIndex,
               <<"value">> := Value
              })->
    RealKey = real_key(Key),
    etcd_json:json_to_erl(RealKey, Value);

process_node(#{<<"createdIndex">> := _CreatedIndex,
               <<"dir">> := true,
               <<"key">> := Key,
               <<"modifiedIndex">> := _ModedifiedIndex,
               <<"nodes">> := Nodes
              })->
    Value = process_nodes(Nodes),
    RealKey = real_key(Key),
    {etcd_json:parse_json_key(jsx:decode(RealKey)), Value};

process_node(#{<<"createdIndex">> := _CreatedIndex,
               <<"dir">> := true,
               <<"key">> := Key,
               <<"modifiedIndex">> := _ModedifiedIndex
              })->
    ?MODULE:get(Key);

process_node(#{<<"dir">> := true,
               <<"nodes">> := Nodes
              })->
    process_nodes(Nodes).
