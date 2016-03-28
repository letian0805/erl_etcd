-module(erl_etcd).
-export([get/1]).



get(Key) ->
    URL = build_get_url(Key),
    handle_json_response(get, http_request(get, URL)).

put(Key, Value) ->
    URL = build_put_url(Key),
    handle_json_response(put, http_request(put, URL, build_value_assignment(Value))).


handle_json_response(API, Result) ->
    %% xio:format("~p -> ~p~n", [API, Result]),
    handle_json_response1(API, Result).


handle_json_response1(get, #{<<"action">> := <<"get">>,
                             <<"node">> :=
                                 #{<<"createdIndex">> := _CreateIndex,
                                   <<"key">> := _Key,
                                   <<"modifiedIndex">> := _ModifiedIndex,
                                   <<"value">> :=  Value}}) ->
    Value;
handle_json_response1(put, #{<<"action">> := <<"set">>,
                             <<"node">> :=
                                 #{<<"createdIndex">> := _CreateIndex,
                                   <<"key">> := _Key,
                                   <<"modifiedIndex">> := _ModifiedIndex,
                                   <<"value">> :=  _Value},
                             <<"prevNode">> :=
                                 #{<<"createdIndex">> := _PrevCreateIndex,
                                   <<"key">> := _Key,
                                   <<"modifiedIndex">> := _PrevModifiedIndex,
                                   <<"value">> :=  PrevValue}
                            }) ->
    PrevValue;
handle_json_response1(put, #{<<"action">> := <<"set">>,
                             <<"node">> :=
                                 #{<<"createdIndex">> := _CreateIndex,
                                   <<"key">> := _Key,
                                   <<"modifiedIndex">> := _ModifiedIndex,
                                   <<"value">> :=  _Value}
                            }) ->
    undefined.


http_request(Method, URL) ->
    HttpOptions = [{autoredirect, true}],
    Options = [],
    Headers = [],
    handle_http_response(Method, "", httpc:request(Method, { URL, Headers}, HttpOptions,Options)).

http_request(Method, URL, Body) ->
    HttpOptions = [{autoredirect, true}],
    Options = [],
    Headers = [],
    BodyType = "application/x-www-form-urlencoded",
    handle_http_response(Method, Body,
                         httpc:request(Method, { URL, Headers, BodyType, Body}, HttpOptions,Options)).


handle_http_response(Method, OrigBody,
                     {ok, {{_HTTPVersion, StatusCode, _ReasonPhrase} = _StatusLine,
                           Headers,
                           Body}}) ->
    handle_status_code_and_body(Method, OrigBody, StatusCode, Headers, Body).

handle_status_code_and_body(_Method, _OrigBody, Code, _Headers, Body) when
      Code == 200; Code == 201 ->
    try case jsx:decode(list_to_binary(Body), [return_maps]) of
            X -> X
        end
    catch
        error:badarg ->
            error({jsx_error, Body})
    end;
handle_status_code_and_body(Method, OrigBody, 301, Headers, Body) ->
    io:format("Headers = ~p~n",[Headers]),
    case proplists:get_value("location", Headers) of
        NewURL when is_list(NewURL) ->
            http_request(Method, NewURL, OrigBody);
        _ ->
            error({error, {301, Headers, Body}})
    end;
handle_status_code_and_body(_Method, _OrigBody, 404, _Headers, _Body) ->
    error({error, not_found}).

build_get_url(Key) ->
    build_url(["/v2/keys/", Key]).

build_put_url(Key) ->
    build_url(["/v2/keys/", Key]).

build_url(Path) ->
    Host = "192.168.99.100",
    Port = "4001",
    build_get_url_internal("http", Host, Port, Path).

build_get_url_internal(Scheme, Host, Port, Path) ->
    iolist_to_string([Scheme, "://", Host, ":", Port, Path]).

iolist_to_string(S) ->
    binary_to_list(iolist_to_binary(S)).

build_value_assignment(Value) ->
    "value=" ++ http_uri:encode(iolist_to_string(Value)).
