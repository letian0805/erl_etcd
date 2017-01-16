-module(etcd_http).
-export([put/2, get/1, get/2, watch/1, watch/2, watch/3]).

put(K, V)->
    URL = build_put_url(K),
    http_request(put, URL, build_assignment(V, value)).

get(K)->
    URL = build_get_url(K),
    http_request(get, URL).

get(K, [])->
    ?MODULE:get(K);
get(K, Opts)->
    OptsUrl = opts_to_url(Opts),
    URL = build_get_url(K),
    NewURL = iolist_to_binary([URL, "?", OptsUrl]),
    http_request(get, binary_to_list(NewURL)).

watch(K)->
    watch(K, []).
watch(K, Opts)when is_list(Opts)->
    URL = make_watch_url(K, Opts),
    http_request(get, binary_to_list(URL));

watch(K, Receiver)->
    watch(K, Receiver, []).
watch(K, Receiver, Opts)->
    URL = make_watch_url(K, Opts),
    http_request(get, binary_to_list(URL), Receiver).

make_watch_url(K, Opts)->
    NewOpts = [{wait, true} | proplists:delete(wait, Opts)],
    NewOpts1 = [{recursive, true} | proplists:delete(recursive, NewOpts)],
    OptsUrl = opts_to_url(NewOpts1),
    URL = build_get_url(K),
    iolist_to_binary([URL, "?", OptsUrl]).

opt_to_url({recursive, true})->
    <<"recursive=true">>;
opt_to_url({sorted, true})->
    <<"sorted=true">>;
opt_to_url({wait, true})->
    <<"wait=true">>.

opts_to_url([FirstOpt | Opts])->
    lists:foldl(fun(Opt, Url)->
                        OptUrl = opt_to_url(Opt),
                        <<Url/binary, "&", OptUrl/binary>>
                end, opt_to_url(FirstOpt), Opts).

http_request(Method, URL) ->
    HttpOptions = [{autoredirect, true}],
    Options = [],
    Headers = [],
    handle_http_response(Method, "", httpc:request(Method, { URL, Headers}, HttpOptions, Options)).

http_request(Method, URL, Receiver) when is_function(Receiver)->
    HttpOptions = [{autoredirect, true}],
    R = fun({RID, C})->
                C1 = handle_http_response(Method, "", {ok ,C}),
                httpc:cancel_request(RID),
                Receiver(C1)
        end,
    Options = [{sync, false}, {receiver, R}],
    Headers = [],
    httpc:request(Method, { URL, Headers}, HttpOptions, Options);

http_request(Method, URL, Body) ->
    HttpOptions = [{autoredirect, true}],
    Options = [],
    Headers = [],
    BodyType = "application/x-www-form-urlencoded",
    handle_http_response(Method, Body,
                         httpc:request(Method, { URL, Headers, BodyType, Body}, HttpOptions, Options)).

http_request(Method, URL, Receiver, Body)when is_function(Receiver)->
    HttpOptions = [{autoredirect, true}],
    R = fun({RID, C})->
                C1 = handle_http_response(Method, Body, {ok, C}),
                httpc:cancel_request(RID),
                Receiver(C1)
        end,
    Options = [{sync, false}, {receiver, R}],
    Headers = [],
    BodyType = "application/x-www-form-urlencoded",
    httpc:request(Method, { URL, Headers, BodyType, Body}, HttpOptions, Options).

handle_http_response(Method, OrigBody,
                     {ok, {{_HTTPVersion, StatusCode, _ReasonPhrase} = _StatusLine,
                           Headers,
                           Body}}) ->
    handle_status_code_and_body(Method, OrigBody, StatusCode, Headers, Body).

handle_status_code_and_body(_Method, _OrigBody, Code, _Headers, Body) when is_list(Body)->
    handle_status_code_and_body(_Method, _OrigBody, Code, _Headers, list_to_binary(Body));

handle_status_code_and_body(_Method, _OrigBody, Code, _Headers, Body) when
      Code == 200; Code == 201 ->
    try case jsx:decode(Body, [return_maps]) of
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
    {Host, Port} = case application:get_env(etcd, hosts) of
                       {ok, [{H, P}]}->{H, P};
                       _->{"localhost", 8080}
                   end,
    build_get_url_internal("http", Host, integer_to_list(Port), Path).

build_get_url_internal(Scheme, Host, Port, Path) ->
    iolist_to_string([Scheme, "://", Host, ":", Port, Path]).

iolist_to_string(S)->
    binary_to_list(iolist_to_binary(S)).

build_assignment(V, value) ->
    %io:format("build_assignment  value: ~p~n", [V]),
    "value=" ++ http_uri:encode(iolist_to_string(V));
build_assignment(K, dir) ->
    "dir=" ++ http_uri:encode(iolist_to_string(K)).
