-module(etcd_yml).

-export([yml_to_etcd/2]).

yml_to_etcd(File, Name)->
    {ok, Document} = read_yml_file(File),
    %io:format("~p~n", [Document]),
    im_etcd:save_config(Name, Document).

read_yml_file(File)->
    case p1_yaml:decode_from_file(File, [plain_as_atom]) of
        {ok, []} ->
            {ok, []};
        {ok, [Document|_]} ->
            {ok, Document};
        {error, Err} ->
            {error, p1_yaml:format_error(Err)}
    end.
