-module(etcd_watcher).

-behavior(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export([start_link/3, start/2, stop/1]).

-record(state, {name, key, module, listener}).

start(Key, Listener)->
    ProcName = get_proc_name(Key),
    ChildSpec = {ProcName, {?MODULE, start_link, [ProcName, Key, Listener]},
                 permanent, 5000, worker, [?MODULE]},
    io:format("-------start~n"),
    supervisor:start_child(etcd_sup, ChildSpec).

start_link(ProcName, Key, Listener)->
    io:format("-------start_link~n"),
    gen_server:start_link({local, ProcName}, ?MODULE, [ProcName, Key, Listener], []).

stop(Key)->
    ProcName = get_proc_name(Key),
    gen_server:call(ProcName, stop).

init([ProcName, Key, Listener])->
    httpc:set_options([{pipeline_timeout, 0}]),
    io:format("-------init~n"),
    gen_server:cast(ProcName, listen),
    {ok, #state{name = ProcName, key = Key, listener = Listener}}.

handle_call(stop, _From, State)->
    {stop, normal, ok,  State};

handle_call(_Data, _From, State)->
    {reply, ok, State}.

handle_cast(listen, #state{name = Proc, key = Key, listener = Listener} = State)->
    io:format("-------listen~n"),
    C = etcd_opt:listen_config(Key, Listener),
    spawn(fun()->
                  Listener(Key, C),
                  io:format("----------- ~p~n", [Key])
          end),
    gen_server:cast(Proc, listen),
    {noreply, State};

handle_cast(_Data, State)->
    {noreply, State}.

handle_info(_Info, State)->
    {noreply, State}.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.

terminate(_Reason, _State)->
    ok.

get_proc_name(Key)->
    BK = atom_to_binary(Key, latin1),
    NewBK = binary:replace(BK, <<"/">>, <<"_">>, [global]),
    Module = atom_to_binary(?MODULE, latin1),
    ProcName = <<Module/binary, "_", NewBK/binary>>,
    binary_to_atom(ProcName, latin1).
