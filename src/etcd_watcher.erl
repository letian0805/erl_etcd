-module(etcd_watcher).

-behavior(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export([start_link/3, start/2, stop/1]).

-record(state, {name, key, module, receiver}).

start(Key, Receiver)->
    ProcName = get_proc_name(Key),
    ChildSpec = {ProcName, {?MODULE, start_link, [ProcName, Key, Receiver]},
                 permanent, 5000, worker, [?MODULE]},
    io:format("-------start~n"),
    supervisor:start_child(etcd_sup, ChildSpec).

start_link(ProcName, Key, Receiver)->
    io:format("-------start_link~n"),
    gen_server:start_link({local, ProcName}, ?MODULE, [ProcName, Key, Receiver], []).

stop(Key)->
    ProcName = get_proc_name(Key),
    gen_server:call(ProcName, stop).

init([ProcName, Key, Receiver])->
    io:format("-------init~n"),
    gen_server:cast(ProcName, watch),
    {ok, #state{name = ProcName, key = Key, receiver = Receiver}}.

handle_call(stop, _From, State)->
    {stop, normal, ok,  State};

handle_call(_Data, _From, State)->
    {reply, ok, State}.

handle_cast(watch, #state{name = Proc, key = Key, receiver = Receiver} = State)->
    io:format("-------watch~n"),
    do_watch(Proc, Key, Receiver),
    {noreply, State};

handle_cast(_Data, State)->
    {noreply, State}.

handle_info(_Info, State)->
    {noreply, State}.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.

terminate(_Reason, _State)->
    ok.

do_watch(Proc, Key, Receiver)->
    R = fun(K, V)->
                Receiver(K, V),
                gen_server:cast(Proc, watch)
        end,
    etcd_opt:watch_config(Key, R).

get_proc_name(Key)->
    BK = atom_to_binary(Key, latin1),
    NewBK = binary:replace(BK, <<"/">>, <<"_">>, [global]),
    Module = atom_to_binary(?MODULE, latin1),
    ProcName = <<Module/binary, "_", NewBK/binary>>,
    binary_to_atom(ProcName, latin1).
