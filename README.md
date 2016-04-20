###etcd data format
- key
- value
- dir

###ejabberd.yml file
- in ejabberd.yml, the type of key is atom, string or integer. the type of value is atom, string(no $), string(with $), integer, bool, struct, list.

###etcd main format
to convert etcd data to erlang data, we use json as the main format of key and value of etcd

###etcd key to erlang key
- to erlang atom

        etcd:
            muc_room
            
        erlang:
            muc_room
            
- to erlang binary (string is formatted to binary in erlang)

        etcd:
            $muc_room

        erlang:
            <<"muc_room">>
            
- to erlang int

        etcd:
            10
            
        erlang:
            10
        
###etcd value to erlang value
    etcd use json as the format of value, use $ to mark binary of erlang

###etcd dir to erlang data
- to erlang plist

        etcd(dir with key-value):
            muc_room:
                muc_presence: true
                muc_presence_async: false

        erlang:
            {muc_room, [{muc_presence, true}, {muc_presence_async, false}]}
            
        erlang code:
            get(K, dir)->
                Res = proccess_node(K),
                lists:map(fun({K, V}->
                                {decode_key(K, V), decode_value(V)}
                            end, Res);

###DO NOT USE THESE SYMBOL IN KEY
        /   \   ,   +   %   ;
        and other symbol which is specific to URL

###compare with using $ for atom
- using $ for atom can cause a problem when encoding atom and binary with $.
        example: foo is encoded to "$foo", but <<"$foo">> is encoded to "$foo" too.
        so, we use $ for binary only, and forbidden using the $ for atom.
