## json to erlang data
### 1. atom key with atom value
   - json:   {'$foo':'bar'}
   - erlang: [{foo, bar}]

### 2. string key with string value
   - json: {'"foo"': 'bar'}
   - erlang: [{<<"foo">>, <<"bar">>}]

### 3. string key with atom value
   - json: {'$"foo"': 'bar'}
   - erlang: [{<<"foo">>, bar}]

### 4. atom key with string array
   - json: {'foo': ['bar 1', 'bar 2']}
   - erlang: [{foo, [<<"bar 1">>, <<"bar 2">>]}]

### 5. atom key with object array
   - json: {'foo': [{'a':'b'}, {'$c':'d'}]}
   - erlang: [{foo, [[{a, <<"b">>}], [{c, d}]]}]


## api of im_etcd
### 1. put
   - put single value as string when the directory not exists
     <pre><code>  im_etcd:put(<<"/'test'/'foo'">>, <<"'bar'">>).</code></pre>
     
     this will create the directory 'test' automatically and set the value 'bar' for key 'foo' to etcd
     
   - put single value as atom
    <pre><code>  im_etcd:put(<<"'$foo'">>, <<"'bar'">>). </code></pre>
    
    this will add an atom key-value to etcd
    
   - put string list
    <pre><code>  im_etcd:put(<<"'foo'">>, <<"['a', 'b']">>). </code></pre>
    
    this whill add string list for key 'foo' to etcd
    
   - put object
    <pre><code>  im_etcd:put(<<"'foo'">>, <<"{'a':'b', '$c':'d', '\"e\"':'f'}">>). </code></pre>
    
    this will add an object for key 'foo' to etcd
    
   - put object list
    <pre><code>  im_etcd:put(<<"'foo'">>, <<"[{'a':'b'}, {'c':'d'}]">>). </code></pre>
    
    this will add an object list for key 'foo' to etcd
    
### 2. get
   - get whole key-value of directory or get value of key
    <pre><code>  im_etcd:get(<<"/'foo'">>). </pre></code>
