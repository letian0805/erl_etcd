#!/bin/sh
erl -sname etcd1-cli -pa ebin deps/jsx/ebin deps/p1_yaml/ebin/ deps/p1_utils/ebin/ -s etcd
