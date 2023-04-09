#!/bin/bash
module=$1 && shift
action=$1 && shift
test -z "$action" && { echo "not enough arguments given" 1>&2; exit 1; }
erl -noshell -s $module $action -s init stop $@
