#!/bin/bash
DEPS=$(ls -d deps/*/ebin |xargs)
NAME="investor@10.0.11.68"
START="auto_run"
COOKIE="gdjy"
CMD="erl -pa $DEPS -pa ebin -setcookie $COOKIE -name $NAME -s $START"

$CMD