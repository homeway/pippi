#!/bin/bash
DEPS=$(ls -d ../deps/*/ebin |xargs)
NAME="mytest@127.0.0.1"
SYNC="executable notification_center"
START="auto_test"
COOKIE="mytest"
CMD="erl -pa $DEPS -pa ../ebin -pa ebin -setcookie $COOKIE -name $NAME -sync $SYNC -s $START"

$CMD