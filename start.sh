#!/bin/bash
DEPS=$(ls -d deps/*/ebin |xargs)
NAME="investor@10.0.11.68"
SYNC="executable notification_center"
START="auto_run"
COOKIE="gdjy"
CMD="erl -pa $DEPS -pa ebin -setcookie $COOKIE -name $NAME -sync $SYNC -s $START"

$CMD