#!/bin/bash
#DEPS=$(ls -d deps/*/ebin |xargs)
NAME="mytest@127.0.0.1"
SYNC="executable notification_center"
START="auto_test"
COOKIE="mytest"
CMD="erl -pa deps/*/ebin ebin tests/ebin tests/amqp_client tests/rabbit_common tests/amqp_client/ebin tests/rabbit_common/ebin -setcookie $COOKIE -name $NAME -sync $SYNC -s $START"

$CMD
