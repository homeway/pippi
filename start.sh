erl -sync executable notification_center -pa deps/cowboy/ebin -pa deps/cowlib/ebin -pa deps/ranch/ebin -pa ebin
# [application:start(A) || A <- [crypto, cowlib, ranch, cowboy, websocket]].