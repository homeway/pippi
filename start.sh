erl -setcookie gdjy -name investor@10.0.11.68 -sync executable notification_center -pa deps/cowboy/ebin -pa deps/cowlib/ebin -pa deps/ranch/ebin -pa deps/jiffy/ebin -pa ebin
# [application:start(A) || A <- [crypto, cowlib, ranch, cowboy, websocket]].