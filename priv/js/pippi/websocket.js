angular.module('pippi.websocket', [])

.factory('ws', function() {
    var dataMap = {wsHost: 'ws://localhost:8080/ws'};
    var msgQueue = [];
    var callQueue = [];
    var callSeq = 0;

    var fire = function(eventType, event) {
      // console.log(dataMap);
      console.log(eventType + ":" + event.data);
      if (dataMap[eventType]) {
        for (var i = 0; i < dataMap[eventType].length; i++) {
          dataMap[eventType][i](event);
        }
      }
    };
    var add_to = function(eventType, handler) {
      //multiple event listener
      if (!dataMap[eventType]) {
        dataMap[eventType] = [];
      };
      dataMap[eventType].push(handler);
      // console.log(dataMap);
    };

    var is_onnected = function() {
      if (dataMap.websocket) {
        return dataMap.websocket.readyState ==  dataMap.websocket.OPEN;
      }
      else {
        return false
      }      
    };

    var confirm_connect = function() {
      if(!is_onnected() && dataMap.wsHost) {
        var websocket = new WebSocket(dataMap.wsHost);
        websocket.onopen = function(evt) {
          while(msgQueue.length > 0) {
            websocket.send(msgQueue.pop())
          };
          fire("onOpen", evt)
        };
        websocket.onclose   = function(evt) { fire("onClose", evt) };
        websocket.onerror   = function(evt) { fire("onError", evt) };
        websocket.onmessage = function(evt) {
          var Res = JSON.parse(evt.data);
          if(Res.length > 1) {
            fire("onMessage."+Res[0], {data: Res})
          }
          else {
            fire("onMessage", evt)
          }
        };
        dataMap['websocket']  = websocket;
      }
    };

    var Methods = {
      connect : function() {
        confirm_connect();
      },
      reconnect : function(wsHost) {
        if (dataMap.websocket) {
          dataMap.websocket.close();
        };
        connect(wsHost);
      },
      close : function() {
        if (dataMap.websocket) {
          dataMap.websocket.close();
        }
      },

      onOpen    : function(handler) { add_to("onOpen", handler) },
      onClose   : function(handler) { add_to("onClose", handler) },
      onError   : function(handler) { add_to("onError", handler) },
      onMessage : function(Cmd, handler) { add_to("onMessage."+Cmd, handler) },

      send : function(Msg) {
        confirm_connect();
        if(is_onnected()) {
          dataMap.websocket.send(Msg);
        }
        else {
          msgQueue.push(Msg);
        }
      },

      call : function(Cmd, Func) {
        confirm_connect();
        if(is_onnected()) {
          callQueue.push(Func);
          dataMap.websocket.send(JSON.stringify(['call', callSeq++, Cmd]));
        }
      },

      isConnect : function() { is_onnected() },

      clean : function() {
        if (dataMap.websocket) {
          dataMap.websocket.close();
        }
        dataMap = {};
      }
    };
    return Methods;
});
