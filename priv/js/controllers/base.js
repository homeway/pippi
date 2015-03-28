angular
  .module('app')

  .controller('Base.Controller', function($rootScope, $scope, $state, ws) {
    $rootScope.presence = 'offline';
    ws.connect();
    ws.onMessage('presence', function(e) {
      if(e.data[0] == 'offline') {
        console.log(e.data[0]);
        $rootScope.presence = 'offline';
        $state.go('login');
      }
      else {
        console.log(e.data);
      }
    });
    ws.onClose(function() {
      $rootScope.presence = 'offline';
      $state.go('login');
    });
  })
