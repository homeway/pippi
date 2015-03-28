angular
  .module('app')

  .controller('Base.Controller', function($scope, $state, ws) {
    $scope.presence = 'offline';
    ws.connect();
    ws.onMessage('presence', function(e) {
      if(e.data[0] == 'offline') {
      $scope.presence = 'offline';
        console.log(e.data[0]);
        $state.go('login');
      }
      else {
        console.log(e.data);
      }
    });
    ws.onClose(function() {
      $scope.presence = 'offline';
      $state.go('login');
    });
  })
