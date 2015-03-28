angular
  .module('app')

  .controller('Main.Controller', function($scope, $state, ws) {
    $scope.logout = function() {
      ws.call(['logout'], function(Resp) {
        if(Resp == 'ok') {
          presence = 'offline';
          console.log('logout');
          $state.go('login');
        }
      })
    }
  })
