angular
  .module('app')

  .controller('Main.Controller', function($scope, $state, ws) {
    // redirect to login when offline
    if($scope.presence != 'online') {
      $state.go('login');
    };

    $scope.$on('$stateChangeStart', 
      // redirect to login when offline
      function(event, toState, toParams, fromState, fromParams){ 
        if(toState.name != 'login' && $scope.presence != 'online') {
          console.log($scope.presence);
          console.log(toState);
          event.preventDefault();
          $state.go('login');
        }
    });

    // logout
    $scope.logout = function() {
      ws.call(['logout'], function(Resp) {
        if(Resp == 'ok') {
          $rootScope.presence = 'offline';
          console.log('logout');
          $state.go('login');
        }
      })
    }
  })
