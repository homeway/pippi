angular
  .module('app')

  .controller('Login.Controller', function($scope, $state, ws) {
    $scope.user = "adi";
    $scope.pass = "123";

    $scope.login = function() {
      ws.call(['login', [$scope.user, $scope.pass]], function(Resp) {
        if(Resp=='ok') {
          $scope.presence = 'online';
          console.log('login success!');
          $state.go('main.contacts.list');
        }
        else if(Resp[0] == 'error' && Resp[1] == 'online') {
          console.log('already login!');
          $state.go('main.contacts.list');
        }
        else{
          console.log(Resp);
        }
      });
    }
  })
