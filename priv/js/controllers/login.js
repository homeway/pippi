angular
  .module('app')

  .controller('LoginController', function($scope, $location, ws) {
    $scope.user = "adi";
    $scope.pass = "123";

    $scope.login = function() {
      ws.call(['login', [$scope.user, $scope.pass]], function(Resp) {
        if(Resp=='ok') {
          console.log('login success!');
          $scope.$apply(function() {
            $location.path('/contacts/list');
          })
        }
        else if(Resp[0] == 'error' && Resp[1] == 'online') {
          console.log('already login!');
          $scope.$apply(function() {
            $location.path('/contacts/list');
          })
        }
        else{
          console.log(Resp);
        }
      });
    }
  })
