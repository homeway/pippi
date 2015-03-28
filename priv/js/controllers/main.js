angular
  .module('app')

  .controller('MainController', function($scope, $location, ws) {
    ws.connect();
    ws.onMessage('presence', function(e) {
      if(e.data[0] == 'offline') {
        console.log(e.data[0]);
        $scope.$apply(function() {
          $location.path('/login');
        })
      }
      else {
        console.log(e.data);
      }
    });
  })
