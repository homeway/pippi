angular.module('app', [
  'ui.router',                    // Routing
  'pippi.websocket'
])

.config(function($stateProvider, $urlRouterProvider) {
  $urlRouterProvider.otherwise("/login");

  $stateProvider
    .state('login', {
      // abstract: true,
      url: '/login',
      templateUrl: 'views/login.html',
      controller: function($scope, $window, ws){
        ws.connect();
        ws.send(JSON.stringify(['account', 'login', {user: 'adi', pass: '123'}]));
        $scope.login = function() {
          console.log($scope.user + "/" + $scope.pass)
        }
      }

    })

    .state('contacts', {
      abstract: true,
      url: '/contacts',
      templateUrl: 'views/contacts.html',
      controller: function($scope){
        $scope.contacts = [
          {id: "a001", name: "Alice" },
          {id: "a002", name: "Bob" }
        ];
      }
    })

    .state('contacts.list', {
      url: '/list',
      templateUrl: 'views/contacts.list.html'
    })

    .state('contacts.detail', {
      url: '/:id',
      templateUrl: 'views/contacts.detail.html',
      controller: function($scope, $stateParams){
        var get_item = function() {
          var Items = $scope.contacts;
          var Id = $stateParams.id;
          for(var i=0; i<Items.length; i++) {
            if(Items[i].id==Id){
              return Items[i];
            }
          }
          return undefined;
        }
        $scope.myperson = get_item();
      }
    })  
});