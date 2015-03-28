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
      templateUrl: 'views/login.html'
    })

    .state('contacts', {
      abstract: true,
      url: '/contacts',
      templateUrl: 'views/contacts.html'
    })

    .state('contacts.list', {
      url: '/list',
      templateUrl: 'views/contacts.list.html'
    })

    .state('contacts.detail', {
      url: '/:id',
      templateUrl: 'views/contacts.detail.html'
    })  
});
