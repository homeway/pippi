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

    .state('main', {
      templateUrl: 'views/main.html'
    })

    .state('main.contacts', {
      abstract: true,
      url: '^/contacts',
      templateUrl: 'views/contacts.html'
    })

    .state('main.contacts.list', {
      url: '/list',
      templateUrl: 'views/contacts.list.html'
    })

    .state('main.contacts.detail', {
      url: '/:id',
      templateUrl: 'views/contacts.detail.html'
    })  
});
