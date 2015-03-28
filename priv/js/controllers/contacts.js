angular
  .module('app')

  .controller('Contacts.Controller', function($scope, ws) {
    $scope.contacts = [
      {id: "a001", name: "Alice" },
      {id: "a002", name: "Bob" }
    ];
  })

  .controller('Contacts.Detail.Controller', function($scope, $stateParams, ws) {
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
  });
