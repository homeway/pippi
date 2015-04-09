pippi
=====

Pippi is an erlang web framework with websocket. 

Pippi need R17+. 
<br>If you want to use [gut](https://github.com/unbalancedparentheses/gut), you should not use 17.3 because an exist ssl bug known.

You can use pippi to create an erlang websocket backend, and use angularJS as web client.

Follow is for pippi:
- [pippi-gutenberge-generator](https://github.com/homeway/pippi-gutenberg-generator) is a gut generator for pippi backend app
- [angular-pippi](https://github.com/homeway/angular-pippi) is an angular lib for pippi

## features
- a cowboy websocket backend
- use javascript call erlang MFA
- an account/role model to control access right
- nosqlite: an nosql database with mnesia backend

## todo
- integerate amqp
- integerate ejabberd/xmpp

## usage
### how to create an pippi backend?
We use [gut](https://github.com/unbalancedparentheses/gut), a pretty generator.

1) gut new pippi {{you_app_name}}
```
$ gut new pippi myapp
Cloning pippi hosted at https://github.com/homeway/pippi-gutenberg-generator
Please submit a github issue if you find any problem with this generator

Working dir /Users/homeway/erlang/workspace/taian_erp/backend3
* creating .gitignore
* creating README.md
* creating auto_test.sh
* creating rebar.config
...
```
2) input y if you want to auto run rebar tasks
```
The generator wants to run the following list of commands:
1. rebar g-d co
2. cd tests & rebar co & cd ..
3. chmod a+x *.sh
Are you sure you want to continue: [y/n] y

```
3) then you got a app dir and files like this:
```
- src
- tests/src
- README.md
- auto_test.sh
- run.sh
- rebar.config
```

4) run your app and test:
```
$ ./auto_test
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.3  (abort with ^G)
(erp_backend_test@127.0.0.1)1> Starting Sync (Automatic Code Compiler / Reloader)
Scanning source files...

```

### how to use angular-pippi in angular?
1) add this line to bower.json:
```
  "dependencies": {
    ...
    "angular-pippi": "~0.1.2"
  }
```

2) import pippi module in your angular project:
```
angular.module('inspinia', ['pippi'])
  .config(function ($websocketProvider) {
    ...
    $websocketProvider.setServer('ws://127.0.0.1:8080/ws')
  });
```

3) use $webservice and $auth in pippi module:
```
angular.module('inspinia')
  .controller('MainCtrl', function ($scope, $websocket, $auth) {
      $websocket.connect();
      $auth.login("adi", "123", function(Resp) {
        console.log(Resp);
      });
    });
```
