数据库创建
========
##在shell中建立mnesia数据库
```
    mnesia:create_schema([node()]).
    mnesia:start().
    res_account:init_tables().
    res_sms_records:init_tables().
    mnesia:stop().
```

##git 安装脚本
```
$ git clone homeway@vvstyle.com:/home/git/projects/gdjy/mysample.git
```