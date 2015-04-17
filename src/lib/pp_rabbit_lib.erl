%% -*- mode: nitrogen -*-
-module(pp_rabbit_lib).
-export([connect/0, connect/1, disconnect/1, channel/1, close_channel/1,
    queue_delete/2, exchange_delete/2,
    queue_declare/1, queue_declare/2, queue_declare/3,
    queue_bind/3, queue_bind/4,
    exchange_declare/3, basic_publish/4, basic_publish/5,
    basic_qos/2, ack/2, basic_consume/2, basic_consume/3, basic_consume_ack/2, basic_consume_ack/3,
    got_msg/0, got_msg/1,
    service_call/3, service_call/4, service_reg/6,
    rpc_call/3, rpc_reg/3, rpc_stop/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

connect() -> connect("localhost").
connect(Server) ->
    {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = Server}),
    {?MODULE, Connection}.

channel({?MODULE, Connection}) ->
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {?MODULE, Channel}.

disconnect({?MODULE, Connection}) ->
    ok = amqp_connection:close(Connection).

close_channel({?MODULE, Channel}) ->
    ok = amqp_channel:close(Channel).

queue_declare({?MODULE, Channel}) ->
    case amqp_channel:call(Channel, #'queue.declare'{exclusive = true}) of
        #'queue.declare_ok'{queue = Queue} -> Queue;
        Reason -> Reason
    end.

queue_declare(Queue, Ch) ->
    Ch:queue_declare(Queue, false).

queue_declare(Queue, Durable, {?MODULE, Channel}) ->
    case amqp_channel:call(Channel, #'queue.declare'{queue = pp:to_binary(Queue), durable=Durable}) of
        {'queue.declare_ok', Queue1 , _, _} -> Queue1;
        Reason -> Reason
    end.

queue_delete(Queue, {?MODULE, Channel}) ->
    Delete = #'queue.delete'{queue = pp:to_binary(Queue)},
    #'queue.delete_ok'{} = amqp_channel:call(Channel, Delete).

queue_bind(Exchange, Queue, {?MODULE, Channel}) ->
    amqp_channel:call(Channel, #'queue.bind'{exchange = pp:to_binary(Exchange),
        queue = pp:to_binary(Queue)}).

queue_bind(Exchange, BindingKey, Queue, {?MODULE, Channel}) ->
    amqp_channel:call(Channel, #'queue.bind'{exchange = pp:to_binary(Exchange),
        routing_key = list_to_binary(BindingKey),
        queue = Queue}).

exchange_declare(Exchange, Type, {?MODULE, Channel}) ->
    {'exchange.declare_ok'} =
    amqp_channel:call(Channel, #'exchange.declare'{
        exchange = pp:to_binary(Exchange),
        type = pp:to_binary(Type)}).

exchange_delete(Exchange, {?MODULE, Channel}) ->
    Delete = #'exchange.delete'{exchange = pp:to_binary(Exchange)},
    #'exchange.delete_ok'{} = amqp_channel:call(Channel, Delete).

%% default delivery as persit mode(2)
basic_publish(Exchange, RoutingKey, Body, Ch) ->
    Ch:basic_publish(Exchange, RoutingKey, #{}, Body).

%% rpc call > Ch:basic_publish(Ex, RoutingKey, #{reply_to=>Q, id=>Id}, Body)
basic_publish(Exchange, RoutingKey, Props, Body, {?MODULE, Channel}) ->
    amqp_channel:cast(Channel,
        #'basic.publish'{
            exchange = pp:to_binary(Exchange),
            routing_key = pp:to_binary(RoutingKey)},
        #amqp_msg{props = #'P_basic'{
            delivery_mode = maps:get(delivery_mode, Props, 2),
            reply_to = maps:get(reply_to, Props, <<"">>),
            correlation_id = maps:get(id, Props, <<"">>)},
            payload = jiffy:encode(Body)}).

basic_qos(Options, {?MODULE, Channel}) ->
    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = maps:get(prefetch_count, Options, 1)}).

basic_consume(RoutingKey, Ch) ->
    Ch:basic_consume(RoutingKey, self()).

basic_consume(RoutingKey, From, {?MODULE, Channel}) ->
    amqp_channel:subscribe(Channel,
        #'basic.consume'{
            queue = pp:to_binary(RoutingKey),
            no_ack = true},
        From),
    receive
        #'basic.consume_ok'{} -> ok
    after 100 -> {error, no_consume_ok} end.

basic_consume_ack(RoutingKey, Ch) ->
    Ch:basic_consume_ack(RoutingKey, self()).

basic_consume_ack(RoutingKey, From, {?MODULE, Channel}) ->
    amqp_channel:subscribe(Channel,
        #'basic.consume'{
            queue = pp:to_binary(RoutingKey)},
        From),
    receive
        #'basic.consume_ok'{} -> ok
    after 100 -> {error, no_consume_ok} end.

ack(Tag, {?MODULE, Channel}) ->
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}).

got_msg() -> got_msg(3000).
got_msg(Timeout) ->
    receive
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Body}} ->
            {Tag, jiffy:decode(Body)};
        Reason -> Reason
    after Timeout ->
        % io:format("timeout to got_msg~n"),
        none end.

%% RpcFun as RoutingKey
%%
%% rpc call > Resp = Ch:rpc_call(RpcFun, Param)
%% RpcFun : to be binary
%% Param : to be json string binary
%%
%% rpc call > Resp = Ch:rpc_call("users", "index", 1)
%% rpc call > Resp = Ch:rpc_call("users", "create", ["001", #{name=>"adi"}])
%% rpc call > Resp = Ch:rpc_call("users", "read", "001")
%% rpc call > Resp = Ch:rpc_call("users", "update", ["001", #{age=>"10"}])
%% rpc call > Resp = Ch:rpc_call("users", "delete", "001")
rpc_call(RpcName, Param, Ch) ->
    Pid = self(),
    Ref = make_ref(),
    spawn_link(fun() ->
        %% accept response queue
        Queue = Ch:queue_declare(),
        % pp:display({rpc_call, Queue}),
        %% return message to self()
        Ch:basic_consume(Queue),
        %% publish call
        Ch:basic_publish(<<"">>, RpcName, #{reply_to=>Queue}, Param),
        %% wait rpc server for 1 secs
        %% client can do with it manual for more time waiting
        Pid ! {Ref, got_msg(1000)}
    end),
    receive
        {Ref, Msg} -> Msg
    after 10000 -> rpc_timeout end.

%% routing_key call by direct,fanout or topic
%% amqp can use many service for a special exchange-routing
%% so service_call cannot use rpc mode, it only acceept async respond
%%
service_call(Exchange, Param, Ch) ->
    Ch:basic_publish(Exchange, <<"">>, Param).

service_call(Exchange, RoutingKey, Param, Ch) ->
    Ch:basic_publish(Exchange, RoutingKey, Param).

%% simple queue
rpc_reg(RpcName, Fun, Ch) when is_function(Fun) ->
    Pid = spawn_link(fun() ->
        %% register a rpc call as routing_key
        Queue = Ch:queue_declare(RpcName),
        Ch:basic_qos(#{}),
        Ch:basic_consume_ack(Queue),
        rpc_handle(Fun, Ch)
    end),
    Pid.

%% handle loop
rpc_handle(Fun, Ch) ->
    receive
        stop -> ok;
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{
            payload = Body,
            props = #'P_basic'{reply_to = ReplyTo}}} ->

            spawn_link(fun() ->
                Result = try apply(Fun, jiffy:decode(Body)) of
                    R -> R
                catch
                    _:E -> {error, E}
                end,
                Ch:basic_publish(<<"">>, ReplyTo, jiffy:encode(pp:confirm_json(Result))),
                Ch:ack(Tag)
            end),
            rpc_handle(Fun, Ch);
        Reason ->
            Reason
    end.

%% Type :: direct | fanout | topic
service_reg(Exchange, RoutingKey, Type, ServiceFun, CallFun, Ch)
  when is_function(ServiceFun) and is_function(CallFun) ->

    Pid = spawn_link(fun() ->
        %% register a rpc call as routing_key
        Ch:exchange_declare(Exchange, Type),
        Queue = Ch:queue_declare(),
        case Type of
            fanout -> Ch:queue_bind(Exchange, Queue);
            direct -> Ch:queue_bind(Exchange, RoutingKey, Queue);
            topic  -> Ch:queue_bind(Exchange, RoutingKey, Queue)
        end,
        Ch:basic_qos(#{}),
        Ch:basic_consume(Queue),
        service_handle(ServiceFun, CallFun, Ch)
    end),
    Pid.

%% handle loop
service_handle(ServiceFun, CallFun, Ch) ->
    receive
        stop -> ok;
        {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
            spawn_link(fun() ->
                Result = try apply(ServiceFun, jiffy:decode(Body)) of
                    R -> R
                catch
                    _:E -> {error, E}
                end,
                CallFun(Result)
            end),
            service_handle(ServiceFun, CallFun, Ch);
        Reason ->
            Reason
    end.

rpc_stop(Pid, _Ch) -> Pid ! stop.
