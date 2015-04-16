%% -*- mode: nitrogen -*-
-module(pp_rabbit_lib).
-export([connect/0, connect/1, disconnect/1, channel/1, close_channel/1,
    queue_declare/2, queue_declare/3, queue_declare_exclusive/1,
    queue_bind/3, queue_bind/4, 
    exchange_declare/2, basic_publish/4, basic_publish/5,
    basic_qos/2, ack/2, basic_consume/2, basic_consume/3, basic_consume_ack/2, basic_consume_ack/3,
    got_msg/0, got_msg/1,
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

queue_declare_exclusive({?MODULE, Channel}) ->
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

queue_bind(Exchange, Queue, {?MODULE, Channel}) ->
    amqp_channel:call(Channel, #'queue.bind'{exchange = pp:to_binary(Exchange),
        queue = pp:to_binary(Queue)}).

queue_bind(Exchange, BindingKey, Queue, {?MODULE, Channel}) ->
    amqp_channel:call(Channel, #'queue.bind'{exchange = pp:to_binary(Exchange),
        routing_key = list_to_binary(BindingKey),
        queue = Queue}).

exchange_declare(Exchange, {?MODULE, Channel}) ->
    amqp_channel:call(Channel, #'exchange.declare'{exchange = pp:to_binary(Exchange),
        type = pp:to_binary(Exchange)}).

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

got_msg() -> got_msg(100).
got_msg(Timeout) ->
    receive
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Body}} ->
            {Tag, jiffy:decode(Body)};
        Reason -> Reason
    after Timeout -> none end.

%% rpc call > Resp = Ch:rpc_call(RpcName, Param)
%% RpcName : to be binary
%% Param : to be json string binary
rpc_call(RpcName, Param, Ch) ->
    Pid = self(),
    Ref = make_ref(),
    spawn_link(fun() ->
        %% accept response queue
        Queue = Ch:queue_declare_exclusive(),
        % pp:display({rpc_call, Queue}),
        %% return message to self()
        % Ch:basic_consume(Queue),
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

%% rpc call by direct queue/routing_key
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
            props = #'P_basic'{reply_to = ReplyTo}}} ->  % pp:display({replyto, ReplyTo}),
            Result = try apply(Fun, jiffy:decode(Body)) of
                R -> R
            catch
                _:E -> {error, E}
            end,
            Ch:basic_publish(<<"">>, ReplyTo, pp:confirm_json(Result)),
            Ch:ack(Tag),                                 % pp:display({tag, Tag}),

            rpc_handle(Fun, Ch);
        Reason -> % pp:display({reason, Reason}),
            Reason
    end.

rpc_stop(Pid, _Ch) -> Pid ! stop.


