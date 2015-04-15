%% -*- mode: nitrogen -*-
-module(pp_rabbit_lib).
-export([connect/0, connect/1, disconnect/1, channel/1, close_channel/1,
    queue_declare/2, queue_declare/3, queue_declare_exclusive/1, queue_bind/3, 
    exchange_declare/2, basic_publish/4, basic_publish/5,
    basic_qos/2, basic_consume/2, basic_consume_ack/2, ack/2, 
    got_msg/0, got_msg/1]).

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
    #'queue.declare_ok'{queue = Queue} =
    amqp_channel:call(Channel, #'queue.declare'{exclusive = true}),
    Queue.

queue_declare(Queue, {?MODULE, Channel}) ->
    queue_declare(Queue, false, {?MODULE, Channel}).

queue_declare(Queue, Durable, {?MODULE, Channel}) ->
    amqp_channel:call(Channel,
        #'queue.declare'{queue = pp:to_binary(Queue), durable=Durable}).

queue_bind(Exchange, Queue, {?MODULE, Channel}) ->
    amqp_channel:call(Channel, #'queue.bind'{exchange = pp:to_binary(Exchange),
        queue = pp:to_binary(Queue)}).

exchange_declare(Exchange, {?MODULE, Channel}) ->
    amqp_channel:call(Channel, #'exchange.declare'{exchange = pp:to_binary(Exchange),
        type = pp:to_binary(Exchange)}).

basic_publish(Exchange, RoutingKey, Body, {?MODULE, Channel}) ->
    basic_publish(Exchange, RoutingKey, 2, Body, {?MODULE, Channel}).

basic_publish(Exchange, RoutingKey, DeliveryMode, Body, {?MODULE, Channel}) ->
    amqp_channel:cast(Channel,
        #'basic.publish'{
            exchange = pp:to_binary(Exchange),
            routing_key = pp:to_binary(RoutingKey)},
        #amqp_msg{props = #'P_basic'{delivery_mode = DeliveryMode},
            payload = pp:to_binary(Body)}).

basic_qos(PrefetchCount, {?MODULE, Channel}) ->
    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = PrefetchCount}).

basic_consume(RoutingKey, {?MODULE, Channel}) ->
    amqp_channel:subscribe(Channel,
        #'basic.consume'{
            queue = pp:to_binary(RoutingKey),
            no_ack = true},
        self()),
    receive
        #'basic.consume_ok'{} -> ok
    after 100 -> {error, no_consume_ok} end.

basic_consume_ack(RoutingKey, {?MODULE, Channel}) ->
    amqp_channel:subscribe(Channel,
        #'basic.consume'{
            queue = pp:to_binary(RoutingKey)},
        self()),
    receive
        #'basic.consume_ok'{} -> ok
    after 100 -> {error, no_consume_ok} end.

ack(Tag, {?MODULE, Channel}) ->
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}).

got_msg() -> got_msg(100).
got_msg(Timeout) ->
    receive
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Body}} ->
            {Tag, Body}
    after Timeout -> none end.
