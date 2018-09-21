%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc Interface to AMQP client library
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(amqp_interface).

-include_lib("amqp_client/include/amqp_client.hrl").

-compile([{parse_transform, lager_transform}]).

-export([connect/1, publish/3, consume/2, handle/3]).

connect(Creds) ->
    Params = #amqp_params_network{
                  username = maps:get(user, Creds),
				  password = maps:get(pass, Creds),
				  virtual_host = <<"/cvmfs">>,
				  host = binary_to_list(maps:get(url, Creds)),
				  port = maps:get(port, Creds),
				  channel_max = 2047, frame_max = 0,
				  heartbeat = 30},

    {ok, Connection} = amqp_connection:start(Params),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    Exchange = maps:get(exchange, Creds),

    #{connection => Connection,
      channel => Channel,
      exchange => Exchange}.


publish(Repo, Msg, ConnectionState) ->
    #{channel := Channel,
      exchange := Exchange} = ConnectionState,
    Publish = #'basic.publish'{exchange = Exchange,
                               routing_key = Repo},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Msg}).


consume(ConnectionState, Repo) ->
    #{channel := Channel, exchange := Exchange} = ConnectionState,
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel,
                                                           #'queue.declare'{exclusive=true}),
    Binding = #'queue.bind'{
        queue       = Queue,
        exchange    = Exchange,
        routing_key = Repo
    },
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),

    Sub = #'basic.consume'{queue = Queue},
    #'basic.consume_ok'{consumer_tag = _Tag} = amqp_channel:subscribe(Channel, Sub, self()).


handle(Msg, ConnectionState, Reaction) ->
    #{repo_name := Repo, channel := Channel} = ConnectionState,
    case Msg of
        #'basic.consume_ok'{} ->
            lager:debug("Subscription confirmed for repo: ~p", [Repo]);
        #'basic.cancel_ok'{} ->
            lager:debug("Subscription cancelled for repo: ~p", [Repo]);
        {#'basic.deliver'{delivery_tag = Tag}, Body} ->
            #amqp_msg{payload = Payload} = Body,
            amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
            lager:debug("Back-end message received for repo: ~p, msg: ~p",
                        [Repo, Payload]),
            Reaction(Repo, Payload)
    end,
    ok.