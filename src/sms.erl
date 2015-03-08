-module(sms).
-export([send/2, send_multi/2, status/1]).

gate() -> {smsBox, 'java_sendSms_service@10.0.11.67'}.

send(Contact, Sms) ->
  send_multi(sendSingle, [Contact], Sms).

send_multi(Contacts, Sms) ->
  send_multi(sendMulti, Contacts, Sms).

send_multi(Mode, Contacts, Sms) when is_list(Contacts) ->
  Batch = uuid(),
  M = #{
    busi => Mode,
    data => #{
      list => [ #{tel => ss_convert:to_binary(To), msg => ss_convert:to_binary(Sms)} || To <- Contacts ],
      batch => uuid()
    }
  },
  erlang:display(M),
  gate() ! {self(), jiffy:encode(M)},
  receive {_From, Resp} ->
    io:format("~ts\n", [Resp]),
    case jiffy:decode(Resp, [return_maps]) of
      #{<<"code">> := <<"200">>} -> {ok, Batch};
      Error -> {error, Error}
    end
  after 1000 ->
    io:format("timeout\n"),
    {error, timeout} end.

status(Batch) ->
  M = #{
    busi => findReports,
    data => #{
      batch => Batch
    }
  },
  gate() ! {self(), jiffy:encode(M)},
  receive {_From, Resp} ->
    io:format("~ts\n", [Resp]),
    case jiffy:decode(Resp, [return_maps]) of
      #{<<"code">> := <<"200">>} -> {ok, Batch};
      Error -> {error, Error}
    end
  after 3000 ->
    io:format("timeout\n"),
    {error, timeout} end.

uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:rand_bytes(16),
    Str = io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b", 
                        [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]),
    list_to_binary(Str).