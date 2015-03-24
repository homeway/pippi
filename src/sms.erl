-module(sms).
-export([send/2, send_multi/2, status/1]).

gate() -> {smsBox, 'java_sendSms_service@10.0.11.67'}.

send(Contact, Sms) ->
  send_multi(sendSingle, [Contact], Sms).

send_multi(Contacts, Sms) ->
  send_multi(sendMulti, Contacts, Sms).

send_multi(Mode, Contacts, Sms) when is_list(Contacts) ->
  BatchId = pp:uuid(),
  M = #{
    busi => Mode,
    data => #{
      list => [#{
        tel => pp:to_binary(To),
        msg => pp:to_binary(Sms)} || To <- Contacts
      ],
      batch => BatchId
    }
  },
  % io:format("send_multi, batchId: ~ts\n", [BatchId]),
  % pp:display(M),
  gate() ! {self(), jiffy:encode(M)},
  receive {_From, {Resp, BatchId}} ->
    io:format("~ts\n", [Resp]),
    case jiffy:decode(Resp, [return_maps]) of
      #{<<"code">> := <<"200">>} -> {ok, BatchId};
      Error -> {error, Error}
    end
  after 30000 ->
    % mock gate
    % {ok, <<"MockBatchId">>} end.
    io:format("timeout\n"),
    {error, timeout} end.

status(BatchId) ->
  status(BatchId, 10).
status(BatchId, Times) ->
  receive undefined -> ok after 3000 -> ok end,
  erlang:display({find_sms_gate_status, Times, times_left}),
  M = #{
    % busi => findReports,
    busi => findResps,
    data => #{
      batch => BatchId
    }
  },
  gate() ! {self(), jiffy:encode(M)},
  receive {_From, {Resp, BatchId}} ->
    io:format("~ts\n", [Resp]),
    case jiffy:decode(Resp, [return_maps]) of
      #{<<"code">> := <<"200">>, <<"data">> := Data} ->
        if
          length(Data) > 0 -> {ok, Data};
          Times > 0 -> status(BatchId, Times - 1)
        end;
      Error -> {error, Error}
    end
  after 10000 ->
    % {ok, <<"MockBatchId">>} end.
    io:format("timeout\n"),
    {error, timeout} end.
