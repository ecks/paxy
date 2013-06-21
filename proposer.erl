-module(proposer).
-export([start/4]).

-define(timeout, 2000).
-define(backoff, 10).
-define(delay, 20).

start(Name, Proposals, Acceptors, Seed) ->
  spawn(fun() -> init(Name, Proposals, Acceptors, Seed) end).

init(Name, Proposals, Acceptors, Seed) ->
  random:seed(Seed, Seed, Seed),
  Round = order:null(Name),
  Decided = Round,
  round(Name, ?backoff, Round, Proposals, Acceptors, Decided).

round(_, _, _, [], _, _) ->
  ok;
round(Name, Backoff, Round, [Proposal|Proposals], Acceptors, Decided) ->
  io:format("Started Round ~w~n", [Round]),
  case ballot(Round, Proposal, Acceptors, Decided) of
    {ok, Decision} ->
      {Round_Num,_} = Round,
      io:format("~w decided ~w in round ~w~n", [Name, Decision, Round_Num]),
      Next = order:inc(Round),
      round(Name, Backoff, Next, Proposals, Acceptors, Round);
    abort ->
      timer:sleep(random:uniform(?delay)),
      Next = order:inc(Round),
      round(Name, (2*Backoff), Next, Proposal, Acceptors, Decided)
  end.

ballot(Round, Proposal, Acceptors, Decided) ->
  prepare(Round, Acceptors),
  Quorum = (length(Acceptors) div 2)+1,
  Max = Decided,
  case collect(Quorum, Round, Max, Proposal) of
    {accepted, Value} ->
      {Round_Num, Proposer_Name} = Round,
      io:format("~w collected ~w on round ~w~n", [Proposer_Name, Value, Round_Num]),
      accept(Round, Value, Acceptors),
      case vote(Quorum, Round) of
        ok ->
          {ok, Value};
        abort ->
          abort
      end;
    abort ->
      abort
  end.

collect(0, _, _, Proposal) ->
  {accepted, Proposal};
collect(N, Round, Max, Proposal) ->
  receive
    {promise, Round, _, na} ->
      {Round_Num, Proposer_Name} = Round,
      io:format("~w: na promise on round ~w, waiting on ~w acceptors~n", [Proposer_Name, Round_Num, N-1]),
      collect(N-1, Round, Max, Proposal);
    {promise, Round, Voted, Value} ->
      {Round_Num, Proposer_Name} = Round,
      io:format("~w: (~w,~w) promise on round ~w, waiting on ~w acceptors~n", [Proposer_Name, Voted, Value, Round_Num, N-1]),
      case order:gr(Voted, Max) of
        true ->
          collect(N-1, Round, Voted, Value);
        false ->
          collect(N-1, Round, Max, Proposal)
      end;
    {promise, _, _, _} ->
      collect(N, Round, Max, Proposal);
    {sorry, Round} ->
      {Round_Num, Proposer_Name} = Round,
      io:format("~w: sorry on round ~w~n", [Proposer_Name, Round_Num]),
      collect(N, Round, Max, Proposal);
    {sorry, _} ->
      collect(N, Round, Max, Proposal)    
  after ?timeout ->
    abort
  end.

vote(0, _) ->
  ok;
vote(N, Round) ->
  receive
    {vote, Round} ->
      vote(N-1, Round);
    {vote, _} ->
      vote(N, Round);
    {sorry, Round} ->
      vote(N, Round);
    {sorry, _} ->
      vote(N, Round)
  after ?timeout ->
      abort
  end.

prepare(Round, Acceptors) ->
  Fun = fun(Acceptor) -> send(Acceptor, {prepare, self(), Round}) end,
  lists:map(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
  Fun = fun(Acceptor) -> send(Acceptor, {accept, self(), Round, Proposal}) end,
  lists:map(Fun, Acceptors).

send(Name, Message) ->
  Name ! Message.
