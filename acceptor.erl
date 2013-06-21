-module(acceptor).
-export([start/1]).

start(Name) ->
  spawn(fun() -> init(Name) end).

init(Name) ->
  Promise = order:null(),
  Voted = order:null(),
  Accepted = na,
  acceptor(Name, Promise, Voted, Accepted).

acceptor(Name, Promise, Voted, Accepted) ->
 receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promise) of
        true ->
          Proposer ! {promise, Round, Voted, Accepted},
          acceptor(Name, Round, Voted, Accepted);
        false ->
          Proposer ! {sorry, Round},
          acceptor(Name, Promise, Voted, Accepted)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promise) of
        true ->
          Proposer ! {vote, Round},
          case order:goe(Round, Voted) of
            true ->
              acceptor(Name, Promise, Round, Proposal);
            false ->
              acceptor(Name, Promise, Voted, Accepted)
          end;
        false ->
          Proposer ! {sorry, Round},
          acceptor(Name, Promise, Voted, Accepted)
     end;
   stop ->
      ok
  end.
