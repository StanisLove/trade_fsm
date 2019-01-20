-module(trade_fsm).
-behaviour(gen_fsm).
-record(state, {name="", % our player name
                other,
                ownitems=[], % our offered items
                otheritems=[], % other player offered items
                monitor,
                from}). % pid of sync call sender

%% Public API
-export([start/1, start_link/1, trade/2, accept_trade/1,
         make_offer/2, retract_offer/2, ready/1, cancel/1]).

%% Callback functions for gen_fsm
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
         %% state names
         idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2, negotiate/3,
         wait/2, ready/2, ready/3]).





%%% Public (client) API

start(Name) -> gen_fsm:start(?MODULE, [Name], []).
start_link(Name) -> gen_fsm:start_link(?MODULE, [Name], []).

%% Start session request. Returns when other side accepts the request.
trade(OwnPid, OtherPid) ->
  gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

%% Accept somebody offer to start negotiation
accept_trade(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, accept_negotiate).

%% Put item to change
make_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {make_offer, Item}).

%% Remove item from changing
retract_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%% Confirm that you are ready to change. Deal process when other player confirm about self readiness.
ready(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, ready, infinity).

%% Cancel the deal at any time.
cancel(OwnPid) ->
  gen_fsm:sync_send_all_state_event(OwnPid, cancel).





%%% Functions between state machines

%% Ask negtiate
ask_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

%% Accept negotiation offer
accept_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

%% Offer item to other pid
do_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {do_offer, Item}).

%% Remove item from negotiation
undo_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {undo_offer, Item}).

%% Ask other pid about readiness to deal
are_you_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, are_you_ready).

%% Answer that we are not ready to deal (i.e. not in 'wait' state)
not_yet(OtherPid) ->
  gen_fsm:send_event(OtherPid, not_yet).

%% Tell to other FSM, that user waiting now 'ready' state
%% State must change to 'ready'
am_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, 'ready!').

%% Fucnctions that calls in both FSM to finish the deal

%% Confirm that FSM in ready state
ack_trans(OtherPid) ->
  gen_fsm:send_event(OtherPid, ack).

%% Ask other side about readiness to finish the deal
ask_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, ask_commit).

%% Start sync finishing the deal
do_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
  gen_fsm:send_all_state_event(OtherPid, cancel).



%%% Callback functions

init(Name) ->
  {ok, idle, #state{name=Name}}.

%% Send notification to clients. (print to console)
notice(#state{name=N}, Str, Args) ->
  io:format("~s: "++Str++"~n", [N|Args]).

%% Log unexpected messages
unexpected(Msg, State) ->
  io:format("~p unknown message received ~p in state ~p~n", [self(), Msg, State]).

%% other side ask to start negotiation
idle({ask_negotiate, OtherPid}, S=#state{}) ->
  Ref = monitor(process, OtherPid), % to proceed with suddenly negotiator death
  notice(S, "~p offer to start negotiation", [OtherPid]),
  {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref}};
idle(Event, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

%% ower client ask to start negotiation
idle({negotiate, OtherPid}, From, S=#state{}) ->
  ask_negotiate(OtherPid, self()),
  notice(S, "offer ~p to start negotiation", [OtherPid]),
  Ref = monitor(process, OtherPid),
  {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref, from=From}};
idle(Event, _From, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.


%% Other side offer to negotiate simultaneously with us
idle_wait({ask_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "start negotiation", []),
  {next_state, negotiate, S};
%% Other side accepts our offer to negotiate. Go to negotiate state.
idle_wait({accept_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "start negotiation", []),
  {next_state, negotiate, S};
idle_wait(Event, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

%% our client accepts offer to negotiate
idle_wait(accept_negotiate, _From, S=#state{other=OtherPid}) ->
  accept_negotiate(OtherPid, self()),
  notice(S, "accept negotiation", []),
  {reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.


%% Add item to list to change
add(Item, Items) ->
  [Item|Items].
%% Remove item from list to change
remove(Item, Items) ->
  Items -- [Item].


negotiate({make_offer, Item}, S=#state{ownitems=OwnItems}) ->
  do_offer(S#state.other, Item),
  notice(S, "offer ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=add(Item, OwnItems)}};
%% Own side retracting offer
negotiate({retract_offer, Item}, S=#state{ownitems=OwnItems}) ->
  undo_offer(S#state.other, Item),
  notice(S, "retract offer ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=remove(Item, OwnItems)}};
%% Other side offering an item
negotiate({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
  notice(S, "other side offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
negotiate({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
  notice(S, "other side wihtdraw the offer of item ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};
negotiate(are_you_ready, S=#state{other=OtherPid}) ->
  io:format("Other side ready to change.~n"),
  notice(S,
         "Other side ready to send items:~n"
         "You'll get ~p, Ohter side gets ~p",
         [S#state.otheritems, S#state.ownitems]),
  not_yet(OtherPid),
  {next_state, negotiate, S};
negotiate(Event, Data) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, Data}.

negotiate(ready, From, S = #state{other=OtherPid}) ->
  are_you_ready(OtherPid),
  notice(S, "ask is other side ready and waiting", []),
  {next_state, wait, S#state{from=From}};
negotiate(Event, _From, S) ->
  unexpected(Event, idle_wait),
  {next_state, negotiate, S}.

wait({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "other side offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
wait({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "other side wihtdraw the offer of item ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};
wait(are_you_ready, S=#state{}) ->
  am_ready(S#state.other),
  notice(S, "ask about readiness. me ready too. waiting for same answer", []),
  {next_state, wait, S};
wait(not_yet, S=#state{}) ->
  notice(S, "Other side is not ready", []),
  {next_state, wait, S};
wait('ready!', S=#state{}) ->
  am_ready(S#state.other),
  ack_trans(S#state.other),
  gen_fsm:reply(S#state.from, ok),
  notice(S, "other side is ready. go to ready state", []),
  {next_state, ready, S};
wait(Event, Data) ->
  unexpected(Event, wait),
  {next_state, wait, Data}.

priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.

ready(ack, S=#state{}) ->
  case priority(self(), S#state.other) of
    true ->
      try
        notice(S, "ask deal beginning", []),
        ready_commit = ask_commit(S#state.other),
        notice(S, "command to start the deal", []),
        ok = do_commit(S#state.other),
        notice(S, "commiting data...", []),
        commit(S),
        {stop, normal, S}
      catch Class:Reason ->
        %% Cancel! ready_commit or do_commit command didn't performe
        notice(S, "commit failed", []),
        {stop, {Class, Reason}, S}
      end;
    false ->
      {next_state, ready, S}
  end;
ready(Event, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

ready(ask_commit, _From, S)->
  notice(S, "respond to ask_commit", []),
  {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
  notice(S, "commit data...", []),
  commit(S),
  {stop, normal, ok, S};
ready(Event, _From, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

commit(S = #state{}) ->
  io:format("Deale complete for ~s. "
            "Sent items: ~n~p, ~nGot items: ~n~p.~n"
            "This operation must perform atomic write to data base.~n",
            [S#state.name, S#state.ownitems, S#state.otheritems]).

%% Other user send cancel event. Stop doing anything and finish work!
handle_event(cancel, _StateName, S=#state{}) ->
  notice(S, "get cancel request", []),
  {stop, other_canceled, S};
handle_event(Event, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

%% This event comes from our client. We need to let other player know.
handle_sync_event(cancel, _From, _StateName, S=#state{}) ->
  notify_cancel(S#state.other),
  notice(S, "canceling trade, sending cancel event", []),
  {stop, cancelled, ok, S};
%% Note: DON'T REPLY on unexpected requests. Let them fall off by timeout.
handle_sync_event(Event, _From, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_info({'DOWN', Ref, process, Pid, Reason},
            _StateName,
            S=#state{other=Pid, monitor=Ref}) ->
  notice(S, "Lost connection with other side", []),
  {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
  unexpected(Info, StateName),
  {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

%% Transaction finished
terminate(normal, ready, S=#state{}) ->
  notice(S, "FSM ends", []);
terminate(_Reason, _StateName, _StateData) ->
  ok.
