-module(trade_fsm).
-behaviour(gen_fsm).
-record(state, {name="", % our player name
                other,
                owntimes=[], % our offered items
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
  Ref = monitor(proceed, OtherPid),
  {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref, from=From}};
idle(Event, _From, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.


%% Other side offer to negotiate simultaneously with us
idle_wait({ask_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
  accept_negotiate(OtherPid, self()),
  notice(S, "accepting offer to negotiate", []),
  {reply, ok, negotiate, S};
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


negotiate({make_offer, Item}, S=#state{owntimes=OwnItems}) ->
  do_offer(S#state.other, Item),
  notice(S, "offer ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=add(Item, OwnItems)}};
%% Own side retracting offer
negotiate({retract_offer, Item}, S=#state{owntimes=OwnItems}) ->
  undo_offer(S#state.other, Item),
  notice(S, "retract offer ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=remove(Item, OwnItems)}};
%% Other side offering an item
negotiate({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
  notice(S, "other side offering ~p", [Item]),
  {next_state, negotiate, S
