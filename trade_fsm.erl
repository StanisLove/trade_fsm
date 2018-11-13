-module(trade_fsm).
-behaviour(gen_fsm).

%% Public API
-export([start/1, start_link/1, trade/2, accept_trade/1,
         make_offer/2, retract_offer/2, ready/1, cancel/1]).

%% Callback functions for gen_fsm
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
         %% state names
         idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2, negotiate/3,
         wait/2, ready/2, ready/3]).

%%% Public API

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
