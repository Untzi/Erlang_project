-module(picture_fsm).
-author("Chen_Shay").

-behaviour(gen_fsm).

-include_lib("defines.hrl").

%% API
-export([start/1, start_link/1]). %, collision/3, kill/1]).

%% gen_fsm callbacks
-export([init/1, handle_info/3, handle_event/3, handle_sync_event/4, terminate/3, code_change/4]).
-export([move/2, collision/2, generate_position/2, random_movement/0, set_position/1]).

% ------------------------------------------------ %

start_link({Picture_Name, Owner, Mov_Delay, TTL}) ->
  Data = {Picture_Name, Owner, Mov_Delay, TTL},
  gen_fsm:start_link({global, Picture_Name}, ?MODULE, Data, []).

start({Picture_Name, Owner, Mov_Delay, TTL}) ->
  Data = {Picture_Name, Owner, Mov_Delay, TTL},
  gen_fsm:start({global, Picture_Name}, ?MODULE, Data, []).

init({Picture_Name, Owner, Mov_Delay, TTL}) ->
  Owner ! {update_pid, Picture_Name, self()},
  Collision = false,
  {MovX, MovY} = {random_movement(), random_movement()},
  StateData = {Picture_Name, Owner, {MovX, MovY}, Collision, Mov_Delay, TTL},
  io:format("Finished initilize new picture process.~n"),
  {ok, generate_position, StateData, 0}.

generate_position(timeout, StateData) ->
  {Picture_Name, Owner, {MovX, MovY}, _, _, _} = StateData,
  {PosX, PosY} = {set_position(?WIDTH), set_position(?HEIGHT)},
  Owner ! {generate_position, Picture_Name, PosX, PosY, MovX, MovY},
  {next_state, generate_position, StateData}.

move(timeout, {Picture_Name, Owner, {PosX, PosY},{MovX, MovY}, _Collision, Mov_Delay, TTL}) ->
  {New_Pos, New_Mov} = update_pos({PosX, PosY},{MovX, MovY}),
  Owner ! {update_position, Picture_Name, New_Pos, New_Mov},
  Data = {Picture_Name, Owner, New_Pos, New_Mov, false, Mov_Delay, TTL},
  {next_state, move, Data, Mov_Delay};

move(terminate, _State) ->
  {stop, normal, normal}.

collision(timeout, {{NewMovX, NewMovY}, {Picture_Name, Owner, Pos, {MovX, MovY}, Collision, Mov_Delay, TTL}}) ->
  case Collision of
    true  ->
      {next_state, move, {Picture_Name, Owner, Pos,{MovX, MovY}, Collision, Mov_Delay, TTL}, trunc(Mov_Delay/2)};
    false ->
      case (TTL - 1 =:= 0) of
        true  ->
          Owner ! {self_kill, Picture_Name},
          {stop, normal, normal};
        false ->
          {next_state, move, {Picture_Name, Owner, Pos, {NewMovX, NewMovY}, true, Mov_Delay, TTL-1}, 0}
      end
  end.

handle_sync_event(Event, From, StateName, StateData) ->
  io:format("handle_event, unexpected event (~p), from: ~p.~n", [Event, From]),
  {next_state, StateName, StateData}.

handle_event(Event, StateName, StateData) ->
  io:format("handle_event, unexpected event: ~p.~n", [Event]),
  {next_state, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  io:format("picture_fsm not support code changing.~n"),
  {ok, StateName, StateData}.

%%%===================================================================
%%%                 Messages from current node                     %%%
%%%===================================================================

handle_info({collision, New_Mov}, _StateName, StateData) ->
  io:format("handle_info: collision message.~n"),
  {next_state, collision, {New_Mov, StateData}, 0};

handle_info(kill, _StateName, _StateData) ->
  io:format("handle_info: kill message.~n"),
  {stop, normal, normal};

handle_info(bad_position, StateName, StateData) ->
  io:format("handle_info: bad_position message.~n"),
  {Picture_Name, Owner, {MovX, MovY}, _, _, _} = StateData,
  {PosX, PosY} = {set_position(?WIDTH), set_position(?HEIGHT)},
  Owner ! {generate_position, Picture_Name, PosX, PosY, MovX, MovY},
  {next_state, StateName, StateData};

handle_info({good_position, {PosX, PosY}}, _StateName, StateData) ->
  io:format("handle_info: good_position message.~n"),
  {Picture_Name, Owner, {MovX, MovY}, Collision, Mov_Delay, TTL} = StateData,
  io:format("------------ Picture starts here movement from position (~p, ~p) ------------~n", [PosX, PosY]),
  {next_state, move, {Picture_Name, Owner, {PosX, PosY}, {MovX, MovY}, Collision, Mov_Delay, TTL}, Mov_Delay};

handle_info(Msg, StateName, StateData) ->
  io:format("handle_event, unexpected message (~p).~n", [Msg]),
  {_, Owner, _, _, Mov_Delay, _} = StateData,
  Owner ! {print, "handle_event, unexpected message.~n"},
  case StateName of
    move -> {next_state, move     , StateData};
    _    -> {next_state, StateName, StateData, Mov_Delay}
  end.

terminate(_Reason, _StateName, _State) ->
  io:format("terminate event. :) go eat some humus :)~n"),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

set_position(Length) ->
  1 + rand:uniform(Length - 2*?ImgEdge).

random_movement() ->
  R = rand:uniform(),
  if R < 0.5 -> -2;
    true -> 2
  end .

update_pos({Pos_X, Pos_Y}, {Movment_X, Movment_Y})->
  %%lower bound
  if (Pos_Y  + 1.5 * ?ImgEdge) >= ?HEIGHT ->
    Direction_Y = -1 * Movment_Y;
    true ->
      %%upper bound
      if Pos_Y  =< 0  ->
        Direction_Y = -1 * Movment_Y;
        true         ->
          Direction_Y = Movment_Y
      end
  end,
  %%right side
  if Pos_X +?ImgEdge>= ?WIDTH  ->
    Direction_X = -1 * Movment_X;
    true ->
      %%left side
      if Pos_X =< 0  ->
        Direction_X = -1 * Movment_X;
        true        ->
          Direction_X = Movment_X
      end
  end,
  {{Pos_X + Direction_X, Pos_Y + Direction_Y}, {Direction_X, Direction_Y}}.