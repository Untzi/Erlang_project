-module(picture_fsm).
-author("Chen_Shay").

-behaviour(gen_fsm).

-include_lib("defines.hrl").

%% API
-export([start/1]).

%% gen_fsm callbacks
-export([init/1, handle_info/3, handle_event/3, handle_sync_event/4, terminate/3, code_change/4]).
-export([move/2, collision/2, generate_position/2, set_position/1, graphics_turn_down/2]).

% ------------------------------------------------ %

start({Picture_Name, Owner, Pos, Mov, Collision, Delay, TTL}) ->
  Data = {Picture_Name, Owner, Pos, Mov, Collision, Delay, TTL},
  gen_fsm:start({global, Picture_Name}, ?MODULE, Data, []);

start({Picture_Name, Owner, Mov_Delay, TTL}) ->
  Data = {Picture_Name, Owner, Mov_Delay, TTL},
  gen_fsm:start({global, Picture_Name}, ?MODULE, Data, []).

init({Picture_Name, Owner, Pos, Mov, Collision, Delay, TTL}) ->
  Owner ! {update_pid, Picture_Name, self()},
  StateData = {Picture_Name, Owner, Pos, Mov, Collision, Delay, TTL},
  io:format("Finished initilize picture process from another node.~n"),
  {ok, move, StateData, 0};

init({Picture_Name, Owner, Mov_Delay, TTL}) ->
  Owner ! {update_pid, Picture_Name, self()},
  Collision = false,
  StateData = {Picture_Name, Owner, Collision, Mov_Delay, TTL},
  io:format("Finished initilize a new picture process.~n"),
  {ok, generate_position, StateData, 0}.

% ------------------------------------------------ %

generate_position(timeout, StateData) ->
  {Picture_Name, _, _, _, _} = StateData,
  {PosX, PosY} = {set_position(?WIDTH), set_position(?HEIGHT)},
  io:format("Picture_fsm, pid: ~p, wating for starting position approval from server.~n", [self()]),
  gen_server:cast({global, gui_server}, {generate_position, Picture_Name, PosX, PosY}),
  {next_state, generate_position, StateData};

generate_position({generate_position, approved, Pos, Mov}, StateData) ->
  io:format("Picture_fsm, pid: ~p, starting position approved.~n", [self()]),
  {Picture_Name, Owner, Collision, Mov_Delay, TTL} = StateData,
  {next_state, move, {Picture_Name, Owner, Pos, Mov, Collision, Mov_Delay, TTL}, Mov_Delay};

generate_position({generate_position, reject}, StateData) ->
  io:format("Picture_fsm, pid: ~p, starting position were reject.~n", [self()]),
  {next_state, generate_position, StateData, 0}.

% ------------------------------------------------ %

graphics_turn_down(timeout, {Data, update_position, Picture_Name, New_Pos, New_Mov}) ->
  try
    global:send(graphics, {update_position, Picture_Name, New_Pos, New_Mov, get_timestamp()})
  of
    _-> {next_state, move, Data, ?PIC_PROCESS_DELAY}
  catch
    _:_ -> {next_state, graphics_turn_down, {Data, update_position, Picture_Name, New_Pos, New_Mov}, ?PIC_PROCESS_DELAY}
  end.

move(timeout, {Picture_Name, Owner, {PosX, PosY},{MovX, MovY}, _Collision, Mov_Delay, TTL}) ->
  {New_Pos, New_Mov} = update_pos({PosX, PosY},{MovX, MovY}),
  Data = {Picture_Name, Owner, New_Pos, New_Mov, false, Mov_Delay, TTL},
  try
    global:send(graphics, {update_position, Picture_Name, New_Pos, New_Mov, get_timestamp()})
  of
    _-> {next_state, move, Data, Mov_Delay}
  catch
    _:_ -> {next_state, graphics_turn_down, {Data, update_position, Picture_Name, New_Pos, New_Mov}, Mov_Delay}
  end;

move({collision, New_Mov}, StateData) ->
  io:format("Picture process notified about collision event.~n"),
  {next_state, collision, {New_Mov, StateData}, 0};

move({move_to_node, Node}, StateData) ->
  {Picture_Name, _, Pos, Mov, Collision, Mov_Delay, TTL} = StateData,
  io:format("Picture process, pid: ~p, moving to another node.~n", [self()]),
  gen_server:cast({global, Node}, {moving, {Picture_Name, Pos, Mov, Collision, Mov_Delay, TTL}}),
  {stop, normal, normal};

move(terminate, _State) ->
  {stop, normal, normal}.

collision(timeout, {{NewMovX, NewMovY}, {Picture_Name, Owner, Pos, {MovX, MovY}, Collision, Mov_Delay, TTL}}) ->
  case Collision of
    true  ->
      {next_state, move, {Picture_Name, Owner, Pos,{MovX, MovY}, true, Mov_Delay, TTL}, Mov_Delay/2};
    false ->
      case (TTL - 1 =:= 0) of
        true  ->
          Owner ! {self_kill, Picture_Name},
          io:format("Picture process, pid: ~p, eliminates (TTL = 0). boom\\paw\\kapaw event :-O.~n", [self()]),
          picture_temporary(rand_image(), Pos, 1000),
          {stop, normal, normal};
        false ->
          {next_state, move, {Picture_Name, Owner, Pos, {NewMovX, NewMovY}, true, Mov_Delay, TTL-1}, 0}
      end
  end;

collision({move_to_node, Node}, StateData) ->
  {Picture_Name, _, Pos, Mov, Collision, Mov_Delay, TTL} = StateData,
  io:format("Picture process, pid: ~p, moving to another node.~n", [self()]),
  gen_server:cast({global, Node}, {moving, {Picture_Name, Pos, Mov, Collision, Mov_Delay, TTL}}),
  {stop, normal, normal};

collision(terminate, _State) ->
  {stop, normal, normal}.

handle_sync_event(Event, From, StateName, StateData) ->
  io:format("Picture_fsm received unexpected sync event:~n~p.~nfrom: ~p.~n", [Event, From]),
  {next_state, StateName, StateData}.

handle_event(Event, StateName, StateData) ->
  io:format("Picture_fsm received unexpected event:~n~p.~n", [Event]),
  {next_state, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  io:format("Picture_fsm not support code changing.~n"),
  {ok, StateName, StateData}.

%%%===================================================================
%%%                 Messages from current node                     %%%
%%%===================================================================

handle_info(terminate, _StateName, _StateData) ->
  io:format("Picture received terminate message... bye bye picture... :(~n"),
  node() ! {self_kill_pid, self()},
  {stop, normal, normal};

handle_info(kill, _StateName, _StateData) ->
  io:format("Picture received kill message... bye bye picture... :(~n"),
  node() ! {self_kill_pid, self()},
  {stop, normal, normal};

handle_info(Msg, StateName, StateData) ->
  io:format("handle_event, unexpected message: ~p.~n",[Msg]),
  {next_state, StateName, StateData, 0}.

terminate(_Reason, _StateName, _State) ->
  io:format("terminate event. go eat some humus.!~n"),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

set_position(Length) ->
  1 + rand:uniform(Length - 2*?ImgEdge).

update_pos({Pos_X, Pos_Y}, {Movment_X, Movment_Y})->
  %%lower bound
  if (Pos_Y  + ?ImgEdge) >= ?HEIGHT -> %(Pos_Y  + 1.5 * ?ImgEdge) >= ?HEIGHT ->
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

picture_temporary(PictureType, Pos, Delay) ->
  try global:send(graphics, {insert_temporary, Pos, PictureType}) of
    _ ->
      receive
      after Delay
        -> try global:send(graphics, {kill_temporary, Pos, PictureType}) of
             _ -> ok
           catch
             _:_ -> ok
           end
      end
  catch
    _:_ ->
      receive after
        Delay -> picture_temporary(PictureType, Pos, Delay)
      end
  end.

rand_image() ->
  List =[?PAW,?KAPAW,?BOOM],
  Index = rand:uniform(length(List)),
  lists:nth(Index,List).

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).