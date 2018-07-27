-module(gui_server).
-author("Chen_Shay").

%% API
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-include_lib("defines.hrl").

-define(NODES,['shch1@127.0.0.1','shch2@127.0.0.1','shch3@127.0.0.1','shch4@127.0.0.1']).
-define(ImgEdge, 50).
-define(RADIUS, 25).
-define(WIDTH, 700).
-define(HEIGHT, 700).
-define(TTL, 3).

-define(DRAW_TIMEOUT, 10).
-define(GRAPHICS_LOOP_TIMEOUT, 10).
-define(PIC_PROCESS_TIMEOUT, 10).

% ------------------------------------------------- %

start() ->
  file_scanner(?RESOURCE_FOLDER,?RECEIVE_FOLDER,1),
  wx:new(),
  Frame = wxFrame:new(wx:null(), -1, "Pictures",[{size,{?WIDTH,?HEIGHT}}]),
  wxFrame:show(Frame),
  ets:new(data_base, [named_table, public, set]),
  spawn( fun() -> file_scanner(?RECEIVE_FOLDER, ?RESOURCE_FOLDER,0) end),
  register(master, spawn( fun() -> listener() end)),
  register(graphics_procces, self()),
  graphics_loop(Frame).

graphics_loop(Frame) ->
  receive
    {insert_picture, {Pic_Name_Atom, Owner, {PosX, PosY},{MovX, MovY}}} ->
      ets:insert(data_base, {Pic_Name_Atom, Owner, {PosX, PosY},{MovX, MovY}});
    {self_kill, Picture_Name} ->
      ets:delete(data_base, Picture_Name),
      master ! {kill, Picture_Name}
  after 0 -> ok
  end,
  %timeout(?GRAPHICS_LOOP_TIMEOUT),
  show_graphics(Frame),
  graphics_loop(Frame).


picture_process_loop(Picture_Name, _, _, _, _, _, 0) ->
  master ! {self_kill, Picture_Name},
  receive
    kill -> io:format("bye bye~n")
    %{Event, Picture} -> Event(Picture, 1000/?PIC_PROCESS_TIMEOUT)
  end;
picture_process_loop(Picture_Name, Owner, {PosX, PosY},{MovX, MovY}, Were_Collision, TimeOut, TTL) ->
  Start_Timer = os:system_time(millisecond),
  receive
    {collision, NewMov} ->
      Time = os:system_time(millisecond) - Start_Timer,
      case Were_Collision of
        true  -> picture_process_loop(Picture_Name, Owner, {PosX, PosY}, {MovX, MovY}, Were_Collision, Time, TTL);
        false -> picture_process_loop(Picture_Name, Owner, {PosX, PosY}, NewMov, true, Time, TTL-1)
      end

  after TimeOut ->
    {NextPos, Movement} = update_pos({PosX, PosY}, {MovX, MovY}),
    master ! {Picture_Name, NextPos, Movement},
    picture_process_loop(Picture_Name, Owner, NextPos, Movement, false, ?PIC_PROCESS_TIMEOUT, TTL)
  end.

listener() ->
  receive
    {kill, Picture_Name} -> Picture_Name ! kill;
    {self_kill, Picture_Name} -> graphics_procces ! {self_kill, Picture_Name};
    {Picture_Name, NextPos, Movement} -> ets:update_element(data_base,Picture_Name,[{3,NextPos},{4,Movement}])
  end,
  listener().

update_pos({Pos_X, Pos_Y}, {Movment_X, Movment_Y})->
  %%upper bound
  if Pos_Y  + ?ImgEdge>= ?HEIGHT ->
    Direction_Y = -1 * Movment_Y;
    true ->
      %%lower bound
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

collision_detect('$end_of_table') -> ok;
collision_detect(Picture) ->
  Next_Picture = ets:next(data_base, Picture),
  collision_detect(ets:lookup(data_base, Picture), Next_Picture),
  collision_detect(Next_Picture).

collision_detect(_, '$end_of_table') -> ok;
collision_detect(Picture_Line, Picture_To_Campre) ->
  Picture_Line_Campre = ets:lookup(data_base, Picture_To_Campre),
  {Flag, Mov, MovCmp}= collision_check(Picture_Line, Picture_Line_Campre),
  case Flag of
    false -> collision_detect(Picture_Line, ets:next(data_base, Picture_To_Campre));
    true  ->
      [{Picture_Name, _, _, _}] = Picture_Line,
      [{Picture_Name_Cmp, _, _, _}]= Picture_Line_Campre,
      Picture_Name ! {collision, Mov},
      Picture_Name_Cmp ! {collision, MovCmp}
  end.

%% this function work only for circle pictures.
collision_check(Line1, Line2) ->
  [{_, _, {X1,Y1}, {MovX1, MovY1}}] = Line1,
  [{_, _, {X2,Y2}, {MovX2, MovY2}}] = Line2,
  Flag = (abs(X1-X2) =< ?ImgEdge) and (abs(Y1-Y2) =< ?ImgEdge),
  case Flag of
    true  -> {true, {MovX2, MovY2}, {MovX1, MovY1}};
    false -> {false, false, false}
  end.

% ------------------------------------------------- %

file_scanner(RecFolder, ResFolder,Debug)->
  {ok, File_Names} = file:list_dir(RecFolder),
  File_Names_Dir = [RecFolder ++ "/" ++ X || X <- File_Names],

  iterate_update_move(File_Names, File_Names_Dir, ResFolder,Debug),
  timeout(500),
  if Debug =:= 1->
    ok;
    true->
      file_scanner(RecFolder, ResFolder,Debug)
  end.

iterate_update_move([], [], _,_Debug) -> ok;
iterate_update_move([H1| File_Names], [H2| File_Names_Dir], ResFolder,Debug)->
  file:copy(H2, ResFolder ++ "/" ++ H1),
  if Debug=:=0->
    insert_picture(ResFolder ++ "/" ++ H1);
    true->
      ok
    end,
  file:delete(H2),
  iterate_update_move(File_Names, File_Names_Dir, ResFolder,Debug).

% ------------------------------------------------- %

insert_picture(Picture_Name) ->
  {PosX, PosY} = {trunc(50 + (?WIDTH - 50) * rand:uniform()), trunc(50 + (?HEIGHT - 50) * rand:uniform())},
  {MovX, MovY} = {random_movement(), random_movement()},
  Owner = set_owner(?NODES),
  Pic_Name_Atom = list_to_atom(Picture_Name),
  Picture = {Pic_Name_Atom, Owner, {PosX, PosY},{MovX, MovY}},
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % TODO: here we need to send data to destenation node.%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  graphics_procces ! {insert_picture, Picture},
  %ets:insert(data_base, Picture),
  Pid = spawn_link(fun() -> picture_process_loop(Pic_Name_Atom, Owner, {PosX, PosY},{MovX, MovY}, false, ?PIC_PROCESS_TIMEOUT, ?TTL) end),
  register(Pic_Name_Atom, Pid).

random_movement() ->
  R = rand:uniform(),
  if R < 0.5 -> -1;
    true -> 1
  end .

get_node_to_transmit([HNodes|[]], _, [_|[]]) -> HNodes;
get_node_to_transmit([HNodes|_], Min, [Min|_]) -> HNodes;
get_node_to_transmit([_|TNodes], Min, [_|T]) -> get_node_to_transmit(TNodes, Min, T).

% TODO: make it work following our logic.
set_owner(Nodes) -> ok.
%set_owner(Nodes) -> this code old as fuck.
%  NodesNumOfPictures = [ length(X) || X <- [ ets:lookup(data_base, X) || X <- Nodes]],
%  Min = lists:min(NodesNumOfPictures),
%  get_node_to_transmit(Nodes, Min, NodesNumOfPictures).

% ------------------------------------------------- %

show_graphics(Frame) ->
  collision_detect(ets:first(data_base)),
  ClientDC = wxClientDC:new(Frame),
  BufferDC = wxBufferedDC:new(ClientDC),
  Background = wxBitmap:new(?BACKGROUND),
  wxDC:drawBitmap(BufferDC, Background, {0,0}),
  timeout(?DRAW_TIMEOUT),
  wxBitmap:destroy(Background),

  First = ets:first(data_base),
  case First of
    '$end_of_table' -> ok;
    _ -> show_graphics(First, BufferDC)
  end,
  wxBufferedDC:destroy(BufferDC),
  wxClientDC:destroy(ClientDC).

show_graphics(Picture_Name, BufferDC) ->
  [{Name, _, Pos, _}] = ets:lookup(data_base, Picture_Name),
  Image = wxBitmap:new(atom_to_list(Name)),
  wxDC:drawBitmap(BufferDC, Image, Pos),
  timeout(?DRAW_TIMEOUT),
  wxBitmap:destroy(Image),
  NextLine = ets:next(data_base, Picture_Name),
  case NextLine of
    '$end_of_table' -> ok;
    _			          -> show_graphics(NextLine, BufferDC)
  end.

% --------------------------------------- %

timeout(T) -> receive after T -> ok end.