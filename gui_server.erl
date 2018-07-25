-module(gui_server).
-author("Chen_Shay").

%% API
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-include_lib("defines.hrl").

-define(NODES,['shch1@127.0.0.1','shch2@127.0.0.1','shch3@127.0.0.1','shch4@127.0.0.1']).
-define(PICTURE_RADIUS, 50).
-define(WIDTH, 1920).
-define(HEIGHT, 950).
-define(DRAW_TIMEOUT, 10).
-define(PIC_PROCESS_TIMEOUT, 100).

% ------------------------------------------------- %

start() ->
  wx:new(),
  Frame = wxFrame:new(wx:null(), -1, "Pictures",[{size,{?WIDTH,?HEIGHT}}]),
  wxFrame:show(Frame),
  ets:new(data_base, [named_table, public, set]), % Any process can read or write to the table and the table is registered under data_base name.
  spawn( fun() -> file_scanner(?RECEIVE_FOLDER, ?RESOURCE_FOLDER) end),
  register(master, spawn( fun() -> listener() end)),
  timeout(200),
  graphics_loop(Frame).

graphics_loop(Frame) ->
  show_graphics(Frame),
  timeout(100),
  % TODO: etch image is procces that roll him self.
  % update_ets(ets:first(data_base)),
  graphics_loop(Frame).

picture_process_loop(Picture_Name, Owner, {PosX, PosY},{MovX, MovY}) ->
  receive
    _ -> picture_process_loop(Picture_Name, Owner, {PosX, PosY},{MovX, MovY})
  after ?PIC_PROCESS_TIMEOUT ->
    {NextPos, Movement} = update_pos({PosX, PosY},{MovX, MovY}),
    master ! {Picture_Name, NextPos, Movement}, % {Picture_Name, {new PosX, new PosY},{new MovX, new MovY}}
    picture_process_loop(Picture_Name, Owner, NextPos, Movement)
  end.

listener() ->
  receive
    {Picture_Name, NextPos, Movement} ->
      [Line] = ets:lookup(data_base, Picture_Name),
      {_, Owner, _, _} = Line,
      ets:delete(data_base, Line),
      ets:insert(data_base,{Picture_Name, Owner, NextPos, Movement})
  end,
  listener().

%% TODO: fix that function
update_pos({Pos_X, Pos_Y},{Movment_X, Movment_Y})->
  if Pos_Y >= ?HEIGHT -> Direction_Y = -1 * Movment_Y;
    true ->
      if Pos_Y =< 0  -> Direction_Y = -1 * Movment_Y;
        true         -> Direction_Y = Movment_Y
      end
  end,
  if Pos_X >= ?WIDTH  -> Direction_X = -1 * Movment_X;
    true ->
      if Pos_X =< 0  -> Direction_X = -1 * Movment_X;
        true        -> Direction_X = Movment_X
      end
  end,
  {{Pos_X + Direction_X, Pos_Y + Direction_Y}, {Direction_X, Direction_Y}}.

% ------------------------------------------------- %

file_scanner(RecFolder, ResFolder)->
  {ok, File_Names} = file:list_dir(RecFolder),
  File_Names_Dir = [RecFolder ++ "/" ++ X || X <- File_Names],

  iterate_update_move(File_Names, File_Names_Dir, ResFolder),
  timeout(500),
  file_scanner(RecFolder, ResFolder).

iterate_update_move([], [], _) -> ok;
iterate_update_move([H1| File_Names], [H2| File_Names_Dir], ResFolder)->
  file:copy(H2, ResFolder ++ "/" ++ H1),
  insert_picture(ResFolder ++ "/" ++ H1),
  file:delete(H2),
  iterate_update_move(File_Names, File_Names_Dir, ResFolder).

% ------------------------------------------------- %

insert_picture(Picture_Name) ->
  {PosX, PosY} = {trunc(?WIDTH * rand:uniform()), trunc(?HEIGHT * rand:uniform())},
  {MovX, MovY} = {random_movement(), random_movement()},
  Owner = set_owner(?NODES),
  Pic_Name_Atom = list_to_atom(Picture_Name),
  Picture = {Pic_Name_Atom, Owner, {PosX, PosY},{MovX, MovY}},
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % TODO: here we need to send data to destenation node.%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ets:insert(data_base, Picture),
  register(Pic_Name_Atom, spawn_link(fun() -> picture_process_loop(Pic_Name_Atom, Owner, {PosX, PosY},{MovX, MovY}) end)).

random_movement() ->
  R = rand:uniform(),
  if R < 0.5 -> -10;
    true -> 10
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
