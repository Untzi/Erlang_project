
-module(v1_gui_server_dumy_move).
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

% ------------------------------------------------- %

start() ->
  wx:new(),
  Frame = wxFrame:new(wx:null(), -1, "pictures",[{size,{?WIDTH,?HEIGHT}}]),
  wxFrame:show(Frame),
  ets:new(data_base, [named_table, public, set]), % Any process can read or write to the table and the table is registered under data_base name.
  spawn( fun() -> file_scanner(?RECEIVE_FOLDER, ?RESOURCE_FOLDER) end),
  timeout(200),
  loop(Frame).

loop(Frame) ->
  show_graphics(Frame),
  timeout(20),
  update_ets(ets:first(data_base)),
  loop(Frame).

update_ets('$end_of_table') -> ok;
update_ets(Picture_Name) ->
  [{Name, Owner, Pos, Mov}] = ets:lookup(data_base, Picture_Name),
  {NewPos, NewMov} = update_pos(Pos, Mov),
  NextLine = ets:next(data_base,Picture_Name),
  ets:delete(data_base, Picture_Name),
  ets:insert(data_base, {Name, Owner, NewPos, NewMov}),
  update_ets(NextLine).

% ------------------------------------------------- %

file_scanner(RecFolder, ResFolder)->
  {ok, Filenames} = file:list_dir(RecFolder),
  Filenames_Dir = [RecFolder ++ "/" ++ X || X <- Filenames],
  iterate_update_move(Filenames, Filenames_Dir, ResFolder),
  timeout(500),
  file_scanner(RecFolder, ResFolder).

iterate_update_move([],[],_) -> ok;
iterate_update_move([H1|Filenames],[H2|Filenames_Dir], ResFolder)->
  file:copy(H2, ResFolder ++ "/" ++ H1),
  insert_picture(ResFolder ++ "/" ++ H1),
  file:delete(H2),
  iterate_update_move(Filenames,Filenames_Dir, ResFolder).

% ------------------------------------------------- %

get_distace({Pos_X, Pos_Y}, {Picture_Pos_X, Picture_Pos_Y}) ->
  math:pow(math:pow(Pos_X - Picture_Pos_X, 2) + math:pow(Pos_Y - Picture_Pos_Y, 2), 0.5).

timeout(T) -> receive after T -> ok end.

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

insert_picture(Picture_Name) ->
  {PosX, PosY} = {trunc(?WIDTH * rand:uniform()), trunc(?HEIGHT * rand:uniform())},
  {MovX, MovY} = {random_movement(), random_movement()},
  Owner = set_owner(?NODES),
  Picture = {list_to_atom(Picture_Name), Owner, {PosX, PosY},{MovX, MovY}},
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % here we need to send data to destenation node.%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ets:insert(data_base, Picture).

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
