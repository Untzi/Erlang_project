
-module(gui_server).
-author("Chen_Shay").

%% API
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-include_lib("defines.hrl").

-define(WIDTH, 1920).
-define(HEIGHT, 950).
-define(DRAW_TIMEOUT, 10).
-define(BACKGROUND,      "/home/chenb/IdeaProjects/untitled/background.jpg").
-define(RECEIVE_FOLDER,  "/home/chenb/IdeaProjects/untitled/receive_folder").
-define(RESOURCE_FOLDER, "/home/chenb/IdeaProjects/untitled/resources_folder").

timeout(T) -> receive after T -> ok end.

update_ets('$end_of_table') -> ok;
update_ets(Line) ->
  {PictureName, Owner, Pos, Mov} = Line,
  {NewPos, NewMov} = update_pos(Pos, Mov),
  NextLine = ets:next(data_base,Line),
  ets:delete(data_base, Line),
  ets:insert(data_base, {{PictureName, Owner, NewPos, NewMov}}),
  update_ets(NextLine).

loop(Frame) ->
  show_graphics(Frame),
  timeout(20),
  update_ets(ets:first(data_base)),
  loop(Frame).

start() ->
  wx:new(),
  Frame = wxFrame:new(wx:null(), -1, "bitmaps",[{size,{?WIDTH,?HEIGHT}}]),
  wxFrame:show(Frame),
  ets:new(data_base, [named_table, public, set]), % Any process can read or write to the table and the table is registered under data_base name.
  spawn( fun() -> file_scanner(?RECEIVE_FOLDER, ?RESOURCE_FOLDER) end),
  timeout(200),
  loop(Frame).

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

random_movement() ->
  R = rand:uniform(),
  if R < 0.5 ->
    -10;
  true
    -> 10
end .

get_node_to_transmit([HNodes|[]], _, [_|[]]) -> HNodes;
get_node_to_transmit([HNodes|_], Min, [Min|_]) -> HNodes;
get_node_to_transmit([_|TNodes], Min, [_|T]) -> get_node_to_transmit(TNodes, Min, T).

set_owner(Nodes) ->
  NodesNumOfPictures = [ length(X) || X <- [ ets:lookup(data_base, X) || X <- Nodes]],
  Min = lists:min(NodesNumOfPictures),
  get_node_to_transmit(Nodes, Min, NodesNumOfPictures).

insert_picture(PictureName) ->
  {PosX, PosY} = {trunc(?WIDTH * rand:uniform()), trunc(?HEIGHT * rand:uniform())},
  {MovX, MovY} = {random_movement(), random_movement()},
  Owner = set_owner(?NODES),
  Picture = {list_to_atom(PictureName), Owner, {PosX, PosY},{MovX, MovY}},
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % here we need to send data to destenation node.%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ets:insert(data_base, {Picture}).

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

show_graphics(Line, BufferDC) ->
  {PictureName, _, Pos, _} = Line,

  Image = wxBitmap:new(atom_to_list(PictureName)),
  wxDC:drawBitmap(BufferDC, Image, Pos),
  timeout(?DRAW_TIMEOUT),
  wxBitmap:destroy(Image),

  NextLine = ets:next(data_base, Line),
  case NextLine of
    '$end_of_table' -> ok;
    _			          -> show_graphics(NextLine, BufferDC)
  end.

% --------------------------------------- %

update_pos({Xpos,Ypos},{Xmov,Ymov})->
  %hit the upper part
  if Ypos >= ?HEIGHT -> DirY = -1*Ymov;
    true ->
      if Ypos =< 0 -> DirY = -1*Ymov;
         true      -> DirY = Ymov
      end
  end,
  if Xpos >= ?WIDTH -> DirX = -1*Xmov;
    true ->
      if Xpos =< 0 -> DirX = -1*Xmov;
         true      -> DirX = Xmov
      end
  end,
  {{Xpos+DirX,Ypos+DirY},{DirX,DirY}}.