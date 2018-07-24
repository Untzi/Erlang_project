
-module(gui_server).
-author("Chen_Shay").

%% API
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-include_lib("defines.hrl").

%-record(pic_state, {picture_name, owner_name, pos, mov}).
%-define(TIME, 100).

-define(WIDTH, 960).
-define(HEIGHT, 720).
-define(RECEIVE_FOLDER,  "/home/chenb/IdeaProjects/untitled/receive_folder").
-define(RESOURCE_FOLDER, "/home/chenb/IdeaProjects/untitled/resources_folder").


start() ->
  wx:new(),
  Frame = wxFrame:new(wx:null(), -1, "bitmaps",[{size,{?WIDTH,?HEIGHT}}]),
  wxFrame:show(Frame),

  ets:new(data_base, [named_table, public, set]), % Any process can read or write to the table and the table is registered under data_base name.

  spawn( fun() -> file_scanner(?RECEIVE_FOLDER, ?RESOURCE_FOLDER) end),
  receive after 1000 -> ok end,

  show_graphics(Frame).

% ------------------------------------------------- %

file_scanner(RecFolder, ResFolder)->
  {ok, Filenames} = file:list_dir(RecFolder),
  Filenames_Dir = [RecFolder ++ "/" ++ X || X <- Filenames],
  iterate_update_move(Filenames, Filenames_Dir, ResFolder),
  receive after 500 -> file_scanner(RecFolder, ResFolder) end.

iterate_update_move([],[],_) -> ok;
iterate_update_move([H1|Filenames],[H2|Filenames_Dir], ResFolder)->
  file:copy(H2, ResFolder ++ "/" ++ H1),

  insert_picture(ResFolder ++ "/" ++ H1),
  %insert_picture(H1),

  file:delete(H2),
  iterate_update_move(Filenames,Filenames_Dir, ResFolder).

% ------------------------------------------------- %

random_movement() ->
  R = rand:uniform(),
  if R < 0.5 ->
    -4;
  true
    -> 4
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
  %Picture = #pic_state{picture_name = PictureName, owner_name = Owner, pos = {PosX, PosY}, mov = {MovX, MovY}},
  Picture = {PictureName, Owner, {PosX, PosY},{MovX, MovY}},
  % need to send data to dest node.
  ets:insert(data_base, {Picture, PictureName}).

% ------------------------------------------------- %

show_graphics(Frame) ->
  First = ets:first(data_base),
  case First of
    '$end_of_table' -> ok;
    _ ->
      ClientDC = wxClientDC:new(Frame),
      BufferDC = wxBufferedDC:new(ClientDC),
      show_graphics(First, BufferDC),
      wxBufferedDC:destroy(BufferDC),
      wxClientDC:destroy(ClientDC)
  end.

show_graphics(Line, BufferDC)->
  {PictureName, _, Pos, _} = Line,
  wxDC:drawBitmap(BufferDC,  wxBitmap:new(PictureName), {10,10}),
  receive after 10 -> ok end,
  NextLine = ets:next(data_base, Line),
  case NextLine of
    '$end_of_table' -> ok;
    _			          -> show_graphics(NextLine, BufferDC)
  end.
