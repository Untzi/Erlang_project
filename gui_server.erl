
-module(gui_server).
-author("Chen_Shay").

%% API
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-include_lib("defines.hrl").

%-record(pic_state, {picture_name, owner_name, pos, mov}).
%-define(TIME, 100).

-define(WIDTH, 1920).
-define(HEIGHT, 950).
-define(BACKGROUND,      "/home/chenb/IdeaProjects/untitled/background.jpg").
-define(RECEIVE_FOLDER,  "/home/chenb/IdeaProjects/untitled/receive_folder").
-define(RESOURCE_FOLDER, "/home/chenb/IdeaProjects/untitled/resources_folder").


update_ets('$end_of_table') -> ok;
update_ets(Line) ->
  {PictureName, Owner, Pos, Mov} = Line,
  {NewPos, NewMov} = update_pos(Pos, Mov),
  ets:delete(data_base, PictureName),
  ets:insert(data_base, {{PictureName, Owner,NewPos, NewMov}}),
  update_ets(ets:next(data_base,Line)).

loop(Frame) ->

  show_graphics(Frame),
  receive after 10 -> ok end,
  update_ets(ets:first(data_base)),
  loop(Frame).

start() ->
  wx:new(),
  Frame = wxFrame:new(wx:null(), -1, "bitmaps",[{size,{?WIDTH,?HEIGHT}}]),
  wxFrame:show(Frame),

  ets:new(data_base, [named_table, public, set]), % Any process can read or write to the table and the table is registered under data_base name.

  spawn( fun() -> file_scanner(?RECEIVE_FOLDER, ?RESOURCE_FOLDER) end),
  receive after 200 -> ok end,

  loop(Frame).

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
  %Picture = #pic_state{picture_name = PictureName, owner_name = Owner, pos = {PosX, PosY}, mov = {MovX, MovY}},
  Picture = {PictureName, Owner, {PosX, PosY},{MovX, MovY}},
  % need to send data to dest node.

  %ets:insert(data_base, {Picture, PictureName}).
  %ets:insert(data_base, {PictureName, Picture}).
  ets:insert(data_base, {Picture}).

% ------------------------------------------------- %

show_graphics(Frame) ->
  ClientDC = wxClientDC:new(Frame),
  %wxDC:clear(ClientDC),%??
  BufferDC = wxBufferedDC:new(ClientDC),
  wxDC:drawBitmap(BufferDC,  wxBitmap:new(?BACKGROUND), {0,0}),
  %wxDC:drawBitmap(BufferDC,  wxBitmap:new(?BACKGROUND,?WIDTH, ?HEIGHT), {0,0}),
  receive after 10 -> ok end,

  First = ets:first(data_base),
  case First of
    '$end_of_table' -> ok;
    _ -> show_graphics(First, BufferDC)
  end,
  wxBufferedDC:destroy(BufferDC),
  wxClientDC:destroy(ClientDC).

show_graphics(Line, BufferDC) ->
  %io:format("~s~n",[Line]),
  {PictureName, _, Pos, _} = Line,
  wxDC:drawBitmap(BufferDC,  wxBitmap:new(PictureName), Pos),
  receive after 10 -> ok end,
  NextLine = ets:next(data_base, Line),
  case NextLine of
    '$end_of_table' -> ok;
    _			          -> show_graphics(NextLine, BufferDC)
  end.

% --------------------------------------- %

update_pos({Xpos,Ypos},{Xmov,Ymov})->
  %hit the upper part
  if Ypos >= ?HEIGHT->
    DirY = -1*Ymov;
    true->
      if Ypos =< 0 ->
        DirY = -1*Ymov;
        true -> DirY = Ymov
      end
  end,
  if Xpos >= ?WIDTH->
    DirX = -1*Xmov;
    true->
      if Xpos =< 0 ->
        DirX = -1*Xmov;
        true->
          DirX = Xmov
      end
  end,
  {{Xpos+DirX,Ypos+DirY},{DirX,DirY}}.