-module(gui_otp_server).
-author("Chen_Shay").

-include_lib("defines.hrl").
-include_lib("wx/include/wx.hrl").
-behaviour(gen_server).

%% API
-export([start/2, start_link/2, random_movement/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
get_owner(Nodes) ->
  [Node | Other_Nodes] = Nodes,
  {Node, Other_Nodes ++ [Node]}.

start_link(Gui_Server, Nodes) ->
  My_Name = {global, Gui_Server},
  Option = [],
  gen_server:start_link(My_Name, ?MODULE, Nodes, Option).

start(Gui_Server, Nodes) ->
  My_Name = {global, Gui_Server},
  Option = [],
  gen_server:start(My_Name, ?MODULE, Nodes, Option).

init(Nodes) ->
  register(gui_server, self()),
  ets:new(data_base, [named_table, public, set]),
  ets:new(wait_for_approve_data_base, [named_table, public, set]),
  ets:new(temporary_data_base, [named_table, public, set]),
  global:register_name(graphics, spawn(fun() -> graphics_init() end)),
  spawn_monitor(fun() -> init_scanner(?RECEIVED, ?RESOURCES) end),
  {ok, Nodes}.

handle_call(_Request, _From, State) ->
  io:format("gui_otp_server not support call functions.~n"),
  {reply, ok, State}.

terminate(Reason, State) ->
  io:format("gui_otp_server server shuting down.~nThe Reason was: ~p.~nServer state was: ~p~n.",[Reason, State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  io:format("gui_otp_server not support code changing operattion.~n"),
  {ok, State}.

%%%===================================================================
%%%                     Messages from Nodes                        %%%
%%%===================================================================

handle_cast({generate_position, Picture_Name, PosX, PosY}, Nodes) ->
  global:send(graphics, {generate_position, Picture_Name, PosX, PosY}),
  {noreply, Nodes};

handle_cast({self_kill, Picture_Name}, Nodes) ->
  io:format("server - {self_kill, Picture_Name}~n"),
  global:send(graphics, {self_kill, Picture_Name}),
  {noreply, Nodes};

handle_cast({self_kill, node, _Node}, Nodes) ->
  %TODO
  {noreply, Nodes};

handle_cast(Request, State) ->
  io:format("gui_otp_server - handle_cast: unknown message: ~p~n",[Request]),
  {noreply, State}.

%%%===================================================================
%%%                  Messages from local process                   %%%
%%%===================================================================

handle_info({insert, Picture_Name, Delay, TTL}, Nodes) ->
  {Node, New_Nodes_State} = get_owner(Nodes),
  ets:insert(wait_for_approve_data_base, {Picture_Name, Node}),
  gen_server:cast({global, Node}, {insert, Picture_Name, Delay, TTL}),
  {noreply, New_Nodes_State};

handle_info(Info, State) ->
  io:format("gui_otp_server - handle_info: unknown message: ~p~n",[Info]),
  {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

random_movement() ->
  R = rand:uniform(),
  if R < 0.5 -> -2;
    true -> 2
  end .

generate_position_check('$end_of_table', Picture_Name, PosX, PosY)  ->
  % TODO - check if NODE is the right one
  [{_, Node}] = ets:lookup(wait_for_approve_data_base, Picture_Name),
  ets:delete(wait_for_approve_data_base, Picture_Name),
  {MovX, MovY} = {random_movement(), random_movement()},
  ets:insert(data_base, {Picture_Name, Node, {PosX, PosY}, {MovX, MovY}}),
  gen_fsm:send_event({global, Picture_Name}, {generate_position, approved, {PosX, PosY}, {MovX, MovY}});

generate_position_check(Line, Picture_Name, NewPosX, NewPosY) ->
  [{_, _, {PosX, PosY}, _}] = ets:lookup(data_base, Line),
  case ((abs(PosY - NewPosY) =< ?ImgEdge) and (abs(PosX - NewPosX) =< ?ImgEdge)) of
    true  ->
      gen_fsm:send_event({global, Picture_Name}, {generate_position, reject});
    false ->
      generate_position_check(ets:next(data_base, Line), Picture_Name, NewPosX, NewPosY)
  end.

% ------------------------------------------------- %

graphics_init() ->
  wx:new(),
  Frame = wxFrame:new(wx:null(), -1, "Take-A-Pic",[{size,{?WIDTH,?HEIGHT}}]),
  wxFrame:show(Frame),
  graphics_process(Frame).

graphics_update_messages(0) -> 0;
graphics_update_messages(N) ->
  receive
    {update_position, Picture_Name, Pos, Mov} ->
      ets:update_element(data_base, Picture_Name, [{3, Pos}, {4, Mov}])
  after 0 -> ok end,
  graphics_update_messages(N-1).

graphics_message()  ->
  receive
    {generate_position, Picture_Name, PosX, PosY} ->
      generate_position_check(ets:first(data_base), Picture_Name, PosX, PosY),
      graphics_message();

    {insert_temporary, PID, Type, Owner, Pos} ->
      ets:insert(temporary_data_base, {PID, Type, Owner, Pos}),
      graphics_message();

    {insert_picture, {Picture_Name, Owner, {PosX, PosY}, {MovX, MovY}}} ->
      io:format("Picture insert at position {~p, ~p}.~n", [PosX, PosY]),
      ets:insert(data_base, {Picture_Name, Owner, {PosX, PosY}, {MovX, MovY}}),
      gen_server:cast({global, Owner}, {generate_position, approved, Picture_Name, PosX, PosY}),
      graphics_message();

    {self_kill, Picture_Name} ->
      ets:delete(data_base, Picture_Name),
      graphics_message();

    {kill_temporary, PID} ->
      ets:delete(temporary_data_base, PID),
      graphics_message()
  after 0 -> ok end.

graphics_process(Frame) ->
  graphics_message(),
  graphics_update_messages(10),
  collision_detect(ets:first(data_base)),
  show_graphics(Frame),
  graphics_process(Frame).

show_graphics(Frame) ->
  ClientDC = wxClientDC:new(Frame),
  BufferDC = wxBufferedDC:new(ClientDC),
  Background = wxBitmap:new(?BACKGROUND),
  wxDC:drawBitmap(BufferDC, Background, {0,0}),
  wxBitmap:destroy(Background),
  show_graphics(ets:first(data_base), BufferDC),
  %show_temporary_graphics(ets:first(temporary_data_base), BufferDC),
  wxBufferedDC:destroy(BufferDC),
  wxClientDC:destroy(ClientDC).

show_graphics('$end_of_table', _) -> ok;
show_graphics(Picture_Name, BufferDC) ->
  [{Name, _, Pos, _}] = ets:lookup(data_base, Picture_Name),
  Image = wxBitmap:new(atom_to_list(Name)),
  wxDC:drawBitmap(BufferDC, Image, Pos),
  %receive after ?DRAW_TIMEOUT -> ok end,
  wxBitmap:destroy(Image),
  show_graphics(ets:next(data_base, Picture_Name), BufferDC).

show_temporary_graphics('$end_of_table', _) -> ok;
show_temporary_graphics(Line, BufferDC) ->
  [{_, Type, _, Pos}] = ets:lookup(temporary_data_base, Line),
  Image = wxBitmap:new(Type),
  wxDC:drawBitmap(BufferDC, Image, Pos),
  %receive after ?DRAW_TIMEOUT -> ok end,
  wxBitmap:destroy(Image),
  show_temporary_graphics(ets:next(temporary_data_base, Line), BufferDC).

% ------------------------------------------------- %

collision_detect('$end_of_table') -> ok;
collision_detect(Picture) ->
  Next_Picture = ets:next(data_base, Picture),
  Line = ets:lookup(data_base, Picture),
  collision_detect(Line, Next_Picture),
  collision_detect(Next_Picture).

collision_detect(_, '$end_of_table') -> ok;
collision_detect(Picture_Line, Picture_To_Campre) ->
  Picture_Line_Campre = ets:lookup(data_base, Picture_To_Campre),
  {Flag, Mov, MovCmp} = collision_check(Picture_Line, Picture_Line_Campre),
  case Flag of
    false ->
      collision_detect(Picture_Line, ets:next(data_base, Picture_To_Campre));
    true  ->
      [{Picture_Name, _, _, OldMov}] = Picture_Line,
      [{Picture_Name_Cmp, _, _, OldCmpMov}]= Picture_Line_Campre,
      gen_fsm:send_event({global, Picture_Name}, {collision, Mov}),
      delete_messages_from_queue(Picture_Name, OldMov),
      gen_fsm:send_event({global, Picture_Name_Cmp}, {collision, MovCmp}),
      delete_messages_from_queue(Picture_Name_Cmp, OldCmpMov)
  end.

collision_check(Line1, Line2) ->
  [{_, _, {X1,Y1}, {MovX1, MovY1}}] = Line1,
  [{_, _, {X2,Y2}, {MovX2, MovY2}}] = Line2,
  Ydis = abs(Y1-Y2),
  Xdis = abs(X1-X2),
  case ((Xdis =< ?ImgEdge) and (Ydis =< ?ImgEdge)) of
    true  ->
      io:format("server colision~n"),
      case (Ydis < Xdis) of
        true  -> {true, {-1 * MovX1, MovY1}, {-1 * MovX2, MovY2}}; % side collision
        false -> {true, {MovX1, -1 * MovY1}, {MovX2, -1 * MovY2}}  % horizontal collision
      end;
    false ->
      {false, false, false}
  end.

delete_messages_from_queue(Picture_Name, Mov)->
  receive
    {update_position, Picture_Name, _, Mov} ->
      delete_messages_from_queue(Picture_Name, Mov)
  after
    0-> ok
  end.

% ------------------------------------------------- %

init_scanner(Received_Folder, Resources_Folder) ->
  io:format("init scanner procces.~n"),
  file_scanner(Resources_Folder, Received_Folder, true),
  file_scanner(Received_Folder, Resources_Folder, false).

file_scanner(Received_Folder, Resources_Folder,Debug)->
  {ok, File_Names} = file:list_dir(Received_Folder),
  File_Names_Dir = [Received_Folder ++ "/" ++ X || X <- File_Names],
  iterate_update_move(File_Names, File_Names_Dir, Resources_Folder, Debug),
  receive after ?FILE_SCANNER_DELAY -> ok end,
  case Debug of
    false -> file_scanner(Received_Folder, Resources_Folder, Debug);
    true  -> done
  end.

iterate_update_move([], [], _,_) -> ok;
iterate_update_move([H1 | File_Names], [H2 | File_Names_Dir], Resources_Folder, Debug)->
  file:copy(H2, Resources_Folder ++ "/" ++ H1),
  case Debug of
    false -> insert_picture(Resources_Folder ++ "/" ++ H1);
    true  -> ok
  end,
  file:delete(H2),
  iterate_update_move(File_Names, File_Names_Dir, Resources_Folder,Debug).

insert_picture(Picture_Name) ->
  Picture_Atom = list_to_atom(Picture_Name),
  gui_server ! {insert, Picture_Atom, ?PIC_PROCESS_DELAY, ?TTL}.