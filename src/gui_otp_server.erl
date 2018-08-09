-module(gui_otp_server).
-author("Chen_Shay").

-include_lib("defines.hrl").
-include_lib("wx/include/wx.hrl").
-behaviour(gen_server).

%% API
-export([start/0, start_link/0, random_movement/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  My_Name = {global, gui_server},
  Option = [],
  gen_server:start_link(My_Name, ?MODULE, [], Option).

start() ->
  My_Name = {global, gui_server},
  Option = [],
  gen_server:start(My_Name, ?MODULE, [], Option).

init([]) ->
  io:format("Started GUI OTP Server.~n"),
  register(gui_server, self()),
  ets:new(data_base, [named_table, public, set]),
  ets:new(wait_for_approve_data_base, [named_table, public, set]),
  ets:new(temporary_data_base, [named_table, public, set]),
  global:register_name(graphics, spawn_link(fun() -> graphics_init() end)),
  spawn_link(fun() -> init_scanner(?RECEIVED, ?RESOURCES) end),
  {ok, []}.

handle_call({connect_node, New_Node}, _From, Nodes) ->
  io:format("some node has been arived. All Nodes: ~p~n", [[New_Node] ++ Nodes]),
  Replay = connected,
  case Nodes of
    [] ->
      spawn_link(fun() -> send_to_new_node_data(empty, New_Node, ets:first(wait_for_approve_data_base)) end),
      {reply, Replay, [New_Node]};
    _  ->
      [_, _, _, _, _, _, _, {_, Size}, _, _, _, _, _] = ets:info(data_base),
      spawn_link(fun() -> send_to_new_node_data(Nodes, New_Node, trunc(Size/(length(Nodes)+1))) end),
      {reply, Replay, Nodes ++ [New_Node]}
  end;

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

handle_cast({insert, Picture_Name, Node}, Nodes) ->
  ets:insert(wait_for_approve_data_base, {Picture_Name, Node}),
  gen_server:cast({global, Node}, {insert, Picture_Name, ?PIC_PROCESS_DELAY, ?TTL}),
  {noreply, Nodes};

handle_cast({generate_position, Picture_Name, PosX, PosY}, Nodes) ->
  global:send(graphics, {generate_position, Picture_Name, PosX, PosY}),
  {noreply, Nodes};

handle_cast({self_kill, Picture_Name}, Nodes) ->
  io:format("server - {self_kill, Picture_Name}~n"),
  global:send(graphics, {self_kill, Picture_Name}),
  {noreply, Nodes};

handle_cast(Request, State) ->
  io:format("unknown request: ~p~n",[Request]),
  {noreply, State}.

%%%===================================================================
%%%                  Messages from local process                   %%%
%%%===================================================================

handle_info({insert, Picture_Name}, _Nodes) ->
  Node = get_random_owner(nodes()),
  case Node of
    [] ->
      io:format("there is no nodes for pictures.~n"),
      ets:insert(wait_for_approve_data_base, {Picture_Name, no_owner});
    _  ->
      ets:insert(wait_for_approve_data_base, {Picture_Name, Node}),
      gen_server:cast({global, Node}, {insert, Picture_Name, ?PIC_PROCESS_DELAY, ?TTL})
  end,
  {noreply, nodes()};

handle_info(Info, State) ->
  io:format("unknown message: ~p~n",[Info]),
  {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

% if there are K nodes (with the new one) and N pictures so each node need to have trace(N/K) pictures.
% each node need to pass the new node Total/K - (current number of pictures) pictures.
send_to_new_node_data([H|T], New_Node, Limit) ->
  gen_server:cast({global, H}, {send_pictures_to_node, New_Node, Limit}),
  send_to_new_node_data(T, New_Node, Limit);
send_to_new_node_data([], _, _) -> connected;

% first node that enter to the server.
send_to_new_node_data(empty, _, '$end_of_table') -> connected;
send_to_new_node_data(empty, New_Node, Line) ->
  [{Picture_Name, _}] = ets:lookup(wait_for_approve_data_base, Line),
  ets:update_element(wait_for_approve_data_base, Picture_Name, {2, New_Node}),
  gen_server:cast({global, New_Node}, {insert, Picture_Name, ?PIC_PROCESS_DELAY, ?TTL}),
  send_to_new_node_data(empty, New_Node, ets:next(wait_for_approve_data_base, Line)).

get_random_owner([]) -> [];
get_random_owner(Nodes) ->
  Index = rand:uniform(length(Nodes)),
  lists:nth(Index,Nodes).

random_movement() ->
  R = rand:uniform(),
  if R < 0.5 -> -1 * ?STEP;
    true -> ?STEP
  end .

generate_position_check('$end_of_table', Picture_Name, PosX, PosY)  ->
  [{_, Node}] = ets:lookup(wait_for_approve_data_base, Picture_Name),
  ets:delete(wait_for_approve_data_base, Picture_Name),
  {MovX, MovY} = {random_movement(), random_movement()},
  ets:insert(data_base, {Picture_Name, Node, {PosX, PosY}, {MovX, MovY}, get_timestamp()}),
  gen_fsm:send_event({global, Picture_Name}, {generate_position, approved, {PosX, PosY}, {MovX, MovY}});

generate_position_check(Line, Picture_Name, NewPosX, NewPosY) ->
  [{_, _, {PosX, PosY}, _, _}] = ets:lookup(data_base, Line),
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
    {update_position, Picture_Name, Pos, Mov, Time_Stamp} ->
      case ets:update_element(data_base, Picture_Name, [{3, Pos}, {4, Mov}, {5, Time_Stamp}]) of
        false -> global:send(graphics, {insert_picture, {Picture_Name, global:whereis_name(Picture_Name), Pos, Mov}});
        true  -> all_good
      end
  after 0 -> ok end,
  graphics_update_messages(N-1).

graphics_message()  ->
  receive
    {update_Owner, Picture_Name, Node} ->
      ets:update_element(data_base, Picture_Name, {2, Node});

    {self_kill, Picture_Name} ->
      ets:delete(data_base, Picture_Name),
      graphics_message();

    {kill_temporary, Pos, _PictureType} ->
      ets:delete(temporary_data_base, Pos),
      graphics_message();

    {generate_position, Picture_Name, PosX, PosY} ->
      Data = ets:lookup(data_base, Picture_Name),
      case Data of
        [] -> generate_position_check(ets:first(data_base), Picture_Name, PosX, PosY);
        [{Picture_Name, _, Pos, Mov, _}] ->
          ets:delete(wait_for_approve_data_base, Picture_Name),
          ets:update_element(data_base, Picture_Name, {5, get_timestamp()}),
          gen_fsm:send_event({global, Picture_Name}, {generate_position, approved, Pos, Mov})
      end,
      graphics_message();

    {insert_picture, {Picture_Name, Owner, {PosX, PosY}, {MovX, MovY}}} ->
      io:format("Picture insert at position {~p, ~p}.~n", [PosX, PosY]),
      ets:insert(data_base, {Picture_Name, Owner, {PosX, PosY}, {MovX, MovY}, get_timestamp()}),
      gen_server:cast({global, Owner}, {generate_position, approved, Picture_Name, PosX, PosY}),
      graphics_message();

    {insert_temporary, Pos, PictureType} ->
      ets:insert(temporary_data_base, {Pos, PictureType}),
      graphics_message()

  after 0 -> ok end.

graphics_process(Frame) ->
  graphics_message(),
  graphics_update_messages(50),
  collision_detect_and_is_alive(ets:first(data_base), get_timestamp()),
  %ets_is_alive_scanner(ets:first(data_base), get_timestamp()),
  show_graphics(Frame),
  graphics_process(Frame).

show_graphics(Frame) ->
  ClientDC = wxClientDC:new(Frame),
  BufferDC = wxBufferedDC:new(ClientDC),
  Background = wxBitmap:new(?BACKGROUND),
  wxDC:drawBitmap(BufferDC, Background, {0,0}),
  wxBitmap:destroy(Background),
  show_temporary_graphics(ets:first(temporary_data_base), BufferDC),
  show_graphics(ets:first(data_base), BufferDC),
  wxBufferedDC:destroy(BufferDC),
  wxClientDC:destroy(ClientDC).

show_graphics('$end_of_table', _) -> ok;
show_graphics(Picture_Name, BufferDC) ->
  [{Name, _, Pos, _, _}] = ets:lookup(data_base, Picture_Name),
  Image = wxBitmap:new(atom_to_list(Name)),
  wxDC:drawBitmap(BufferDC, Image, Pos),
  wxBitmap:destroy(Image),
  show_graphics(ets:next(data_base, Picture_Name), BufferDC).

show_temporary_graphics('$end_of_table', _) -> ok;
show_temporary_graphics(Key, BufferDC) ->
  [{Pos, Type}] = ets:lookup(temporary_data_base, Key),
  Image = wxBitmap:new(Type),
  wxDC:drawBitmap(BufferDC, Image, Pos),
  wxBitmap:destroy(Image),
  show_temporary_graphics(ets:next(temporary_data_base, Key), BufferDC).

% ------------------------------------------------- %

collision_detect_and_is_alive('$end_of_table', _) -> ok;
collision_detect_and_is_alive(Picture, T) ->
  Line = ets:lookup(data_base, Picture),
  Next_Picture = ets:next(data_base, Picture),
  spawn_link(fun() -> collision_detect_and_is_alive(Next_Picture, T) end),

  case ets_is_alive_scanner(Picture, T) of
    delete -> ets:delete(data_base, Picture);
    update -> ignore;
    _      -> collision_detect(Line, Next_Picture)
  end. %,  collision_detect_and_is_alive(Next_Picture, T).


collision_detect(_, '$end_of_table') -> ok;
collision_detect(Picture_Line, Picture_To_Campre) ->
  Picture_Line_Campre = ets:lookup(data_base, Picture_To_Campre),
  {Flag, Mov, MovCmp} = collision_check(Picture_Line, Picture_Line_Campre),
  case Flag of
    false ->
      collision_detect(Picture_Line, ets:next(data_base, Picture_To_Campre));
    true  ->
      [{Picture_Name, _, _, OldMov, _}] = Picture_Line,
      [{Picture_Name_Cmp, _, _, OldCmpMov, _}]= Picture_Line_Campre,
      gen_fsm:send_event({global, Picture_Name}, {collision, Mov}),
      gen_fsm:send_event({global, Picture_Name_Cmp}, {collision, MovCmp}),
      spawn_link(fun() -> delete_messages_from_queue(Picture_Name, OldMov) end),
      spawn_link(fun() -> delete_messages_from_queue(Picture_Name_Cmp, OldCmpMov) end)
  end.

collision_check(Line1, Line2) ->
  [{_, _, {X1,Y1}, {MovX1, MovY1}, _}] = Line1,
  [{_, _, {X2,Y2}, {MovX2, MovY2}, _}] = Line2,
  Ydis = abs(Y1-Y2),
  Xdis = abs(X1-X2),
  case ((Xdis =< ?ImgEdge) and (Ydis =< ?ImgEdge)) of
    true  ->
      io:format("server detect colision~n"),
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

ets_is_alive_scanner(Picture, Time) ->
  [{Picture_Name, Owner, Pos, _, Time_Stamp}] = ets:lookup(data_base, Picture),
  T = Time - Time_Stamp,
  case (T > 2000) of
    true  ->
      case ((Picture_Name /= ?BOOM) and (Picture_Name /= ?KAPAW) and (Picture_Name /= ?PAW)) of
        false  -> delete;
        true ->
          New_Owner = get_random_owner(nodes()),
          gen_server:cast({global, Owner}, {kill, Picture_Name}),
          ets:update_element(data_base, Picture_Name, [{2, New_Owner}, {5, get_timestamp()}]),
          gen_server:cast({global, gui_server}, {insert, Picture_Name, New_Owner}),
          io:format("picture at ~p stoped~n", [Pos]),
          update
      end;
    false ->
      none
  end.

%%ets_is_alive_scanner('$end_of_table', _) -> ok;
%%ets_is_alive_scanner(Line, Time) ->
%%  [{Picture_Name, Owner, Pos, _, Time_Stamp}] = ets:lookup(data_base, Line),
%%  Next_Line = ets:next(data_base, Line),
%%  T = Time - Time_Stamp,
%%  case (T > 800) of
%%    true  ->
%%      case ((Picture_Name /= ?BOOM) and (Picture_Name /= ?KAPAW) and (Picture_Name /= ?PAW) and (T > 2000)) of
%%        true -> ets:delete(data_base, Picture_Name);
%%        false ->
%%          New_Owner = get_random_owner(nodes()),
%%          gen_server:cast({global, Owner}, {kill, Picture_Name}),
%%          ets:update_element(data_base, Picture_Name, [{2, New_Owner}, {5, get_timestamp()}]),
%%          gen_server:cast({global, gui_server}, {insert, Picture_Name, New_Owner}),
%%          io:format("picture at ~p stoped~n", [Pos])
%%      end;
%%    false ->
%%      ok
%%  end,
%%  ets_is_alive_scanner(Next_Line, Time).


% ------------------------------------------------- %

init_scanner(Received_Folder, Resources_Folder) ->
  io:format("init file scanner procces at server.~n"),
  %%file_scanner(Resources_Folder, Received_Folder, true),
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
    false ->
      io:format("file scanner found new picture.~n"),
      insert_picture(Resources_Folder ++ "/" ++ H1);
    true  ->
      ok
  end,
  file:delete(H2),
  iterate_update_move(File_Names, File_Names_Dir, Resources_Folder,Debug).

insert_picture(Picture_Name) ->
  Picture_Atom = list_to_atom(Picture_Name),
  gui_server ! {insert, Picture_Atom}.
