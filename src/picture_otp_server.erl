-module(picture_otp_server).
-author("Chen_Shay").

-include_lib("defines.hrl").

-behaviour(gen_server).

%% API
-export([start/1, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Gui_Server) ->
  ServerName = {global, node()},
  Option = [],
  gen_server:start_link(ServerName, ?MODULE, Gui_Server, Option).

start(Gui_Server) ->
  ServerName = {global, node()},
  Option = [],
  gen_server:start(ServerName, ?MODULE, Gui_Server, Option).

init(Gui_Server) ->
  cover:compile_directory(),
  ets:new(data_base, [named_table, public, set]),
  net_kernel:connect(Gui_Server),
  try_to_connect(Gui_Server),
  spawn_link(fun() -> keep_connection(Gui_Server) end),
  {ok, [], infinity}.

handle_call(_Request, _From, State) ->
  io:format("picture_otp_server not support call functions.~n"),
  {reply, ok, State}.

terminate(Reason, State) ->
  io:format("gui_otp_server server shuting down.~nServer state was: ~p.~nThe Reason was: ~p.~n",[Reason, State]),
  gen_server:cast({global, gui_server}, {self_kill, node, self()}),
  ok.

code_change(_OldVsn, State, _Extra) ->
  io:format("picture_otp_server not support code changing operattion.~n"),
  {ok, State}.

%%%===================================================================
%%%                   Messages from Gui Server                     %%%
%%%===================================================================

% create new picture process.
handle_cast({insert, Picture_Name, Delay, TTL}, State) ->
  ets:insert(data_base, {Picture_Name, empty_pid}),
  Owner = self(),
  spawn(fun() -> picture_fsm:start({Picture_Name, Owner, Delay, TTL}) end),
  io:format("New picture has arrived from the server - creat a new picture_fsm process.~n"),
  {noreply, State};

handle_cast({send_pictures_to_node, Node, Limit}, State) ->
  [_, _, _, _, _, _, _, {_, Size}, _, _, _, _, _] = ets:info(data_base),
  io:format("Start sending pictures to node ~p.~n", [Node]),
  send_pictures(Node, Size, Limit),
  {noreply, State};

handle_cast({moving, {Picture_Name, Pos, Mov, Collision, Delay, TTL}}, State) ->
  io:format("New picture has arrived from another node - creat a new picture_fsm process.~n"),
  ets:insert(data_base, {Picture_Name, empty_pid}),
  Owner = self(),
  spawn_monitor(fun() -> picture_fsm:start({Picture_Name, Owner, Pos, Mov, Collision, Delay, TTL}) end),
  receive after 8 -> ok end,
  {noreply, State};

handle_cast({kill, Picture_Name}, State) ->
  Data = ets:lookup(data_base, Picture_Name),
  case Data of
    [] -> ok;
    [{Picture_Name, empty_pid}] -> ok;
    [{Picture_Name, PID}] -> PID ! terminate
  end,
  {noreply, State};

% unknown message handle.
handle_cast(Request, State) ->
  io:format("picture_otp_server received unknown cast message:~n~p~n",[Request]),
  {noreply, State}.

%%%===================================================================
%%%                 Messages from picture process                  %%%
%%%===================================================================

handle_info({update_pid, Picture_Name, PID}, State) ->
  ets:update_element(data_base, Picture_Name, {2, PID}),
  {noreply, State};

% picture termination.
handle_info({self_kill, Picture_Name}, State) ->
  io:format("Picture process terminate.~n"),
  ets:delete(data_base, Picture_Name),
  gen_server:cast({global, gui_server}, {self_kill, Picture_Name}),
  {noreply, State};

handle_info({self_kill_pid, PID}, State) ->
  io:format("Picture process terminate.~n"),
  find_pid(ets:first(data_base), PID),
  {noreply, State};

% unknown message handle.
handle_info(Info, State) ->
  io:format("picture_otp_server received unknown info message:~n~p~n",[Info]),
  {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

keep_connection(Gui_Server) ->
  receive after 500 ->
    case lists:member(Gui_Server, nodes()) of
      true  -> ok;
      false ->
        io:format("Connection with the server has been terminated.~n"),
        try_to_connect(Gui_Server)
    end
  end,
  keep_connection(Gui_Server).

try_to_connect(Gui_Server) ->
  net_kernel:connect(Gui_Server),
  receive after 1000 ->
    try
      Return = gen_server:call({global, gui_server}, {connect_node, node()}),
      io:format("Connection with server status: ~p~n",[Return])
    catch
     _:_ -> try_to_connect(Gui_Server)
    end
  end.

send_pictures(_,    Size, Limit) when Size =< Limit -> done;
send_pictures(Node, Size, Limit) ->
  [{Picture_Name, PID}] = ets:lookup(data_base, ets:first(data_base)),
  try
    gen_fsm:send_event(PID, {move_to_node, Node}),
    global:send(graphics, {update_Owner, Picture_Name, Node})
  of
    _-> io:format("Sent picture to another node.~n")
    catch
      _:_ -> ok
  end,
  ets:delete(data_base, Picture_Name),
  send_pictures(Node, Size - 1, Limit).

find_pid('$end_of_table', _PID) -> done;
find_pid(Picture_Name, PID) ->
  [{_, PID_Tmp}] = ets:lookup(data_base, Picture_Name),
  case (PID_Tmp =:= PID) of
    true  -> gen_server:cast({global, gui_server}, {self_kill, Picture_Name});
    false -> find_pid(ets:next(data_base, Picture_Name), PID)
  end.