-module(picture_otp_server).
-author("Chen_Shay").

-include_lib("defines.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(My_Name, Gui_server) ->
  ServerName = {global, My_Name},
  Option = [],
  gen_server:start_link(ServerName, ?MODULE, Gui_server, Option).

init(Gui_server) ->
  io:format("Gui_server: ~p~n",[Gui_server]),
  ets:new(data_base, [named_table, public, set]),
  {ok, Gui_server, infinity}.

handle_call(_Request, _From, Gui_Server) ->
  {reply, ok, Gui_Server}.

handle_cast(_Request, Gui_Server) ->
  {noreply, Gui_Server}.

% ------------------------- %
% create new picture process.
handle_info({insert, Picture_Name, Delay, TTL}, Gui_Server) ->
  io:format("handle_info: insert picture event.~n"),
  Owner = self(),
  spawn(fun() -> picture_fsm:start_link({Picture_Name, Owner, Delay, TTL}) end),
  {noreply, Gui_Server};

handle_info({insert, Picture_Name, PID}, Gui_Server) ->
  ets:insert(data_base, {Picture_Name, PID}),
  {noreply, Gui_Server};

% ------------------------- %
% set picture start position.
handle_info({generate_position, Picture_Name, PosX, PosY}, Gui_Server) ->
  io:format("handle_info: process try to set pictures position.~n"),
  Gui_Server ! {generate_position, Picture_Name, PosX, PosY},
  {noreply, Gui_Server};

handle_info({generate_position, reject, Picture_Name}, Gui_Server) ->
  io:format("handle_info: picture position rejectd.~n"),
  send_to_picture(Picture_Name, bad_position),
  {noreply, Gui_Server};

handle_info({generate_position, approved, Picture_Name, PosX, PosY}, Gui_Server) ->
  io:format("handle_info: picture position has been approved.~n"),
  send_to_picture(Picture_Name, {good_position, {PosX, PosY}}),
  {noreply, Gui_Server};

% ---------------------------%
% send to gui_server updates.
handle_info({update_position, Picture_Name, New_Pos, New_Mov}, Gui_Server) ->
  io:format("handle_info: ~p picture position event.~n",[update_position]),
  Gui_Server ! {Picture_Name, New_Pos, New_Mov},
  {noreply, Gui_Server};

% ------------------------------------------%
% update picture data due to collision event.
handle_info({collision, Picture_Name, New_Pos, New_Mov}, Gui_Server) ->
  io:format("handle_info: ~p event.~n",[collision]),
  send_to_picture(Picture_Name, {collision, New_Pos, New_Mov}),
  {noreply, Gui_Server};

% ---------------------------%
% picture terminate himself.
handle_info({self_kill, Picture_Name}, Gui_Server) ->
  io:format("handle_info: process ~p event.~n",[self_kill]),
  Gui_Server ! {self_kill, Picture_Name},
  ets:delete(data_base, Picture_Name),
  {noreply, Gui_Server};

% terminate himself.
handle_info({kill, Picture_Name}, Gui_Server) ->
  io:format("handle_info: process ~p event.~n",[self_kill]),
  send_to_picture(Picture_Name, kill),
  ets:delete(data_base, Picture_Name),
  {noreply, Gui_Server};
% ---------------------------%
handle_info(_Info, State) ->
  %io:format("Unknown message has been arrived.~n"),
  {noreply, State}.


terminate(Reason, Gui_Server) ->
  A = {Reason, Gui_Server},
  io:format("Picture server shuting down: ~p.~n",[A]),
  Gui_Server ! {self_kill, self()},
  ok.

code_change(_OldVsn, State, _Extra) ->
  io:format("Not support code_change operattion.~n"),
  {ok, State}.

send_to_picture(Picture_Name, Data) ->
  [{_, PID}] = ets:lookup(data_base, Picture_Name),
  PID ! Data.
