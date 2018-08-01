-module(picture_otp_server).
-author("Chen_Shay").

-include_lib("defines.hrl").

-behaviour(gen_server).

%% API
-export([start/2, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Gui_server, My_Name) ->
  ServerName = {global, My_Name},
  Option = [],
  gen_server:start_link(ServerName, ?MODULE, Gui_server, Option).

start(Gui_server, My_Name) ->
  ServerName = {global, My_Name},
  Option = [],
  gen_server:start(ServerName, ?MODULE, Gui_server, Option).

init(Gui_server) ->
  ets:new(data_base, [named_table, public, set]),
  {ok, Gui_server, infinity}.

handle_call(_Request, _From, Gui_Server) ->
  io:format("picture_otp_server not support call functions.~n"),
  {reply, ok, Gui_Server}.

terminate(Reason, Gui_Server) ->
  io:format("gui_otp_server server shuting down.~nThe Reason was: ~p.~nServer state was: ~p~n.",[Reason, Gui_Server]),
  gen_server:cast({global, Gui_Server}, {self_kill, node, self()}),
  ok.

code_change(_OldVsn, State, _Extra) ->
  io:format("picture_otp_server not support code changing operattion.~n"),
  {ok, State}.

send_to_picture(Picture_Name, Data) ->
  [{_, PID}] = ets:lookup(data_base, Picture_Name),
  case PID of
    killed -> ok;
    _      -> PID ! Data
  end.

%%%===================================================================
%%%                   Messages from Gui Server                     %%%
%%%===================================================================

% create new picture process.
handle_cast({insert, Picture_Name, Delay, TTL}, Gui_Server) ->
  io:format("handle_cast: insert picture event.~n"),
  ets:insert(data_base, {Picture_Name, empty_pid}),
  Owner = self(),
  spawn(fun() -> picture_fsm:start({Picture_Name, Owner, Delay, TTL}) end),
  {noreply, Gui_Server};

% terminate picture process.
handle_cast({kill, Picture_Name}, Gui_Server) ->
  io:format("handle_cast: process ~p event.~n",[self_kill]),
  send_to_picture(Picture_Name, kill),
  %ets:delete(data_base, Picture_Name),
  ets:update_element(data_base, Picture_Name, {2,killed}),
  {noreply, Gui_Server};

% unknown message handle.
handle_cast(Request, Gui_Server) ->
  io:format("picture_otp_server - handle_cast: unknown message: ~p~n",[Request]),
  {noreply, Gui_Server}.

%%%===================================================================
%%%                 Messages from picture process                  %%%
%%%===================================================================

handle_info({update_pid, Picture_Name, PID}, Gui_Server) ->
  ets:update_element(data_base, Picture_Name, {2, PID}),
  {noreply, Gui_Server};

% picture termination.
handle_info({self_kill, Picture_Name}, Gui_Server) ->
  io:format("handle_info: process ~p was terminate.~n",[self_kill]),
  %ets:delete(data_base, Picture_Name),
  ets:update_element(data_base, Picture_Name, {2,killed}),
  gen_server:cast({global, Gui_Server}, {self_kill, Picture_Name}),
  {noreply, Gui_Server};

% unknown message handle.
handle_info(Info, State) ->
  io:format("picture_otp_server - handle_info: unknown message: ~p~n",[Info]),
  {noreply, State}.