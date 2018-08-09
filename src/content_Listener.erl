-module(content_Listener).
-behaviour(gen_server).
-include_lib("defines.hrl").
-author("Chen_Shay").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  io:format("Started content listener~n"),
  spawn_link(fun() -> start_content_listener() end),
  {ok, []}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_content_listener() ->
%%  os:cmd("fuser -k 6020/udp"),
  {ok,P} = python:start([{python_path, ?PY_PATH},{python, "python2"}]),
  python:call(P,server,listen_for_image,[?Pre_Processed++"/",1,
    ?PY_SERVER_IP,?RECEIVED ++ "/"]).