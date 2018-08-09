-module(governer).
-author("Chen_Shay").

-behavior(supervisor).
-export([start_link/0, init/1,start_link_shell/0]).

%% API
%-export([start_link()]).
start_link_shell()->
  {ok,Pid} = supervisor:start_link({global,?MODULE}, ?MODULE,[]),
  unlink(Pid).

start_link() ->
  supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init(_Args) ->
  io:format("restart~n"),
  SupFlag = #{strategy  => one_for_one,
              intensity => 600,
              period    => 5000},
  Restart = permanent,
  Shutdown = infinity,
  Type = worker,

  Gui_Server       = {server,   {gui_otp_server, start_link, []}  , Restart, Shutdown, Type, [gui_otp_server]},
  Content_Listener = {listener, {content_Listener, start_link, []}, Restart, Shutdown, Type, [content_Listener]},
  {ok, {SupFlag, [Gui_Server, Content_Listener]}}.

%%  {ok, {SupFlag,
%%    [{server, %  identifier of he child.
%%      {gui_otp_server, start_link, []}, % function call used to start the child process.
%%      permanent, % child process is always restart.
%%      infinity,  % shutdown definition.
%%      worker,
%%      [gui_otp_server]}]}}.