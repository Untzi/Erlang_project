
-module(picture_OTP_governor).
-author("Chen_Shay").

-behavior(supervisor).
-export([start_link/1, init/1,start_link_shell/1]).

%% API
%-export([start_link()]).
start_link_shell(Gui_Server)->
  {ok,Pid} = supervisor:start_link({global,?MODULE}, ?MODULE, Gui_Server),
  unlink(Pid).

start_link(Gui_Server) ->
  supervisor:start_link({global, ?MODULE}, ?MODULE, Gui_Server).

init(Gui_Server) ->
  io:format("restart~n"),
  SupFlag = #{strategy  => one_for_one,
    intensity => 600,
    period    => 5000},
  Restart = permanent,
  Shutdown = infinity,
  Type = worker,

  Node = {server,   {picture_otp_server, start_link, [Gui_Server]}  , Restart, Shutdown, Type, [gui_otp_server]},
  {ok, {SupFlag, [Node]}}.
