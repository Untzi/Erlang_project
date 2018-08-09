
-module(client).
-author("Chen_Shay").
-include_lib("defines.hrl").
%% API
-export([client_upload/0]).


client_upload()->
  Wx = wx:new(),
  F = wxFrame:new(Wx,-1,"choose an image to upload"),
  D = wxFileDialog:new(F,[]),
  wxFileDialog:showModal(D),
  Path = wxFileDialog:getPath(D),
  io:format("~s~n",[Path]),
  {ok,P} = python:start([{python_path, ?PY_PATH},{python, "python2"}]),
  python:call(P,client,clientSend,[Path,?PY_SERVER_IP]),
  Path.
