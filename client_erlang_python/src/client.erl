%%%-------------------------------------------------------------------
%%% @author osboxes
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2018 1:50 PM
%%%-------------------------------------------------------------------
-module(client).
-author("osboxes").

%% API
-export([client_upload/0]).


client_upload()->
  Wx = wx:new(),
  F = wxFrame:new(Wx,-1,"choose an image to upload"),
  D = wxFileDialog:new(F,[]),
  wxFileDialog:showModal(D),
  Path = wxFileDialog:getPath(D),
  io:format("~s~n",[Path]),
  {ok,P} = python:start([{python_path, filename:absname("")++"/pymodule"},{python, "python2"}]),
  python:call(P,client,clientSend,[Path,"osboxes"]),
  Path.
