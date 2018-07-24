%%%-------------------------------------------------------------------
-module(file_scanner).
-author("osboxes").

%% API
-export([file_scanner/2]).


file_scanner(Folder,OtherFolder)->

  {ok, Filenames} = file:list_dir(Folder),
  Filenames_Dir = [Folder ++ "/" ++X || X<- Filenames],
  iterate_update_move(Filenames,Filenames_Dir,OtherFolder),
  receive after 500 ->file_scanner(Folder,OtherFolder) end.

iterate_update_move([],[],_)->ok;
iterate_update_move([H1|Filenames],[H2|Filenames_Dir],OtherFolder)->
  io:format("~s~n",[H1]),
  file:copy(H2,OtherFolder++ "/" ++ H1),
  file:delete(H2),
  iterate_update_move(Filenames,Filenames_Dir,OtherFolder).
