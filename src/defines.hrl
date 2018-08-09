-define(BACKGROUND,filename:join(lists:droplast(filename:split(filename:absname(""))))++"/animations/" ++ "background.jpg").
-define(RECEIVED, filename:join(lists:droplast(filename:split(filename:absname(""))))++ "/received").
-define(RESOURCES, filename:join(lists:droplast(filename:split(filename:absname(""))))++ "/resources").
-define(PY_PATH,filename:join(lists:droplast(filename:split(filename:absname(""))))++"/pymodule").
-define(PY_SERVER_IP,"127.0.0.1").
-define(Pre_Processed,filename:join(lists:droplast(filename:split(filename:absname("")))) ++"/unprocessed").
-define(BOOM,  filename:join(lists:droplast(filename:split(filename:absname(""))))++"/animations/" ++ "BOOM.png").
-define(KAPAW, filename:join(lists:droplast(filename:split(filename:absname(""))))++"/animations/" ++ "kapaw.png").
-define(PAW,   filename:join(lists:droplast(filename:split(filename:absname(""))))++"/animations/" ++ "paw.png").

-define(STEP, 1).
-define(ImgEdge, 75).
-define(RADIUS,  25).
-define(WIDTH,  1500 ).
-define(HEIGHT, 1200).
-define(TTL, 3).

-define(FILE_SCANNER_DELAY, 1000).
-define(DRAW_DELAY, 0).
-define(GRAPHICS_LOOP_DELAY, 0).
-define(PIC_PROCESS_DELAY, 40).

-define(T, 1000).
