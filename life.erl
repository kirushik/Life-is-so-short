-module(life).
-export([hello/0]).

hello()->
  paint_life([[1,0],[0,1]]).

paint_life([])->
  ok;
paint_life([ROW|OTHER])->
  paint_row(ROW),
  io:format("~n"),
  paint_life(OTHER).

paint_row([])->
  ok;
paint_row([CELL|OTHER])->
  paint_cell(CELL),
  paint_row(OTHER).

paint_cell(1)->
  io:put_chars("X");
paint_cell(_)->
  io:put_chars(".").
