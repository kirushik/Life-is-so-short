-module(life).
-export([run/0]).

run()->
  application:start(cecho),

  %% Set attributes
  cecho:cbreak(),
  cecho:noecho(),

  cecho:curs_set(0),

  FIELD = generate(),
  event_loop(FIELD).

generate()->[
    [0,0,1,0,0,0,0],
    [0,0,1,0,0,0,0],
    [0,0,1,0,0,0,0],
    [0,0,1,0,0,0,0],
    [0,0,1,0,0,0,0],
    [0,0,1,0,0,1,0],
    [0,0,1,0,0,0,0]
  ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
event_loop(FIELD)->
  paint_life(FIELD),
  cecho:refresh(),
  cecho:move(0,0),

  C = cecho:getch(),
  case C of
    $q ->
      %% If we get a 'q' then exit
      application:stop(cecho),
      erlang:halt();
    $  ->
      %% If we get a ' ' then iterate
      NEW_FIELD = iterate_field(FIELD),
      event_loop(NEW_FIELD);
    _ ->
      %% ignore anything else
      event_loop(FIELD)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iterate_field(FIELD)->
  toggle(FIELD).

toggle([])->
  [];
toggle([ROW|OTHER])->
 [toggle_row(ROW) | toggle(OTHER)].

toggle_row([])->
  [];
toggle_row([A|OTHER])->
 [1-A | toggle_row(OTHER)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
paint_life(FIELD)->
  paint_life(FIELD, 0).

paint_life([],_X)->
  ok;
paint_life([ROW|OTHER], X)->
  paint_row(ROW, X, 0),
  io:format("~n"),
  paint_life(OTHER, X+1).

paint_row([], _X, _Y)->
  ok;
paint_row([CELL|OTHER], X, Y)->
  paint_cell(CELL, X, Y),
  paint_row(OTHER, X, Y+1).

paint_cell(1, X, Y)->
  cecho:mvaddstr(X, Y, "#");
paint_cell(_, X, Y)->
  cecho:mvaddstr(X, Y, ".").
