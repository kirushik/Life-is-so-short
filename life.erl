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
    [0,0,0,0,0,0,0],
    [0,1,0,0,0,0,0],
    [0,0,1,0,0,0,0],
    [1,1,1,0,0,0,0],
    [0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0]
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
  toggle(add_padding(FIELD, 7)).

add_padding(FIELD, Ncols) ->
  [lists:duplicate(Ncols + 2, 0) |
    lists:append([add_row_padding(ROW) || ROW <- FIELD],
    [lists:duplicate(Ncols + 2, 0)])].

add_row_padding(ROW)->
    [0 | lists:append(ROW, [0])].

toggle([X,Y,Z|W])->
  [toggle_row(X,Y,Z)|toggle([Y,Z|W])];
toggle(_)->
  [].

toggle_row([A,B,C|R1],[D,E,F|R2],[G,H,I|R3])->
  [toggle_cell(A,B,C,D,E,F,G,H,I) |toggle_row([B,C|R1],[E,F|R2],[H,I|R3])];
toggle_row(_,_,_)->
  [].

toggle_cell(A,B,C,D,E,F,G,H,I)->
  dead_or_alive(E, A+B+C+D+F+G+H+I).

dead_or_alive(0, 3) -> 1;
dead_or_alive(1, 3) -> 1;
dead_or_alive(1, 2) -> 1;
dead_or_alive(_, _) -> 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
paint_life(FIELD)->
  paint_life(FIELD, 0).

paint_life([],_X)->
  ok;
paint_life([ROW|OTHER], X)->
  paint_row(ROW, X, 0),
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
