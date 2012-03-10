-module(life).
-export([run/0]).

run()->
  application:start(cecho),

  %% Set attributes
  cecho:cbreak(),
  cecho:noecho(),

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
  cecho:curs_set(0),

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
    $e ->
      %% If we get a 'e' then enter edit mode
      edit_loop(FIELD);
    _ ->
      %% ignore anything else
      event_loop(FIELD)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
edit_loop(FIELD)->
  edit_loop(FIELD,0,0).

edit_loop(FIELD,X,Y) when X<0 ->
  edit_loop(FIELD,0,Y);
edit_loop(FIELD,X,Y) when Y<0 ->
  edit_loop(FIELD,X,0);
edit_loop(FIELD,X,Y)->

  cecho:curs_set(1),

  paint_life(FIELD),
  cecho:move(X,Y),
  cecho:refresh(),

  C1 = cecho:getch(),
  case C1 of
    $q ->
      %% If we get a 'q' then exit
      application:stop(cecho),
      erlang:halt();
    $e ->
      %% Turn off editor mode
      event_loop(FIELD);
    27 ->
      %% Extended keycode
      process_keycode(FIELD,X,Y);
    $  ->
      toggle_cell(FIELD,X,Y);
    _ ->
      edit_loop(FIELD, X, Y)
  end.

toggle_cell(FIELD, X, Y)->
  {_, [ROW|_]} = lists:split(X, FIELD),
  NEW_ROW=setnth(Y, ROW, 1-lists:nth(Y+1, ROW)),
  NEW_FIELD = setnth(X, FIELD, NEW_ROW),
  edit_loop(NEW_FIELD, X, Y).

%% setnth(Index, List, NewElement) -> List.
setnth(0, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

process_keycode(FIELD,X,Y)->
  C2 = cecho:getch(),
  case C2 of
    91 ->
      C3 = cecho:getch(),
      case C3 of
        65 ->
          %% Up keycode
          edit_loop(FIELD, X-1, Y);
        66 ->
          %% Down keycode
          edit_loop(FIELD, X+1, Y);
        67 ->
          %% Right keycode
          edit_loop(FIELD, X, Y+1);
        68 ->
          %% Left keycode
          edit_loop(FIELD, X, Y-1);
        _ ->
          edit_loop(FIELD, X, Y)
      end;
    _ ->
      edit_loop(FIELD, X, Y)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iterate_field(FIELD)->
  iterate_padded_field(add_padding(FIELD, 7)).

add_padding(FIELD, Ncols) ->
  [lists:duplicate(Ncols + 2, 0) |
    lists:append([add_row_padding(ROW) || ROW <- FIELD],
    [lists:duplicate(Ncols + 2, 0)])].

add_row_padding(ROW)->
    [0 | lists:append(ROW, [0])].

iterate_padded_field([X,Y,Z|W])->
  [iterate_row(X,Y,Z)|iterate_padded_field([Y,Z|W])];
iterate_padded_field(_)->
  [].

iterate_row([A,B,C|R1],[D,E,F|R2],[G,H,I|R3])->
  [iterate_cell(A,B,C,D,E,F,G,H,I) |iterate_row([B,C|R1],[E,F|R2],[H,I|R3])];
iterate_row(_,_,_)->
  [].

iterate_cell(A,B,C,D,E,F,G,H,I)->
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
