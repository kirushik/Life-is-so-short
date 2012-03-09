-module(life).
-export([run/0]).

run()->
  application:start(cecho),

  %% Set attributes
  cecho:cbreak(),
  cecho:noecho(),

  paint_life(generate()),

  event_loop().

generate()->[
    [0,0,0,0,0,0,0],
    [0,0,1,0,0,0,0],
    [0,0,1,0,0,0,0],
    [0,0,1,0,0,0,0],
    [0,0,0,0,0,0,0],
    [0,0,0,0,0,1,0],
    [0,0,0,0,0,0,0]
  ].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
event_loop()->
  C = cecho:getch(),
  case C of
    $q ->
      %% If we get a 'q' then exit the mover and stop cecho
      application:stop(cecho),
      erlang:halt();
    _ ->
      %% ignore anything else
      event_loop()
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
  io:put_chars("#");
paint_cell(_)->
  io:put_chars(".").
