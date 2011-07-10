-module(game_runner).
-export([start/0]).

start() ->
    application:start(sasl),
    application:start(game_app).
