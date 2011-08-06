-module(game_runner).
-export([start/0, stop/0]).

start() ->
    application:start(sasl),
		application:start(crypto),
		application:start(mongo_db),
    application:start(drago).

stop() ->
		application:stop(drago).
