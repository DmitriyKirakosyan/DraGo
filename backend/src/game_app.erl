-module(game_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    game_sup:start_link().

stop(_) ->
    ok.
