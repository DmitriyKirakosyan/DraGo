%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc batoo.

-module(batoo).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the batoo server.
start() ->
    batoo_deps:ensure(),
    ensure_started(crypto),
    %application:start(mongodb),
    application:start(batoo).


%% @spec stop() -> ok
%% @doc Stop the batoo server.
stop() ->
    %application:stop(monogodb),
    application:stop(batoo).
