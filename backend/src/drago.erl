-module(drago).

-export([start_session/2]).

-define(interval, 5000).

start_session(UserId, Sock) ->
		loop().

loop() ->
		ok.
