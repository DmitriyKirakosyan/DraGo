-module(game_authorize).
-export([authorize/2]).

authorize(Sock, Module) ->
    case catch start_game(Sock, Module) of
	{'EXIT', _Err} ->
	    auth_error;
	_ ->
	    ok
    end,
    gen_tcp:close(Sock).

start_game(Sock, Module) ->
    FirstRequset = game_protocol:parce_event(Sock, fun ({_ActionId, Request}) -> Request end),
    UserId = proplists:get_value(<<"user_id">>, FirstRequest),
    Module:start_session(UserId, Sock).

%    case gen_tcp:recv(Sock, 1, 5000) of
%	{ok, _} ->
%	_Err ->
%	    {error, auth_failed}
%    end.
