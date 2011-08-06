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
    case game_protocol:parce_event(Sock) of
				{ok, {_ActionId, FirstRequest}} ->
						UserId = proplists:get_value(<<"user_id">>, FirstRequest),
						Module:start_session(UserId, Sock);
				_ ->
						{error, cant_parse}
		end.
