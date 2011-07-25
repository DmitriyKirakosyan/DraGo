-module(drago).

-export([start_session/2]).

-define(interval, 5000).

start_session(UserId, Sock) ->
		State = drago_state:get_state(UserId),
		GameSession = sessions_manager:register_session(UseId, Sock, self(), State),
		loop(GameSession).

loop(GameSession) ->
		erlang:garbage_collect(self()),
		case game_protocol:parse_event(Sock) of
				{ok, {ActionId, Requets}} ->
						handle_event(GameSession, ActionId, Request),
						loop(GameSession);
				{error, timeout} ->
						loop(GameState);
				{error, Reason} ->
						io:format("error in receive~n")
		end.

handle_event(GameSession, ActionId, Request) ->
		Encoded = mochijson2:encode({ok, response}),
		send_event(GameSession#session.sock, {ActionId, iolist_to_binary(Encoded)}).

send_event(Sock, Response) ->
		game_protocol:send_event(Sock, Response).
