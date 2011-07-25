-module(session_manager).

-export([start_link/0]).
-include("game.hrl").

start_link() ->
		Pid = spawn_link(fun start/0),
		register(session, Pid),
		{ok, Pid}.

start() ->
		loop([]).

loop(Sessions) ->
		receive
				{register_session, From, UserId, Sock, Pid, State} when is_binary(UserId) andalso is_pid(Pid) andalso is_record(State, state) ->
						NewSession = #session {
							user_id=UserId,
							sock=Sock,
							pid=Pid,
							state=State,
							start_time=0
						 },
						From ! {new_session, NewSession},
						loop([NewSession | Sessions]);
				_ ->
						io:format("something wrong in sessions_manager"),
						loop(Sessions)
		end.
