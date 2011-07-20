-module(session_manager).

-export([start_link/0]).

start_link() ->
		Pid = spawn_link(fun start/0),
		register(session, Pid),
		{ok, Pid}.

start() ->
		loop([]).

loop(Sessions) ->
		receive
				{register_session, From, UserId, Sock, Pid, State} when is_binary(UserId) andalso is_pid(Pid) andalso is_record(State, state) ->
						NewSession = 
