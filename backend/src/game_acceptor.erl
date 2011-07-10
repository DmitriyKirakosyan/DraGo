-module(game_acceptor).
-export([start_link/2]).

start_link(Module, Port) ->
    {ok, spawn_link(fun () ->
			    start(Module, Port) end)}.

start(Module, Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary,
					 {packet, 0},
					 {send_timeout_close, true},
					 {send_timeout, 5000},
					 {reuseaddr, true},
					 {active, false}]),
    accept(Listen, Module).

accept(Listen, Module) ->
    case catch gen_tcp:accept(Listen) of
	{ok, Sock} ->
	    spawn(fun () -> game_authorize:authorize(Sock, Module) end);
	{error, Reason} ->
	    error_logger:error_msg("error:~p~n", [Reason]);
	Other ->
	    error_logger:error_msg("Other error~p~n", [Other])
    end,
    accept(Listen, Module).
