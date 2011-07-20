-module(game_sup).

-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link()->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, Port} = application:get_env(game_app, listen_port),
    SocketServer = {game_acceptor, {game_acceptor, start_link, [drago, Port]}, permament, 5000, worker, dynamic},
    
		GameSession = {session_manager, {session_manager, start_link, []}, permament, ?interval, worker, dynamic},

		{ok, {{one_for_one, 1, 1}, [SocketServer, GameSession]}}.
