-module(game_sup).

-behaviour(supervisor).
-export([start_link/0]).

-export([init/1]).

-define(interval, 5000).

start_link()->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Port} = application:get_env(drago, listen_port),
    SocketServer = {game_acceptor, {game_acceptor, start_link, [drago, Port]}, permanent, 5000, worker, dynamic},
		Web = web_specs(drago_web, 4444),
		GameDB = {game_db, {game_db, start_link, []}, permanent, 5000, worker, dynamic},
		GameSession = {session_manager, {session_manager, start_link, []}, permanent, ?interval, worker, dynamic},
		{ok, {{one_for_one, 1, 1}, 
					[SocketServer, Web, GameDB, GameSession]}}.

web_specs(Mod, Port) ->
		Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end,
    WebConfig = [{ip, Ip},
                 {port, Port},
                 {docroot, drago_deps:local_path(["priv", "www"])}],
    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.
