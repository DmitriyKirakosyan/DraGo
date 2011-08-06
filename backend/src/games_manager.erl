-module(games_manager).
-export([start_link/0]).
-export([get_game/1, get_partner/1]).

-include("game.hrl").

start_link() ->
    Pid = spawn_link(fun start/0),
    case register(games_manager, Pid) of
				true ->
						{ok, Pid};
				_ ->
						{error, cant_register_game_manager}
    end.

start() ->
    loop([]).

loop(Games) ->
    receive
				{get_game, UserId, From} when is_binary(UserId) ->
						From ! findGame(UserId, Games),
						loop(Games);
				{register, UserId, StonesColor} when is_binary(StonesColor) ->
						case StonesColor of
								<<"white">> ->
										loop([#game{player_white=UserId} | Games]);
								_ ->
										loop([#game{player_black=UserId} | Games])
						end;
				_ ->
						loop(Games)
    end.

get_game(UserId) ->
    games_manager ! {get_game, UserId, self()},
    receive
				{ok, Game} ->
						{ok, Game};
				_ ->
						{error, none}
    after 1000 ->
						{error, timeout}
    end.

get_partner(UserId) ->
		games_manager ! {get_game, UserId, self()},
		receive {ok, Game} ->
						case Game#game.player_white =:= UserId of
								true ->
										Game#game.player_black;
								false ->
										Game#game.player_white
						end
		after 1000 ->
						{error, none}
		end.


findGame(_UserId, []) ->
    {error, not_exists};
findGame(UserId, [Game|Games]) when is_record(Game, game) ->
    case inGame(UserId, Game) of
				true ->
						{ok, Game};
				false -> findGame(UserId, Games)
    end.

inGame(UserId, Game) when Game#game.player_white == UserId orelse
													Game#game.player_black == UserId
													->
    true;
inGame(_UserId, _Game) ->
    false.
