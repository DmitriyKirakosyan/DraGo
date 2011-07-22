-module(games_manager).
-export([start_link/0]).

-include("game.hrl").

start_link() ->
    Pid = spawn_link(fun start/0),
    case register(games_manager, Pid) of
				true ->
						{ok, Pid};
				False ->
						{error, cant_register_game_manager}
    end.

start() ->
    loop([]).

loop(Games) ->
    receive
				{get_game, UserId, From} when is_binary(UserId) ->
						From ! findGame(UseId, Games),
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

get_game(UseId) ->
    games_manager ! {get_game, UserId, self()},
    receive
				{ok, Game} ->
						{ok, Game};
				_ ->
						{error, none}
    after 1000 ->
						{error, timeout}
    end.

get_partner(UseId) ->
		games_manager ! {get_game, UserId, self()},
		receive {ok, Game} ->
						case Game#game.player_white =:= UserId of
								true ->
										Game#game.player_black;
								false ->
										game#game.player_white;
								_ ->
										{error, bad_match_dima_idiot}
						end
		after 1000 ->
						{error, none}
		end.


findGame(UserId, []) ->
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
