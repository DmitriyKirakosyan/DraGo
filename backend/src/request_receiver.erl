-module(request_receiver).

-export([handle_event/3]).

-include("game.hrl").

handle_event(<<"move">>, UserId,  Params) when is_record(Move, game_move) ->
    X = proplists:get_value(<<"x">>, Params),
    Y = proplists:get_value(<<"y">>, Params),
    set_move(UserId, #game_move{x=X, y=Y}).

%just sending about move to partner
set_move(UserId, GameMove) ->
    PartnerId = games_manager:get_partner(UserId),
    PartnerPid = sessions_manager:get_user_pid(PartnerId),
    PartnerPid ! {move, GameMove}.
