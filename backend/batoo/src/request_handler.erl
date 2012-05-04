-module(request_handler).

-export([handle/3]).

-include("batoo.hrl").

%% -spec handle(binary(), typle(), UserState) -> {ok, NewUserState, Reply}.

handle(<<"get_state">>, _RequestData, UserState) ->
    {ok, UserState, {ok, [{users_online, 0}]}};

handle(<<"create_request">>, RequestData, UserState) ->
    FriendUserId = proplists:get_value(<<"friend_user_id">>, RequestData),
    Reply = game_room:create_request(UserState#user_state.user_id, FriendUserId),
    {ok, UserState, Reply};

handle(<<"approve_request">>, RequestData, UserState) ->
    OwnerUserId = proplists:get_value(<<"owner_user_id">>, RequestData),
    case game_room:approve_request(UserState#user_state.user_id, OwnerUserId) of
        OkReply = {ok, game_started} ->
            {ok, UserState#user_state{active=false}, OkReply};
        _Error ->
            {ok, UserState, {error, cant_started}}
    end;

handle(_, _, UserState) -> {ok, UserState, {ok, empty}}.