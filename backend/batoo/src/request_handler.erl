-module(request_handler).

-export([handle/3]).

-include("batoo.hrl").

%% -spec handle(binary(), typle(), UserState) -> {ok, NewUserState, Reply}.

handle(<<"get_state">>, _RequestData, UserState) ->
    Reply = case UserState#user_state.in_game of
        true ->
            {ok, state};
        _False ->
            {ok, [{users, session_manager:get_users()}]}
    end,
    {ok, UserState, Reply};

handle(<<"create_request">>, RequestData, UserState) ->
    FriendUserId = proplists:get_value(<<"friend_user_id">>, RequestData),
    Reply = game_room:create_request(UserState#user_state.user_id, FriendUserId),
    {ok, UserState, Reply};

handle(<<"approve_request">>, RequestData, UserState) ->
    OwnerUserId = proplists:get_value(<<"owner_user_id">>, RequestData),
    case game_room:approve_request(UserState#user_state.user_id, OwnerUserId) of
        OkReply = {ok, game_started} ->
            {ok, UserState#user_state{in_game=true}, OkReply};
        _Error ->
            {ok, UserState, {error, cant_started}}
    end;

handle(<<"decline_request">>, RequestData, UserState) ->
    OwnerUserId = proplists:get_value(<<"owner_user_id">>, RequestData),
    FriendUserId = proplists:get_value(<<"friend_user_id">>, RequestData),
    if
        OwnerUserId =:=  UserState#user_state.user_id orelse FriendUserId =:= UserState#user_state.user_id ->
            game_room:decline_request(OwnerUserId, FriendUserId),
            {ok, UserState, {ok, declined}};
        true ->
            {ok, UserState, {error, denied}}
    end;

handle(_, _, UserState) -> {ok, UserState, {ok, empty}}.