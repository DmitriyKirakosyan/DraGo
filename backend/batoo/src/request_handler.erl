-module(request_handler).

-export([handle/3]).

-include("batoo.hrl").

%% -spec handle(binary(), typle(), UserState) -> {ok, NewUserState, Reply}.

handle(<<"get_state">>, _RequestData, UserState = #user_state{in_game=true}) ->
    {ok, UserState, {ok, state}};

handle(<<"get_state">>, _RequestData, UserState) ->
    UserId = UserState#user_state.user_id,
    {ok, AllRequests} = game_room:get_request_list_for(UserId),
    RequestsForMe = [proplists:get_value(white_user_id, Request) || Request <- AllRequests, proplists:get_value(black_user_id, Request) =:= UserId],
    RequestsByMe = [proplists:get_value(black_user_id, Request) || Request <- AllRequests, proplists:get_value(white_user_id, Request) =:= UserId],
    RequestsObject = [{for_me, RequestsForMe}, {by_me, RequestsByMe}],
    Reply = {ok, [{users, session_manager:get_users()}, {requests, RequestsObject}]},
    {ok, UserState, Reply};

handle(<<"create_request">>, RequestData, UserState) ->
    FriendUserId = proplists:get_value(<<"friend_user_id">>, RequestData),
    Reply = case session_manager:has_session_for(FriendUserId) of
        true ->
            game_room:create_request(UserState#user_state.user_id, FriendUserId);
        _False ->
            {error, friend_is_offline}
    end,
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
            Reply = game_room:decline_request(OwnerUserId, FriendUserId),
            {ok, UserState, Reply};
        true ->
            {ok, UserState, {error, denied}}
    end;

handle(_, _, UserState) -> {ok, UserState, {ok, empty}}.