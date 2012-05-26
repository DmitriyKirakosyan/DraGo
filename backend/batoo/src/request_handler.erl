-module(request_handler).

-export([handle/3]).

-include("batoo.hrl").

%% -spec handle(binary(), typle(), UserState) -> {ok, NewUserState, Reply}.

handle(<<"get_state">>, _RequestData, UserState = #user_state{in_game=true}) ->
    {ok, UserState, game_room:get_game_state(UserState#user_state.user_id)};

handle(<<"get_state">>, _RequestData, UserState) ->
    UserId = UserState#user_state.user_id,
    {ok, AllRequests} = game_room:get_request_list_for(UserId),
    case game_room:get_game_state(UserState#user_state.user_id) of
        {error, game_not_found} ->
            RequestsForMe = [proplists:get_value(white_user_id, Request) || Request <- AllRequests, proplists:get_value(black_user_id, Request) =:= UserId],
            RequestsByMe = [proplists:get_value(black_user_id, Request) || Request <- AllRequests, proplists:get_value(white_user_id, Request) =:= UserId],
            RequestsObject = [{for_me, RequestsForMe}, {by_me, RequestsByMe}],
            Reply = {ok, [{users, session_manager:get_users()}, {requests, RequestsObject}]},
            {ok, UserState, Reply};
        %% if game exists, change in_game to true and return game state
        GameState ->
            {ok, UserState#user_state{in_game = true}, GameState}
    end;

handle(<<"make_move">>, RequestData, UserState) ->
    UserId = UserState#user_state.user_id,
    X = proplists:get_value(<<"x">>, RequestData),
    Y = proplists:get_value(<<"y">>, RequestData),
    Hidden = proplists:get_value(<<"hidden">>, RequestData),
    {ok, UserState, game_room:make_move(UserId, X, Y, Hidden)};

handle(<<"pass">>, RequestData, UserState) ->
    UserId = UserState#user_state.user_id,
    {ok, UserState, game_room:pass(UserId)};

handle(<<"click_capture_stone">>, RequestData, UserState) ->
    UserId = UserState#user_state.user_id,
    X = proplists:get_value(<<"x">>, RequestData),
    Y = proplists:get_value(<<"y">>, RequestData),
    {ok, UserState, game_room:click_capture_stone(UserId, x, y)};

handle(<<"unclick_capture_stone">>, RequestData, UserState) ->
    UserId = UserState#user_state.user_id,
    X = proplists:get_value(<<"x">>, RequestData),
    Y = proplists:get_value(<<"y">>, RequestData),
    {ok, UserState, game_room:unclick_capture_stone(UserId, x, y)};

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
            {ok, UserState, {error, cant_start}}
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