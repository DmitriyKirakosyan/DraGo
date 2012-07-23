-module (ability_executor).
%% @doc Используется исключительно модулем game

-export ([execute/4]).

-include ("batoo.hrl").

-spec execute(#game{}, binary(), atom(), tuple()) -> {ok, #game{}, any()}.
execute(State, UserId, AbilityName, Params) ->
    Player = get_player(State, UserId),
    AbilityList = Player#player.ability_list,
    AbilNum = proplists:get_value(AbilityName, AbilityList, 0),
    if AbilNum > 0 ->
        {ok, NewState, Reply} = execute_ability(State, UserId, AbilityName, Params),
        NewAbilList = [
            {AbilityName, AbilNum - 1} |
            proplists:delete(AbilityName, AbilityList)
        ],
        FinalyState = set_player(NewState, Player#player{ability_list = NewAbilList}),
        {ok, FinalyState, Reply}; 
    true ->
        {ok, State, {error, no_ability}}
    end.

-spec execute_ability(#game{}, binary(), atom(), tuple()) -> {ok, #game{}, any()}.
execute_ability(_State, UserId, add_hidden_stone, Params) ->
    X = proplists:get_value(<<"x">>, Params),
    Y = proplists:get_value(<<"y">>, Params),
    {reply, Reply, NewState} = gen_server:call(self(), {make_move, UserId, X, Y, true}),
    {ok, NewState, Reply}.


%% Internal functions %%

get_player(State = #game{white_player = #player{user_id = UserId}}, UserId) ->
    State#game.white_player;
get_player(State = #game{black_player = #player{user_id = UserId}}, UserId) ->
    State#game.black_player.	

set_player(State = #game{white_player = #player{user_id = UserId}}, Player = #player{user_id = UserId}) ->
    State#game{white_player = Player};
set_player(State = #game{black_player = #player{user_id = UserId}}, Player = #player{user_id = UserId}) ->
    State#game{black_player = Player}.