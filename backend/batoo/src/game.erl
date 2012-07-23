-module (game).

-behaviour (gen_server).

-export ([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include ("batoo.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(WhiteUserId, BlackUserId) ->
    gen_server:start_link(?MODULE, [WhiteUserId, BlackUserId], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([WhiteUserId, BlackUserId]) ->
    WhitePlayer = #player{user_id=WhiteUserId},
    BlackPlayer = #player{user_id=BlackUserId},
    Game = #game{white_player = WhitePlayer, black_player = BlackPlayer,
                    phase=?BASIC_PHASE, stones=[], move_player=WhiteUserId},
    {ok, Game}.

%% state %%

handle_call({get_game_state, UserId}, _From, State) ->
    Reply = state_to_proplist(State, UserId),
    {reply, {ok, [{game, Reply}]}, State};

%% move %%
%% basic phase

handle_call({make_move, UserId, X, Y, _Hidden}, _From, State = #game{phase = ?BASIC_PHASE}) ->
    StoneColor = user_stone_color(State, UserId),
    UserStones = get_stones_by_color(StoneColor, State#game.stones),
    UserStonesLength = lists:flatlength(UserStones),
    {Reply, State1} = if
         UserStonesLength < 3 ->
            NewStoneNumber = get_new_stone_number(State#game.stones),
            {{ok, move_saved}, State#game{stones = [#stone{color=StoneColor, x=X, y=Y, hidden=false, basic=true, number=NewStoneNumber} | State#game.stones]}};
        true ->
            {{error, too_many_stones}, State}
    end,
    %% if 3 black and 3 white stones, then change phase to "main"
    WhiteStones = get_stones_by_color(white, State1#game.stones),
    BlackStones = get_stones_by_color(black, State1#game.stones),
    WhiteStonesLength = lists:flatlength(WhiteStones),
    BlackStonesLength = lists:flatlength(BlackStones),
    io:format("white stones length : ~p, black stones length : ~p~n", [WhiteStonesLength, BlackStonesLength]),
    NewState = if
        WhiteStonesLength =:= 3 andalso BlackStonesLength =:= 3 ->
            MergedStones = [Stone || Stone <- State1#game.stones, not has_same_placed_stone(Stone, State1#game.stones)],
            State1#game{phase = ?MAIN_PHASE, stones = MergedStones};
        true -> State1
    end,
    {reply, Reply, NewState};


%% main phase

handle_call({make_move, UserId, X, Y, Hidden}, _From, State = #game{move_player=UserId, phase = ?MAIN_PHASE}) ->
    StoneColor = user_stone_color(State, UserId),
    MovePlayer = get_opponent(State, UserId),
    NewStoneNumber = get_new_stone_number(State#game.stones),
    Stone = #stone{color=StoneColor, x=X, y=Y, hidden=Hidden, number=NewStoneNumber},
    NewState = State#game{move_player=MovePlayer, stones=[Stone | State#game.stones]},
    {reply, {ok, move_saved}, NewState};

handle_call({pass, UserId}, _From, State = #game{move_player=UserId, phase = ?MAIN_PHASE, stones = [#stone{pass = true} | _OtherStones]}) ->
    {reply, {ok, game_stopped}, State#game{phase=?END_PHASE}};

handle_call({pass, UserId}, _From, State = #game{move_player=UserId, phase = ?MAIN_PHASE}) ->
    StoneColor = user_stone_color(State, UserId),
    MovePlayer = get_opponent(State, UserId),
    
    Stones = State#game.stones,
    NewStoneNumber = get_new_stone_number(Stones),
    PassStone = #stone{color=StoneColor, number = NewStoneNumber, pass = true},
    {reply, {ok, pass_saved}, State#game{move_player=MovePlayer, stones = [PassStone | Stones]}};

handle_call({use_ability, UserId, AbilityName}, _From, State = #game{move_player=UserId, phase = ?MAIN_PHASE}) ->
    {ok, NewState, Reply} = ability_executor:execute(State, UserId, AbilityName),
    {reply, Reply, NewState}; %%TODO предусмотреть ошибку использования способности

%% end phase %%

handle_call({click_capture_stone, _UserId, X, Y}, _From, State = #game{phase = ?END_PHASE}) ->
    Clicks = State#game.clicks,
    case lists:member({X, Y}, Clicks) of
        false ->
            {reply, {ok, click_saved}, State#game{clicks = [{X, Y} | Clicks]}};
        _True ->    {reply, {error, already_clicked}, State}
    end;

handle_call({unclick_capture_stone, _UserId, Points}, _From, State = #game{phase = ?END_PHASE}) ->
    NewClicks = lists:filter(fun(Elem) ->
        not lists:member(Elem, Points)
    end, State#game.clicks),
    {reply, {ok, deleted}, State#game{clicks=NewClicks}};

handle_call({set_result_opinion, UserId, Opinion}, _From, State = #game{phase = ?END_PHASE}) ->
    {Reply, NewState} = case State#game.result_opinion of
        {OtherUserId, OtherOpinion} when OtherUserId =/= UserId andalso OtherOpinion =/= Opinion ->
            {{ok, game_finished}, State};
        {OtherUserId, _OtherOpinion} when OtherUserId =/= UserId ->
            {{error, conflict}, State#game{result_opinion=none}};
        _Else ->
            {{ok, opinion_saved}, State#game{result_opinion={UserId, Opinion}}}
    end,
    {reply, Reply, NewState};


handle_call({pass, _UserId}, _From, State) ->
    {reply, {error, denied}, State};

handle_call({make_move, _UserId, _X, _Y, _Hidden}, _From, State) ->
    {reply, {error, denied}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

user_stone_color(#game{white_player = #player{user_id = UserId}}, UserId) ->
    white;
user_stone_color(#game{black_player = #player{user_id = UserId}}, UserId) ->
    black.

get_opponent(State = #game{white_player = #player{user_id = UserId}}, UserId) ->
    State#game.black_player#player.user_id;
get_opponent(State = #game{black_player = #player{user_id = UserId}}, UserId) ->
    State#game.white_player#player.user_id.

state_to_proplist(State = #game{}, UserId) ->
    {MovePlayer, Stones} = if
        State#game.phase =:= ?BASIC_PHASE ->
            StoneColor = user_stone_color(State, UserId),
            {UserId, get_stones_by_color(StoneColor, State#game.stones)};
        true -> {State#game.move_player, State#game.stones} end,
    EncodedStones = stones_to_proplist(Stones),
    WhitePlayer = player_to_proplist(State#game.white_player),
    BlackPlayer = player_to_proplist(State#game.black_player),
    Clicks = lists:map( fun({X, Y}) -> [{x, X}, {y, Y}] end, State#game.clicks),
    [
        {phase, State#game.phase},
        {white_player, WhitePlayer},
        {black_player, BlackPlayer},
        {move_player, MovePlayer},
        {stones, EncodedStones},
        {clicks, Clicks}
    ].

player_to_proplist(Player = #player{}) ->
    [{user_id, Player#player.user_id}, {ability_list, Player#player.ability_list}].

stones_to_proplist(Stones) ->
    lists:map(
        fun(Stone) -> [{color, Stone#stone.color}, {x, Stone#stone.x},
                        {y, Stone#stone.y}, {hidden, Stone#stone.hidden},
                        {basic, Stone#stone.basic}, {number, Stone#stone.number},
                        {pass, Stone#stone.pass}]
        end
    , Stones).

    

get_new_stone_number([]) -> 0;
get_new_stone_number([Stone | _]) -> Stone#stone.number+1.

get_stones_by_color(Color, Stones) ->
    lists:filter(
        fun(Stone) -> Stone#stone.color =:= Color end
    , Stones).

has_same_placed_stone(PlacedStone, Stones) ->
    StonesWithXY = [Stone || Stone <- Stones, Stone#stone.x =:= PlacedStone#stone.x andalso Stone#stone.y =:= PlacedStone#stone.y],
    lists:flatlength(StonesWithXY) > 1.