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
    Game = #game{white_user_id=WhiteUserId, black_user_id=BlackUserId,
                    phase=?BASIC_PHASE, stones=[], move_player=WhiteUserId},
    {ok, Game}.

%% state %%

handle_call({get_game_state, UserId}, _From, State) ->
    {MovePlayer, Stones} = if
        State#game.phase =:= ?BASIC_PHASE ->
            StoneColor = if UserId =:= State#game.white_user_id -> white; true -> black end,
            {UserId, get_stones_by_color(StoneColor, State#game.stones)};
        true -> {State#game.move_player, State#game.stones} end,
    EncodedStones = lists:map(
        fun(Stone) -> [{color, Stone#stone.color}, {x, Stone#stone.x},
                        {y, Stone#stone.y}, {hidden, Stone#stone.hidden},
                        {basic, Stone#stone.basic}, {number, Stone#stone.number},
                        {pass, Stone#stone.pass}]
        end
    , Stones),
    Clicks = lists:map( fun({X, Y}) -> [{x, X}, {y, Y}] end, State#game.clicks),
    Reply = [{phase, State#game.phase}, {white_user_id, State#game.white_user_id}, {black_user_id, State#game.black_user_id},
                {move_player, MovePlayer}, {stones, EncodedStones}, {clicks, Clicks}],
    {reply, {ok, [{game, Reply}]}, State};

%% move %%
%% basic phase

handle_call({make_move, UserId, X, Y, _Hidden}, _From, State = #game{phase = ?BASIC_PHASE}) ->
    StoneColor = if UserId =:= State#game.white_user_id -> white; true -> black end,
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
    {StoneColor, MovePlayer} = if UserId =:= State#game.white_user_id -> {white, State#game.black_user_id};
                                    true -> {black, State#game.white_user_id} end,
    NewStoneNumber = get_new_stone_number(State#game.stones),
    NewState = State#game{move_player=MovePlayer, stones=[#stone{color=StoneColor, x=X, y=Y, hidden=Hidden, number=NewStoneNumber} | State#game.stones]},
    {reply, {ok, move_saved}, NewState};

handle_call({pass, UserId}, _From, State = #game{move_player=UserId, phase = ?MAIN_PHASE, stones = [#stone{pass = true} | _OtherStones]}) ->
    {reply, {ok, game_stopped}, State#game{phase=?END_PHASE}};

handle_call({pass, UserId}, _From, State = #game{move_player=UserId, phase = ?MAIN_PHASE}) ->
    {StoneColor, MovePlayer} = if UserId =:= State#game.white_user_id -> {white, State#game.black_user_id};
                                    true -> {black, State#game.white_user_id} end,

    Stones = State#game.stones,
    NewStoneNumber = get_new_stone_number(State#game.stones),
    PassStone = #stone{color=StoneColor, number = NewStoneNumber, pass = true},
    {reply, {ok, pass_saved}, State#game{move_player=MovePlayer, stones = [PassStone | Stones]}};

%% end phase %%

handle_call({click_capture_stone, _UserId, X, Y}, _From, State = #game{phase = ?END_PHASE}) ->
    Clicks = State#game.clicks,
    case lists:member({X, Y}, Clicks) of
        false ->
            {reply, {ok, click_saved}, State#game{clicks = [{X, Y} | Clicks]}};
        _True ->    {reply, {error, already_clicked}, State}
    end;

handle_call({unclick_capture_stone, _UserId, X, Y}, _From, State = #game{phase = ?END_PHASE}) ->
    {reply, {ok, deleted}, State#game{clicks=lists:delete({X, Y}, State#game.clicks)}};

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

get_new_stone_number([]) -> 0;
get_new_stone_number([Stone | _]) -> Stone#stone.number+1.

get_stones_by_color(Color, Stones) ->
    lists:filter(
        fun(Stone) -> Stone#stone.color =:= Color end
    , Stones).

has_same_placed_stone(PlacedStone, Stones) ->
    StonesWithXY = [Stone || Stone <- Stones, Stone#stone.x =:= PlacedStone#stone.x andalso Stone#stone.y =:= PlacedStone#stone.y],
    lists:flatlength(StonesWithXY) > 1.


make_board_matrix() ->
    [ { X, [ {Y, 0} || Y <- [0,1,2,3,4,5,6,7,8,9,10]] } || X <- [0,1,2,3,4,5,6,7,8,9,10]].


%%%===================================================================
%%% Tests
%%%===================================================================

make_board_matrix_test() ->
    [{0, [{0, 0} | [{1, 0} | _] ]} | [{1, [{0, 0} | _]} | _] ] = make_board_matrix().