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

handle_call({get_game_state, UserId}, _From, State) ->
    Stones = lists:map(
        fun(Stone) -> [{color, Stone#stone.color}, {x, Stone#stone.x},
                        {y, Stone#stone.y}, {hidden, Stone#stone.hidden}, {base, Stone#stone.basic}]
        end
    , State#game.stones),
    MovePlayer = if State#game.phase =:= ?BASIC_PHASE -> UserId; true -> State#game.move_player end,
    Reply = [{phase, State#game.phase}, {white_user_id, State#game.white_user_id}, {black_user_id, State#game.black_user_id},
                {move_player, MovePlayer}, {stones, Stones}],
    {reply, {ok, [{game, Reply}]}, State};

%% move %%
%% basic phase

handle_call({make_move, UserId, X, Y, _Hidden}, _From, State = #game{phase = ?BASIC_PHASE}) ->
    StoneColor = if UserId =:= State#game.white_user_id -> white; true -> black end,
    UserStones = get_stones_by_color(StoneColor, State#game.stones),
    UserStonesLength = lists:flatlength(UserStones),
    {Reply, State1} = if
         UserStonesLength < 3 ->
            {{ok, move_saved}, State#game{stones = [#stone{color=StoneColor, x=X, y=Y, hidden=false} | State#game.stones]}};
        true ->
            {{error, too_many_stones}, State}
    end,
    %% if 3 black and 3 white stones, then change phase to "main"
    WhiteStonesLength = lists:flatlength(get_stones_by_color(white, State#game.stones)),
    BlackStonesLength = lists:flatlength(get_stones_by_color(black, State#game.stones)),
    NewState = if
        WhiteStonesLength =:= 3 andalso BlackStonesLength =:= 3 ->
            State1#game{phase = ?MAIN_PHASE};
        true -> State1
    end,
    {reply, Reply, NewState};

%% main phase

handle_call({make_move, UserId, X, Y, Hidden}, _From, State = #game{move_player=UserId, phase = ?MAIN_PHASE}) ->
    {StoneColor, MovePlayer} = if UserId =:= State#game.white_user_id -> {white, State#game.black_user_id};
                                    true -> {black, State#game.white_user_id} end,
    NewState = State#game{move_player=MovePlayer, stones=[#stone{color=StoneColor, x=X, y=Y, hidden=Hidden} | State#game.stones]},
    {reply, {ok, move_saved}, NewState};

handle_call({make_move, _UserId, _X, _Y, _Hidden}, _From, State) ->
    {reply, {error, denied}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

get_stones_by_color(Color, Stones) ->
    lists:filter(
        fun(Stone) -> Stone#stone.color =:= Color end
    , Stones).

make_board_matrix() ->
    [ { X, [ {Y, 0} || Y <- [0,1,2,3,4,5,6,7,8,9,10]] } || X <- [0,1,2,3,4,5,6,7,8,9,10]].


%%%===================================================================
%%% Tests
%%%===================================================================

make_board_matrix_test() ->
    [{0, [{0, 0} | [{1, 0} | _] ]} | [{1, [{0, 0} | _]} | _] ] = make_board_matrix().