-record(user_state, {
    in_game,
    user_id,
    last_time
}).

-define (REQUEST_TIME, 10000).

-record (stone, {color, x = -1, y = -1, hidden = false, basic = false, number = 0, pass = false}).

-record (game_request, {white_user_id, black_user_id, white_ready=false, black_ready=false, time_left=?REQUEST_TIME, last_update}).

%% @doc
%% white_player, black_player: #player
%% stones : [#stone, #stone, ...]
%% phase : ?BASIC_PHASE | ?MAIN_PHASE | ?END_PHASE
%% move_player : binary()
%% clicks : [[{x, X}, {y, Y}], ...]
%% result_opinion : {UserId, Opinion}
-record (game, {white_player, black_player, stones, phase, move_player, clicks = [], result_opinion}).


-define (BASIC_ABILITY_PACK, [{hidden_stone, 1}, {try_find, 1}]).

%% @doc
%% user_id : binary()
%% ability_list : [{AbilityName, Num}, ...]
-record (player, {user_id, ability_list = ?BASIC_ABILITY_PACK}).


-define (BASIC_PHASE, basic_phase).
-define (MAIN_PHASE, main_phase).
-define (END_PHASE, end_phase).


%% depricated
% board_matrix:
% [{x, [{y, value}, {y, value}, ...]}, {x, ...}, ...]
% example : [{0, [{0, 0}, {1, 0}, {2, 0}]}, {1, []}, {2, []}]
% map values: 0 : empty, 1 : white stone, 2 : black stone
