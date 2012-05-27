-record(user_state, {
    in_game,
    user_id,
    last_time
}).

-define (REQUEST_TIME, 10000).

-record (stone, {color, x, y, hidden = false, basic = false, number = 0}).

-record (game_request, {white_user_id, black_user_id, white_ready=false, black_ready=false, time_left=?REQUEST_TIME, last_update}).

-record (game, {white_user_id, black_user_id, board_matrix, stones, phase, move_player, clicks = [], result_opinion}).
% board_matrix:
% [{x, [{y, value}, {y, value}, ...]}, {x, ...}, ...]
% example : [{0, [{0, 0}, {1, 0}, {2, 0}]}, {1, []}, {2, []}]
% values map: 0 : empty, 1 : white stone, 2 : black stone
%
% stones : [#stone, #stone, ...]
%
% phase : ?BASIC_PHASE | ?MAIN_PHASE | ?END_PHASE
%
% move : white | black

-define (BASIC_PHASE, basic_phase).
-define (MAIN_PHASE, main_phase).
-define (END_PHASE, end_phase).