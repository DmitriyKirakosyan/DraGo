-record(user_state, {
    active,
    user_id,
    last_time
}).

-define (REQUEST_TIME, 10000).

-record (stone, {color, x, y, hidden, basic}).

-record (game_request, {white_user_id, black_user_id, white_ready=false, black_ready=false, time_left=?REQUEST_TIME, last_update}).

-record (game, {white_user_id, black_user_id, board_matrix, stones, period, move}).
% board_matrix:
% [{x, [{y, value}, {y, value}, ...]}, {x, ...}, ...]
% example : [{0, [{0, 0}, {1, 0}, {2, 0}]}, {1, []}, {2, []}]
% values map: 0 : empty, 1 : white stone, 2 : black stone
%
% stones : [#stone, #stone, ...]
%
% period : ?BASIC_PERIOD | ?MAIN_PERIOD | ?END_PERIOD
%
% move : white | black

-define (BASIC_PERIOD, basic_period).
-define (MAIN_PERIOD, main_period).
-define (END_PERIOD, end_period).