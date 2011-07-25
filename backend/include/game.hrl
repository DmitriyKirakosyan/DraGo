-record(session, {
					user_id,
					sock,
					pid,
					state,
					start_time
				 }).

-record(state, {
					user_id,
					rank,
					money
				 }).

-record(game, {
					player_white,
					player_black
				 }).

-record(game_move, {
					x,
					y
				 }).
