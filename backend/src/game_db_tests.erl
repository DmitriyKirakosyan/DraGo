-module(game_db_tests).

-include_lib("eunit/include/eunit.hrl").

-export([test/0]).

test() ->
    eunit:test(%{setup, fun () -> game_db:start_link() end, fun (_) -> game_db:stop() end,
      [
       fun insert_test/0
      ]).

insert_test() ->
    ok = game_db:delete(test_col, <<"55743">>, {}),
    {ok, <<"55743">>} = game_db:insert(test_col, <<"55743">>, {test_key, 4}),
    {ok, [{'_id', <<"55743">>, test_key, 4}]} = game_db:find(test_col, <<"55743">>, {}),
    ok = game_db:modify(test_col, <<"55743">>, {'$set', {test_key, 5, new_key, 1, one_more_new_key, 2}}),
    trace_find(),
    {ok, [{'_id', <<"55743">>, new_key, 1, one_more_new_key, 2, test_key, 5}]} = game_db:find(test_col, <<"55743">>, {}),
    ok = game_db:save(test_col, <<"55743">>, {test_key, 9}),
    trace_find(),
    ok = game_db:delete(test_col, <<"55743">>, {}),
    {ok, []} = game_db:find(test_col, <<"55743">>, {}).

trace_find() ->    
    ?debugVal(game_db:find(test_col, <<"55743">>, {})).
