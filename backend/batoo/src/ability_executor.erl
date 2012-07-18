-module (ability_executor).

-export ([execute/3]).

execute(State, UserId, hidden_stone) ->
    {ok, State};

execute(State, UserId, try_find) ->
    {ok, State}.