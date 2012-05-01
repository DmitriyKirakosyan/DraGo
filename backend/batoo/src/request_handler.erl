-module(request_handler).

-export([handle/3]).

-include("mysteryville.hrl").

handle(<<"add_experience">>, RequestData, UserState) ->
    Experience = proplists:get_value(<<"experience">>, RequestData),
    {ok, _NewUserState} = add_experience(Experience, UserState);

handle(<<"add_mastery">>, RequestData, UserState) ->
    Mastery = proplists:get_value(<<"mastery">>, RequestData),
    {ok, _NewUserState} = add_mastery(Mastery, UserState);

handle(_, _, UserState) -> {ok, UserState}.

add_experience(Experience, UserState) ->
    case Experience < 0 of
        true ->
            {ok, UserState};
        false ->
            NewExperience = UserState#user_state.experience + Experience,
            {ok, UserState#user_state{experience = NewExperience}}
    end.

add_mastery(Mastery, UserState) ->
    {ok, UserState#user_state{mastery=UserState#user_state.mastery+Mastery}}.