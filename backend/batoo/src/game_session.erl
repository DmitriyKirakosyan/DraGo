%% @author Beenza Games <dev@beenza.ru>
%% @copyright 2011 Beenza Games <dev@beenza.ru>

%% @doc Game session generic server

-module(game_session).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-define(collection, state).

-include("mysteryville.hrl").

-record(state, {alive=true, user_id, user_state=#user_state{}}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(UserId) ->
    gen_server:start_link(?MODULE, UserId, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(UserId) ->
    UserState = case game_db:find(?collection, UserId, {}) of
        {ok, [UserStateBson]} ->
            bsonState_to_state(UserStateBson);
        _Error ->
            #user_state{last_time=game_util:milliseconds_now()}
    end,
    {ok, #state{user_id=UserId, user_state=UserState}, 4000}.

handle_call(_, _From, State=#state{alive=false}) ->
    {stop, [], {error, started_in_other_window}, State};

handle_call({die_after_request, _}, _From, State) ->
    {reply, {ok, i_will_die}, State#state{alive=false}};

handle_call(Request, _From, State) ->
    RequestName = proplists:get_value(<<"request_name">>, Request),
    RequestData = proplists:delete(<<"request_name">>, Request),
    {ok, UserState} = requets_handler:handle(RequestName, RequestData,
                                                State#state.user_state),
    NewUserState = state_accounter:recompute(UserState),
    ParsedUserState = state_to_proplist(NewUserState),
    {reply, {ok, ParsedUserState}, State#state{user_state=NewUserState}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    case Info of
        timeout ->
            {stop, [], State};
        _ ->
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

state_to_proplist(UserState) ->
    [{level, UserState#user_state.level},
     {world_name, UserState#user_state.mastery}
    ].

%session_to_bson(UserState) ->
%    {energy, UserState#user_state.energy,
%     cash, UserState#user_state.cash,
%     experience, UserState#user_state.experience
%    }.

bsonState_to_state(Bson) ->
    Level = get_bson_value(level, Bson),
    Mastery = get_bson_value(mastery, Bson),
    #user_state{level = Level, mastery = Mastery}.

get_bson_value(Variable, Bson) ->
    case bson:lockup(Variable, Bson) of
        {} ->
            undefined;
        {Value} ->
            Value
    end.
