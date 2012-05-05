%% @author Beenza Games <dev@beenza.ru>
%% @copyright 2011 Beenza Games <dev@beenza.ru>

%% @doc Game session generic server

-module(session).

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

-include("batoo.hrl").

-record(state, {alive=true, user_state=#user_state{}}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(UserId) ->
    gen_server:start_link(?MODULE, UserId, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(UserId) ->
    UserState = #user_state{user_id= UserId, last_time=utils:milliseconds_now(), in_game=false},
    {ok, #state{user_state=UserState}, 4000}.

handle_call(_, _From, State=#state{alive=false}) ->
    {stop, [], {error, started_in_other_window}, State};

handle_call({die_after_request, _}, _From, State) ->
    {reply, {ok, i_will_die}, State#state{alive=false}};

handle_call(get_user_state, _From, State) ->
    {reply, {ok, State#state.user_state}, State};

handle_call({RequestName, RequestData}, _From, State) ->
    {ok, NewUserState, Reply} = request_handler:handle(RequestName, RequestData, State#state.user_state),
    {reply, Reply, State#state{user_state=NewUserState}, 4000};

handle_call(_Request, _From, State) ->
    {reply, {ok, empty_request}, State, 4000}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    case Info of
        timeout ->
            io:format("timeout [Sessoin.handle_info]"),
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

%session_to_bson(UserState) ->
%    {energy, UserState#user_state.energy,
%     cash, UserState#user_state.cash,
%     experience, UserState#user_state.experience
%    }.