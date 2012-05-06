-module (game_room).

-behaviour (gen_server).

-export ([start_link/0]).

-export ([create_request/2, get_request_list/0, get_request_list_for/1, approve_request/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include ("batoo.hrl").

-record (state, {games, requests}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, game_room}, ?MODULE, [], []).

create_request(OwnerUserId, FriendUserId) ->
    gen_server:call(game_room, {create_game_request, OwnerUserId, FriendUserId}).

approve_request(UserId, OwnerUserId) ->
    gen_server:call(game_room, {approve_request, UserId, OwnerUserId}).

get_request_list() ->
    gen_server:call(game_room, get_request_list).

get_request_list_for(UserId) ->
    gen_server:call(game_room, {get_request_list, UserId}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{games=[], requests=[]}}.

handle_call({create_game_request, OwnerUserId, FriendUserId}, _From, State) ->
    case game_session_manager:has_session_for(FriendUserId) of
        true ->
            Request = #game_request{white_user_id=OwnerUserId, black_user_id=FriendUserId, white_ready=true, last_update=utils:milliseconds_now()},
            Requests = [Request | State#state.requests],
            {reply, {ok, created}, State#state{requests=Requests}};
        _False ->
            {reply, {error, friend_is_offline}, State}
    end;

handle_call({approve_request, UserId, OwnerUserId}, _From, State) ->
    case find_request(OwnerUserId, UserId, State#state.requests) of
        #game_request{white_ready=true} ->
            RequestsWithoutOwner = requests_without_user(OwnerUserId, State#state.requests),
            Requests = requests_without_user(UserId, RequestsWithoutOwner),
            {ok, Pid} = game:start_link(OwnerUserId, UserId),
            Game = {{OwnerUserId, UserId}, Pid},
            {reply, {ok, game_started}, State#state{requests=Requests, games= [Game | State#state.games]}};
        none ->
            {reply, {error, cant_find_request}, State}
    end;

handle_call(get_request_list, _From, State) ->
    Requests = update_requests(State#state.requests),
    RequestList = requests_to_proplists(Requests),
    {reply, {ok, RequestList}, State#state{requests=Requests}};

handle_call({get_request_list, UserId}, _From, State) ->
    RequestsForUser = [Request || Request <- State#state.requests,
                                    Request#game_request.white_user_id == UserId orelse Request#game_request.black_user_id == UserId],
    Requests = update_requests(RequestsForUser),
    RequestList = requests_to_proplists(Requests),
    {reply, {ok, RequestList}, State#state{requests=Requests}};



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

%% Internal functinos

find_request(_, _, []) -> none;
find_request(OwnerUserId, FriendUserId, [Request = #game_request{white_user_id=OwnerUserId, black_user_id=FriendUserId} | _Requests]) ->
    Request;
find_request(OwnerUserId, FriendUserId, [_Request | Requests]) ->
    find_request(OwnerUserId, FriendUserId, Requests).

requests_without_user(UserId, Requests) ->
    [Request || Request <- Requests, Request#game_request.white_user_id =/= UserId andalso
                Request#game_request.black_user_id =/= UserId].


update_requests(Requests) ->
    TimeNow = utils:milliseconds_now(),
    %% use lists:map or smthg else for apply TimeDiff
    NewRequests = [Request#game_request{time_left=Request#game_request.time_left-TimeDiff}
                    || Request <- Requests, TimeDiff = TimeNow - Request#game_request.last_update],
    [Request#game_request{last_update=TimeNow} || Request <- NewRequests, Request#game_request.time_left > 0].

requests_to_proplists(Requests) ->
    [ [{white_user_id, Request#game_request.white_user_id},
        {black_user_id, Request#game_request.black_user_id},
        {time_left, Request#game_request.time_left}]
    || Request <- Requests ].