-module (game_room).

-behaviour (gen_server).

-export ([start_link/0]).

-export ([get_game_state/1, get_game_list/0, remove_all_games/0, make_move/4, pass/1, click_capture_stone/3, unclick_capture_stone/2, set_result_opinion/2]).
-export ([create_request/2, get_request_list/0, get_request_list_for/1, approve_request/2, decline_request/2]).

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

%% game

get_game_state(UserId) ->
    gen_server:call(game_room, {get_game_state, UserId}).

make_move(UserId, X, Y, Hidden) ->
    gen_server:call(game_room, {make_move, UserId, X, Y, Hidden}).
pass(UserId) ->
    gen_server:call(game_room, {pass, UserId}).

click_capture_stone(UserId, X, Y) ->
    gen_server:call(game_room, {click_capture_stone, UserId, X, Y}).
unclick_capture_stone(UserId, Points) ->
    gen_server:call(game_room, {unclick_capture_stone, UserId, Points}).

set_result_opinion(UserId, Opinion) ->
    gen_server:call(game_room, {set_result_opinion, UserId, Opinion}).

get_game_list() ->
    gen_server:call(game_room, get_game_list).

remove_all_games() ->
    gen_server:call(game_room, remove_all_games).

%% game requests

create_request(OwnerUserId, FriendUserId) ->
    gen_server:call(game_room, {create_request, OwnerUserId, FriendUserId}).

approve_request(UserId, OwnerUserId) ->
    gen_server:call(game_room, {approve_request, UserId, OwnerUserId}).

decline_request(OwnerUserId, FriendUserId) ->
    gen_server:call(game_room, {decline_request, OwnerUserId, FriendUserId}).

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

%% game

handle_call({get_game_state, UserId}, _From, State) ->
    case find_game(UserId, State#state.games) of
        {{_WhiteUserId, _BlackUserId}, Pid} ->
            {ok, GameState} = gen_server:call(Pid, {get_game_state, UserId}),
            {reply, {ok, GameState}, State};
        _None ->
            {reply, {error, game_not_found}, State}
    end;

handle_call({make_move, UserId, X, Y, Hidden}, _From, State) ->
    case find_game(UserId, State#state.games) of
        {{_, _}, Pid} ->
            Reply = gen_server:call(Pid, {make_move, UserId, X, Y, Hidden}),
            {reply, Reply, State};
        _None ->
            {reply, {error, game_not_found}, State}
    end;

handle_call({pass, UserId}, _From, State) ->
    case find_game(UserId, State#state.games) of
        {{_, _}, Pid} ->
            Reply = gen_server:call(Pid, {pass, UserId}),
            {reply, Reply, State};
        _None ->
            {reply, {error, game_not_found}, State}
    end;

handle_call({click_capture_stone, UserId, X, Y}, _From, State) ->
    case find_game(UserId, State#state.games) of
        {{_, _}, Pid} ->
            Reply = gen_server:call(Pid, {click_capture_stone, UserId, X, Y}),
            {reply, Reply, State};
        _None ->
            {reply, {error, game_not_found}, State}
    end;

handle_call({unclick_capture_stone, UserId, Points}, _From, State) ->
    case find_game(UserId, State#state.games) of
        {{_, _}, Pid} ->
            Reply = gen_server:call(Pid, {unclick_capture_stone, UserId, Points}),
            {reply, Reply, State};
        _None ->
            {reply, {error, game_not_found}, State}
    end;

handle_call({set_result_opinion, UserId, Opinion}, _From, State) ->
    case find_game(UserId, State#state.games) of
        FoundedGame = {{_, _}, Pid} ->
            Reply = gen_server:call(Pid, {set_result_opinion, UserId, Opinion}),
            NewState = case Reply of
                {ok, game_finished} ->
                    gen_server:cast(Pid, stop),
                    NewGames = [Game || Game <- State#state.games, Game =/= FoundedGame],
                    State#state{games = NewGames};
                _Else -> State
            end,
            {reply, Reply, NewState};
        _None ->
            {reply, {error, game_not_found}, State}
    end;


handle_call(get_game_list, _From, State) ->
    {reply, {ok, State#state.games}, State};
handle_call(remove_all_games, _From, State) ->
    {reply, {ok, removed}, State#state{games = []}};


%% game requests

handle_call({create_request, OwnerUserId, FriendUserId}, _From, State) ->
    Request = #game_request{white_user_id=OwnerUserId, black_user_id=FriendUserId, white_ready=true, last_update=utils:milliseconds_now()},
    Requests = [Request | State#state.requests],
    {reply, {ok, created}, State#state{requests=Requests}};

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

handle_call({decline_request, OwnerUserId, FriendUserId}, _From, State) ->
    case find_request(OwnerUserId, FriendUserId, State#state.requests) of
        Request = #game_request{} ->
            io:format("request founded : ~p~n", [Request]),
            Requests = [RequestItem || RequestItem <- State#state.requests, RequestItem =/= Request],
            {reply, {ok, declined}, State#state{requests=Requests}};
        none ->
            io:format("request not found ~n"),
            {reply, {error, cant_find_request}, State}
    end;


handle_call(get_request_list, _From, State) ->
    Requests = update_requests(State#state.requests),
    RequestList = requests_to_proplists(Requests),
    {reply, {ok, RequestList}, State#state{requests=Requests}};

handle_call({get_request_list, UserId}, _From, State) ->
    Requests = update_requests(State#state.requests),
    RequestsForUser = [Request || Request <- Requests,
                                    Request#game_request.white_user_id == UserId orelse Request#game_request.black_user_id == UserId],
    RequestList = requests_to_proplists(RequestsForUser),
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

find_game(_, []) -> none;
find_game(UserId, [Game = {{WhiteUserId, BlackUserId}, _Pid} | _Games]) when WhiteUserId =:= UserId orelse
                                        BlackUserId =:= UserId ->
    Game;
find_game(UserId, [_Game | Games]) ->
    find_game(UserId, Games).

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
    NewRequests = lists:map(
        fun(Request) ->
            TimeDiff = TimeNow - Request#game_request.last_update,
            Request#game_request{time_left=Request#game_request.time_left-TimeDiff}
        end, Requests
    ),
    [Request#game_request{last_update=TimeNow} || Request <- NewRequests, Request#game_request.time_left > 0].

requests_to_proplists(Requests) ->
    [ [{white_user_id, Request#game_request.white_user_id},
        {black_user_id, Request#game_request.black_user_id},
        {time_left, Request#game_request.time_left}]
    || Request <- Requests ].
