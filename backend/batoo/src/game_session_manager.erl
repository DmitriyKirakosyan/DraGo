%% @author Beenza Games <dev@beenza.ru>
%% @copyright 2011 Beenza Games <dev@beenza.ru>

%% @doc Game session manager

-module(game_session_manager).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% public functions
-export([run_session/1, handle_request/2]).

-define(SERVER, ?MODULE).

-record(session, {user_id, pid}).
-record(state, {sessions=[]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, game_session_manager}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({run_session, UserId}, _From, State) ->
                                                % remove dead sessions
    Sessions = [{Key, Session} || {Key, Session} <- State#state.sessions, erlang:is_process_alive(Session#session.pid)],
    ExistingSessions = [Session || {_Key, Session} <- Sessions, Session#session.user_id == UserId],
    case length(ExistingSessions) == 0 of
        true ->
            ok;
        false ->
            lists:foreach(fun(Session) -> gen_server:call(Session#session.pid, {die_after_request, []}) end, Sessions)
    end,
    {ok, Pid} = game_session:start_link(UserId),
    SessionKey = generate_session_key(UserId),
    Reply = {ok, {Pid, SessionKey}},
    NewState = State#state{sessions=[{SessionKey, 
                                      #session{user_id=UserId, pid=Pid}} | Sessions]},
    {reply, Reply, NewState};

handle_call({get_session_pid, SessionKey}, _From, State) ->
    Sessions = [{Key, Session} || {Key, Session} <- State#state.sessions, erlang:is_process_alive(Session#session.pid)],
    case proplists:get_value(SessionKey, Sessions, none) of
        none ->
            {reply, {error, no_session}, State};
        Session when is_record(Session, session) ->
            {reply, {ok, Session#session.pid}, State}
    end.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Public functions
%%%===================================================================

run_session(UserId) ->
    gen_server:call(game_session_manager, {run_session, UserId}).

handle_request(SessionKey, Request) ->
    case get_session_pid(SessionKey) of
        {error, no_session} ->
            {error, no_session};
        {ok, Pid} ->
            RequestName = binary_to_atom(proplists:get_value(<<"request_name">>, Request), utf8),
            gen_server:call(Pid, {RequestName, proplists:delete(<<"request_name">>, Request)})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_session_pid(SessionKey) ->
    gen_server:call(game_session_manager, {get_session_pid, SessionKey}).

generate_session_key(UserId) ->
    MillisecondsNow = game_util:milliseconds_now(),
    Random = trunc(random:uniform() * 1000),
    io:format("random number is : ~p~n", [Random]),
    SessionKeyMD5 = erlang:md5(<<UserId/binary, MillisecondsNow/integer, Random/integer>>),
    list_to_binary(game_util:hexstring(SessionKeyMD5)).
