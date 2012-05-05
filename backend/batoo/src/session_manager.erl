%% @doc Game session manager

-module(session_manager).

-behaviour(gen_server).

-export([start_link/0]).

-export ([has_session_for/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% public functions
-export([run_session/0, handle_request/2, get_users/0, get_sessions/0, clean_sessions/0]).

-record(state, {sessions=[]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, session_manager}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(run_session, _From, State) ->
                                                % remove dead sessions
    Sessions = [{Key, Pid} || {Key, Pid} <- State#state.sessions, erlang:is_process_alive(Pid)],
    SessionKey = generate_session_key(<<"0">>),
    {ok, Pid} = session:start_link(SessionKey),
    Reply = {ok, {Pid, SessionKey}},
    NewState = State#state{sessions=[{SessionKey, Pid} | Sessions]},
    {reply, Reply, NewState};

handle_call({get_session_pid, SessionKey}, _From, State) ->
    Sessions = [{Key, Pid} || {Key, Pid} <- State#state.sessions, erlang:is_process_alive(Pid)],
    case proplists:get_value(SessionKey, Sessions, none) of
        none ->
            {reply, {error, no_session}, State};
        Pid ->
            {reply, {ok, Pid}, State}
    end;

handle_call(get_sessions, _From, State) ->
    {reply, {ok, State#state.sessions}, State};

handle_call(clean_sessions, _From, State) ->
    Sessions = [{Key, Pid} || {Key, Pid} <- State#state.sessions, erlang:is_process_alive(Pid)],
    {reply, {ok, cleaned}, State#state{sessions=Sessions}};

handle_call(_Request, _From, State) ->
    {reeply, {ok, empty_request}, State}.


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

run_session() ->
    case gen_server:call(session_manager, run_session) of
        {ok, {_Pid, SessionKey}} ->
            {ok, SessionKey};
        _Error ->
            {error, cant_run_session}
    end.

has_session_for(UserId) ->
    case get_session_pid(UserId) of
        {error, no_session} ->
            false;
        {ok, _Pid} ->
            true
    end.

get_users() ->
    gen_server:call(session_manager, clean_sessions),
    {ok, Sessions} = gen_server:call(session_manager, get_sessions),
    [SessionKey || {SessionKey, _Pid} <- Sessions].

handle_request(SessionKey, Request) ->
    case get_session_pid(SessionKey) of
        {error, no_session} ->
            {error, no_session};
        {ok, Pid} ->
            RequestName = proplists:get_value(<<"request">>, Request),
            gen_server:call(Pid, {RequestName, proplists:delete(<<"request">>, Request)})
    end.


%% debug

get_sessions() ->
    gen_server:call(session_manager, get_sessions).

clean_sessions() ->
    gen_server:call(session_manager, clean_sessions).

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_session_pid(SessionKey) ->
    gen_server:call(session_manager, {get_session_pid, SessionKey}).

generate_session_key(UserId) ->
    MillisecondsNow = utils:milliseconds_now(),
    Random = trunc(random:uniform() * 1000),
    io:format("random number is : ~p~n", [Random]),
    SessionKeyMD5 = erlang:md5(<<UserId/binary, MillisecondsNow/integer, Random/integer>>),
    list_to_binary(utils:hexstring(SessionKeyMD5)).
