-module(gamedb).
-export ([start/0]).
-export ([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export ([insert/2, modify/3, find_all/2, find_one/2, remove_collection/1]).

-behaviour(gen_server).

-record (state, {conn}).
-define (DB_NAME, kingdom).

start() ->
  gen_server:start_link({local, gamedb}, gamedb, [], []).

init([]) ->
  {ok, Conn} = mongo:connect({localhost, 27017}),
  {ok, #state{conn=Conn}}.


%% API

insert(Collection, Data) ->
    gen_server:call(gamedb, {insert, Collection, Data}).

modify(Collection, Selector, Data) ->
    gen_server:call(gamedb, {modify, Collection, Selector, Data}).

find_all(Collection, Selector) ->
    gen_server:call(gamedb, {find_all, Collection, Selector}).
find_one(Collection, Selector) ->
    gen_server:call(gamedb, {find_one, Collection, Selector}).

remove_collection(Collection) ->
    gen_server:call(gamedb, {remove_collection, Collection}).

%% Internal functions

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({insert, Collection, Data}, _From, State) ->
    mongo:do(safe, master, State#state.conn, ?DB_NAME, fun() ->
        mongo:insert(Collection, Data)
    end),
    {reply, ok, State};

handle_call({modify, Collection, Selector, Data}, _From, State) ->
    mongo:do(safe, master, State#state.conn, ?DB_NAME, fun() ->
        mongo:modify(Collection, Selector, {'$set', Data})
    end),
    {reply, ok, State};

handle_call({find_all, Collection, Selector}, _From, State) ->
    Result = mongo:do(safe, master, State#state.conn, ?DB_NAME, fun() ->
                mongo:rest(mongo:find(Collection, Selector))
    end),
    {reply, Result, State};

handle_call({find_one, Collection, Selector}, _From, State) ->
    {ok, Result} = mongo:do(safe, master, State#state.conn, ?DB_NAME, fun() ->
                mongo:next(mongo:find(Collection, Selector))
    end),
    FinalyResult = case Result of {Data} -> [Data]; {} -> [] end,
    {reply, {ok, FinalyResult}, State};

handle_call({remove_collection, Collection}, _From, State) ->
    mongo:do(safe, master, State#state.conn, ?DB_NAME, fun() ->
        mongo:delete(Collection, {})
    end),
    {reply, {ok, removed}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    mongo:disconnect(State#state.conn).

