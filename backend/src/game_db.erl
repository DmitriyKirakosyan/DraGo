-module(game_db).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-include("mongo_protocol.hrl").

-define(db_name, drago).

start_link() ->
    Pid = spawn_link(fun connect/0),
    register(game_db, Pid),
    {ok, Pid}.

stop() ->
    game_db ! {stop, end_session}.

do(Conn, DoFunc) ->
    mongo:do(safe, master, Conn, ?db_name, DoFunc). 

connect() ->
    case mongo:connect("127.0.0.1") of
        {ok, Connect} ->
            loop(Connect),
            mongo:close(Connect);
        Error ->
            io:format("mongo connect error: ~p~n", [Error])
    end.

loop(Conn) ->
    receive
        {insert, From, Collection, UserId, BsonData} ->
            Ids = do(Conn, fun () ->
                                   mongo:insert(Collection, bson:append({'_id', UserId}, BsonData)) end),
            From ! Ids,
            loop(Conn);
        {modify, From, Collection, UserId, ModifyBson} ->
            do(Conn, fun () -> mongo:modify(Collection, {'_id', UserId}, ModifyBson) end ),
            io:format("ModifyBson : ~p, Selector : ~p, Collection : ~p~n", [ModifyBson, {'_id', UserId}, Collection]),
            From ! ok,
            loop(Conn);
        {update, From, Collection, UserId, UpdateBson} ->
            mongo_query:write({?db_name, Conn}, #update{collection=Collection, upsert=true, multiupdate=false,
                                            selector={'_id', UserId}, updater={'$set', UpdateBson}}),
            From ! ok,
            loop(Conn);
        {save, From, Collection, UserId, DataBson} ->
            do(Conn, fun() -> mongo:save(Collection, bson:append({'_id', UserId}, DataBson)) end),
            From ! ok,
            loop(Conn);
        {find, From, Collection, UserId, Options} ->
            Elements = do(Conn, fun() -> mongo:rest(mongo:find(Collection, bson:append({'_id', UserId}, Options))) end),
            From ! Elements,
            loop(Conn);
        {delete, From, Collection, UserId, Options} ->
            do(Conn, fun () -> mongo:delete(Collection, bson:append({'_id', UserId}, Options)) end),
            From ! ok,
            loop(Conn);
        {get_connection, From} ->
            From ! Conn,
            loop(Conn);
        {stop, end_session} ->
            ok;
        _ ->
            false,
            loop(Conn)
    end.

-spec wrap_with_user_id(tuple(), binary()) -> tuple().
wrap_with_user_id(BsonData, UserId) ->
    bson:append({'_id', UserId}, BsonData).

-spec insert(atom(), binary(), tuple()) -> atom().
insert(Collection, UserId, BsonData) ->
    game_db ! {insert, self(), Collection, UserId, BsonData},
    receive
        Ids ->
            Ids
    after 1000 ->
            {error, false}
    end.

-spec modify(atom(), binary(), tuple()) -> ok.
modify(Collection, UserId, ModifyBson) ->
    game_db ! {modify, self(), Collection, UserId, ModifyBson},
    receive
        ok ->
            ok
    after 100 ->
            io:format("100 so small................."),
            non_ok
    end.

-spec update(atom(), binary(), tuple()) ->ok.
update(Collection, UserId, UpdateBson) ->
    game_db ! {update, self(), Collection, UserId, UpdateBson},
    receive
        ok ->
            ok
    after 500 ->
            ok
    end.

-spec save(atom(), binary(), tuple()) -> ok.
save(Collection, UserId, DataBson) ->
    game_db ! {save, self(), Collection, UserId, DataBson},
    receive
        ok ->
            ok
    after 100 ->
            ok
    end.

-spec find(atom(), binary(), tuple()) -> tuple().
find(Collection, UserId, Options) ->
    game_db ! {find, self(), Collection, UserId, Options},
    receive
        Elements ->
            Elements
    after 1000 ->
            {error, not_founded}
    end.

-spec delete(atom(), binary(), tuple()) -> ok.
delete(Collection, UserId, Options) ->
    game_db ! {delete, self(), Collection, UserId, Options},
    receive
        _ ->
            ok
    after 100 ->
            ok
    end.

-spec get_connection() -> mongo:connection().
%@doc for debug only
get_connection() ->
    game_db ! {get_connection, self()},
    receive
        Connection ->
            Connection
    after 1000 ->
            {error, not_connection}
    end.
