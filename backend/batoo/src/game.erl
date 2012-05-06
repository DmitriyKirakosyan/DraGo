-module (game).

-behaviour (gen_server).

-export ([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include ("batoo.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(WhiteUserId, BlackUserId) ->
    gen_server:start_link(?MODULE, [WhiteUserId, BlackUserId], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([WhiteUserId, BlackUserId]) ->
    Game = #game{white_user_id=WhiteUserId, black_user_id=BlackUserId, board_matrix=make_board_matrix(), period=?BASIC_PERIOD},
    {ok, Game}.

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_board_matrix() ->
    [ { X, [ {Y, 0} || Y <- [0,1,2,3,4,5,6,7,8,9,10]] } || X <- [0,1,2,3,4,5,6,7,8,9,10]].


%%%===================================================================
%%% Tests
%%%===================================================================

make_board_matrix_test() ->
    [{0, [{0, 0} | [{1, 0} | _] ]} | [{1, [{0, 0} | _]} | _] ] = make_board_matrix().