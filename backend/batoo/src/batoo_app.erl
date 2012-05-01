%% @author Mochi Media <dev@mochimedia.com>
%% @copyright batoo Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the batoo application.

-module(batoo_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for batoo.
start(_Type, _StartArgs) ->
    batoo_deps:ensure(),
    batoo_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for batoo.
stop(_State) ->
    ok.
