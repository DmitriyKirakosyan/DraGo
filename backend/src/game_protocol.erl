-module(game_protocol).

-export([parse_event/1, parse_event_and_callback/2, send_event/2]).

read_string(Sock) ->
    case gen_tcp:recv(Sock, 2) of 
        {ok, <<Length:16>>} ->
            case Length of
                0 -> <<>>;
                SomeLength -> read_string(Sock, SomeLength)
            end;
        {error, _} -> {error, can_not_read}
    end.
read_string(Sock, Length) ->
    case gen_tcp:recv(Sock, Length) of
        {ok, String} -> String;
        {error, _} -> {error, can_not_read}
    end.

read_number(Sock, Length, Interval) ->
    BitLength = Length bsl 3,
    case gen_tcp:recv(Sock, Length, Interval) of
        {ok, <<Number:BitLength>>} -> Number;
				{error, timeout} -> {error, timeout};
        {error, _} -> {error, can_not_read}
    end.


parse_event(Sock) ->
		parse_event(Sock, 0).

parse_event(Sock, Interval) when is_integer(Interval)->
    case read_number(Sock, 4, Interval) of
        {error, Reason} -> {error, Reason};
        ActionId -> 
            case read_string(Sock) of
                {error, _} -> {error, can_not_parse};
                Request -> {ok, {ActionId, Request}}
            end
    end.

parse_event_and_callback(Sock, HandleFunction) ->
		case parse_event(Sock) of
				{ok, Result} ->
						HandleFunction(Result);
				_ -> {error, can_not_parse}
		end.


send_event(Sock, {Action_id, Response}) when is_integer(Action_id) andalso Action_id >= 0 andalso Action_id =< 4228250625 andalso is_binary(Response) ->
  Message =  <<Action_id:(4 bsl 3), (erlang:size(Response)):64, Response/binary>>,
  gen_tcp:send(Sock, Message).

