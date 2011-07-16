-module(game_rpc).
-export([handle_json_event/4, parse_json_request/1]).


parse_json_request(JsonRequest) ->
    case catch mochijson2:decode(JsonRequest) of
        {struct, DeserializedRequest} -> {ok, DeserializedRequest};
        _ -> {error, parse_error}
    end.

handle_json_event(UserId, State, JsonRequest, Module) when is_binary(UserId) andalso is_binary(JsonRequest) ->
    case parse_json_request(JsonRequest) of
        {ok, DeserializedRequest} -> handle_event(UserId, State, DeserializedRequest, Module);
        _ -> [{error, parse_error}]
    end.

handle_event(UserId, State, Request, Module) ->
    case proplists:get_value(<<"request">>, Request) of
        RequestName when is_binary(RequestName) ->
            RequestParams = proplists:delete(<<"request">>, Request),
            case catch Module:handle_event(RequestName, UserId, State, RequestParams) of
                Result when is_list(Result) -> Result;
                UnexpectedResult -> 
                    error_logger:error_report({rpc_error, UserId, UnexpectedResult}),
                    [{error, internal_error}, {error_details, list_to_binary(io_lib:format("~p", [UnexpectedResult]))}]
            end;
        _ -> [{error, wrong_request_field}]
    end.

