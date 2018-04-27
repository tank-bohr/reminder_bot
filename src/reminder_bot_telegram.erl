-module(reminder_bot_telegram).
-include_lib("reminder_bot_telegram.hrl").

-export([
    get_me/0,
    get_updates/0,
    send_message/2
]).

-define(REQUEST_TIMEOUT_MILLIS, 5000).

-spec get_me() -> map().
get_me() ->
    api_call("getMe").

-spec get_updates() -> map().
get_updates() ->
    api_call("getUpdates").

-spec send_message(binary(), binary()) -> map().
send_message(ChatId, Text) ->
    api_call("sendMessage", #{chat_id => ChatId, text => Text}).

api_call(Method) ->
    api_call(Method, #{}).

api_call(Method, Params) ->
    api_call(Method, Params, ?TELEGRAM_BASE_URL).

api_call(Method, Params, BaseUrl) ->
    Url = url(Method, BaseUrl),
    {ok, Body} = request(Url, Params),
    #{ok := true, result := Result} = reminder_bot_json:decode(Body),
    Result.

url(Method, BaseUrl) ->
    Token = reminder_bot:token(),
    binary_to_list(iolist_to_binary([BaseUrl, Token, "/", Method])).

request(Url, Params) ->
    RequestBody = reminder_bot_json:encode(Params),
    {ok, {StatusCode, Body}} = httpc:request(post,
        {Url, [], "application/json", RequestBody},   %% Request: {url, headers, content_type, body}
        [{timeout, ?REQUEST_TIMEOUT_MILLIS}],         %% HTTP Options
        [{body_format, binary}, {full_result, false}] %% Options
    ),
    case StatusCode of
        StatusCode when (StatusCode >= 200) andalso (StatusCode < 300) ->
            {ok, Body};
        StatusCode when (StatusCode >= 400) andalso (StatusCode < 500) ->
            {client_error, StatusCode};
        StatusCode when (StatusCode >= 500) andalso (StatusCode < 600) ->
            {server_error, StatusCode};
        StatusCode ->
            {unknown_error, StatusCode}
    end.
