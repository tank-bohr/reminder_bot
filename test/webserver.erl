-module(webserver).
-include_lib("eunit/include/eunit.hrl").

-export([
    start/0,
    wait/1,
    stop/1,
    accept_connection/2
]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(5432, [
        binary,
        {packet, http},
        {active, false}
    ]),
    wait(ListenSocket),
    ListenSocket.

wait(ListenSocket) ->
    spawn_link(?MODULE, accept_connection, [self(), ListenSocket]).

stop(ListenSocket) ->
    gen_tcp:close(ListenSocket).

accept_connection(Parent, ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    Request = receive_request(Socket),
    ok = reply(Socket),
    ok = gen_tcp:close(Socket),
    Parent ! {?MODULE, Request}.

receive_request(Socket) ->
    receive_request(Socket, #{}).

receive_request(Socket, Headers) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_request, _HttpMethod, _HttpUri, _HttpVersion}} ->
            receive_request(Socket, Headers);
        {ok, {http_header, _, Header, _, Value}} ->
            ?debugVal(Header),
            ?debugVal(Value),
            receive_request(Socket, maps:put(Header, Value, Headers));
        {ok, http_eoh} ->
            Body = read_body(Socket, Headers),
            {Headers, Body}
        end.

read_body(Socket, #{'Content-Length' := ContentLength}) ->
    Length = list_to_integer(ContentLength),
    inet:setopts(Socket, [{packet, raw}]),
    {ok, Body} = gen_tcp:recv(Socket, Length),
    inet:setopts(Socket, [{packet, http}]),
    Body;
read_body(_Socket, _Headers) ->
    <<>>.

reply(Socket) ->
    gen_tcp:send(Socket, [
        <<"HTTP/1.1 200 OK\r\n">>,
        <<"Server: nginx/1.12.2\r\n">>,
        <<"Date: Sat, 28 Apr 2018 05:51:50 GMT\r\n">>,
        <<"Content-Type: application/json\r\n">>,
        <<"Content-Length: 23\r\n">>,
        <<"Connection: keep-alive\r\n">>,
        <<"Access-Control-Allow-Origin: *\r\n">>,
        <<"\r\n">>,
        <<"{\"ok\":true,\"result\":{}}\r\n">>
    ]).
