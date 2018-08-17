-module(hello_handler).

-export([init/2, content_types_provided/2]).
-export([terminate/3]).

% -record(state, {}).

% more inspiration from http://davekuhlman.org/simple-rest-app-cowboy.html

-export([
         data_to_html/2,
         data_to_json/2,
         data_to_text/2
        ]).

% init(Req, _Opts) ->	{ok, Req, #state{}}.
init(Req0, State) ->
    {cowboy_rest, Req0, State}.
%    Req = cowboy_req:reply(200, #{
%       <<"content-type">> => <<"text/plain">>
%    }, <<"Hello Erlang!">>, Req0),
%    {ok, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, data_to_html},
        {<<"application/json">>, data_to_json},
        {<<"text/plain">>, data_to_text}
    ], Req, State}.

terminate(_Reason, _Req, _State) ->
	ok.

%

make_body_time(Body) ->
    {Hour, Minute, Second} = erlang:time(),
    {Year, Month, Day} = erlang:date(),
    Body1 = io_lib:format(Body, [
        Hour, Minute, Second,
        Year, Month, Day
    ]),
    list_to_binary(Body1).

data_to_html(Req, State) ->
    Body = "<html>
<head>
    <meta charset=\"utf-8\">
    <title>REST Time</title>
</head>
<body>
    <h1>REST time server</h1>
    <ul>
        <li>Time -- ~2..0B:~2..0B:~2..0B</li>
        <li>Date -- ~4..0B/~2..0B/~2..0B</li>
</body>
</html>",
    {make_body_time(Body), Req, State}.

data_to_json(Req, State) ->
    Body = "
{
    \"time\": \"~2..0B:~2..0B:~2..0B\",
    \"date\": \"~4..0B/~2..0B/~2..0B\"
}
",
    {make_body_time(Body), Req, State}.

data_to_text(Req, State) ->
    Body = "
    time: ~2..0B:~2..0B:~2..0B,
    date: ~4..0B/~2..0B/~2..0B
",
    {make_body_time(Body), Req, State}.
