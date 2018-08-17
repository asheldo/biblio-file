-module(help_handler).

-export([init/2]).
-export([terminate/3]).

% -record(state, {}).

% init(Req, _Opts) ->	{ok, Req, #state{}}.
init(Req0, State) ->
    Style = cowboy_req:binding(style, Req0),
    Req = case Style of
	      <<"verbose">> ->
		  cowboy_req:reply(200, 
				   #{<<"content-type">> => <<"text/plain">>},
				   <<"Howdy-do Erlang!">>, Req0);
	      _Else ->
		  cowboy_req:reply(200, 
				   #{<<"content-type">> => <<"text/plain">>},
				   <<"Hey Erl">>, Req0)				    
	  end,
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
	ok.
