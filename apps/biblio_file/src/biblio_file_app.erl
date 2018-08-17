%%%-------------------------------------------------------------------
%% @doc biblio_file public API
%% @end
%%%-------------------------------------------------------------------
-module(biblio_file_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    %% {HostMatch, 
    %%  list({PathMatch, Handler, InitialState})}
    Dispatch = cowboy_router:compile(
		 [
		  {'_', [{"/help/:style", help_handler, #{}},
               {"/list", db_update_handler, [list]},
               {"/get/:record_id", db_update_handler, [get]},
               {"/create", db_update_handler, [create]},
               {"/update/:record_id", db_update_handler, [update]},
               {"/delete/:record_id", db_update_handler, [delete]},
               {"/help", db_update_handler, [help]},
			 {"/", hello_handler, #{}}] }]),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    {ok, _} = cowboy:start_clear(
		http, 
		[{port, 8080}],
		#{ env => #{dispatch => Dispatch} }),
    biblio_file_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
