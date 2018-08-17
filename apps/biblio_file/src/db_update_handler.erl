-module(db_update_handler).

%% api

-export([init/2, 
	 allowed_methods/2, 
	 content_types_provided/2,
	 content_types_accepted/2,
	 resource_exists/2,
	 delete_resource/2]).

-export([db_to_json/2, db_to_text/2, 
	 text_to_db/2]).

% -import(db_crudder).

% 
-record(state, {op}).

init(Req0, Opts) ->
    [Op | _] = Opts,
    State = #state{op=Op},
    {cowboy_rest, Req0, State}.

allowed_methods(Req0, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"DELETE">>],
    {Methods, Req0, State}.

content_types_provided(Req0, State) ->
    {[{<<"application/json">>, db_to_json},
      {<<"text/plain">>, db_to_text}],
     Req0, State}.

content_types_accepted(Req0, State) ->
    {[{<<"text/plain">>, text_to_db},
      {<<"application/json">>, text_to_db},
      {<<"application/x-www-form-urlencoded">>, text_to_db}],
     Req0, State}.

db_to_json(Req0, #state{op=Op} = State) ->
    {Body, Req0, State1} = 
    case Op of
	list -> db_crudder:get_record_list(Req0, State);
	get -> db_crudder:get_one_record(Req0, State);
	help -> db_crudder:get_help(Req0, State)
    end,
    {Body, Req0, State1}.
				   
db_to_text(Req0, #state{op=Op} = State) ->
    {Body, Req0, State1} = 
    case Op of
	list -> db_crudder:get_record_list_text(Req0, State);
	get -> db_crudder:get_one_record_text(Req0, State);
	help -> db_crudder:get_help_text(Req0, State)
    end,
    {Body, Req0, State1}.
				   
text_to_db(Req0, #state{op=Op} = State) ->
    {Body, Req0, State1} = 
    case Op of
	create -> db_crudder:create_record_to_json(Req0, State);
	delete -> db_crudder:delete_record_to_json(Req0, State);
	update -> db_crudder:update_record_to_json(Req0, State)
    end,
    {Body, Req0, State1}.   
				   
resource_exists(Req0, State) ->
    case cowboy_req:method(Req0) of
	<<"DELETE">> ->
	    RecordId = cowboy_req:binding(record_id, Req0),
	    RecordId1 = binary_to_list(RecordId),
	    {ok, Recordfilename} = application:get_env(
				     biblio_file, records_file_name),
	    {ok, _} = dets:open_file(
			records_db, [{file, Recordfilename}, {type, set}]),
	    Records = dets:lookup(records_db, RecordId1),
	    ok = dets:close(records_db),
	    % Response = 
	    case Records of
		[_] -> {true, Req0, State};
		_ -> {false, Req0, State}
	    end; 
            % Response				     
	_ -> {true, Req0, State}
    end.	 

delete_resource(Req, State) ->
    RecordId = cowboy_req:binding(record_id, Req),
    RecordId1 = binary_to_list(RecordId),
    {ok, Recordfilename} = application:get_env(biblio_file, records_file_name),
    {ok, _} = dets:open_file(records_db, 
			     [{file, Recordfilename}, 
			      {type, set}]),
    Result = dets:delete(records_db, RecordId1),
    ok = dets:close(records_db),
    Response = case Result of
		   ok -> true;
		   {error, _Reason} -> false
	       end,
    {Response, Req, State}.
