-module(rr_server).
-export([main/1]).

%% @doc entry point for the server
main(_Args) ->
    observer:start(),
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = rr_db:start(),
    Props = rr:read_config("rr_server.config"),
    rr_config:init(Props),
    rr_log:new(std_err, debug),
    Dispatch = cowboy_router:compile(
		 [ 
		   {'_', [
			  {"/api/rf", rf_handler, []},
			  {"/api/dataset/files", files_get_all_handler, []},
			  {"/api/evaluator/get-all", eval_get_all_handler, []},
			  {"/api/machine-learning/get-all", ml_get_all_handler, []},
			  {"/api/machine-learning/get/:id", ml_get_handler, []},
			  {"/api/result/get/:id", result_get_handler, []},
			  
			  {"/[...]", cowboy_static, 
			   [
			    {directory, "ui"},
			    {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
			    ]}
						     
			 ]}
		 ]
		),


    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(my_http_listener, 100,
		      [{port, 8080}],
		      [{env, [{dispatch, Dispatch}]}]),
    rr_log:info("please stop the server with ^c"),
    receive
	crap ->
	    ok
    end.
