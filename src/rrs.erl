-module(rrs).
-export([main/1]).

%% @doc entry point for the server
main(_Args) ->
    observer:start(),
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    rr_config:init("rrs.config", []), %% ok =
    ok = rrs_database:start(),

    Port = rr_config:get_value('port', 8080),
    Gui = rr_config:get_value('ui.path', "ui"),
    rr_log:new(rr_config:get_value('log.target', std_err), rr_config:get_value('log.level', debug)),

    V = erlang:system_info(otp_release),
    rr_log:info("require erlang version >= R16. current version: ~s ~n", [V]),

    
    Dispatch = cowboy_router:compile(
		 [ 
		   {'_', [
			  {"/api/experiment", rrs_experiment, []},
			  {"/api/dataset/files", rrs_dataset_getall, []},
			  {"/api/evaluator/get-all", rrs_validation_getall, []},
			  {"/api/machine-learning/get-all", rrs_learning_getall, []},
			  {"/api/result/get/:id", rrs_result_get, []},
			  
			  {"/[...]", cowboy_static, 
			   [
			    {directory, Gui},
			    {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
			    ]}
						     
			 ]}
		 ]
		),


    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(rrs_listener, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
    io:format("please stop the server with ^c"),
    receive
	wait_for_c ->
	    ok
    end.
