-module(rr_server).
-export([main/1]).

%% @doc entry point for the server
main(_Args) ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    rr_log:new(std_err, debug),
    Dispatch = cowboy_router:compile(
		 [ 
		   {'_', [
			  {"/api/rf", rf_handler, []},
			  {"/api/file", file_handler, []},
			  {"/[...]", cowboy_static, 
			   [
			    {directory, "priv"},
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
