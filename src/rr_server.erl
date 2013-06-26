-module(rr_server).
-export([main/1]).

main(_StartType) ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    rr_log:new(std_err, debug),
    Dispatch = cowboy_router:compile(
		 [ 
		   {'_', [{"/rf", rf_handler, []}]}
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
