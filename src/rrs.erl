-module(rrs).
-export([main/1]).

-define(DATE, "2013-08-07").
-define(MAJOR_VERSION, "1").
-define(MINOR_VERSION, "0").
-define(REVISION, "0.0").

-define(AUTHOR, "Isak Karlsson <isak-kar@dsv.su.se>").
-define(CMD_SPEC, 
	[{<<"help">>, $h, "help", undefined,
	  "Show usage information"},
	 {<<"version">>, $v, "version", undefined,
	  "Show the program version."},
	 {<<"observer">>, undefined, "observer", undefined,
	  "Observe the program execution (requires: observer)"},
	 {<<"config">>, undefined, "config", {string, <<"rrs.config">>},
	  "Use config file <file> (default: rrs.config)"},
	 {<<"generate-dataconfig">>, undefined, "generate-dataconfig", {string, []},
	  "Generate config for files in <folder>"}
	]).

show_information() -> 
    io_lib:format("rrs (Machine Learning Server) ~s.~s.~s (build date: ~s)
Copyright (C) 2013+ ~s

Written by ~s ~n", [?MAJOR_VERSION, ?MINOR_VERSION, ?REVISION, ?DATE, ?AUTHOR, ?AUTHOR]).

%% @doc entry point for the server
main(Args) ->
    Options = rr:parse(Args, ?CMD_SPEC),
    case rr:any_opt([<<"help">>, <<"version">>, <<"observer">>], Options) of
	<<"help">> ->
	    getopt:usage(?CMD_SPEC, "rrs"),
	    halt();
	<<"version">> ->
	    show_information(),
	    halt();
	<<"observer">> ->
	    observer:start();
	false ->
	    ok
    end,

    rr_config:init(proplists:get_value(<<"config">>, Options, "rrs.config"), []), 
    rr_log:new(rr_config:get_value('log.target', std_err), rr_config:get_value('log.level', debug)),
    case proplists:get_value(<<"generate-dataconfig">>, Options) of
	[] -> ok;
	Folder ->
	    generate_dataconfig(Folder),
	    halt()
    end,
    

    ok = rrs_database:start(),
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),

    Port = rr_config:get_value('port', 8080),
    Gui = rr_config:get_value('ui.path', "ui"),

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
    io:format("server started. please stop the server with ^c ~n"),
    receive
	wait_for_c ->
	    ok
    end.

generate_dataconfig(Folder) ->
    {ok, Filenames} = file:list_dir(Folder),
    Info = lists:foldl(
	     fun (File, Acc) ->
		     rr_log:info("parsing ~s", [File]),
		     try
			 Csv = csv:binary_reader(filename:join(Folder, File)),
			 {Features, Examples, ExConf} = rr_example:load(Csv, erlang:system_info(schedulers)),
			 rr_example:kill(ExConf),
			 [{iolist_to_binary(File), [{file, iolist_to_binary(File)},
						    {name, iolist_to_binary(File)},
						    {no_features, length(Features)},
						    {no_examples, rr_example:count(Examples)}]}|Acc]
			     
		     catch
			 _:_ ->
			     rr_log:debug("failed to parse ~s", [File]),
			     Acc
		     end				      
	     end, [], Filenames),
    lists:foreach(fun (In) ->
			  io:format("~p,~n", [In])
		  end, Info).
