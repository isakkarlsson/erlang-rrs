-module(rrs_experiment).

-export([
	 init/3,
	 websocket_init/3,
	 websocket_handle/3,
	 websocket_info/3,
	 websocket_terminate/3
	]).

-export([
	 spawn_model_evaluator/2,
	 spawn_model_evaluator/5
	]).

%% @headerfile "rrs.hrl"
-include("rrs.hrl").
-include_lib("rr/include/rr.hrl").

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    self() ! {progress, 0},
    {ok, Req, #rrs_experiment{current=undefined}}.

websocket_handle({text, Json}, Req, State) ->
    Data = rrs_json:safe_decode(Json),
    websocket_handle_json(Data, Req, State);
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%% @doc handle experiment messages
websocket_handle_json(error, Req, State) ->
    {shutdown, Req, State};
websocket_handle_json(Obj, Req, State) ->
    Current = State#rrs_experiment.current,
    if Current == undefined -> %% note: only one experiment per client
	    Process = spawn_model(Obj),
	    {ok, Req, #rrs_experiment{current=Process}};
       true ->
	    {reply, {text, rrs_json:error("invalid-experiment")}, Req, State}
    end.
    
%% @doc send messages to the client
websocket_info({message, Msg}, Req, State) ->
    {reply, {text, rrs_json:reply(message, [{text, rrs_json:sanitize(Msg)}])}, Req, State};
websocket_info({error, Msg}, Req, State) ->
    {reply, {text, rrs_json:reply(error, [{text, rrs_json:sanitize(Msg)}])}, Req, State};
websocket_info({progress, Msg}, Req, State) ->
    {reply, {text, rrs_json:reply(progress, [{value, rrs_json:sanitize(Msg)}])}, Req, State};
websocket_info({completed, R}, Req, State) ->

    Id = rrs_database:insert(R),
    {reply, {text, rrs_json:reply(completed, [{result_id, Id}])}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

%% @doc terminate any running experiments when the client disconnects
websocket_terminate(_Reason, _Req, State) ->
    Process = State#rrs_experiment.current,
    exit(Process, terminate),
    ok.

parse_file_json(Json) ->
    File = proplists:get_value(<<"file">>, Json),
    proplists:get_value(<<"file">>, File).

parse_evaluator_json(Json) ->
    Eval = proplists:get_value(<<"evaluator">>, Json),
    case proplists:get_value(<<"id">>, Eval) of
	<<"cross-validation">> ->
	    {cv, 
	     proplists:get_value(<<"folds">>, Eval),
	     proplists:get_value(<<"store">>, Eval) == <<"yes">>
	    };
	undefined ->
	    throw({error})
    end.

parse_machine_json(Json) ->
    Machine = proplists:get_value(<<"learner">>, Json),
    case proplists:get_value(<<"id">>, Machine) of
	<<"rf">> ->
	    Args = rf:args(Machine, fun (R, V) -> rr_log:info("~p ~p", [R, V]), undefined end),
	    rr_log:info("~p", [Args]),
	    Args;
	_ ->
	    []
    end.

spawn_model(Props) ->
    rr_log:info("spawning experiment evaluator"),
    spawn_link(?MODULE, spawn_model_evaluator, [self(), Props]).

spawn_model_evaluator(Self, Props) ->
    process_flag(trap_exit, true),
    
    File = parse_file_json(Props),
    Eval = parse_evaluator_json(Props),
    Machine = parse_machine_json(Props),
    %% try / catch
    rr_log:info("~p", [filename:join(rr_config:get_value('dataset.folder', "../data"), File)]),
    Csv = csv:binary_reader(filename:join(rr_config:get_value('dataset.folder', "../data"), File)),
    ExSet = rr_example:load(Csv, 4),
    Pid = spawn_link(?MODULE, spawn_model_evaluator, [Self, Eval, Machine, Props, ExSet]),
    receive
	{'EXIT', _, terminate} ->
	    rr_log:debug("terminating model ~p", [Pid]),
	    exit(Pid),
	    csv:kill(Csv),
	    rr_example:kill(ExSet),
	    ok
    end.

spawn_model_evaluator(Self, Eval, Machine, Props, ExSet) ->
    try
	case Eval of
	    {cv, NoFolds, Store} ->
		Progress = fun (done, done) ->
				   Self ! {progress, 100};
			       (X, Y) -> 
				   Self ! {progress, round((X/Y)*100)}
			   end,
		Rf = rf:new(Machine ++ [{progress, Progress}]),
		Build = rf:partial_build(Rf),
		Evaluate = rf:partial_evaluate(Rf),
		{Result, _M} = cross_validation:evaluate(
				 ExSet,
				 [{build, Build},
				  {folds, NoFolds},
				  {evaluate, rf:killer(Evaluate)}, 
				  {progress, fun (Fold) -> 
						     Self ! {progress, 0},
						     Self ! {message, io_lib:format("Running fold ~p of ~p", 
										    [Fold, NoFolds])}
					     end}]),
		#rr_exset {
		   features = Features,
		   examples = Examples, 
		   exconf = ExConf
		  } = ExSet,
		BinaryModel = if Store ->
				      Self ! {progress, 0},
				      Self ! {message, "Storing model of complete data set"},
				      Model = Build(Features, Examples, ExConf),
				      rf:serialize(Rf, Model);
				 true ->
				      undefined
			      end,
		Ex = #rrs_experiment_data {
			model = BinaryModel,
			properties = Props,
			evaluation = Result,
			predictions = rr_example:predictions(ExConf, Examples),
			classes = lists:map(fun ({Class, Count, _}) -> {Class, Count} end, Examples),
			features = lists:map(fun ({Type, Id}) -> 
						     {Type, rr_example:feature_name(ExConf, Id)}
					     end, Features)
		       },
		Self ! {completed, Ex};
	    _ ->
		ok
	end
    catch
	X:Y ->
	    rr_log:debug("build failed with reason: ~p", [{X, Y}]),
	    erlang:display(erlang:get_stacktrace()),
	    Self ! {error, "build-fail"}
    end.


