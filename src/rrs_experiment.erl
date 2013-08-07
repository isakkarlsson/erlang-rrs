-module(rrs_experiment).
-behaviour(cowboy_websocket_handler).

-export([
	 init/3,
	 websocket_init/3,
	 websocket_handle/3,
	 websocket_info/3,
	 websocket_terminate/3
	]).

-export([
	 spawn_model_evaluator/2,
	 spawn_model_evaluator/7
	]).

%% @headerfile "rr_server.hrl"
-include("rrs.hrl").

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
	    {cv, proplists:get_value(<<"folds">>, Eval)};
	undefined ->
	    throw({error})
    end.

parse_machine_json(Json) ->
    Machine = proplists:get_value(<<"learner">>, Json),
    case proplists:get_value(<<"id">>, Machine) of
	<<"rf">> ->
	    Prior = [{no_features, proplists:get_value(<<"no_features">>, Machine)}],
	    Args = rf:args(Machine, Prior),
	    rr_log:info("~p", [Args]),
	    Args;
	_ ->
	    []
    end.

spawn_model(Props) ->
    rr_log:info("spawning experiment evaluator"),
    rr_log:debug(" with properties ~p", [Props]),
    spawn_link(?MODULE, spawn_model_evaluator, [self(), Props]).

spawn_model_evaluator(Self, Props) ->
    process_flag(trap_exit, true),
    
    File = parse_file_json(Props),
    Eval = parse_evaluator_json(Props),
    Machine = parse_machine_json(Props),
    %% try / catch
    Csv = csv:binary_reader(io_lib:format("../data/~s", [File])),
    {Features, Examples, ExConf} = rr_example:load(Csv, 4),
    Pid = spawn_link(?MODULE, spawn_model_evaluator, [Self, Eval, Machine, Props, Features, Examples, ExConf]),
    receive
	{'EXIT', _, terminate} = R ->
	    io:format("killed!! ~p ~n", [R]),
	    exit(Pid),
	    csv:kill(Csv),
	    rr_example:kill(ExConf),
	    ok
    end.

spawn_model_evaluator(Self, Eval, Machine, Props, Features, Examples, ExConf) ->
    try
	case Eval of
	    {cv, NoFolds} ->
		{Build, Evaluate, _} = rf:new(Machine ++ [{progress, 
							   fun (done, done) ->
								   Self ! {progress, 100};
							       (X, Y) -> 
								   Self ! {progress, round((X/Y)*100)}
							   end}]),
		{R, _M} = rr_eval:cross_validation(
			    Features, Examples, ExConf,
			    [{build, Build},
			     {folds, NoFolds},
			     {evaluate, rf:killer(Evaluate)}, 
			     {progress, fun (Fold) -> 
						Self ! {progress, 0},
						Self ! {message, io_lib:format("Running fold ~p of ~p", [Fold, NoFolds])}
					end}]),
		Predictions = rrs_json:convert_predictions(rr_example:predictions(ExConf, Examples), Examples),
		Self ! {completed, rrs_json:convert_cv(R) ++ Props ++ [{predictions, Predictions}]};
	    _ ->
		ok
	end
    catch
	X:Y ->
	    rr_log:debug("build failed with reason: ~p", [{X, Y}]),
	    erlang:display(erlang:get_stacktrace()),
	    Self ! {error, "build-fail"}
    end.


