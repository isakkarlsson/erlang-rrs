-module(rf_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-export([
	 spawn_model_evaluator/2,
	 spawn_model_evaluator/7,
	 to_json/1
	]).

%% @headerfile "rr_server.hrl"
-include("rr_server.hrl").

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    self() ! {message, "Initialized. Waiting for go!"},
    self() ! {progress, 0},
    {ok, Req, #rr_state{current=undefined}}.

websocket_handle({text, Json}, Req, State) ->
    Data = (catch jsx:decode(Json)),
    websocket_handle_json(Data, Req, State);
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_handle_json(Obj, Req, State) ->
    Process = spawn_model(Obj),
    {ok, Req, #rr_state{current=Process}}.
    
websocket_info({message, Msg}, Req, State) ->
    {reply, {text, rr_json:reply(message, [{text, rr_json:sanitize(Msg)}])}, Req, State};
websocket_info({progress, Msg}, Req, State) ->
    {reply, {text, rr_json:reply(progress, [{value, rr_json:sanitize(Msg)}])}, Req, State};
websocket_info({completed, R}, Req, State) ->
    Id = result_db:insert(R),
    {reply, {text, rr_json:reply(completed, [{result_id, Id}])}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
    Process = State#rr_state.current,
    exit(Process, terminate),
    ok.



to_json({cv, NoFolds, Folds}) ->
    [{type, <<"cross-validation">>},
     {no_folds, NoFolds},
     {folds, to_json_cv(Folds, [])}].

to_json_cv([], Acc) ->
    Acc;
to_json_cv([{{_, Fold}, Measures}|Rest], Acc) ->
    NewFold = rr_json:sanitize(Fold),
    to_json_cv(Rest, [[{fold_no, NewFold},
		       {measures, to_json_measures(Measures)}]|Acc]).

to_json_measures(Measures) ->
    lists:foldl(fun ({Key, Value}, Acc) ->
			[{Key, Value}|Acc];
		    ({Key, PerClass, Avg}, Acc) ->
			[{Key, [{average, Avg}|lists:map(fun ({K,_, V}) -> {K, V} end,  PerClass)]}|Acc]
		end, [], Measures). 


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
	    [{no_features, proplists:get_value(<<"no-features">>, Machine)},
	     {no_trees, proplists:get_value(<<"no-trees">>, Machine)}];
	_ ->
	    []
    end.

spawn_model(Props) ->
    rr_log:info("~p ~n", [Props]),
    spawn_link(?MODULE, spawn_model_evaluator, [self(), Props]).

spawn_model_evaluator(Self, Props) ->
    process_flag(trap_exit, true),
    
    File = parse_file_json(Props),
    Eval = parse_evaluator_json(Props),
    Machine = parse_machine_json(Props),

    Csv = csv:binary_reader(io_lib:format("../erlang-rr/data/~s", [File])),
    {Features, Examples, ExConf} = rr_example:load(Csv, 4),
    Pid = spawn(?MODULE, spawn_model_evaluator, [Self, Eval, Machine, Props, Features, Examples, ExConf]),
    receive
	{'EXIT', _, terminate} = R ->
	    io:format("killed!! ~p ~n", [R]),
	    exit(Pid),
	    csv:kill(Csv),
	    rr_example:kill(ExConf),
	    ok
    end.

spawn_model_evaluator(Self, Eval, Machine, Props, Features, Examples, ExConf) ->
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
	    Predictions = prediction_to_json(rr_example:predictions(ExConf, Examples), Examples),
	    Self ! {completed, to_json(R) ++ Props ++ [{predictions, Predictions}]};
	_ ->
	    ok
    end.

prediction_to_json(Preds, Examples) ->
    P = lists:foldl(
	  fun ({Id, Real, Pred}, Acc) ->
		  [[{exid, Id}, 
		    {real, atom_to_binary(Real, utf8)},
		    {predictions, 
		     lists:reverse(lists:foldl(
				     fun ({Class, Prob}, PredAcc) ->
					     [[{class, atom_to_binary(Class, utf8)}, {probability, Prob}]|PredAcc]
				     end, [], Pred))}]|Acc]
	  end, [], Preds),
    Classes = lists:map(fun ({C, _, _}) -> atom_to_binary(C, utf8) end, Examples),
    [{classes, Classes}, {predictions, P}].
