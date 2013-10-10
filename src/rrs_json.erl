%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created :  2 Aug 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rrs_json).

-export([
         reply/2,
	 error/1,

	 safe_decode/1,
	 sanitize/1,
	 
	 convert_cv/1,
	 convert_predictions/2,
	 convert_features/1	 
	]).

reply(Method, Data) ->
    jsx:encode([{type, sanitize(Method)},
		{data, Data}]).

%% @doc safely decode json
-spec safe_decode(any()) -> any() | error.
safe_decode(Json) ->
    try
	jsx:decode(Json)
    catch
	_:_ ->
	    error
    end.

%% @doc convert predictions to json
convert_predictions(Preds, Examples) ->
    P = lists:foldl(
	  fun ({Id, Real, Pred}, Acc) ->
		  [[{exid, Id}, 
		    {real, atom_to_binary(Real, utf8)},
		    {predictions, 
		     lists:reverse(lists:foldl(
				     fun ({Class, Prob, Votes}, PredAcc) ->
					     [[{class, atom_to_binary(Class, utf8)}, 
					       {probability, Prob},
					       {votes, Votes}]|PredAcc]
				     end, [], Pred))}]|Acc]
	  end, [], Preds),
    Classes = lists:map(
		fun ({Class, Count}) -> 
			[{class, atom_to_binary(Class, utf8)}, {count, Count}]
		end, Examples),
    [{classes, Classes}, {predictions, P}].

convert_features(Features) ->
    lists:map(fun ({Type, Name}) ->
		      [{type, sanitize(Type)},
		       {name, sanitize(Name)}]
	      end, Features).

%% @doc convert a cross-validation to json
convert_cv({cv, NoFolds, Folds}) ->
    [{type, <<"cross-validation">>},
     {no_folds, NoFolds},
     {folds, convert_cv_folds(Folds, [])}].

convert_cv_folds([], Acc) ->
    Acc;
convert_cv_folds([{{_, Fold}, Measures}|Rest], Acc) ->
    NewFold = rrs_json:sanitize(Fold),
    convert_cv_folds(Rest, [[{fold_no, NewFold},
			     {measures,convert_cv_measures(Measures)}]|Acc]).

convert_cv_measures(Measures) ->
    lists:foldl(fun ({Key, {Type, Values, Average}}, Acc) ->
			[{Key, [{average, Average}|lists:map(fun ({K, {_, V}}) -> {K, sanitize(V)} end, Values)]}|Acc];
		    ({Key, Value}, Acc) ->
			[{Key, sanitize(Value)}|Acc];		    
		    ({Key, {PerClass, Avg}}, Acc) ->
			[{Key, [{average, Avg}|lists:map(fun ({K, {_, V}}) -> {K, sanitize(V)} end, PerClass)]}|Acc];
		    ({Key, Value}, Acc) when is_list(Value) ->
			[{Key, lists:map(fun ({K, V}) -> {K, sanitize(V)} end, Value)}|Acc];
		    ({Key, Value}, Acc) ->
			[{Key, sanitize(Value)}|Acc];
		    ({Key, PerClass, Avg}, Acc) ->
			[{Key, [{average, Avg}|lists:map(fun ({K, {_, V}}) -> {K, sanitize(V)} end, PerClass)]}|Acc]
		end, [], Measures). 


%% @doc return an error reply
error(Data) ->
    reply(error, Data).

sanitize(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
sanitize(V) when is_list(V) ->
    iolist_to_binary(V);
sanitize(V) ->
    V.
