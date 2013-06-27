-module(rf_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%% @headerfile "rr_server.hrl"
-include("rr_server.hrl").

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    self() ! {msg, "loading file.."},
    {ok, Req, #rr_state{}}. %% init with some sort of state 

websocket_handle({text, Msg}, Req, State) ->
    rr_log:info("~p ~n", [State]),
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({msg, Msg}, Req, State) ->
    {reply, {text, json_reply(message, [{text, sanitize_value(Msg)}])}, Req, State};
websocket_info({result, R}, Req, State) ->
    {reply, {text, json_reply(result, R)}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    rr_log:info("client is terminating.."),
    ok.

json_reply(Method, Data) ->
    jsx:encode([{method, sanitize_value(Method)},
		{data, Data}]).


to_json({cv, NoFolds, Folds}) ->
    [{type, <<"cross-validation">>},
     {no_folds, NoFolds},
     {folds, to_json_cv(Folds, [])}].

to_json_cv([], Acc) ->
    Acc;
to_json_cv([{{_, Fold, _}, Measures}|Rest], Acc) ->
    NewFold = sanitize_value(Fold),
    to_json_cv(Rest, [[{fold_no, NewFold},
		       {measures, to_json_measures(Measures)}]|Acc]).

to_json_measures(Measures) ->
    lists:foldl(fun ({Key, Value}, Acc) ->
			[{Key, Value}|Acc];
		    ({Key, PerClass, Avg}, Acc) ->
			[{Key, [{average, Avg}|lists:map(fun ({K,_, V}) -> {K, V} end,  PerClass)]}|Acc]
		end, [], Measures). 

sanitize_value(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
sanitize_value(V) when is_list(V) ->
    iolist_to_binary(V);
sanitize_value(V) ->
    V.




%% spawn(fun() ->
%% 		  Csv = csv:binary_reader("../erlang-rr/data/iris.txt"),
%% 		  {Features, Examples, ExConf} = rr_example:load(Csv, 4),
%% 		  {Build, Evaluate, _} = rf:new([{no_features, trunc(math:log(length(Features))/math:log(2))},
%% 						 {progress, fun (X, Y) -> 
%% 								    Self ! {msg, io_lib:format("~p of ~p trees done", [X, Y])}
%% 							    end}]),
%% 		  R = rr_eval:cross_validation(Features, Examples, ExConf,
%% 					       [{build, Build},
%% 						{folds, 2},
%% 						{evaluate, rf:killer(Evaluate)}, 
%% 						{progress, fun (Fold) -> Self ! {msg, io_lib:format("Fold ~p", [Fold])} end}]),
%% 		  Self ! {result, to_json(R)}
