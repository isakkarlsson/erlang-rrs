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
    self() ! {progress, 0},
    {ok, Req, #rr_state{current=0}}. %% init with some sort of state 

websocket_handle({text, Json}, Req, State) ->
    Obj = (catch jsx:decode(Json)),
    {Resp, NewState} = websocket_handle_(Obj, State),
    {reply, {text, json_reply(response, Resp)}, Req, NewState};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_handle_(Obj, State) ->
    rr_log:info("~p~p ~n", [Obj, State]),
    {[{started, <<"job x2>>">>}], State#rr_state{current=State#rr_state.current + 1}}.
    
websocket_info({progress, Msg}, Req, State) ->
    {reply, {text, json_reply(progress, [{value, sanitize_value(Msg)}])}, Req, State};
websocket_info({completed, R}, Req, State) ->
    {reply, {text, json_reply(completed, R)}, Req, State};
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
