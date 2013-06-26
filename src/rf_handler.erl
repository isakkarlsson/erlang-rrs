-module(rf_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    self() ! {msg, "loading file.."},
    Self = self(),
    
    spawn(fun() ->
		  Csv = csv:binary_reader("../erlang-rr/data/spambase.txt"),
		  {Features, Examples, ExConf} = rr_example:load(Csv, 4),
		  {Build, Evaluate, _} = rf:new([{no_features, trunc(math:log(length(Features))/math:log(2))},
						 {progress, fun (X, Y) -> 
								    Self ! {msg, io_lib:format("~p of ~p trees done", [X, Y])}
							    end}]),
		  rr_eval:cross_validation(Features, Examples, ExConf,
					   [{build, Build}, 
					    {evaluate, rf:killer(Evaluate)}, 
					    {progress, fun (Fold) -> Self ! {msg, io_lib:format("Fold ~p", [Fold])} end}])
	  end),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({msg, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    rr_log:info("client is terminating.."),
    ok.
