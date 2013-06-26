%% -module(rf_handler).

%% -export([
%% 	 init/3,
%% 	 hello/2,
%% 	 content_types_provided/2
%% 	]).

%% content_types_provided(Req, State) ->
%% 	{[
%% 		{<<"text/html">>, hello},
%% 		{<<"application/json">>, hello},
%% 		{<<"text/plain">>, hello}
%% 	], Req, State}.

%% init({tcp, http}, _Req, _Opts) ->
%%     {upgrade, protocol, cowboy_rest}.

%% hello(Req, State) ->
%%     Body = <<"<html>
%% <head>
%% 	<meta charset=\"utf-8\">
%% 	<title>REST Hello World!</title>
%% </head>
%% <body>
%% 	<p>REST Hello World as HTML!</p>
%% </body>
%% </html>">>,
%%     {Body, Req, State}.
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
		  rr_example:init(),
		  {F, E} = rr_example:load(Csv, 4),
		  
		  {Build, Evaluate, _} = rf:new([{no_features, trunc(math:log(length(F))/math:log(2))},
						 {progress, fun (X, Y) -> 
								    Self ! {msg, io_lib:format("~p of ~p trees done", [X, Y])}
							    end}]),
		  rr_eval:cross_validation(F, E, 
					   [{build, Build}, 
					    {evaluate, Evaluate}, 
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
