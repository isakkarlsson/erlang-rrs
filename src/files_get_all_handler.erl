%%% @author  <Isak@ISAK-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2013 by  <Isak@ISAK-PC>

-module(files_get_all_handler).


-export([init/3]).
-export([content_types_provided/2]).
-export([get_all/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, get_all}
     ], Req, State}.

get_all(Req, State) ->
    Files = rr_config:get_value('dataset.files'),
    {jsx:encode(Files), Req, State}.
