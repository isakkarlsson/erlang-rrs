%%% @author  <Isak@ISAK-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2013 by  <Isak@ISAK-PC>

-module(eval_get_all_handler).


-export([init/3]).
-export([content_types_provided/2]).
-export([get_all/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, get_all}
     ], Req, State}.

get_all(Req, State) ->
    Methods = rr_config:get_value('evaluation.methods'),
    {jsx:encode(Methods), Req, State}.
