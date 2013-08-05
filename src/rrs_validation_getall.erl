%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2013 Isak Karlsson <isak-kar@dsv.su.se>
-module(rrs_validation_getall).

-export([
	 init/3,
	 content_types_provided/2,
	 get_all/2
	]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, get_all}
     ], Req, State}.

get_all(Req, State) ->
    Methods = rr_config:get_value('evaluation.methods'),
    {jsx:encode(Methods), Req, State}.
