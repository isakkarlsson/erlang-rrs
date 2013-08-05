%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rrs_dataset_getall).

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
    Files = rr_config:get_value('dataset.files'),
    {jsx:encode(Files), Req, State}.
