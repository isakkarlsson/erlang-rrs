%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rrs_result_get).

-export([
	 init/3,
	 content_types_provided/2,
	 get/2
	]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, get}
     ], Req, State}.

get(Req, State) ->
    {Id, _} = cowboy_req:binding(id, Req),
    if Id == <<"null">> ->
	    {rr_json:error("not_found"), Req, State};
       true ->
	    case result_db:get_value(list_to_integer(binary_to_list(Id))) of
		not_found ->
		    {rr_json:error("not_found"), Req, State};
		Data ->
		    {rr_json:reply(result, Data), Req, State}
	    end
    end.
