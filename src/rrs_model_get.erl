%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rrs_model_get).

-export([
	 init/3,
	 content_types_provided/2,
	 get/2
	]).

%% @headerfile "rrs.hrl"
-include("rrs.hrl").

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
      {<<"application/octet-binary">>, get}
     ], Req, State}.

get(Req, State) ->
    {Id, _} = cowboy_req:binding(id, Req),
    if Id == <<"null">> ->
	    {rrs_json:error("not_found"), Req, State};
       true ->
	    case rrs_database:get_value(list_to_integer(binary_to_list(Id))) of
		not_found ->
		    {halt, Req, State};
		Data ->
		    Model = Data#rrs_experiment_data.model,
		    {Model, Req, State}
	    end
    end.
