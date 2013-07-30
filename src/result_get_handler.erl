%%% @author  <Isak@ISAK-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2013 by  <Isak@ISAK-PC>

-module(result_get_handler).


-export([init/3]).
-export([content_types_provided/2]).
-export([get/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, get}
     ], Req, State}.

get(Req, State) ->
    {Id, _} = cowboy_req:binding(id, Req),
    if Id == <<"null">> ->
	    {rr_json:error("not_found"), Req, State};
       true ->
	    case result_db:get_value(binary_to_integer(Id)) of
		not_found ->
		    {rr_json:error("not_found"), Req, State};
		Data ->
		    {rr_json:reply(result, Data), Req, State}
	    end
    end.
