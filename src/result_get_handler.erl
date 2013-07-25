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
    rr_log:info("~p ~n", [binary_to_integer(Id)]),
    case rr_db:get_json(binary_to_integer(Id)) of
	not_found ->
	    {"", Req, State};
	Data ->
	    {Data, Req, State}
    end.
