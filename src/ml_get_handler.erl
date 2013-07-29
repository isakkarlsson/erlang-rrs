%%% @author  <Isak@ISAK-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2013 by  <Isak@ISAK-PC>

-module(ml_get_handler).


-export([init/3]).
-export([content_types_provided/2]).
-export([get/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, get}
     ], Req, State}.

get(Req, State) ->
    Methods = rr_config:get_value('machine-learning.methods'),
    {Id, _} = cowboy_req:binding(id, Req),
    case find(Id, Methods) of
	undefined ->
	    {rr_json:error("not_found"), Req, State};
	Method ->
	    {rr_json:reply(ok, Method), Req, State}
    end.

find(_, []) ->
    undefined;
find(Atom, [{M, Method}|Rest]) ->
    if Atom == M ->
	    Method;
       true -> find(Atom, Rest)
    end.
    
    
