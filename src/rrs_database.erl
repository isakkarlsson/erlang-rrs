%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 24 Jul 2013 by Isak Kalsson <isak-kar@dsv.su.se>

-module(rrs_database).
-compile(export_all).

-record(counter, {id=0, ver=1}).
-record(result, {id, json}).

start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),

    mnesia:create_table(result, [{disc_copies, [node()]},
                              {attributes, record_info(fields, result)}]),

    mnesia:create_table(counter, [{disc_copies, [node()]},
                                  {attributes, record_info(fields, counter)}]),
    mnesia:wait_for_tables([result, counter], 2000),
    ok.

stop() ->
    mnesia:stop().

insert(Json) ->
    T = fun() ->
		Id = next_int(),
		New = #result{id=Id, json=Json},
		mnesia:write(New),
		Id
		    
	end,
    {atomic, Val} = mnesia:transaction(T),
    Val.

   
get_value(Id) ->
    F = fun() -> mnesia:read(result, Id, read) end,
    {atomic, Val} = mnesia:transaction(F),
    case Val of
	[] ->
	    not_found;
	[#result{json=Data}|_] ->
	    Data
    end.

next_int() ->
    mnesia:dirty_update_counter(counter, id, 1).
