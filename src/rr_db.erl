%%% @author  <Isak@ISAK-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 24 Jul 2013 by  <Isak@ISAK-PC>

-module(rr_db).
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
    

put_json(Json) ->
    T = fun() ->
		Id = next_int(),
		New = #result{id=Id, json=Json},
		mnesia:write(New)
	end,
    {atomic, Val} = mnesia:transaction(T),
    Val.
   
get_json(Id) ->
    F = fun() -> mnesia:read(result, Id, read) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

next_int() ->
    mnesia:dirty_update_counter(counter, id, 1).
