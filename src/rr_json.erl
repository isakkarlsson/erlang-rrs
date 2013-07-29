-module(rr_json).

-export([
         reply/2,
	 error/1,

	 sanitize/1
	]).

reply(Method, Data) ->
    jsx:encode([{type, sanitize(Method)},
		{data, Data}]).

error(Data) ->
    reply(error, Data).

sanitize(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
sanitize(V) when is_list(V) ->
    iolist_to_binary(V);
sanitize(V) ->
    V.
