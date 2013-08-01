-module(rrs_json).

-export([
         reply/2,
	 error/1,

	 safe_decode/1,

	 sanitize/1
	]).

reply(Method, Data) ->
    jsx:encode([{type, sanitize(Method)},
		{data, Data}]).

%% @doc safely decode json
-spec safe_decode(any()) -> any() | error.
safe_decode(Json) ->
    try
	jsx:decode(Json)
    catch
	_:_ ->
	    error
    end.

%% @doc return an error reply
error(Data) ->
    reply(error, Data).

sanitize(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
sanitize(V) when is_list(V) ->
    iolist_to_binary(V);
sanitize(V) ->
    V.
