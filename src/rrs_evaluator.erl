-module(rrs_evaluator).

-export([
         init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3,

         spawn_load_model/3,
         spawn_load_data/3
        ]).

%% @headerfile "rrs.hrl"
-include("rrs.hrl").
-include_lib("rr/include/rr.hrl").

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    self() ! {progress, 0},
    {ok, Req, undefined}.

websocket_handle({text, Json}, Req, State) ->
    Data = rrs_json:safe_decode(Json),
    websocket_handle_json(Data, Req, State);
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%% @doc handle experiment messages
websocket_handle_json(error, Req, State) ->
    {shutdown, Req, State};
websocket_handle_json(Obj, Req, State) ->
    Id = proplists:get_value(<<"id">>, Obj),
    Example = proplists:get_value(<<"example">>, Obj),
%%    rr_log:info("~p ~s", [Id, Example]),
    Process = load_model(Id, Example),
    {ok, Req, Process}.

    
%% @doc send messages to the client
websocket_info({message, Msg}, Req, State) ->
    {reply, {text, rrs_json:reply(message, [{text, rrs_json:sanitize(Msg)}])}, Req, State};
websocket_info({error, Msg}, Req, State) ->
    {reply, {text, rrs_json:reply(error, [{text, rrs_json:sanitize(Msg)}])}, Req, State};
websocket_info({progress, Msg}, Req, State) ->
    {reply, {text, rrs_json:reply(progress, [{value, rrs_json:sanitize(Msg)}])}, Req, State};
websocket_info({completed, R}, Req, State) ->
    Prediction = json(R),
    {reply, {text, rrs_json:reply(completed, Prediction)}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

%% @doc terminate any running experiments when the client disconnects
websocket_terminate(_Reason, _Req, Process) ->
    exit(Process, terminate),
    ok.

load_model(Id, Example) ->
    spawn_link(?MODULE, spawn_load_data, [self(), Id, Example]).

spawn_load_data(Parent, Id, Binary) ->
    process_flag(trap_exit, true),
    Csv = {csv_reader, spawn_link(csv, parse_binary_incremental, [Binary, 1, false])},
    ExSet = rr_example:load(Csv, 4),
    Pid = spawn_link(?MODULE, spawn_load_model, [Parent, Id, ExSet#rr_exset.exconf]),
    receive
        {'EXIT', _, terminate} ->
            rr_log:debug("terminating evaluator ~p", [Pid]),
            exit(Pid),
            csv:kill(Csv),
            rr_example:kill(ExSet),
            ok
    end.
spawn_load_model(Parent, Id, ExConf) ->
    #rrs_experiment_data {
       model = ModelDump
      } = rrs_database:get_value(Id),
    {Module, Dump} = rr_system:unserialize_model(ModelDump),
    {Model, Conf} = Module:unserialize(Dump),
    P = rf:predict(Conf, Model, 1, ExConf),
    Parent ! {completed, P}.

json({Pred, Rest}) ->
    [{prediction, json_pred(Pred)},
     {probabilities, lists:map(fun json_pred/1, Rest)}].

json_pred({Class, Prob, _Votes}) ->
    [{class, rrs_json:sanitize(Class)},
     {prob, Prob}].

    
    
    
