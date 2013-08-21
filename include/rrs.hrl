%%% @author  <Isak@ISAK-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%% 
%%% @end
%%% Created : 27 Jun 2013 by  <Isak@ISAK-PC>

%% @doc 
-record(rrs_experiment, {
	  current :: pid()
	 }).

%% @doc record containing data for an experiemtn
-record(rrs_experiment_data, {
	  model = undefined :: binary(),
	  properties,
	  evaluation,
	  predictions,
	  classes,
	  features
	 }).
	  
