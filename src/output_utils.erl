%%% @doc 
%%% Get and Set Block Output values   
%%%               
%%% @end 

-module(output_utils).

-author("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([set_value_status/3, set_value_normal/2, set_status/2]).
-export([create_output_array/2]).
-export([log_error/3]).


%%
%% Set block output value and status
%% Block output value and status attributes are often set at the same time.
%% This is a shortcut to do that.
%% 
-spec set_value_status(Outputs :: list(), 
                       Value :: term(), 
                       Status :: block_status()) -> list().

set_value_status(Outputs, Value, Status) ->
  {ok, Outputs1} = block_utils:set_values(Outputs, 
                       [{value, Value}, {status, Status}]),
  Outputs1.
  
%%
%% Set block output value and set status to normal
%% When setting the output value block status is usually normal.
%% This is a shortcut to do that.
%% 
-spec set_value_normal(Outputs :: list(), 
                       Value :: term()) -> list().

set_value_normal(Outputs, Value) ->
  {ok, Outputs1} = block_utils:set_values(Outputs, 
                                 [{value, Value}, {status, normal}]),
  Outputs1.

%%
%% Set status output value
%% 
-spec set_status(Outputs :: list(attribute()), 
                 Status :: block_status()) -> list(attribute()).

set_status(Outputs, Status) ->
  {ok, Outputs1} = block_utils:set_value(Outputs, status, Status),
  Outputs1.
  

%%
%% Create an array of outputs, with a common base ValueName plus index number
%% Default output value is always not_active
%% Create an associated list of value names, to assist in accessing the output array
%%
-spec create_output_array(Quant :: integer(),
                          BaseValueName :: atom()) -> {list(), list()}.
                              
create_output_array(Quant, BaseValueName) ->
  create_output_array([], [], Quant, BaseValueName).

  
-spec create_output_array(Outputs :: list(),
                          ValueNames :: list(),
                          Quant :: integer(),
                          BaseValueName :: atom()) -> {list(), list()}.
                              
create_output_array(Outputs, ValueNames, 0, _BaseValueName) ->
  {lists:reverse(Outputs), lists:reverse(ValueNames)};

create_output_array(Outputs, ValueNames, Quant, BaseValueName) ->
  ValueNameStr = iolib:format("~s_~2..0w", BaseValueName, Quant),
  ValueName = list_to_atom(ValueNameStr),
  Output = {ValueName, not_active, []},
  create_output_array([Output | Outputs], [ValueName | ValueNames], Quant - 1, 
                      BaseValueName).


%%
%% Log output value error
%%
-spec log_error(Config :: list(),
                ValueName :: atom(),
                Reason :: atom()) -> ok.
                  
log_error(Config, ValueName, Reason) ->
  BlockName = config_utils:name(Config),
  error_logger:error_msg("~p Invalid '~p' input value: ~p~n", 
                            [BlockName, ValueName, Reason]),
  ok.
  
  
%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% Test input value list
test_outputs() ->
  [ {float_good, 123.45, {null, block1, value}},
    {float_bad, xyz, ?EMPTY_LINK}
    {integer_good, 12345, {null, block2, value}},
    {integer_bad, "bad", ?EMPTY_LINK},
    {boolean_good, true, ?EMPTY_LINK},
    {boolean_bad, 0.0, ?EMPTY_LINK},
    {not_active_good, not_active, ?EMPTY_LINK},
    {empty_good, empty, ?EMPTY_LINK},
    {empty_bad, empty, {knot, empty, link}},
    {not_input, 123, [test1,test2]}
  ].
  
  
get_value_test() ->
  TestInputs = test_inputs().
    


-endif.