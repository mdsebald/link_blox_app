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
-export([set_array_value/3]).
-export([resize_attribute_array_value/5]).
-export([log_error/3]).


%%
%% Set block output value and status
%% Block output value and status attributes are often set at the same time.
%% This is a shortcut to do that.
%% 
-spec set_value_status(Outputs :: list(output_attr()), 
                       Value :: value(), 
                       Status :: block_status()) -> list(output_attr()).

set_value_status(Outputs, Value, Status) ->
  {ok, Outputs1} = attrib_utils:set_values(Outputs, 
                       [{value, Value}, {status, Status}]),
  Outputs1.
  
%%
%% Set block output value and set status to normal
%% When setting the output value block status is usually normal.
%% This is a shortcut to do that.
%% 
-spec set_value_normal(Outputs :: list(output_attr()), 
                       Value :: value()) -> list(output_attr()).

set_value_normal(Outputs, Value) ->
  {ok, Outputs1} = attrib_utils:set_values(Outputs, 
                                 [{value, Value}, {status, normal}]),
  Outputs1.

%%
%% Set status output value
%% 
-spec set_status(Outputs :: list(output_attr()), 
                 Status :: block_status()) -> list(output_attr()).

set_status(Outputs, Status) ->
  {ok, Outputs1} = attrib_utils:set_value(Outputs, status, Status),
  Outputs1.
  
%%
%% Set status array value output values
%% 
-spec set_array_value(Outputs :: list(output_attr()), 
                      ArrayValueName :: value_name(),
                      ArrayValues :: list(value())) -> list(output_attr()).

set_array_value(Outputs, ArrayValueName, ArrayValue) ->
  set_array_value(Outputs, ArrayValueName, ArrayValue, 1).
  
set_array_value(Outputs, _ArrayValueName, [], _Index) ->
  Outputs;

set_array_value(Outputs, ArrayValueName, [Value | ArrayValue], Index) ->
  {ok, NewOutputs} = attrib_utils:set_value(Outputs, ArrayValueName, Value, Index),
  set_array_value(NewOutputs, ArrayValueName, ArrayValue, (Index + 1)).


%%
%% Resize an array value in the Inputs attribute list
%% to match the target quantity
%% Returns updated Inputs attribute list
%%
-spec resize_attribute_array_value(BlockName :: block_name(),
                                   Outputs :: list(output_attr()),
                                   ArrayValueName :: value_name(),
                                   TargQuant :: pos_integer(),
                                   DefaultValue :: output_value()) -> list(output_attr()).
                             
resize_attribute_array_value(BlockName, Outputs, ArrayValueName, TargQuant, DefaultValue)->
   % Function to dereference the delete array values' references to input block values
   DeleteExcess = fun(DeleteArrayValues) -> 
      lists:map(
        fun(DeleteValue) -> 
          DeleteAttr = {ArrayValueName, DeleteValue},
          link_utils:deref(BlockName, DeleteAttr)
		      end, 
          DeleteArrayValues) end,
       
  resize_attribute_array_value(Outputs, ArrayValueName, TargQuant, DefaultValue, DeleteExcess).


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