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
-export([set_array_value/3, update_all_outputs/3]).
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
%% Set an array of output values to ArrayValues
%% Number of values in ArrayValues, must match the number of array values
%% in the ArrayValueName output value
%% 
-spec set_array_value(Outputs :: list(output_attr()), 
                      ArrayValueName :: value_name(),
                      ArrayValues :: list(value())) -> list(output_attr()).

set_array_value(Outputs, ArrayValueName, ArrayValues) ->
  set_array_value(Outputs, ArrayValueName, 1, ArrayValues).
  
set_array_value(Outputs, _ArrayValueName, _Index, []) ->
  Outputs;

set_array_value(Outputs, ArrayValueName, Index, [Value | ArrayValues]) ->
  {ok, NewOutputs} = attrib_utils:set_value(Outputs, {ArrayValueName, Index}, Value),
  set_array_value(NewOutputs, ArrayValueName, (Index + 1), ArrayValues).


%% 
%% Update all outputs to the New value,
%% except update status output to the New Staus value
%% Used to mass update block outputs in disabled or error conditions
%% 
-spec update_all_outputs(Outputs :: list(output_attr()), 
                         NewValue :: value(), 
                         NewStatus :: block_status()) -> list(output_attr()).

update_all_outputs(Outputs, NewValue, NewStatus) ->
  lists:map(
    fun(Output) ->
      case Output of 
        {ValueName, {_Value, Refs}} ->
          case ValueName of
            status -> {ValueName, {NewStatus, Refs}};
                 _ -> {ValueName, {NewValue,  Refs}}
          end;
        {ValueName, ArrayValues} ->
          {ValueName, update_all_array_values(ArrayValues, NewValue)}      
      end  
    end,
    Outputs).

 
%%
%% Set all of the values in ArrayValues to NewValue
%%
-spec update_all_array_values(ArrayValues :: list(),
                              NewValue :: value()) -> list().
                                
update_all_array_values(ArrayValues, NewValue) ->
  lists:map(fun({_Value, Refs}) -> {NewValue, Refs} end, ArrayValues).
 
    
%%
%% Resize an array value in the Outputs attribute list
%% to match the target quantity
%% Returns updated Outputs attribute list
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
  
  attrib_utils:resize_attribute_array_value(Outputs, ArrayValueName, TargQuant, 
                                            DefaultValue, DeleteExcess).


%%
%% Log output value error
%%
-spec log_error(Config :: list(config_attr()),
                ValueName :: value_name(),
                Reason :: atom()) -> ok.
                  
log_error(Config, ValueName, Reason) ->
  BlockName = config_utils:name(Config),
  error_logger:error_msg("~p Invalid '~p' output value: ~p~n", 
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

% ====================================================================
% Test data
%
test_config_attribs1() ->
  [ {block_name, {test_input_utils}},
    {block_module, {type_test}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}}
  ].

test_input_attribs1() ->
  [ {disable, {true, ?EMPTY_LINK}},
    {freeze, {false, ?EMPTY_LINK}},
    {exec_in, {empty, ?EMPTY_LINK}},
    {exec_interval, {0, ?EMPTY_LINK}},
    {float_good, {123.45, {null, block1, value}}},
    {float_bad, {xyz, ?EMPTY_LINK}},
    {integer_good, {12345, {null, block2, value}}},
    {integer_bad, {"bad", ?EMPTY_LINK}},
    {boolean_good, {true, ?EMPTY_LINK}},
    {boolean_bad, {0.0, ?EMPTY_LINK}},
    {not_active_good, {not_active, ?EMPTY_LINK}},
    {empty_good, {empty, ?EMPTY_LINK}},
    {empty_bad, {empty, {knot, empty, link}}},
    {not_input, {123, [test1,test2]}},
    {integer_array, [{123, {}}, {789, {null, test_block, test_output}}]}
  ].
  
test_input_attribs2() ->
  InputList = test_input_attribs1(),
  ModifiedAttribute = {integer_array, 
                       [{123, {}}, 
                        {789, {null, test_block, test_output}},
                        {empty, ?EMPTY_LINK},
                        {empty, ?EMPTY_LINK}]},
   attrib_utils:replace_attribute(InputList, integer_array, 
                            ModifiedAttribute).
test_output_attribs1() ->
  [
    {exec_out, {false, []}},                         
    {status, {created, []}},     
    {exec_method, {empty, []}},
    {last_exec, {empty, []}},
    {value, {not_active, []}},
    {integer_array_out, [{0, []}, {1,[]}, {2, []}]}
  ].
 
  test_output_attribs2() ->
    OutputList = test_output_attribs1(),
    ModifiedAttribute = {integer_array_out, 
    [{0, []}, {1,[]}, {2, []}, {not_active, []}, {not_active, []}, {not_active, []}]},
    
    attrib_utils:replace_attribute(OutputList, integer_array_out, 
                            ModifiedAttribute).
 
 test_output_attribs3() ->
    OutputList = test_output_attribs1(),
    ModifiedAttribute = {integer_array_out, 
    [{6, []}, {7,[]}, {8, []}]},
    
    attrib_utils:replace_attribute(OutputList, integer_array_out, 
                            ModifiedAttribute).
                            
test_output_attribs4() ->
  [
    {exec_out, {not_active, []}},                         
    {status, {input_err, []}},     
    {exec_method, {not_active, []}},
    {last_exec, {not_active, []}},
    {value, {not_active, []}},
    {integer_array_out, [{not_active, []}, {not_active,[]}, {not_active, []}]}
  ].
  
% ====================================================================

% ====================================================================
% Test name()
%   
get_value_test() ->
  _TestInputs = test_input_attribs1().

% ====================================================================

% ====================================================================
% Test set_array_value()  
set_array_value_test() ->
  Outputs = test_output_attribs1(),
  ArrayValueName = integer_array_out,
  ArrayValues = [6,7,8],
  
  ExpectedResult = test_output_attribs3(),
  
  Result = set_array_value(Outputs, ArrayValueName, ArrayValues),
  ?assertEqual(ExpectedResult, Result). 
  
% ====================================================================

% ====================================================================
% Test update_all_outputs()  
update_all_outputs_test() ->
  Outputs = test_output_attribs1(),
  Value = not_active,
  Status = input_err,
  
  ExpectedResult = test_output_attribs4(),
  
  Result = update_all_outputs(Outputs, Value, Status),
  ?assertEqual(ExpectedResult, Result). 
  
% ====================================================================
  

% ====================================================================
% Test resize_attribute_array_value()  
%
%   Test input array attribute doesn't change size
resize_attribute_array_value_nochange_test() ->
  BlockName = test_output_utils,
  Outputs = test_output_attribs1(),
  ArrayValueName = integer_array_out,
  TargQuant = 3,
  DefaultValue = {not_active, []},
  
  ExpectedResult = test_output_attribs1(),
  
  Result = resize_attribute_array_value(BlockName, Outputs, 
                         ArrayValueName, TargQuant, DefaultValue),
  ?assertEqual(ExpectedResult, Result).
  
%   Test input array attribute increases in size
resize_attribute_array_value_increase_test() ->
  BlockName = test_output_utils,
  Outputs = test_output_attribs1(),
  ArrayValueName = integer_array_out,
  TargQuant = 6,
  DefaultValue = {not_active, []},
  
  ExpectedResult = test_output_attribs2(),
  
  Result = resize_attribute_array_value(BlockName, Outputs, 
                         ArrayValueName, TargQuant, DefaultValue),
                         
  ?assertEqual(ExpectedResult, Result).

% ====================================================================
% Test log_error()
%     
log_error_test() ->
  Config = test_config_attribs1(),
  
  ExpectedResult =  ok,
  
  Result = log_error(Config, value_name, bad_value),
  ?assertEqual(ExpectedResult, Result) .
% ====================================================================

-endif.