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
-export([
          set_value_status/3, 
          set_value_normal/2, 
          set_status/2,
          get_status/1,
          set_array_value/3,
          set_tristate_outputs/4,
          update_all_outputs/3,
          % clear_output_refs/1,
          resize_attribute_array_value/4
]).


%%
%% Set block output value and status
%% Block output value and status attributes are often set at the same time.
%% This is a shortcut to do that.
%% 
-spec set_value_status(Outputs :: output_attribs(), 
                       Value :: value(), 
                       Status :: block_status()) -> output_attribs().

set_value_status(Outputs, Value, Status) ->
  {ok, Outputs1} = attrib_utils:set_values(Outputs, 
                       [{value, Value}, {status, Status}]),
  Outputs1.
  
%%
%% Set block output value and set status to normal
%% When setting the output value block status is usually normal.
%% This is a shortcut to do that.
%% 
-spec set_value_normal(Outputs :: output_attribs(), 
                       Value :: value()) -> output_attribs().

set_value_normal(Outputs, Value) ->
  {ok, Outputs1} = attrib_utils:set_values(Outputs, 
                                 [{value, Value}, {status, normal}]),
  Outputs1.

%%
%% Set status output value
%% 
-spec set_status(Outputs :: output_attribs(), 
                 Status :: block_status()) -> output_attribs().

set_status(Outputs, Status) ->
  {ok, Outputs1} = attrib_utils:set_value(Outputs, status, Status),
  Outputs1.


%%
%% Get status output value
%% 
-spec get_status(Outputs :: output_attribs()) -> block_status().

get_status(Outputs) ->
  case attrib_utils:get_value(Outputs, status) of
    {ok, Status} -> Status;
    {error, _Reason} -> error
  end.
 

%%
%% Set an array of output values to ArrayValues
%% Number of values in ArrayValues, must match the number of array values
%% in the ArrayValueName output value
%% 
-spec set_array_value(Outputs :: output_attribs(), 
                      ArrayValueName :: value_name(),
                      ArrayValues :: attrib_value_array()) -> output_attribs().

set_array_value(Outputs, ArrayValueName, ArrayValues) ->
  set_array_value(Outputs, ArrayValueName, 1, ArrayValues).
  
set_array_value(Outputs, _ArrayValueName, _Index, []) ->
  Outputs;

set_array_value(Outputs, ArrayValueName, Index, [Value | ArrayValues]) ->
  {ok, NewOutputs} = attrib_utils:set_value(Outputs, {ArrayValueName, Index}, Value),
  set_array_value(NewOutputs, ArrayValueName, (Index + 1), ArrayValues).


%%
%% Update a set of tristate ouputs, value, active_true, and active_false
%%
-spec set_tristate_outputs(InputValId :: value_id(),  % Only needed for error logging
                           OutputVal :: {ok, boolean()} | {error, atom()},
                           Config :: config_attribs(),  % Only Needed for error logging
                           Outputs :: output_attribs()) -> {ok, list(output_attr)}.

set_tristate_outputs(InputValId, OutputVal, Config, Outputs) ->
  case OutputVal of
    {ok, null} ->
      Status = no_input,
      Value = null,
      ActiveTrue = null,
      ActiveFalse = null;
   
    {ok, true} -> 
      Status = normal,
      Value = true,
      ActiveTrue = true,
      ActiveFalse = null;
  
    {ok, false} -> 
      Status = normal,
      Value = false,
      ActiveTrue = null,
      ActiveFalse = false;
    
    {error, Reason} ->
      {Value, Status} = input_utils:log_error(Config, InputValId, Reason),
      ActiveTrue = null,
      ActiveFalse = null
  end,
  
  % Update the output status and values
  {ok, Outputs1} = attrib_utils:set_values(Outputs, [{status, Status}, {value, Value}, 
                                           {active_true, ActiveTrue}, {active_false, ActiveFalse}]),
  Outputs1.


%% 
%% Update all outputs to the New value,
%% except update status output to the New Staus value
%% Used to mass update block outputs in disabled or error conditions
%% 
-spec update_all_outputs(Outputs :: output_attribs(), 
                         NewValue :: value(), 
                         NewStatus :: block_status()) -> output_attribs().

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
-spec update_all_array_values(ArrayValues :: attrib_value_array(),
                              NewValue :: value()) -> attrib_value_array().
                                
update_all_array_values(ArrayValues, NewValue) ->
  lists:map(fun({_Value, Refs}) -> {NewValue, Refs} end, ArrayValues).


%% 
%% Clear references to other blocks in the Output values
%% Used to mass update block outputs in disabled or error conditions
%% 
% -spec clear_output_refs(Outputs :: output_attribs()) -> output_attribs().

% clear_output_refs(Outputs) ->
%   lists:map(
%     fun(Output) ->
%       case Output of 
%         {ValueName, {Value, _Refs}} ->
%            {ValueName, {Value, []}};

%         {ValueName, ArrayValues} ->
%           {ValueName, clear_array_output_refs(ArrayValues)}      
%       end  
%     end,
%     Outputs).

 
%%
%% Clear references to other blocks, in output arrray values
%%
% -spec clear_array_output_refs(ArrayValues :: list(atrib_value_array())) -> 
%                                       list(atrib_value_array()).
                                
% clear_array_output_refs(ArrayValues) ->
%   lists:map(fun({Value, _Refs}) -> {Value, []} end, ArrayValues).  


%%
%% Resize an array value in the Outputs attribute list
%% to match the target quantity
%% Returns updated Outputs attribute list
%%
-spec resize_attribute_array_value(Outputs :: output_attribs(),
                                   ArrayValueName :: value_name(),
                                   TargQuant :: pos_integer(),
                                   DefaultValue :: output_value()) -> output_attribs().
                             
resize_attribute_array_value(Outputs, ArrayValueName, TargQuant, DefaultValue)->
   % Function to remove the delete array values' Links to input block values
   DeleteExcess = fun(DeleteArrayValues, StartIndex) -> 
      lists:foldl(fun(DeleteValue, Index) -> 
          {_Value, Links} = DeleteValue,
          block_common:update_linked_inputs(empty, Links),
          Index + 1
          end, 
          StartIndex,
          DeleteArrayValues) end,
  
  attrib_utils:resize_attribute_array_value(Outputs, ArrayValueName, TargQuant, 
                                            DefaultValue, DeleteExcess).
  
  
%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


% ====================================================================
% Test name()
%   
get_value_test() ->
  _TestInputs = test_data:output_utils_input_attribs1().

% ====================================================================

% ====================================================================
% Test set_array_value()  
set_array_value_test() ->
  Outputs = test_data:output_attribs1(),
  ArrayValueName = integer_array_out,
  ArrayValues = [6,7,8],
  
  ExpectedResult = test_data:output_utils_output_attribs3(),
  
  Result = set_array_value(Outputs, ArrayValueName, ArrayValues),
  ?assertEqual(ExpectedResult, Result). 
  
% ====================================================================

% ====================================================================
% Test update_all_outputs()  
update_all_outputs_test() ->
  Outputs = test_data:output_attribs1(),
  Value = null,
  Status = input_err,
  
  ExpectedResult = test_data:output_utils_output_attribs4(),
  
  Result = update_all_outputs(Outputs, Value, Status),
  ?assertEqual(ExpectedResult, Result). 
  
% ====================================================================
  

% ====================================================================
% Test resize_attribute_array_value()  
%
%   Test input array attribute doesn't change size
resize_attribute_array_value_nochange_test() ->
  Outputs = test_data:output_attribs1(),
  ArrayValueName = integer_array_out,
  TargQuant = 3,
  DefaultValue = {null, []},
  
  ExpectedResult = test_data:output_attribs1(),
  
  Result = resize_attribute_array_value(Outputs, 
                         ArrayValueName, TargQuant, DefaultValue),
  ?assertEqual(ExpectedResult, Result).
  
%   Test input array attribute increases in size
resize_attribute_array_value_increase_test() ->
  Outputs = test_data:output_attribs1(),
  ArrayValueName = integer_array_out,
  TargQuant = 6,
  DefaultValue = {null, []},
  
  ExpectedResult = test_data:output_utils_output_attribs2(),
  
  Result = resize_attribute_array_value(Outputs, 
                         ArrayValueName, TargQuant, DefaultValue),
                         
  ?assertEqual(ExpectedResult, Result).

% ====================================================================

-endif.