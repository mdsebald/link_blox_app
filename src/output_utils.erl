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
          update_all_outputs/3,
          clear_output_refs/1,
          resize_attribute_array_value/5
]).


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
%% Get status output value
%% 
-spec get_status(Outputs :: list(output_attr())) -> block_status().

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
-spec set_array_value(Outputs :: list(output_attr()), 
                      ArrayValueName :: value_name(),
                      ArrayValues :: list(attr_value_array())) -> list(output_attr()).

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
-spec update_all_array_values(ArrayValues :: list(attr_value_array()),
                              NewValue :: value()) -> list(attr_value_array()).
                                
update_all_array_values(ArrayValues, NewValue) ->
  lists:map(fun({_Value, Refs}) -> {NewValue, Refs} end, ArrayValues).


%% 
%% Clear references to other blocks in the Output values
%% Used to mass update block outputs in disabled or error conditions
%% 
-spec clear_output_refs(Outputs :: list(output_attr())) -> list(output_attr()).

clear_output_refs(Outputs) ->
  lists:map(
    fun(Output) ->
      case Output of 
        {ValueName, {Value, _Refs}} ->
           {ValueName, {Value, []}};

        {ValueName, ArrayValues} ->
          {ValueName, clear_array_output_refs(ArrayValues)}      
      end  
    end,
    Outputs).

 
%%
%% Clear references to other blocks, in output arrray values
%%
-spec clear_array_output_refs(ArrayValues :: list(attr_value_array())) -> 
                                      list(attr_value_array()).
                                
clear_array_output_refs(ArrayValues) ->
  lists:map(fun({Value, _Refs}) -> {Value, []} end, ArrayValues).  


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
          {_Value, Refs} = DeleteValue,
          link_utils:unlink_output(BlockName, ArrayValueName, Refs)
          end, 
          DeleteArrayValues) end,
  
  attrib_utils:resize_attribute_array_value(Outputs, ArrayValueName, TargQuant, 
                                            DefaultValue, DeleteExcess).


% TODO: Delete, not used
%%
%% Log output value error
%%
%-spec log_error(Config :: list(config_attr()),
%                ValueId :: value_id(),
%                Reason :: atom()) -> ok.
                  
%log_error(Config, ValueId, Reason) ->
%  BlockName = config_utils:name(Config),
%  ValueIdStr = attrib_utils:value_id_to_str(ValueId),
%  log_server:error(err_invalid_output_value, [BlockName, ValueIdStr, Reason]),
%  ok.
  
  
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
  Value = not_active,
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
  BlockName = test_output_utils,
  Outputs = test_data:output_attribs1(),
  ArrayValueName = integer_array_out,
  TargQuant = 3,
  DefaultValue = {not_active, []},
  
  ExpectedResult = test_data:output_attribs1(),
  
  Result = resize_attribute_array_value(BlockName, Outputs, 
                         ArrayValueName, TargQuant, DefaultValue),
  ?assertEqual(ExpectedResult, Result).
  
%   Test input array attribute increases in size
resize_attribute_array_value_increase_test() ->
  BlockName = testt_utils_output_utils,
  Outputs = test_data:output_attribs1(),
  ArrayValueName = integer_array_out,
  TargQuant = 6,
  DefaultValue = {not_active, []},
  
  ExpectedResult = test_data:output_utils_output_attribs2(),
  
  Result = resize_attribute_array_value(BlockName, Outputs, 
                         ArrayValueName, TargQuant, DefaultValue),
                         
  ?assertEqual(ExpectedResult, Result).

% TODO: Delete not used
% ====================================================================
% Test log_error()
%     
%log_error_test() ->
%  Config = test_data:output_utils_config_attribs1(),
  
%  ExpectedResult =  ok,
  
%  Result = log_error(Config, value_name, bad_value),
%  ?assertEqual(ExpectedResult, Result) .
% ====================================================================

-endif.