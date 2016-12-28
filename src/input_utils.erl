%%% @doc 
%%% Get and Validate Block Input values   
%%%               
%%% @end 

-module(input_utils).

-author("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
          get_any_type/2, 
          get_integer/2,
          get_integer_range/4, 
          get_float/2, 
          get_boolean/2,
          get_string/2,
          get_value/3, 
          check_boolean_input/1, 
          resize_attribute_array_value/5,
          log_error/3
]).


%%
%% Get input value of any type and check for errors.
%%
-spec get_any_type(Inputs :: list(input_attr()),
                   ValueId :: value_id()) -> generic_input_value().

get_any_type(Inputs, ValueId) ->
  % Return true for every value
  CheckType = fun(_Value) -> true end,
  get_value(Inputs, ValueId, CheckType).

%%
%% Get an integer input value and check for errors.
%%
-spec get_integer(Inputs :: list(input_attr()), 
                  ValueId :: value_id()) -> integer_input_value().

get_integer(Inputs, ValueId) ->
  CheckType = fun is_integer/1,
  get_value(Inputs, ValueId, CheckType).


%%
%% Get an integer input value and check for errors.
%%
-spec get_integer_range(Inputs :: list(input_attr()), 
                        ValueId :: value_id(),
                        Min :: integer(),
                        Max :: integer()) -> integer_input_value().

get_integer_range(Inputs, ValueId, Min, Max) ->
  CheckType = fun is_integer/1,
  case get_value(Inputs, ValueId, CheckType) of
    {error, Reason} ->  {error, Reason};

    {ok, not_active} -> {ok, not_active};
    
    {ok, Value} ->
      if (Value < Min ) orelse (Max < Value) ->
        {error, range};
      true -> 
        {ok, Value}
      end
  end.


%%
%% Get a floating point input value and check for errors.
%%
-spec get_float(Inputs :: list(input_attr()), 
                ValueId :: value_id()) -> float_input_value().

get_float(Inputs, ValueId) ->
  CheckType = fun is_float/1,
  get_value(Inputs, ValueId, CheckType).
  
  
%%
%% Get a boolean input value and check for errors
%%
-spec get_boolean(Inputs :: list(input_attr()), 
                  ValueId :: value_id()) -> boolean_input_value().

get_boolean(Inputs, ValueId) ->
  CheckType = fun is_boolean/1,
  get_value(Inputs, ValueId, CheckType).


%%
%% Get a string input value and check for errors
%%
-spec get_string(Inputs :: list(input_attr()), 
                 ValueId :: value_id()) -> string_input_value().

get_string(Inputs, ValueId) ->
  CheckType = fun block_utils:is_string/1,
  get_value(Inputs, ValueId, CheckType).


%%
%% Generic get input value, check for errors.
%%
-spec get_value(Inputs :: list(input_attr()),
                ValueId :: value_id(),
                CheckType :: fun()) -> term().
                
get_value(Inputs, ValueId, CheckType) ->
  % get the whole attribute, not just the value
  case attrib_utils:get_attribute(Inputs, ValueId) of
    {error, not_found}  -> {error, not_found};
    
    % If this is a non-array value, check it
    {ok, {_ValueName, {Value, Link}}} ->
      check_value(Value, Link, CheckType);

    % if this is an array value, check the index, and get the nth element
    {ok, {ValueName, ArrayValue}} ->
      % if this is an array value, the ValueName from get_attribute()
      % will match ValueName in the ValueId tuple
      case ValueId of
        {ValueName, ArrayIndex} ->
          if (0 < ArrayIndex) andalso (ArrayIndex =< length(ArrayValue)) ->
            {Value, Link} = lists:nth(ArrayIndex, ArrayValue),
            check_value(Value, Link, CheckType);

          true->  % array index is out of bounds
            {error, invalid_index}
          end;
        _ -> {error, invalid_value}
      end;
       
    % Attribute value was not an input value  
    _ -> {error, not_input}    
  end.

%% Check the value and link for both non-array and array values
check_value(Value, Link, CheckType) ->
  case Value of
    % not_active is a valid value
    not_active -> {ok, not_active};
        
    empty ->   
      case Link of
        % if the input value is empty and the link is empty
        % treat this like a not_active value
        ?EMPTY_LINK -> {ok, not_active};

        % input is linked to another block but value is empty,
        % this is an error
         _ -> {error, bad_link}
      end;

    Value ->
      case CheckType(Value) of
        true  -> {ok, Value};
        false -> {error, bad_type}
      end
  end.


%
% Check the value of the disable or freeze control value input
%
check_boolean_input(Value) ->
  case Value of
    true       -> active;
    false      -> not_active; 
    not_active -> not_active;
    empty      -> not_active;
    _Error     -> error
  end.


%%
%% Resize an array value in the Inputs attribute list
%% to match the target quantity
%% Returns updated Inputs attribute list
%%
-spec resize_attribute_array_value(BlockName :: block_name(),
                                   Inputs :: list(input_attr()),
                                   ArrayValueName :: value_name(),
                                   TargQuant :: pos_integer(),
                                   DefaultValue :: input_value()) -> list(input_attr()).
                             
resize_attribute_array_value(BlockName, Inputs, ArrayValueName, TargQuant, DefaultValue)->
  % Function to unlink the deleted array values if they are linked to an output value
  DeleteExcess = fun (DeleteArrayValues) ->
      lists:map(
        fun(DeleteValue) -> 
          {_Value, Link} = DeleteValue,
          link_utils:unlink_input(BlockName, ArrayValueName, Link)
          end, 
          DeleteArrayValues) end,
          
  attrib_utils:resize_attribute_array_value(Inputs, ArrayValueName, TargQuant, 
                                              DefaultValue, DeleteExcess).

  
%%
%% Log input value error
%%
-spec log_error(Config :: list(config_attr()),
                ValueName :: value_name(),
                Reason :: atom()) -> {not_active, input_err}.
                  
log_error(Config, ValueName, Reason) ->
  BlockName = config_utils:name(Config),
  error_logger:error_msg("~p Invalid '~p' input value: ~p~n", 
                            [BlockName, ValueName, Reason]),
  {not_active, input_err}.
  
  
%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


% ====================================================================
% Test TODO:
%   
get_value_test() ->
  _TestInputs = test_data:input_utils_input_attribs1().

% ====================================================================


% ====================================================================
% Test resize_attribute_array_value()  
%
%   Test input array attribute doesn't change size
resize_attribute_array_value_nochange_test() ->
  BlockName = test_input_utils,
  Inputs = test_data:input_utils_input_attribs1(),
  ArrayValueName = integer_array,
  TargQuant = 2,
  DefaultValue = {empty, ?EMPTY_LINK},
  
  ExpectedResult = test_data:input_utils_input_attribs1(),
  
  Result = resize_attribute_array_value(BlockName, Inputs, 
                         ArrayValueName, TargQuant, DefaultValue),
  ?assertEqual(ExpectedResult, Result).
  
%   Test input array attribute increases in size
resize_attribute_array_value_increase_test() ->
  BlockName = test_input_utils,
  Inputs = test_data:input_utils_input_attribs1(),
  ArrayValueName = integer_array,
  TargQuant = 4,
  DefaultValue = {empty, ?EMPTY_LINK},
  
  ExpectedResult = test_data:input_utils_input_attribs2(),
  
  Result = resize_attribute_array_value(BlockName, Inputs, 
                         ArrayValueName, TargQuant, DefaultValue),
  ?assertEqual(ExpectedResult, Result).

% ====================================================================
% Test log_error()
%     
log_error_test() ->
  Config = test_data:input_utils_config_attribs1(),
  
  ExpectedResult =  {not_active, input_err},
  
  Result = log_error(Config, value_name, bad_value),
  ?assertEqual(ExpectedResult, Result) .
% ====================================================================

-endif.