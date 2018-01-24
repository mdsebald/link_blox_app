%%% @doc 
%%% Common Attribute value utility functions     
%%%               
%%% @end 

-module(attrib_utils).

-author("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
          get_attribute/2,
          get_value/2, 
          get_value_any/2,
          set_value/3, 
          set_values/2,
          is_attribute/2,
          is_attribute_type/2,
          update_attribute_list/2, 
          merge_attribute_lists/2,
          replace_attribute/3, 
          add_attribute/2,
          resize_attribute_array_value/4, 
          resize_attribute_array_value/5,
          replace_array_value/3,
          str_to_value_id/1,
          value_id_to_str/1,
          log_error/3,
          log_error/4
]).


%%
%% Get the attribute for the given ValueName in the list of Attributes
%% List of attributes may be Config, Inputs, Outputs, or Private type
%%
-spec get_attribute(Attributes :: attribs(), 
                    ValueId :: value_id()) -> {ok, attrib()} | {error, not_found}.

get_attribute(Attributes, ValueId) ->
  % Get the ValueName from the ValueId
  case ValueId of
    {ValueName, _ArrayIndex} -> ok;
    ValueName -> ok
  end,
  
  % ValueName is always the first element in the tuple, regardless of the attribute type
  case lists:keyfind(ValueName, 1, Attributes) of 
    false     -> {error, not_found};
    Attribute -> {ok, Attribute}
  end.


%%
%% Get the value of the ValueId attribute in the list of Attributes
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec get_value(Attributes :: attribs(), 
                ValueId :: value_id()) -> attrib_result_value().
 
get_value(Attributes, ValueId) ->
  case get_attribute(Attributes, ValueId) of
    {error, not_found} -> {error, not_found};
    
    % Array value attribute
    {ok, {ValueName, ArrayValue}} when is_list(ArrayValue) ->
      case ValueId of
        % For an array values, ValueId is composed of ValueName and ArrayIndex
        {ValueName, ArrayIndex} ->
          if (0 < ArrayIndex) andalso (ArrayIndex =< length(ArrayValue)) ->
            case lists:nth(ArrayIndex, ArrayValue) of
              % Config or Private array value
              {Value} -> {ok, Value};
              % Input or Output array value
              {Value, _DefValOrLinks} -> {ok, Value}
            end;
          true -> {error, invalid_index}
          end;
        _InvalidValue -> {error, invalid_value}
      end;

    % For non-array values, the ValueId is the attribute ValueName
    % Config value
    {ok, {ValueId, {Value}}} -> {ok, Value};
    
    % Input or Output value
    {ok, {ValueId, {Value, _DefValueOrLinks}}} -> {ok, Value};

    % Otherwise an invalid value
    _InvalidValue -> {error, invalid_value}
  end.


%%
%% Get the value of the attribute ValueName
%% Check attribute types: Config, Inputs, and Outputs
%%
%% This is used by processes external to the block
%% so don't allow getting Private values 
%%
-spec get_value_any(BlockState :: block_state(), 
                    ValueId :: value_id()) -> attrib_result_value().

% Get array value
get_value_any(BlockState, ValueId) ->

  {Config, Inputs, Outputs, _Private} = BlockState,

  case get_value(Config, ValueId) of
    {error, not_found} ->

      case get_value(Inputs, ValueId) of
        {error, not_found} ->

          case get_value(Outputs, ValueId) of
            {error, Reason} -> {error, Reason};
            % Output value
            {ok, Value} -> {ok, Value}
          end;

        % Input value get failed for reason other than not found  
        {error, Reason} -> {error, Reason};  
        % Input value
        {ok, Value} -> {ok, Value}
      end;
    
    % Config value get failed for reason other than not found  
    {error, Reason} -> {error, Reason};  
    % Config value  
    {ok, Value} -> {ok, Value}  
  end.


%%
%% Set a value in attribute ValueId
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec set_value(Attributes :: attribs(), 
                ValueId :: value_id(), 
                NewValue :: value()) -> {ok, attribs()} | attrib_errors().
              
set_value(Attributes, ValueId, NewValue)->
  case get_attribute(Attributes, ValueId) of
    {error, not_found} -> {error, not_found};
    
    % Array value
    {ok, {ValueName, ArrayValue}} when is_list(ArrayValue) ->
      case ValueId of 
        % if this is an array value, the ValueName from get_attribute()
        % will match ValueName in the ValueId tuple
        {ValueName, ArrayIndex} ->
          if (0 < ArrayIndex) andalso (ArrayIndex =< length(ArrayValue)) ->
            case lists:nth(ArrayIndex, ArrayValue) of
              % Config or Private value
              {_OldValue} ->
                NewArrayValue = 
                  replace_array_value(ArrayValue, ArrayIndex, {NewValue}),
                {ok, replace_attribute(Attributes, ValueName, 
                                                      {ValueName,NewArrayValue})};
            
              % Input or Output value
              {_OldValue, DefValOrLinks} ->  
                NewArrayValue = 
                  replace_array_value(ArrayValue, ArrayIndex, {NewValue, DefValOrLinks}),
                {ok, replace_attribute(Attributes, ValueName, {ValueName,NewArrayValue})}
            end;
          true ->
            {error, invalid_index}
          end;
        _InvalidValue -> {error, invalid_value}
      end;

    % For non-array values, the ValueId is the attribute value name
    % Non-array Config or Private value
    {ok, {ValueId, {_OldValue}}} -> 
      NewAttribute = {ValueId, {NewValue}},
      {ok, replace_attribute(Attributes, ValueId, NewAttribute)};
    
    % Non-array Input or Output value
    {ok, {ValueId, {_OldValue, DefValOrLinks}}} ->  
      NewAttribute = {ValueId, {NewValue, DefValOrLinks}},
      {ok, replace_attribute(Attributes, ValueId, NewAttribute)}
  end.


%%
%% is ValueID an attribute of this block
%% Don't check the private attributes list
%%
-spec is_attribute(BlockState :: block_state(),
                   ValueId :: value_id()) -> boolean().

is_attribute(BlockState, ValueId) ->
  {Config, Inputs, Outputs, _Private} = BlockState,
  case is_attribute_type(Config, ValueId) of
    true -> true;

    false ->
      case is_attribute_type(Inputs, ValueId) of
        true -> true;

        false ->
          case is_attribute_type(Outputs, ValueId) of
            true -> true;

            false -> false
          end
      end
  end.


%%
%% Is ValueID an attribute in this list of attributes
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec is_attribute_type(Attribs :: attribs(),
                        ValueId :: value_id()) -> boolean().

is_attribute_type(Attribs, ValueId) ->
  case get_value(Attribs, ValueId) of
    {ok, _Value} -> true;
               _ -> false
  end. 


%% replace a value in the array of values
-spec replace_array_value(ArrayValues :: attrib_value_array(), 
                          ArrayIndex :: pos_integer(),
                          NewValue :: term()) -> attrib_value_array().
                         
replace_array_value(ArrayValues, ArrayIndex, NewValue) ->
  lists:sublist(ArrayValues, ArrayIndex-1) 
                  ++ [NewValue] 
                  ++ lists:nthtail(ArrayIndex, ArrayValues).


%%
%% Set multiple values in the attribute list 
%% Values are in the form of ValueID, Value tuples
%% All values must belong to the same attribute list
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec set_values(Attributes :: attribs(), 
                 Values :: block_values()) -> {ok, attribs()} | attrib_errors().
 
set_values(Attributes, []) ->{ok, Attributes};

set_values(Attributes, [{ValueId, NewValue} | RemainingValues]) ->
  case set_value(Attributes, ValueId, NewValue) of
    % Return immediately on error
    {error, Reason}     -> {error, Reason};
    {ok, NewAttributes} -> set_values(NewAttributes, RemainingValues)
  end.


%%
%% Update attributes in the Attribute List with the New Attributes list 
%% Add any new attributes if they are not already in the Attribute list
%% Both lists of Attributes must be the same type
%% Attributes may be Config, Inputs, Outputs, or Private type
%%
-spec merge_attribute_lists(Attributes :: attribs(), 
                            NewAttributes :: attribs()) -> attribs().

merge_attribute_lists(Attributes, []) -> Attributes;

merge_attribute_lists(Attributes, [NewAttribute | NewAttributes]) ->
  UpdatedAttributes = update_attribute_list(Attributes, NewAttribute),
  merge_attribute_lists(UpdatedAttributes, NewAttributes).


%%
%% Update the Attribute list with a new attribute
%% Attribute list may be Config, Inputs, Outputs, or Private type
%%
-spec update_attribute_list(Attributes :: attribs(), 
                            NewAttribute :: tuple()) -> attribs().

update_attribute_list(Attributes, NewAttribute) ->
  % First element of any attribute value tuple is always the name 
  AttributeName = element(1, NewAttribute),

  case get_attribute(Attributes, AttributeName) of
    {error, not_found} -> add_attribute(Attributes, NewAttribute);
    {ok, _Value}-> replace_attribute(Attributes, AttributeName, NewAttribute)
  end.


%%
%% Replace the AttributeName  attribute in the Attribute list with the New Attribute
%% Return the updated Attribute List
%% Attribute list may be Config, Inputs, Outputs, or Private type
%%
-spec replace_attribute(Attributes :: attribs(), 
                        ValueName :: value_name(), 
                        NewAttribute :: attrib()) -> attribs().

replace_attribute(Attributes, ValueName, NewAttribute) ->
  % ValueName is always the first element in the tuple, regardless of the attributeValue type
  lists:keyreplace(ValueName, 1, Attributes, NewAttribute).


%%
%% Add a new attribute to the end of the Attribute list 
%% Attribute list may be Config, Inputs, Outputs, or Private type
%%
-spec add_attribute(Attributes :: attribs(), 
                    NewAttribute :: attrib()) -> attribs().

add_attribute(Attributes, Newattribute) ->
  Attributes ++ [Newattribute].
  

%%
%% Resize an array value in an attribute list
%% to match the target quantity, which is always > zero
%% Returns updated attribute list
%% Works for Config, Input, Output, and Private array attributes
%%

%% Specialized resize function for the case when the excess values don't need 
%% any extra processing. Just pass in a DeleteExcess function that does nothing. 
-spec resize_attribute_array_value(Attributes :: attribs(),
                                   ArrayValueName :: value_name(),
                                   TargQuant :: pos_integer(),
                                   DefaultValue :: attrib_value()) -> attribs().
                             
resize_attribute_array_value(Attributes, ArrayValueName, TargQuant, DefaultValue)->
  DeleteExcess = fun(_DeleteArrayValues, _TargetQuant) -> ok end, 
  resize_attribute_array_value(Attributes, ArrayValueName, TargQuant, DefaultValue, DeleteExcess).


-spec resize_attribute_array_value(Attributes :: attribs(),
                                   ArrayValueName :: value_name(),
                                   TargQuant :: pos_integer(),
                                   DefaultValue :: attrib_value(),
                                   DeleteExcess :: fun()) -> attribs().
                             
resize_attribute_array_value(Attributes, ArrayValueName, TargQuant, DefaultValue, DeleteExcess)->
  case get_attribute(Attributes, ArrayValueName) of
    % If attribute not found, just return attribute list unchanged
    {error, not_found} -> Attributes;
      
    {ok, {ArrayValueName, ArrayValues}} ->  
        {NewArrayValues, DeleteArrayValues} = 
          resize_array_value(ArrayValues, TargQuant, DefaultValue),
      
      % Handle the deletion of the excess array values
      % i.e. If inputs, need to delete links to the deleted array value inputs
      % Starting index of the array values to delete, is target array value quantity plus one
      DeleteExcess(DeleteArrayValues, (TargQuant + 1)),
      % Update the attribute list with the resized array value
      replace_attribute(Attributes, ArrayValueName, {ArrayValueName, NewArrayValues})
  end.


%%
%% Resize the array of values to match the target quantity.
%% Always add to or delete from the end of the array.
%% There will always be at least one value in the array. 
%%  
-spec resize_array_value(ValuesArray :: attrib_value_array(),
                         TargQuant :: pos_integer(),
                         DefaultValue :: attrib_value()) -> {attrib_value_array(), attrib_value_array()}.
                                     
resize_array_value(ValuesArray, TargQuant, DefaultValue)->
  ValuesQuant = length(ValuesArray),
  if ValuesQuant == TargQuant ->
    % Quantity of Values in array matches target, nothing to do 
    % No values to delete
    {ValuesArray, []};
  true ->
    if ValuesQuant < TargQuant ->
      % Not enough values, add the required number of default values to match the target quantity
      % No values to delete
      AddedValues = lists:duplicate((TargQuant-ValuesQuant), DefaultValue),
      {ValuesArray ++ AddedValues, []};
    true ->
      %  Too many values, split the array of values into an array to keep and an array to delete 
      %  The deleted values are always taken from the end of the list
      lists:split(TargQuant, ValuesArray)
    end
  end.      


%%
%% Convert a string to a value id. 
%% Need to handle the case of a value name string and
%% the case of a value name string plus an array index
%% Examples:
%%   "value"  -> value (atom type)
%%   "digit[1]"  -> {digit, 1} (atom and array index tuple)
%%
-spec str_to_value_id(ValueIdStr :: string()) -> {ok, value_id()} | {error, invalid}.

str_to_value_id(ValueIdStr) ->
  LeftBracket = string:chr(ValueIdStr, $[),
  if LeftBracket > 0 ->
    RightBracket = string:rchr(ValueIdStr, $]),
    if RightBracket > (LeftBracket + 1) ->
      ArrayIndexStr = string:substr(ValueIdStr, LeftBracket + 1, 
                                     RightBracket - LeftBracket - 1),
      case string:to_integer(ArrayIndexStr) of
        {error, _} -> 
          {error, invalid};

        {ArrayIndex, Rest} ->
          if length(Rest) == 0 ->
            if ArrayIndex > 0 -> 
              ValueNameStr = string:substr(ValueIdStr, 1, LeftBracket-1),
              ValueName = ui_utils:get_attrib_id(ValueNameStr),
              {ok, {ValueName, ArrayIndex}};
            true -> % ArrayIndex =< 0
              {error, invalid}
            end;
          true -> % There are extra characters after the digit(s)
            {error, invalid}
          end
      end;

    true -> % left bracket found, without a corresponding right bracket
      {error, invalid}
    end;

  true ->  % No left bracket found, assume a non array value id
    {ok, ui_utils:get_attrib_id(ValueIdStr)}
  end.


%%
%% Convert a value ID into a string
%%    num_of_inputs => "num_of_inputs"
%%    {inputs, 1} => "inputs[1]"
%%
-spec value_id_to_str(ValueId :: value_id()) -> string().

value_id_to_str(ValueId) ->
  case ValueId of
    {ValueName, Index} ->
      ValueNameStr = ui_utils:get_attrib_string(ValueName),
      lists:flatten(io_lib:format("~s[~b]", [ValueNameStr, Index]));
  
    ValueId -> ui_utils:get_attrib_string(ValueId)
  end.


%%
%% Log value error
%%
-spec log_error(Config :: config_attribs(),
                ValueId :: value_id(),
                Reason :: atom()) -> ok.
                  
log_error(Config, ValueId, Reason) ->
  BlockName = config_utils:name(Config),
  ValueIdStr = value_id_to_str(ValueId),
  logger:error(err_invalid_reason, [BlockName, ValueIdStr, Reason]).


-spec log_error(Config :: config_attribs(),
                ValueId :: value_id(),
                Reason :: atom(),
                Value :: term()) -> ok.
                  
log_error(Config, ValueId, Reason, Value) ->
  BlockName = config_utils:name(Config),
  ValueIdStr = value_id_to_str(ValueId),
  logger:error(err_invalid_reason_value, [BlockName, ValueIdStr, Reason, Value]).


%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% ====================================================================
% Test get_attribute()
% 
%   Test get_attribute() found
get_attribute_found_test() ->
  Attributes = test_data:attrib_utils_config_attribs1(),
  ValueName = number1,
  ExpectedResult = {ok, {number1, {123.45}}},
  
  Result = get_attribute(Attributes, ValueName),
  ?assertEqual(ExpectedResult, Result).

%   Test get_attribute() not_found
get_attribute_not_found_test() ->
  Attributes = test_data:attrib_utils_config_attribs1(),
  ValueName = unexpected,
  ExpectedResult = {error, not_found},
  
  Result = get_attribute(Attributes, ValueName),
  ?assertEqual(ExpectedResult, Result).

% ====================================================================
% Test get_value()
% 
%   Test get_value() input ok
get_value_input_ok_test() ->
  Attributes = test_data:attrib_utils_input_attribs1(),
  ValueName = number_in,
  ExpectedResult = {ok, 123.45},
  
  Result = get_value(Attributes, ValueName),
  ?assertEqual(ExpectedResult, Result).

%   Test get_value() input not_found
get_value_input_not_found_test() ->
  Attributes = test_data:attrib_utils_input_attribs1(),
  ValueName = unexpected,
  ExpectedResult = {error, not_found},
  
  Result = get_value(Attributes, ValueName),
  ?assertEqual(ExpectedResult, Result).

%   Test get_value() input array value ok
get_value_input_array_ok_test() ->
  Attributes = test_data:attrib_utils_input_attribs1(),
  ValueName = integer_array_in,
  ArrayIndex = 2,
  ExpectedResult = {ok, 456},
  
  Result = get_value(Attributes, {ValueName, ArrayIndex}),
  ?assertEqual(ExpectedResult, Result).

%   Test get_value() input array value negative index
get_value_input_array_invalid_index_test() ->
  Attributes = test_data:attrib_utils_input_attribs1(),
  ValueName = bool_array_in,
  ArrayIndex = -23,
  ExpectedResult = {error, invalid_index},
  
  Result = get_value(Attributes, {ValueName, ArrayIndex}),
  ?assertEqual(ExpectedResult, Result).
  
%   Test get_value() config array value 0 index
get_value_config_array_0_index_test() ->
  Attributes = test_data:attrib_utils_config_attribs1(),
  ValueName = integer_array,
  ArrayIndex = 0,
  ExpectedResult = {error, invalid_index},
  
  Result = get_value(Attributes, {ValueName, ArrayIndex}),
  ?assertEqual(ExpectedResult, Result).
  
%   Test get_value() config array value index too large
get_value_config_array_negative_index_test() ->
  Attributes = test_data:attrib_utils_config_attribs1(),
  ValueName = bool_array,
  ArrayIndex = 99,
  ExpectedResult = {error, invalid_index},
  
  Result = get_value(Attributes, {ValueName, ArrayIndex}),
  ?assertEqual(ExpectedResult, Result).


% ====================================================================
% Test is_attribute(), 
% 
% Test config good
is_attribute_config_test() ->
  BlockState = {test_data:attrib_utils_config_attribs1(),
                 test_data:input_utils_input_attribs1(),
                 test_data:output_attribs1(),
                 []}, % No private data

  Result = is_attribute(BlockState, {integer_array, 2}),
  ExpectedResult = true,
  ?assertEqual(ExpectedResult, Result).

% Test input good
is_attribute_input_test() ->
  BlockState = {test_data:attrib_utils_config_attribs1(),
                 test_data:attrib_utils_input_attribs1(),
                 test_data:output_attribs1(),
                 []}, % No private data

  Result = is_attribute(BlockState, {integer_array_in, 2}),
  ExpectedResult = true,
  ?assertEqual(ExpectedResult, Result).

% Test output good
is_attribute_output_test() ->
  BlockState = {test_data:attrib_utils_config_attribs1(),
                 test_data:attrib_utils_input_attribs1(),
                 test_data:output_attribs1(),
                 []}, % No private data

  Result = is_attribute(BlockState, value),
  ExpectedResult = true,
  ?assertEqual(ExpectedResult, Result).

% Test bad value id
is_attribute_bad_value_test() ->
  BlockState = {test_data:attrib_utils_config_attribs1(),
                 test_data:attrib_utils_input_attribs1(),
                 test_data:output_attribs1(),
                 []}, % No private data

  Result = is_attribute(BlockState, does_not_exist),
  ExpectedResult = false,
  ?assertEqual(ExpectedResult, Result).

% Test bad array value id
is_attribute_bad_array_test() ->
  BlockState = {test_data:attrib_utils_config_attribs1(),
                 test_data:attrib_utils_input_attribs1(),
                 test_data:output_attribs1(),
                 []}, % No private data

  Result = is_attribute(BlockState, {integer_array_out, 4}),
  ExpectedResult = false,
  ?assertEqual(ExpectedResult, Result).


% ====================================================================


% ====================================================================
% Test resize_attribute_array_value(), No special delete handling
% 
%   Test config array attribute doesn't change 
resize_attribute_array_value_config_same_test() ->
  Attributes = test_data:attrib_utils_config_attribs1(),
  ArrayValueName = bool_array,
  TargQuant = 2,
  DefaultValue = {true},
  
  ExpectedResult = test_data:attrib_utils_config_attribs1(),
  
  Result = resize_attribute_array_value(Attributes, ArrayValueName, TargQuant, DefaultValue),
  ?assertEqual(ExpectedResult, Result).
  
%   Test config array attribute increases in size 
resize_attribute_array_value_config_increase_test() ->
  Attributes = test_data:attrib_utils_config_attribs1(),
  ArrayValueName = bool_array,
  TargQuant = 10,
  DefaultValue = {true},
  
  ExpectedResult = test_data:attrib_utils_config_attribs2(),
  
  Result = resize_attribute_array_value(Attributes, ArrayValueName, TargQuant, DefaultValue),
  ?assertEqual(ExpectedResult, Result).
  
%   Test config array attribute decreases in size 
resize_attribute_array_value_config_decrease_test() ->
  Attributes = test_data:attrib_utils_config_attribs1(),
  ArrayValueName = integer_array,
  TargQuant = 1,
  DefaultValue = {0},
  
  ExpectedResult = test_data:attrib_utils_config_attribs3(),
  
  Result = resize_attribute_array_value(Attributes, ArrayValueName, TargQuant, DefaultValue),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================


% ====================================================================
% Test resize_array_value()
%
%   Test same array value size
resize_array_value_same_test() ->
  ValuesArray = [{1}, {2}, {3}],
  TargQuant = 3,
  DefaultValue = {0},
  
  KeepValueArray = [{1}, {2}, {3}],
  DeleteValueArray = [],
  ExpectedResult = {KeepValueArray, DeleteValueArray},
  
  Result = resize_array_value(ValuesArray, TargQuant, DefaultValue),
  ?assertEqual(ExpectedResult, Result).

%   Test increase array value size
resize_array_value_increase_test() ->
  ValuesArray = [{1}, {2}, {3}],
  TargQuant = 6,
  DefaultValue = {0},
  
  KeepValueArray = [{1}, {2}, {3}, {0}, {0}, {0}],
  DeleteValueArray = [],
  ExpectedResult = {KeepValueArray, DeleteValueArray},
  
  Result = resize_array_value(ValuesArray, TargQuant, DefaultValue),
  ?assertEqual(ExpectedResult, Result).
  
%   Test decrease array value size  
resize_array_value_decrease_test() ->
  ValuesArray = [{1}, {2}, {3}],
  TargQuant = 1,
  DefaultValue = {0},
  
  KeepValueArray = [{1}],
  DeleteValueArray = [{2},{3}],
  ExpectedResult = {KeepValueArray, DeleteValueArray},
  
  Result = resize_array_value(ValuesArray, TargQuant, DefaultValue),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================


% ====================================================================
% Test str_to_value_id()
% 
%   Test good non-array value id
str_to_value_id_valuename_valid_test() ->
  Result = str_to_value_id("input"),
  ExpectedResult = {ok, input},
  ?assertEqual(ExpectedResult, Result).

%   Test good array value id 1
str_to_value_id_valuename_array_valid1_test() ->
  Result = str_to_value_id("input[10]"),
  ExpectedResult = {ok, {input, 10}},
  ?assertEqual(ExpectedResult, Result).

%   Test good array value id 2
str_to_value_id_valuename_array_valid2_test() ->
  Result = str_to_value_id("input[010]"),
  ExpectedResult = {ok, {input, 10}},
  ?assertEqual(ExpectedResult, Result).

%   Test bad array value id 2
str_to_value_id_valuename_array_invalid2_test() ->
  Result = str_to_value_id("input[0]"),
  ExpectedResult = {error, invalid},
  ?assertEqual(ExpectedResult, Result).

%   Test bad array value id 3
str_to_value_id_valuename_array_invalid3_test() ->
  Result = str_to_value_id("input[-123]"),
  ExpectedResult = {error, invalid},
  ?assertEqual(ExpectedResult, Result).

%   Test bad array value id 4
str_to_value_id_valuename_array_invalid4_test() ->
  Result = str_to_value_id("input[12.34]"),
  ExpectedResult = {error, invalid},
  ?assertEqual(ExpectedResult, Result).
% ====================================================================


% ====================================================================
% Test value_id_to_str())

% Test non-array value id
value_id_to_str_non_array_test() ->
  Result = value_id_to_str(num_of_inputs),
  ExpectedResult = "num_of_inputs",
  ?assertEqual(ExpectedResult, Result).

% Test array value id
value_id_to_str_array_test() ->
  Result = value_id_to_str({inputs, 11}),
  ExpectedResult = "inputs[11]",
  ?assertEqual(ExpectedResult, Result).
% ====================================================================



-endif.