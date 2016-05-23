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
-export([get_attribute/2]).
-export([get_value/2, get_value_any/2]).
-export([set_value/3, set_values/2]).  %, set_value_any/3]).
-export([update_attribute_list/2, merge_attribute_lists/2]).
-export([replace_attribute/3, add_attribute/2]).
-export([resize_attribute_array_value/4, resize_attribute_array_value/5]).
-export([replace_array_value/3]).


%%	
%% Get the attribute for the given ValueName in the list of Attributes
%% List of attributes may be Config, Inputs, Outputs, or Private type
%%
-spec get_attribute(Attributes :: list(attribute()), 
                    ValueId :: value_id()) -> {ok, attribute()} | {error, not_found}.
% Array Value ID case
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
-spec get_value(Attributes :: list(attribute()), 
                ValueId :: value_id()) -> attrib_value().

% Get array value 
get_value(Attributes, ValueId) ->
  case get_attribute(Attributes, ValueId) of
	  {error, not_found} -> {error, not_found};
    
    % For non-array values, the ValueId is the attribute ValueName
    % Non-array Config or Private value
    {ok, {ValueId, {Value}}} -> {ok, Value};
    
     % Input or Output value
    {ok, {ValueId, {Value, _LinkOrRefs}}} -> {ok, Value};
    
    % Assume this is an array value
    {ok, {ValueName, ArrayValue}} ->
        % if this is an array value, the ValueName from get_attribute()
        % will match ValueName in the ValueId tuple
      case ValueId of
        {ValueName, ArrayIndex} ->
          if (0 < ArrayIndex) andalso (ArrayIndex =< length(ArrayValue)) ->
            case lists:nth(ArrayIndex, ArrayValue) of
              % Config or Private array value
              {Value} -> {ok, Value};
              % Input or Output array value
              {Value, _LinkOrRefs} -> {ok, Value}; 
              % Unrecognized value
              _InvalidValue -> {error, invalid_value}
            end;
          true -> {error, invalid_index}
          end;
        _InvalidValue -> {error, invalid_value}
      end;
    _InvalidValue -> {error, invalid_value}
  end.


%%	
%% Get the value of the attribute ValueName
%% Check attribute types: Config, Inputs, and Outputs
%%
%% This is used by processes external to the block
%% so don't allow getting Private values 
%%
-spec get_value_any(BlockValues :: block_state(), 
                    ValueId :: value_id()) -> attrib_value().

% Get array value	
get_value_any(BlockValues, ValueId) ->

  {Config, Inputs, Outputs, _Private} = BlockValues,

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
-spec set_value(Attributes :: list(attribute()), 
                ValueId :: value_id(), 
                NewValue :: value()) -> {ok, list(attribute())} | attrib_errors().
              
set_value(Attributes, ValueId, NewValue)->
  case get_attribute(Attributes, ValueId) of
    {error, not_found} -> {error, not_found};
    
    % For non-array values, the ValueId is the attribute ValueName
    % Non-array Config or Private value
    {ok, {ValueId, {_OldValue}}} -> 
      NewAttribute = {ValueId, {NewValue}},
      {ok, replace_attribute(Attributes, ValueId, NewAttribute)};
    
    % Non-array Input or Output value
    {ok, {ValueId, {_OldValue, LinkOrRefs}}} ->  
      NewAttribute = {ValueId, {NewValue, LinkOrRefs}},
      {ok, replace_attribute(Attributes, ValueId, NewAttribute)};
      
    % Assume this is an array value
    {ok, {ValueName, ArrayValue}} ->
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
              {_OldValue, LinkOrRefs} ->  
                NewArrayValue = 
                  replace_array_value(ArrayValue, ArrayIndex, {NewValue, LinkOrRefs}),
                {ok, replace_attribute(Attributes, ValueName, {ValueName,NewArrayValue})};
            
              _InvalidValue -> {error, invalid_value}
            end;
          true ->
            {error, invalid_index}
          end;
        _InvalidValue -> {error, invalid_value}
      end;
    _InvalidValue -> {error, invalid_value}
  end.


%% replace a value in the array of values
-spec replace_array_value(ArrayValue :: list(), 
                         ArrayIndex :: integer(),
                         NewValue :: term()) -> list().
                         
replace_array_value(ArrayValue, ArrayIndex, NewValue) ->
  lists:sublist(ArrayValue, ArrayIndex-1) 
                  ++ [NewValue] 
                  ++ lists:nthtail(ArrayIndex, ArrayValue).


%%	
%% Set multiple values in the attribute list 
%% Values are in the form of ValueID, Value tuples
%% All values must belong to the same attribute list
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec set_values(Attributes :: list(attribute()), 
                 Values :: list()) -> {ok, list(attribute())} | attrib_errors().
 
set_values(Attributes, []) ->{ok, Attributes};

set_values(Attributes, [{ValueId, NewValue} | RemainingValues]) ->
  case set_value(Attributes, ValueId, NewValue) of
    % Return immediately on error
    {error, Reason}     -> {error, Reason};
    {ok, NewAttributes} -> set_values(NewAttributes, RemainingValues)
  end.


-ifdef(INCLUDE_OBSOLETE).
%% TODO: Don't think we need this cut it out for now	
%% Set the value of the attribute ValueName
%%
-spec set_value_any(BlockValues :: block_state(), 
                    ValueId :: value_id(),
                    NewValue :: value()) -> block_state().

set_value_any(BlockValues, ValueId, NewValue)->
	
	{Config, Inputs, Outputs, Private} = BlockValues,
	% Can't modify Configs, don't bother checking those

  case get_attribute(Inputs, ValueId) of
    {error, not_found} ->
      case get_attribute(Outputs, ValueId) of
        {error, not_found} ->					
          case get_attribute(Private, ValueId) of
            {error, not_found} ->
              BlockName = config_utils:name(Config),
              error_logger:error_msg("~p set_value() Error. ~p not found in the BlockValues list~n", 
                              [BlockName, ValueName]),
              BlockValues;  % Return Block values unchanged
            {ok, {ValueName, _OldValue1}} ->
              NewPrivateValue = {ValueName, NewValue},
              NewPrivate = replace_attribute(Private, ValueName, NewPrivateValue),
              {Config, Inputs, Outputs, NewPrivate}
          end;
        {ok, {ValueName, {_OldValue2, Refs}}} -> 
          NewOutput = {ValueName, {NewValue, Refs}},
          NewOutputs = replace_attribute(Outputs, ValueName, NewOutput),
          {Config, Inputs, NewOutputs, Private}
      end; 
    {ok, {ValueName, {_OldValue3, Link}}} -> 
      NewInput = {ValueName, {NewValue, Link}},
      NewInputs = replace_attribute(Inputs, ValueName, NewInput),
      {Config, NewInputs, Outputs, Private}
  end.
-endif.

%%
%% Update attributes in the Attribute List with the New Attributes list 
%% Add any new attributes if they are not already in the Attribute list
%% Both lists of Attributes must be the same type
%% Attributes may be Config, Inputs, Outputs, or Private type
%%
-spec merge_attribute_lists(Attributes :: list(attribute()), 
                            NewAttributes :: list(attribute())) -> list(attribute()).

merge_attribute_lists(Attributes, []) -> Attributes;

merge_attribute_lists(Attributes, [NewAttribute | NewAttributes]) ->
  UpdatedAttributes = update_attribute_list(Attributes, NewAttribute),
  merge_attribute_lists(UpdatedAttributes, NewAttributes).


%%
%% Update the Attribute list with a new attribute
%% Attribute list may be Config, Inputs, Outputs, or Private type
%%
-spec update_attribute_list(Attributes :: list(attribute()), 
                            NewAttribute :: tuple()) -> list(attribute()).

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
-spec replace_attribute(Attributes :: list(attribute()), 
                        ValueName :: value_name(), 
                        NewAttribute :: attribute()) -> list(attribute()).

replace_attribute(Attributes, ValueName, NewAttribute) ->
  % ValueName is always the first element in the tuple, regardless of the attributeValue type
  lists:keyreplace(ValueName, 1, Attributes, NewAttribute).


%%
%% Add a new attribute to the end of the Attribute list 
%% Attribute list may be Config, Inputs, Outputs, or Private type
%%
-spec add_attribute(Attributes :: list(attribute()), 
                    NewAttribute :: attribute()) -> list(attribute()).

add_attribute(Attributes, Newattribute) ->
  Attributes ++ [Newattribute].
  

%%
%% Resize an array value in an attribute list
%% to match the target quantity, which is alway > zero
%% Returns updated attribute list
%% Works for Config, Input, Output, and Private array attributes
%%

%% Specialized resize function for the case when the excess values don't need 
%% any extra processing. Just pass in a DeleteExcess function that does nothing. 
-spec resize_attribute_array_value(Attributes :: list(attribute()),
                                   ArrayValueName :: value_name(),
                                   TargQuant :: pos_integer(),
                                   DefaultValue :: attr_value()) -> list(attribute()).
                             
resize_attribute_array_value(Attributes, ArrayValueName, TargQuant, DefaultValue)->
  DeleteExcess = fun(_DeleteArrayValues) -> ok end, 
  resize_attribute_array_value(Attributes, ArrayValueName, TargQuant, DefaultValue, DeleteExcess).


-spec resize_attribute_array_value(Attributes :: list(attribute()),
                                   ArrayValueName :: value_name(),
                                   TargQuant :: pos_integer(),
                                   DefaultValue :: attr_value(),
                                   DeleteExcess :: fun()) -> list(attribute()).
                             
resize_attribute_array_value(Attributes, ArrayValueName, TargQuant, DefaultValue, DeleteExcess)->
  case get_attribute(Attributes, ArrayValueName) of
    % If attribute not found, just return attribute list unchanged
    {error, not_found} -> Attributes;
      
    {ok, {ArrayValueName, ArrayValues}} ->  
        {NewArrayValues, DeleteArrayValues} = 
          resize_array_value(ArrayValues, TargQuant, DefaultValue),
      
      % Handle the deletion of the excess array values
      % i.e. deleted values could be block input values containing links to other blocks
      DeleteExcess(DeleteArrayValues),
      % Update the attribute list with the resized array value
      replace_attribute(Attributes, ArrayValueName, {ArrayValueName, NewArrayValues})
  end.


%%
%% Resize the array of values to match the target quantity.
%% Always add to or delete from the end of the array.
%% There will always be at least one value in the array. 
%%  
-spec resize_array_value(ValuesArray :: attr_value_array(),
                         TargQuant :: pos_integer(),
                         DefaultValue :: attr_value()) -> {attr_value_array(), attr_value_array()}.
                                     
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
  [ {block_name, {test_config}},
    {block_module, {type_template}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}},
    {number1, {123.45}},
    {string1, {"Testing"}},
    {bool_array, [{true}, {false}]},
    {integer1, {123}},
    {integer_array, [{234}, {456}, {-123}]}
  ].
  
test_config_attribs2() ->
  [ {block_name, {test_config}},
    {block_module, {type_template}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}},
    {number1, {123.45}},
    {string1, {"Testing"}},
    {bool_array, [{true}, {false}, {true}, {true}, {true}, {true}, {true}, {true}, {true}, {true}]},
    {integer1, {123}},
    {integer_array, [{234}, {456}, {-123}]}
  ].
  
test_config_attribs3() ->
  [ {block_name, {test_config}},
    {block_module, {type_template}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}},
    {number1, {123.45}},
    {string1, {"Testing"}},
    {bool_array, [{true}, {false}]},
    {integer1, {123}},
    {integer_array, [{234}]}
  ].
  
test_input_attribs1() ->
  [ {block_name, {test_config}},
    {block_module, {type_template}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}},
    {number_in, {123.45, {}}},
    {string_in, {"Testing", {}}},
    {bool_array_in, [{true,{}}, {false,{}}]},
    {integer_in, {123}},
    {integer_array_in, [{234,{}}, {456,{}}, {-123,{}}]}
  ].
  
% ====================================================================

% ====================================================================
% Test get_attribute()
% 
%   Test get_attribute() found
get_attribute_found_test() ->
  Attributes = test_config_attribs1(),
  ValueName = number1,
  ExpectedResult = {ok, {number1, {123.45}}},
  
  Result = get_attribute(Attributes, ValueName),
  ?assertEqual(ExpectedResult, Result).

%   Test get_attribute() not_found
get_attribute_not_found_test() ->
  Attributes = test_config_attribs1(),
  ValueName = unexpected,
  ExpectedResult = {error, not_found},
  
  Result = get_attribute(Attributes, ValueName),
  ?assertEqual(ExpectedResult, Result).

% ====================================================================
% Test get_value()
% 
%   Test get_value() input ok
get_value_input_ok_test() ->
  Attributes = test_input_attribs1(),
  ValueName = number_in,
  ExpectedResult = {ok, 123.45},
  
  Result = get_value(Attributes, ValueName),
  ?assertEqual(ExpectedResult, Result).

%   Test get_value() input not_found
get_value_input_not_found_test() ->
  Attributes = test_input_attribs1(),
  ValueName = unexpected,
  ExpectedResult = {error, not_found},
  
  Result = get_value(Attributes, ValueName),
  ?assertEqual(ExpectedResult, Result).

%   Test get_value() input array value ok
get_value_input_array_ok_test() ->
  Attributes = test_input_attribs1(),
  ValueName = integer_array_in,
  ArrayIndex = 2,
  ExpectedResult = {ok, 456},
  
  Result = get_value(Attributes, {ValueName, ArrayIndex}),
  ?assertEqual(ExpectedResult, Result).

%   Test get_value() input array value negative index
get_value_input_array_invalid_index_test() ->
  Attributes = test_input_attribs1(),
  ValueName = bool_array_in,
  ArrayIndex = -23,
  ExpectedResult = {error, invalid_index},
  
  Result = get_value(Attributes, {ValueName, ArrayIndex}),
  ?assertEqual(ExpectedResult, Result).
  
%   Test get_value() config array value 0 index
get_value_config_array_0_index_test() ->
  Attributes = test_config_attribs1(),
  ValueName = string1,
  ArrayIndex = 0,
  ExpectedResult = {error, invalid_index},
  
  Result = get_value(Attributes, {ValueName, ArrayIndex}),
  ?assertEqual(ExpectedResult, Result).
  
%   Test get_value() config array value index too large
get_value_config_array_negative_index_test() ->
  Attributes = test_config_attribs1(),
  ValueName = bool_array,
  ArrayIndex = 99,
  ExpectedResult = {error, invalid_index},
  
  Result = get_value(Attributes, {ValueName, ArrayIndex}),
  ?assertEqual(ExpectedResult, Result).

% ====================================================================
% Test resize_attribute_array_value(), No special delete handling
% 
%   Test config array attribute doesn't change 
resize_attribute_array_value_config_same_test() ->
  Attributes = test_config_attribs1(),
  ArrayValueName = bool_array,
  TargQuant = 2,
  DefaultValue = {true},
  
  ExpectedResult = test_config_attribs1(),
  
  Result = resize_attribute_array_value(Attributes, ArrayValueName, TargQuant, DefaultValue),
  ?assertEqual(ExpectedResult, Result).
  
%   Test config array attribute increases in size 
resize_attribute_array_value_config_increase_test() ->
  Attributes = test_config_attribs1(),
  ArrayValueName = bool_array,
  TargQuant = 10,
  DefaultValue = {true},
  
  ExpectedResult = test_config_attribs2(),
  
  Result = resize_attribute_array_value(Attributes, ArrayValueName, TargQuant, DefaultValue),
  ?assertEqual(ExpectedResult, Result).
  
%   Test config array attribute decreases in size 
resize_attribute_array_value_config_decrease_test() ->
  Attributes = test_config_attribs1(),
  ArrayValueName = integer_array,
  TargQuant = 1,
  DefaultValue = {0},
  
  ExpectedResult = test_config_attribs3(),
  
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
-endif.