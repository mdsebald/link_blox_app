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
-export([get_value/2, get_value/3, get_value_any/3]).
-export([set_value/3, set_value/4, set_values/2, set_value_any/3]).
-export([update_attribute_list/2, merge_attribute_lists/2]).
-export([replace_attribute/3, add_attribute/2]).
-export([resize_attribute_array_value/4, resize_attribute_array_value/5]).


%%	
%% Get the attribute for the given ValueName in the list of Attributes
%% List of attributes may be Config, Inputs, Outputs, or Private type
%%
-spec get_attribute(Attributes :: list(attribute()), 
                    ValueName :: atom()) -> {ok, attribute()} | {error, not_found}.

get_attribute(Attributes, ValueName) ->
  % io:format("~p~n", [Attributes]),
  % ValueName is always the first element in the tuple, regardless of the attribute type
  case lists:keyfind(ValueName, 1, Attributes) of 
    false     -> {error, not_found};
    Attribute -> {ok, Attribute}
 	end.


%%	
%% Simple get value function for non-array attribute values
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec get_value(Attributes :: list(attribute()), 
                ValueName :: atom()) -> attrib_value().

get_value(Attributes, ValueName) ->
  get_value(Attributes, ValueName, 0).
  

%%	
%% Get the value of the ValueName attribute in the list of Attributes
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec get_value(Attributes :: list(attribute()), 
                ValueName :: atom(),
                ArrayIndex :: integer()) -> attrib_value().

% Assumes the attriute is a single value
get_value(Attributes, ValueName, ArrayIndex) when ArrayIndex == 0 ->
  case get_attribute(Attributes, ValueName) of
	  {error, not_found}                      -> {error, not_found};
    % Config or Private value
    {ok, {ValueName, {Value}}}              -> {ok, Value};
     % Input or Output value
    {ok, {ValueName, {Value, _LinkOrRefs}}} -> {ok, Value};
    % Unrecognized value
    {ok, {ValueName, _InvalidValue}}        -> {error, invalid_value}
  end;

% Assumes the attribute is an array value.
get_value(Attributes, ValueName, ArrayIndex) when ArrayIndex > 0 ->
  case get_attribute(Attributes, ValueName) of
	  {error, not_found}            -> {error, not_found};
    {ok, {ValueName, ArrayValue}} ->
      if length(ArrayValue) >= ArrayIndex ->
        case lists:nth(ArrayValue) of
          % Config or Private array value
          {Value}              -> {ok, Value};
          % Input or Output array value
          {Value, _LinkOrRefs} -> {ok, Value}; 
          % Unrecognized value
          _InvalidValue        -> {error, invalid_value}
        end;
      true ->  {error, invalid_index}
      end
  end;
  
% Negative array indexes, not allowed 
get_value(_Attributes, _ValueName, ArrayIndex) when ArrayIndex < 0 ->
  {error, negative_index}.


%%	
%% Get the value of the attribute ValueName
%% Check attribute types: Config, Inputs, and Outputs
%%
-spec get_value_any(BlockValues :: block_state(), 
                    ValueName :: atom(),
                    ArrayIndex :: integer()) -> attrib_value().
	
get_value_any(BlockValues, ValueName, ArrayIndex) ->

  {Config, Inputs, Outputs, _Private} = BlockValues,

  case get_value(Config, ValueName, ArrayIndex) of
    {error, not_found} ->

      case get_value(Inputs, ValueName, ArrayIndex) of
        {error, not_found} ->

          case get_value(Outputs, ValueName, ArrayIndex) of
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
%% Simple set value function for non-array attribute values
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec set_value(Attributes :: list(attribute()), 
                ValueName :: atom(), 
                NewValue :: value()) -> {ok, list(attribute())} | attrib_errors().
                
set_value(Attributes, ValueName, NewValue) ->
  set_value(Attributes, ValueName, NewValue, 0).

%%	
%% Set the value of the attribute ValueName
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec set_value(Attributes :: list(attribute()), 
                ValueName :: atom(), 
                NewValue :: value(), 
                ArrayIndex :: integer()) -> {ok, list(attribute())} | attrib_errors().
                
% Assumes the attriute is a single value
set_value(Attributes, ValueName, NewValue, ArrayIndex) when ArrayIndex == 0 ->
  case get_attribute(Attributes, ValueName) of
    {error, not_found} -> {error, not_found};

    % Config or Private value
    {ok, {ValueName, {_OldValue}}} -> 
      NewAttribute = {ValueName, {NewValue}},
      {ok, replace_attribute(Attributes, ValueName, NewAttribute)};
    
    % Input or Output value
    {ok, {ValueName, {_OldValue, LinkOrRefs}}} ->  
      NewAttribute = {ValueName, {NewValue, LinkOrRefs}},
      {ok, replace_attribute(Attributes, ValueName, NewAttribute)};
    
    % Unrecognized value  
    {ok, {ValueName, _InvalidValue}} -> {error, invalid_value}
  end;

% Assumes the attribute is an array value.
set_value(Attributes, ValueName, NewValue, ArrayIndex) when ArrayIndex > 0 ->
  case get_attribute(Attributes, ValueName) of
    {error, not_found} -> {error, not_found};
      
    {ok, {ValueName, ArrayValue}} ->
      if length(ArrayValue) >= ArrayIndex ->
        case lists:nth(ArrayValue) of
          % Config or Private value
          {_OldValue} ->
            NewArrayValue = 
              insert_array_value({NewValue}, ArrayIndex, ArrayValue),
            {ok, replace_attribute(Attributes, ValueName, NewArrayValue)};
            
          % Input or Output value
          {_OldValue, LinkOrRefs} ->  
            NewArrayValue = 
              insert_array_value({NewValue, LinkOrRefs}, ArrayIndex, ArrayValue),
            {ok, replace_attribute(Attributes, ValueName, NewArrayValue)};
            
         % Unrecognized value
          _InvalidValue -> {error, invalid_value}
        end;
      true ->
        {error, invalid_index}
      end
  end.


%% insert a new value into the array of values
-spec insert_array_value(NewValue :: term(), 
                         ArrayIndex :: integer(),
                         ArrayValue :: list()) -> list().
                         
insert_array_value(NewValue, ArrayIndex, ArrayValue) ->
  lists:sublist(ArrayValue, ArrayIndex-1) ++ 
                               [NewValue] ++ 
                lists:nthtail(ArrayIndex, ArrayValue).


%%	
%% Set multiple values in the attribute list 
%% Values are in the form of Attribute Name, Value tuples
%% All values must belong to the same attribute list
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec set_values(Attributes :: list(attribute()), 
                 Values :: list()) -> {ok, list(attribute())} | attrib_errors().
 
set_values(Attributes, []) ->{ok, Attributes};

set_values(Attributes, [{AttributeName, NewValue} | RemainingValues]) ->
  case set_value(Attributes, AttributeName, NewValue) of
    % Return immediately on error
    {error, Reason}     -> {error, Reason};
    {ok, NewAttributes} -> set_values(NewAttributes, RemainingValues)
  end.


%%	
%% Set the value of the attribute ValueName
%%
-spec set_value_any(BlockValues :: block_state(), 
                    ValueName :: atom(),
                    NewValue :: term()) -> block_state().

set_value_any(BlockValues, ValueName, NewValue)->
	
	{Config, Inputs, Outputs, Private} = BlockValues,
	% Can't modify Configs, don't bother checking those

  case get_attribute(Inputs, ValueName) of
    {error, not_found} ->
      case get_attribute(Outputs, ValueName) of
        {error, not_found} ->					
          case get_attribute(Private, ValueName) of
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
                        ValueName :: atom(), 
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
                         DefaultValue :: attr_value()) -> {attr_value_array(), attr_value_array}.
                                     
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
