%%% @doc 
%%% Common block utility functions     
%%%               
%%% @end 

-module(block_utils).

-author("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_attribute/2, get_value/2, get_value_any/2]).
-export([set_value/3, set_values/2]). 
-export([set_value_any/3, set_input_link/3]).
-export([update_attribute_list/2, merge_attribute_lists/2]).
-export([replace_attribute/3, add_attribute/2, create_attribute_array/2]).
-export([resize_attribute_array_value/4]).
-export([sleep/1]). 


%%	
%% Get the attribute for the given ValueName in the list of Attributes
%% List of attributes may be Config, Inputs, Outputs, or Private type
%%
-spec get_attribute(Attributes :: list(), 
                    ValueName :: atom()) -> tuple() | not_found.

get_attribute(Attributes, ValueName) ->
  % ValueName is always the first element in the tuple, regardless of the Attribute type
  case lists:keyfind(ValueName, 1, Attributes) of 
    false     -> not_found;
    Attribute -> Attribute
 	end.

%%	
%% Get the value of the ValueName attribute in the list of Attributes
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec get_value(Attributes :: list(), 
                ValueName :: atom()) -> term() | not_found.

get_value(Attributes, ValueName) ->
  case get_attribute(Attributes, ValueName) of
	  not_found ->
		  error_logger:error_msg("get_value() Error: ~p not found in attributes list~n", 
                              [ValueName]),
			not_found;
      
    {ValueName, Value}     -> Value; % Config or Private Value
    {ValueName, Value, _ } -> Value  % Input or Output value
  end.


%%	
%% Set the value of the attribute ValueName
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec set_value(Attributes :: list(), 
                ValueName :: atom(), 
                NewValue :: term()) -> list().

set_value(Attributes, ValueName, NewValue) ->
  case get_attribute(Attributes, ValueName) of
    not_found ->
      error_logger:error_msg("set_value() Error: ~p not found in attributes list~n", 
                             [ValueName]),
      Attributes;

    {ValueName, _OldValue} -> % Config or Private value
      NewAttribute = {ValueName, NewValue},
      replace_attribute(Attributes, ValueName, NewAttribute);

    {ValueName, _OldValue, LinkOrConnections} ->  % Input or Output value
      NewAttribute = {ValueName, NewValue, LinkOrConnections},
      replace_attribute(Attributes, ValueName, NewAttribute)
  end.


%%	
%% Set multiple values in the attribute list 
%% Values are in the form of Attribute Name, Value tuples
%% All values must belong to the same attribute list
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec set_values(Attributes :: list(), Values :: list()) -> list().
 
set_values(Attributes, []) -> Attributes;

set_values(Attributes, [{AttributeName, NewValue} | RemainingValues]) ->
  NewAttributes = set_value(Attributes, AttributeName, NewValue),
  set_values(NewAttributes, RemainingValues).


%%	
%% Get the value of the attribute ValueName
%% Check all of the attribute types: Config, Inputs, Outputs, or Private
%%
-spec get_value_any(BlockValues :: block_state(), 
                    ValueName :: atom()) -> term() | not_found.
	
get_value_any(BlockValues, ValueName) ->

  {Configs, Inputs, Outputs, Private} = BlockValues,

  case get_attribute(Configs, ValueName) of
    not_found ->
      case get_attribute(Inputs, ValueName) of
        not_found ->
          case get_attribute(Outputs, ValueName) of
            not_found ->
              case get_attribute(Private, ValueName) of
                not_found ->
                  error_logger:error_msg("get_value_any() Error: ~p not found in Block values~n", 
                                         [ValueName]),
                  not_found;
                {ValueName, Value} -> Value	% Internal value
              end;
            {ValueName, Value, _Connections} -> Value	% Output value
          end;
        {ValueName, Value, _Link} -> Value	% Input value
      end; 
    {ValueName, Value} -> Value   % Config value
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
    not_found ->
      case get_attribute(Outputs, ValueName) of
        not_found ->					
          case get_attribute(Private, ValueName) of
            not_found ->
              BlockName = config_utils:name(Config),
              error_logger:error_msg("~p set_value() Error. ~p not found in the BlockValues list~n", 
                              [BlockName, ValueName]),
              BlockValues;  % Return Block values unchanged
            {ValueName, _OldValue1} ->
              NewPrivateValue = {ValueName, NewValue},
              NewPrivate = replace_attribute(Private, ValueName, NewPrivateValue),
              {Config, Inputs, Outputs, NewPrivate}
          end;
        {ValueName, _OldValue2, Connections} -> 
          NewOutput = {ValueName, NewValue, Connections},
          NewOutputs = replace_attribute(Outputs, ValueName, NewOutput),
          {Config, Inputs, NewOutputs, Private}
      end; 
    {ValueName, _OldValue3, Link} -> 
      NewInput = {ValueName, NewValue, Link},
      NewInputs = replace_attribute(Inputs, ValueName, NewInput),
      {Config, NewInputs, Outputs, Private}
  end.


%%
%% Update the input Link for the input value 'ValueName'
%% TODO: Do we need this? not used right now
%%
-spec set_input_link(BlockValues :: block_state(),
                     ValueName :: atom(),
                     NewLink :: tuple()) -> block_state().
                     
set_input_link(BlockValues, ValueName, NewLink) ->
	
  {Config, Inputs, Outputs, Private} = BlockValues,

  case get_attribute(Inputs, ValueName) of
    not_found ->
      BlockName = config_utils:name(Config),
      error_logger:error_msg("~p set_input_link() Error.  ~p not found in Input values.~n", 
                             [BlockName, ValueName]),
      % Input value not found, just return the BlockValues unchanged
      BlockValues;
      
    {ValueName, Value, _Link} ->
      NewInput = {ValueName, Value, NewLink},
      NewInputs = replace_attribute(Inputs, ValueName, NewInput),
      {Config, NewInputs, Outputs, Private}
  end.


%%
%% Update attributes in the Attribute List with the New Attributes list 
%% Add any new attributes if they are not already in the Attribute list
%% Both lists of Attributes must be the same type
%% Attributes may be Config, Inputs, Outputs, or Private type
%%
-spec merge_attribute_lists(Attributes :: list(), 
                            NewAttributes :: list()) -> list().

merge_attribute_lists(Attributes, []) -> Attributes;

merge_attribute_lists(Attributes, NewAttributes) ->
  [NewAttribute | RemainingNewAttributes] = NewAttributes,
  UpdatedAttributes = update_attribute_list(Attributes, NewAttribute),
  merge_attribute_lists(UpdatedAttributes, RemainingNewAttributes).


%%
%% Update the Attribute list with a new attribute
%% Attribute list may be Config, Inputs, Outputs, or Private type
%%
-spec update_attribute_list(Attributes :: list(), 
                            NewAttribute :: tuple()) -> list().

update_attribute_list(Attributes, NewAttribute) ->
  % First element of any attribute value tuple is always the name 
  AttributeName = element(1, NewAttribute),

  case get_attribute(Attributes, AttributeName) of
    not_found       -> add_attribute(Attributes, NewAttribute);
    _attributeValue -> replace_attribute(Attributes, AttributeName, NewAttribute)
  end.


%%
%% Replace the AttributeName  attribute in the Attribute list with the New Attribute
%% Return the updated Attribute List
%% Attribute list may be Config, Inputs, Outputs, or Private type
%%
-spec replace_attribute(Attributes :: list(), 
                        ValueName :: atom(), 
                        NewAttribute :: tuple()) -> list().

replace_attribute(Attributes, ValueName, NewAttribute) ->
  % ValueName is always the first element in the tuple, regardless of the attributeValue type
  lists:keyreplace(ValueName, 1, Attributes, NewAttribute).


%%
%% Add a new attribute to the end of the Attribute list 
%% Attribute list may be Config, Inputs, Outputs, or Private type
%%
-spec add_attribute(Attributes :: list(), 
                    NewAttribute :: tuple()) -> list().

add_attribute(Attributes, Newattribute) ->
  Attributes ++ [Newattribute].


%%
%% Resize the ValueArrayName array of attribute values in the Attributes list. 
%% If there are fewer values in the array than the target quantity,
%% add values to the array using the DefaultValue
%% If there are more values in the array than the target Quantity,
%% delete the excess values, deleting references to these if needed
%% The Attributes list may be Config, Inputs, Outputs, or Private type
%% Returns updated attribute list, or error
%%
-spec resize_attribute_value_array(Attributes :: list(),
                                   TargQuant :: integer(),
                                   ValueArrayName :: atom(),
                                   DefaultValue :: term()) -> 
                                   {ok, list()} | {error, atom()}.
                             
resize_attribute_value_array(Attributes, TargQuant, ValueArrayName, DefaultValue)->
  case get_attribute(Attributes, ValueArrayName) of
    not_found ->
      {error, not_found};
      
    {ValueArrayName, ValuesArray} ->
      NewValuesArray = 
          resize_array_value(ValuesArray, TargQuant, DefaultValue),
      NewAttributes = 
          replace_attribute(Attributes, ValueArrayName, 
                            {ValueArrayName, NewValuesArray}),
      {ok, NewAttributes}
  end.


%%
%% Resize the array of values to match the target quantity.
%% Always add to or delete from the end of the array.
%% There will always be at least one value in the array. 
%%  
-spec resize_array_value(ValuesArray :: list(),
                         TargQuant :: integer(),
                         DefaultValue :: term()) -> list().
                                     
resize_value_array(ValuesArray, TargQuant, DefaultValue)->
  ValuesQuant = length(ValuesArray),
  if ValuesQuant =:= TargQuant ->
    % quantity of Values in array matches target, nothing to do 
    ValuesArray;
  true ->
    if ValuesQuant < TargQuant ->
      % not enough values, add the required number of default values
      AddedValues = lists:duplicate((TargQuant-ValuesQuant), DefaultValue),
      ValuesArray ++ AddedValues;
    true ->
      %  too many values, need to delete some
      {KeepValues, DeleteValues} = lists:split(TargQuant, ValuesArray),
      delete_array_values(DeleteValues),
      KeepValues
    end
  end.


%%
%% If we are deleting input values
%% Need to remove any links to other blocks
%%

delete_array_values(_DeleteValues) ->
  % TODO:  Need to split link_utils:unlink_blocks() to handle individual inputs
  %    Instead of all of the inputs of one block at a time.
  %    Same thing with output link references
  % ArrayValue [{}]

  ok.
  

  

       
      

%%
%% Create an array of attributes, 
%% Add an index number to the BaseValueName of the BaseAttribute
%% to make a unique Attribute.   
%% Create an associated list of value names, to assist accessing the array.
%% This works for all attribute types (Config, Inputs, Outputs, and Private)
%% Attribute type is specified by the BaseAttribute that is passed in.
%%
-spec create_attribute_array(Quant :: integer(),
                             BaseAttribute :: tuple()) -> {list(), list()}.
                             
create_attribute_array(Quant, BaseAttribute)->
  create_attribute_array([], [], Quant, BaseAttribute).
  
                               
-spec create_attribute_array(Attributes :: list(),
                             ValueNames :: list(),
                             Quant :: integer(),
                             BaseAttribute :: tuple()) -> {list(), list()}.
                              
create_attribute_array(Attributes, ValueNames, 0, _BaseAttribute) ->
  {lists:reverse(Attributes), lists:reverse(ValueNames)};
  
create_attribute_array(Attributes, ValueNames, Quant, BaseAttribute) ->
  % First element of an attribute is always the ValueName
  BaseValueName = element(1, BaseAttribute),
  % Add _xx to the end of the base ValueName
  ValueNameStr = iolib:format("~s_~2..0d", BaseValueName, Quant),
  ValueName = list_to_atom(ValueNameStr),
  % Replace the BaseAttribute BaseValueName, with the indexed ValueName
  Attribute = setelement(1, BaseAttribute, ValueName),
  create_attribute_array([Attribute | Attributes], [ValueName | ValueNames], 
                         Quant - 1, BaseAttribute).
  
    
%% common delay function
sleep(T) ->
  receive
  after T -> ok
  end.


      

%% ====================================================================
%% Internal functions
%% ====================================================================
