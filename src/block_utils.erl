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
-export([get_attribute/2, get_value/2, get_integer/2]). 
-export([get_boolean/2, get_value_any/2]).
-export([name/1, module/1, name_module/1]).
-export([set_value/3, set_values/2, set_value_status/3]).
-export([set_value_normal/2, set_status/2]).
-export([set_value_any/3, set_input_link/3]).
-export([update_attribute_list/2, merge_attribute_lists/2]).
-export([replace_attribute/3, add_attribute/2]).
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
%% Get an integer value, return error if value found is not an integer or other standard value
%%
-spec get_integer(Attributes :: list(), 
                  ValueName :: atom()) -> 
                  integer() | not_found | not_active | empty | error. 

get_integer(Attributes, ValueName) ->
  case get_value(Attributes, ValueName) of
    not_found  -> not_found;
    not_active -> not_active; % not_active is a valid value  
    empty      -> empty;      % empty is a valid value
    Value      -> 
      if  is_integer(Value) -> Value;
        true -> 
          error_logger:error_msg("get_integer() Error: ~p  value is not an integer: ~p~n", 
                                  [ValueName,Value]),
          error
      end
  end.


%%
%% Get a boolean value, return error if value found is not a boolean or other standard value
%%
-spec get_boolean(Attributes :: list(), 
                  ValueName :: atom()) -> 
                  boolean() | not_found | not_active | empty | error. 

get_boolean(Attributes, ValueName) ->
  case get_value(Attributes, ValueName) of
    not_found  -> not_found;
    not_active -> not_active; % not_active is a valid value  
    empty      -> empty;      % empty is a valid value
    Value     -> 
      if  is_boolean(Value) -> Value;
        true -> 
          error_logger:error_msg("get_boolean() Error: ~p  value is not a boolean: ~p~n", 
                                  [ValueName,Value]),
          error
      end
  end.


%%
%%  Get block name from Config attribute values
%%
-spec name(Config :: list() |
           block_defn() | 
           block_state()) -> atom().

name({Config, _Inputs, _Outputs, _Private}) ->
  get_value(Config, block_name);
  
name({Config, _Inputs, _Outputs}) ->  
  get_value(Config, block_name);
  
name(Config) ->
  get_value(Config, block_name).

  

%%
%% Get block module from Config attribute values 
%%
-spec module(Config :: list() | 
             block_defn() | 
             block_state()) -> module().

module({Config, _Inputs, _Outputs, _Private}) ->
  get_value(Config, block_module);
  
module({Config, _Inputs, _Outputs}) ->
  get_value(Config, block_module);

module(Config) ->
  get_value(Config, block_module).
  
%%
%%  Get block name and module from Config attribute values
%%
-spec name_module(Config :: list() |
                  block_defn() | 
                  block_state()) -> {atom(), module()}.

name_module({Config, _Inputs, _Outputs, _Private}) ->
  {get_value(Config, block_name),
   get_value(Config, block_module)};
  
name_module({Config, _Inputs, _Outputs}) ->  
  {get_value(Config, block_name),
   get_value(Config, block_module)};
                     
name_module(Config) ->
  {get_value(Config, block_name),
   get_value(Config, block_module)}.



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
%% Set block output value and status
%% Block output value and status attributes are often set at the same time.
%% This is a shortcut to do that.
%% 
-spec set_value_status(Outputs :: list(), Value :: term(), Status :: block_status()) -> list().

set_value_status(Outputs, Value, Status) ->
  set_values(Outputs, [{value, Value}, {status, Status}]).
  
%%
%% Set block output value and set status to normal
%% When setting the output value block status is usually normal.
%% This is a shortcut to do that.
%% 
-spec set_value_normal(Outputs :: list(), Value :: term()) -> list().

set_value_normal(Outputs, Value) ->
  set_values(Outputs, [{value, Value}, {status, normal}]).

%%
%% Set status output value
%% 
-spec set_status(Outputs :: list(), Status :: block_status()) -> list().

set_status(Outputs, Status) ->
  set_value(Outputs, status, Status).

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
              BlockName = name(Config),
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
      BlockName = name(Config),
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

    
%% common delay function
sleep(T) ->
  receive
  after T -> ok
  end.

%% ====================================================================
%% Internal functions
%% ====================================================================
