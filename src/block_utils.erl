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
-export([get_attribute/2, get_value/2, get_integer/2, get_boolean/2, get_value_any/2]).
-export([set_value/3, set_values/2, set_value_status/3, set_value_normal/2, set_status/2]).
-export([set_value_any/3, set_input_link/3]).
-export([update_attribute_list/2, merge_attribute_lists/2, replace_attribute/3]).
-export([sleep/1]). 


%%	
%% Get the attribute for the given AttributeName in the list of Attributes
%% List of attributes may be Config, Inputs, Outputs, or Private type
%%
-spec get_attribute(Attributes :: list(), AttributeName :: atom()) -> tuple() | not_found.

get_attribute(Attributes, AttributeName) ->
	% ValueName is always the first element in the tuple, regardless of the Attribute type
	case lists:keyfind(AttributeName, 1, Attributes) of 
        false     -> not_found;
        Attribute -> Attribute
 	end.

%%	
%% Get the value of the AttributeName attribute in the list of Attributes
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec get_value(Attributes :: list(), AttributeName :: atom()) -> term() | not_found.

get_value(Attributes, AttributeName) ->
	case get_attribute(Attributes, AttributeName) of
		not_found ->
			error_logger:error_msg("get_value() Error: ~p not found in attributes list~n", [AttributeName]),
			not_found;
		{AttributeName, Value}     -> Value; % Config or Private Value
        {AttributeName, Value, _ } -> Value  % Input or Output value
 	end.


%%
%% Get an integer value, return error if value found is not an integer or other standard value
%%
-spec get_integer(Attributes :: list(), AttributeName :: atom()) -> 
                    integer() | not_found | not_active | empty | error. 

get_integer(Attributes, AttributeName) ->
    case get_value(Attributes, AttributeName) of
        not_found  -> not_found;
        not_active -> not_active; % not_active is a valid value  
        empty      -> empty;      % empty is a valid value
        Value      -> 
            if  is_integer(Value) -> Value;
            true -> 
                error_logger:error_msg("get_integer() Error: ~p  value is not an integer: ~p~n", [AttributeName,Value]),
                error
            end
    end.

%%
%% Get a boolean value, return error if value found is not a boolean or other standard value
%%
-spec get_boolean(Attributes :: list(), AttributeName :: atom()) -> 
                     boolean() | not_found | not_active | empty | error. 

get_boolean(Attributes, AttributeName) ->
    case get_value(Attributes, AttributeName) of
        not_found  -> not_found;
        not_active -> not_active; % not_active is a valid value  
        empty      -> empty;      % empty is a valid value
        Value     -> 
            if  is_boolean(Value) -> Value;
            true -> 
                error_logger:error_msg("get_boolean() Error: ~p  value is not a boolean: ~p~n", [AttributeName,Value]),
                error
            end
    end.


%%	
%% Set the value of the attribute ValueName
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec set_value(Attributes :: list(), AttributeName :: atom(), NewValue :: term()) -> list().
   
set_value(Attributes, AttributeName, NewValue) ->
	case get_attribute(Attributes, AttributeName) of
		not_found ->
			error_logger:error_msg("set_value() Error: ~p not found in attributes list~n", [AttributeName]),
			Attributes;
            
		{AttributeName, _OldValue} -> % Config or Private value
			NewAttribute = {AttributeName, NewValue},
			replace_attribute(Attributes, AttributeName, NewAttribute);
            
        {AttributeName, _OldValue, LinkOrConnections} ->  % Input or Output value
			NewAttribute = {AttributeName, NewValue, LinkOrConnections},
			replace_attribute(Attributes, AttributeName, NewAttribute)
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
-spec get_value_any(BlockValues :: block_state(), AttributeName :: atom()) -> term() | not_found.
	
get_value_any(BlockValues, AttributeName) ->
	
	{_BlockName, _BlockModule, Configs, Inputs, Outputs, Private} = BlockValues,
	
	case get_attribute(Configs, AttributeName) of
		not_found ->
			case get_attribute(Inputs, AttributeName) of
				not_found ->
					case get_attribute(Outputs, AttributeName) of
						not_found ->
							case get_attribute(Private, AttributeName) of
								not_found ->
									error_logger:error_msg("get_value_any() Error: ~p not found in Block values~n", [AttributeName]),
									not_found;
								{AttributeName, Value} -> Value	% Internal value
							end;
						{AttributeName, Value, _Connections} -> Value	% Output value
					end;
				{AttributeName, Value, _Link} -> Value	% Input value
			end; 
		{AttributeName, Value} -> Value   % Config value
 	end.


set_value_any(BlockValues, AttributeName, NewValue)->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Private} = BlockValues,
	
	% Can't modify Configs, don't bother checking those

	case get_attribute(Inputs, AttributeName) of
		not_found ->
			case get_attribute(Outputs, AttributeName) of
				not_found ->					
					case get_attribute(Private, AttributeName) of
						not_found ->
							error_logger:error_msg("~p set_value() Error. ~p not found in the BlockValues list~n", [BlockName, AttributeName]),
							BlockValues;  % Return Block values unchanged
						{AttributeName, _OldValue1} ->
							NewPrivateValue = {AttributeName, NewValue},
							NewPrivate = replace_attribute(Private, AttributeName, NewPrivateValue),
							{BlockName, BlockModule, Configs, Inputs, Outputs, NewPrivate}
					end;
				{AttributeName, _OldValue2, Connections} -> 
					NewOutput = {AttributeName, NewValue, Connections},
					NewOutputs = replace_attribute(Outputs, AttributeName, NewOutput),
					{BlockName, BlockModule, Configs, Inputs, NewOutputs, Private}
			end; 
		{AttributeName, _OldValue3, Link} -> 
			NewInput = {AttributeName, NewValue, Link},
			NewInputs = replace_attribute(Inputs, AttributeName, NewInput),
			{BlockName, BlockModule, Configs, NewInputs, Outputs, Private}
 	end.


%%
%% Update the input Link for the input value 'ValueName'
%% TODO: Do we need this? not used right now
%%
set_input_link(BlockValues, ValueName, NewLink) ->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Private} = BlockValues,
	
	case get_attribute(Inputs, ValueName) of
		not_found ->
			error_logger:error_msg("~p set_input_link() Error.  ~p not found in Input values.~n", [BlockName, ValueName]),
			% Input value not found, just return the BlockValues unchanged
			BlockValues;
		{ValueName, Value, _Link} ->
			NewInput = {ValueName, Value, NewLink},
			NewInputs = replace_attribute(Inputs, ValueName, NewInput),
			{BlockName, BlockModule, Configs, NewInputs, Outputs, Private}
	end.


%%
%% Update attributes in the Attribute List with the New Attributes list 
%% Add any new attributes if they are not already in the Attribute list
%% Both lists of Attributes must be the same type
%% Attributes may be Config, Inputs, Outputs, or Private type
%%
-spec merge_attribute_lists(Attributes :: list(), NewAttributes :: list()) -> list().

merge_attribute_lists(Attributes, []) -> Attributes;

merge_attribute_lists(Attributes, NewAttributes) ->
    [NewAttribute | RemainingNewAttributes] = NewAttributes,
    UpdatedAttributes = update_attribute_list(Attributes, NewAttribute),
    merge_attribute_lists(UpdatedAttributes, RemainingNewAttributes).

%%
%% Update the Attribute list with a new attribute
%% Attribute list may be Config, Inputs, Outputs, or Private type
%%
-spec update_attribute_list(Attribtes :: list(), NewAttribute :: tuple()) -> list().

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
-spec replace_attribute(Attributes :: list(), AttributeName :: atom(), NewAttribute :: tuple()) -> list().

replace_attribute(Attributes, AttributeName, NewAttribute) ->
	% attributeName is always the first element in the tuple, regardless of the attributeValue type
	lists:keyreplace(AttributeName, 1, Attributes, NewAttribute).


%%
%% Add a new attribute to the end of the Attribute list 
%% Attribute list may be Config, Inputs, Outputs, or Private type
%%
-spec add_attribute(Attributes :: list(), NewAttribute :: tuple()) -> list().

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
