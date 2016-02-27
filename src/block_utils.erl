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
-export([get_value/2, get_value_any/2]).
-export([set_value/3, set_value_any/3]).
-export([add_connection/3, set_input_link/3, set_input_link_value/5]).
-export([get_attribute/2, update_attribute_list/2, merge_attribute_lists/2]).
-export([sleep/1]). 


%% TODO: Create update_values function to update more than one value at a time.

%%	
%% Get the attribute for the given AttributeName in the list of Attributes
%% List of attributes may be Config, Inputs, Outputs, or Private
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
			io:format("get_value() Error: ~p not found in attributes list~n", [AttributeName]),
			not_found;
		{AttributeName, Value}     -> Value; % Config or Private Value
        {AttributeName, Value, _ } -> Value  % Input or Output value
 	end.

%%	
%% Set the value of the attribute ValueName
%% List of attributes may be Config, Inputs, Outputs, or Private
%%
-spec set_value(Attributes :: list(), AttributeName :: atom(), NewValue :: term()) -> list().
   
set_value(Attributes, AttributeName, NewValue) ->
	case get_attribute(Attributes, AttributeName) of
		not_found ->
			io:format("set_value() Error: ~p not found in attributes list~n", [AttributeName]),
			Attributes;
		{AttributeName, _OldValue} -> % Config or Private values
			NewAttribute = {AttributeName, NewValue},
			replace_attribute(Attributes, AttributeName, NewAttribute);
        {AttributeName, _OldValue, LinkOrConnections} ->  % Input or Output value
			NewAttribute = {AttributeName, NewValue, LinkOrConnections},
			replace_attribute(Attributes, AttributeName, NewAttribute)
	end.


%%	
%% Get the value of the attribute ValueName
%% Check all of the attribute types: Config, Inputs, Outputs, or Private
%%
-spec get_value_any(BlockValues :: block_state(), AttributeName :: atom()) -> term() | not_found.
	
get_value_any(BlockValues, AttributeName)->
	
	{_BlockName, _BlockModule, Configs, Inputs, Outputs, Private} = BlockValues,
	
	case get_attribute(Configs, AttributeName) of
		not_found ->
			case get_attribute(Inputs, AttributeName) of
				not_found ->
					case get_attribute(Outputs, AttributeName) of
						not_found ->
							case get_attribute(Private, AttributeName) of
								not_found ->
									io:format("get_value_any() Error: ~p not found in Block values~n", [AttributeName]),
									not_found;
								{AttributeName, Value} -> Value	% Internal value
							end;
						{AttributeName, Value, _Connections} -> Value	% Output value
					end;
				{AttributeName, Value, _Link} -> Value	% Input value
			end; 
		{AttributeName, Value} -> Value   % Config value
 	end.


% TODO: Update multiple values, with a list of ValueName, Value pairs


set_value_any(BlockValues, AttributeName, NewValue)->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Private} = BlockValues,
	
	% Can't modify Configs, don't bother checking those

	case get_attribute(Inputs, AttributeName) of
		not_found ->
			case get_attribute(Outputs, AttributeName) of
				not_found ->					
					case get_attribute(Private, AttributeName) of
						not_found ->
							io:format("~p set_value() Error. ~p not found in the BlockValues list~n", [BlockName, AttributeName]),
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


%% Update the input Link for the input value 'ValueName'
%% TODO: Do we need this? not used right now
set_input_link(BlockValues, ValueName, NewLink) ->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Private} = BlockValues,
	
	case get_attribute(Inputs, ValueName) of
		not_found ->
			io:format("~p set_input_link() Error.  ~p not found in Input values.~n", [BlockName, ValueName]),
			% Input value not found, just return the BlockValues unchanged
			BlockValues;
		{ValueName, Value, _Link} ->
			NewInput = {ValueName, Value, NewLink},
			NewInputs = replace_attribute(Inputs, ValueName, NewInput),
			{BlockName, BlockModule, Configs, NewInputs, Outputs, Private}
	end.


%% Update the value of every input in this block, linked to the 'ValueName:FromBlockName:NodeName' output value
%% Return the updated list of Input values
%% TODO: Do we need this? not used right now
set_input_link_value(BlockValues, NewValueName, FromBlockName, NodeName, NewValue) ->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Private} = BlockValues,

	TargetLink = {NewValueName, FromBlockName, NodeName},

	% Update the value of each input record pointing at the given value 
	NewInputs = lists:map(
		fun(Input) -> 
			{ValueName, _Value, Link} = Input,
			case Link =:= TargetLink of
				true  -> {ValueName, NewValue, Link};
				false -> Input	% This block input is not linked to the target block output, don't change the input value 
			end
		end, 
		Inputs),
	
	{BlockName, BlockModule, Configs, NewInputs, Outputs, Private}.


%%
%% Add a connection 'ToBlockName' to the connection list of the ValueName output attribute
%% Returns updated Outputs list
%%
-spec add_connection(Ouutputs :: list(), AttributeName :: atom(), ToBlockName :: atom()) -> list().

add_connection(Outputs, AttributeName, ToBlockName) ->
	 
	case get_attribute(Outputs, AttributeName) of
		
		not_found ->
			% This block doesn't have an output called 'AttributeName'
			% Just return the original Outputs list
			io:format("add_connection() Error. ~p Doesn't exist for this block~n", [AttributeName]),
			Outputs;
		
		{AttributeName, Value, Connections} ->  
			case lists:member(ToBlockName, Connections) of
			true ->
				% This output is already connected to block 'ToBlockName' 
				% Just return the original Outputs list
				Outputs;
			false ->
				% add 'ToBlockName' to list of connection for this output
                % Return updated Outputs list
				NewConnections = [ToBlockName | Connections],
				NewOutput = {AttributeName, Value, NewConnections},
				replace_attribute(Outputs, AttributeName, NewOutput)
			end;
		Unknown ->
			io:format("add_connection() Error. Unknown output value record  ~p~n", [Unknown]),
			Outputs
	end.


%% Update attribute values in the attribute List with the values in the NewattributeList 
%% and add any new attributes if they are not already in the AttributeList
%% This works on all types of paramter value lists, Configs, Inputs, Outputs, and Private
merge_attribute_lists(AttributeList, []) -> AttributeList;

merge_attribute_lists(AttributeList, NewattributeList) ->
    [NewattributeValue | RemainingNewattributes] = NewattributeList,
    UpdatedattributeList = update_attribute_list(AttributeList, NewattributeValue),
    merge_attribute_lists(UpdatedattributeList, RemainingNewattributes).


%% Update the AttributeList with the new attributeValue
%% This works on all types of paramter value lists, Configs, Inputs, Outputs, and Private
update_attribute_list(AttributeList, NewattributeValue) ->
    % First element of any attribute value tuple is always the name 
    AttributeName = element(1, NewattributeValue),
 
    case get_attribute(AttributeList, AttributeName) of
        not_found -> add_attribute(AttributeList, NewattributeValue);
        _attributeValue -> replace_attribute(AttributeList, AttributeName, NewattributeValue)
    end.


%% Replace the attributeName record in the AttributeList with the NewattributeValue
%% Return the updated AttributeList
%% This works on all types of value attribute value lists, Configs, Inputs, Outputs, and Private
replace_attribute(AttributeList, AttributeName, NewattributeValue) ->
	% attributeName is always the first element in the tuple, regardless of the attributeValue type
	lists:keyreplace(AttributeName, 1, AttributeList, NewattributeValue).


%% Add a new attribute {name, value} tuple, 
%% to the end of the given attribute list and return a new list
%% This works on all types of attribute value lists, Configs, Inputs, Outputs, and Private
add_attribute(AttributeList, NewattributeValue) ->
    AttributeList ++ [NewattributeValue].

    
%% common delay function
sleep(T) ->
	receive
	after T -> ok
	end.


	
%% ====================================================================
%% Internal functions
%% ====================================================================
