%%% @doc 
%%% Common block utility functions     
%%%               
%%% @end 

-module(block_utils).

-author("Mark Sebald").


%% ====================================================================
%% API functions
%% ====================================================================
-export([get_config_value/2, get_input_value/2, get_output_value/2, get_private_value/2, get_value/2]).
-export([set_input_value/3, set_output_value/3, set_private_value/3, set_value/3]).
-export([add_connection/3, set_input_link/3, set_input_link_value/5]).
-export([update_attribute_list/2, merge_attribute_lists/2]).
-export([sleep/1]). 

%% Get the Value of ValueName 
%% Return 'not_found', if ValueName is not found in the Values list
%% TODO: Just combine get/set_..._value() to one get/set_value() function
%%       Change get_attribute_value() name to just get_attribute(),
get_config_value(Configs, ValueName) ->
	case get_attribute_value(Configs, ValueName) of
		not_found ->
			io:format("get_value() Error: ~p not found in Config values~n", [ValueName]),
			not_found;
		{ValueName, Value} ->  Value
 	end.

get_input_value(Inputs, ValueName) ->
	case get_attribute_value(Inputs, ValueName) of
		not_found ->
			io:format("get_value() Error: ~p not found in Input values~n", [ValueName]),
			not_found;
		{ValueName, Value, _Link} -> Value
 	end.

get_output_value(Outputs, ValueName) ->
	case get_attribute_value(Outputs, ValueName) of
		not_found -> 
			io:format("get_value() Error: ~p not found in Output values~n", [ValueName]),
			not_found;
		{ValueName, Value, _Connections} -> Value
 	end.

get_private_value(Private, ValueName) ->
	case get_attribute_value(Private, ValueName) of
		not_found -> 
			io:format("get_value() Error: ~p not found in Private values~n", [ValueName]),
			not_found;
		{ValueName, Value} -> Value
 	end.
	
get_value(BlockValues, ValueName)->
	
	{BlockName, _BlockModule, Configs, Inputs, Outputs, Private} = BlockValues,
	
	case get_attribute_value(Configs, ValueName) of
		not_found ->
			case get_attribute_value(Inputs, ValueName) of
				not_found ->
					case get_attribute_value(Outputs, ValueName) of
						not_found ->
							case get_attribute_value(Private, ValueName) of
								not_found ->
									io:format("~p get_value() Error: ~p not found in Block values~n", [BlockName, ValueName]),
									not_found;
								{ValueName, Value} -> Value	% Internal value
							end;
						{ValueName, Value, _Connections} -> Value	% Output value
					end;
				{ValueName, Value, _Link} -> Value	% Input value
			end; 
		{ValueName, Value} -> Value   % Config value
 	end.


% TODO: Update multiple values, with a list of ValueName, Value pairs

%% Return BlockValues list with the Value of ValueName updated 
%% Return original values list if value 'ValueName' not found 
set_input_value(Inputs, ValueName, NewValue) ->
	case get_attribute_value(Inputs, ValueName) of
		not_found ->
			io:format("set_value() Error: ~p not found in Input values~n", [ValueName]),
			Inputs;
		{ValueName, _OldValue, Link} ->
			NewInput = {ValueName, NewValue, Link},
			NewInputs = replace_attribute_value(Inputs, ValueName, NewInput),
			NewInputs
	end.

set_output_value(Outputs, ValueName, NewValue) ->
	case get_attribute_value(Outputs, ValueName) of
		not_found ->
			io:format("set_value() Error: ~p not found in Output values~n", [ValueName]),
			Outputs;
		{ValueName, _OldValue, Connections} ->
			NewOutput = {ValueName, NewValue, Connections},
			NewOutputs = replace_attribute_value(Outputs, ValueName, NewOutput),
			NewOutputs
	end.	

set_private_value(Private, ValueName, NewValue) ->
	case get_attribute_value(Private, ValueName) of
		not_found ->
			io:format("set_value() Error: ~p not found in Private values~n", [ValueName]),
			Private;
		{ValueName, _OldValue} ->
			NewPrivateValue = {ValueName, NewValue},
			NewPrivate = replace_attribute_value(Private, ValueName, NewPrivateValue),
			NewPrivate
	end.	

set_value(BlockValues, ValueName, NewValue)->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Private} = BlockValues,
	
	% Can't modify Configs, don't bother checking those

	case get_attribute_value(Inputs, ValueName) of
		not_found ->
			case get_attribute_value(Outputs, ValueName) of
				not_found ->					
					case get_attribute_value(Private, ValueName) of
						not_found ->
							io:format("~p set_value() Error. ~p not found in the BlockValues list~n", [BlockName, ValueName]),
							BlockValues;  % Return Block values unchanged
						{ValueName, _OldValue1} ->
							NewPrivateValue = {ValueName, NewValue},
							NewPrivate = replace_attribute_value(Private, ValueName, NewPrivateValue),
							{BlockName, BlockModule, Configs, Inputs, Outputs, NewPrivate}
					end;
				{ValueName, _OldValue2, Connections} -> 
					NewOutput = {ValueName, NewValue, Connections},
					NewOutputs = replace_attribute_value(Outputs, ValueName, NewOutput),
					{BlockName, BlockModule, Configs, Inputs, NewOutputs, Private}
			end; 
		{ValueName, _OldValue3, Link} -> 
			NewInput = {ValueName, NewValue, Link},
			NewInputs = replace_attribute_value(Inputs, ValueName, NewInput),
			{BlockName, BlockModule, Configs, NewInputs, Outputs, Private}
 	end.


%% Update the input Link for the input value 'ValueName'
set_input_link(BlockValues, ValueName, NewLink) ->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Private} = BlockValues,
	
	case get_attribute_value(Inputs, ValueName) of
		not_found ->
			io:format("~p set_input_link() Error.  ~p not found in Input values.~n", [BlockName, ValueName]),
			% Input value not found, just return the BlockValues unchanged
			BlockValues;
		{ValueName, Value, _Link} ->
			NewInput = {ValueName, Value, NewLink},
			NewInputs = replace_attribute_value(Inputs, ValueName, NewInput),
			{BlockName, BlockModule, Configs, NewInputs, Outputs, Private}
	end.


%% Update the value of every input in this block, linked to the 'ValueName:FromBlockName:NodeName' output value
%% Return the updated list of Input values
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


%% Add a connection 'ToBlockName' to the connection list of the value record of the given ValueName
add_connection(BlockValues, ValueName, ToBlockName) ->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Private} = BlockValues,
	 
	case get_attribute_value(Outputs, ValueName) of
		
		not_found ->
			% This block doesn't have an output called 'ValueName'
			% Just return the original BlockValues
			io:format("~p: add_connection() Error. ~p Doesn't exist for this block~n", [BlockName, ValueName]),
			BlockValues;
		
		{ValueName, Value, Connections} ->  
			case lists:member(ToBlockName, Connections) of
			true ->
				% This output is already connected to block 'ToBlockName' 
				% Just return the original BlockValues
				BlockValues;
			false ->
				% add 'ToBlockName' to list of connection for this output
				NewConnections = [ToBlockName | Connections],
				NewOutput = {ValueName, Value, NewConnections},
				NewOutputs = replace_attribute_value(Outputs, ValueName, NewOutput),
				{BlockName, BlockModule, Configs, Inputs, NewOutputs, Private}
			end;
		Unknown ->
			io:format("~p add_connection() Error. Unknown output value record  ~p~n", [BlockName, Unknown]),
			BlockValues
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
 
    case get_attribute_value(AttributeList, AttributeName) of
        not_found -> add_attribute_value(AttributeList, NewattributeValue);
        _attributeValue -> replace_attribute_value(AttributeList, AttributeName, NewattributeValue)
    end.

	
%% Get the attribute value record for the given AttributeName
%% This works on all types of paramter value lists, Configs, Inputs, Outputs, and Private
get_attribute_value(AttributeList, AttributeName) ->
	% ValueName is always the first element in the tuple, regardless of the ValueRecord type
	case lists:keyfind(AttributeName, 1, AttributeList) of 
		        false -> not_found;
	   AttributeValue -> AttributeValue
 	end.


%% Replace the attributeName record in the AttributeList with the NewattributeValue
%% Return the updated AttributeList
%% This works on all types of value attribute value lists, Configs, Inputs, Outputs, and Private
replace_attribute_value(AttributeList, AttributeName, NewattributeValue) ->
	% attributeName is always the first element in the tuple, regardless of the attributeValue type
	lists:keyreplace(AttributeName, 1, AttributeList, NewattributeValue).


%% Add a new attribute value, {name, value} tuple, 
%% to the end of the given attribute list and return a new list
%% This works on all types of attribute value lists, Configs, Inputs, Outputs, and Private
add_attribute_value(AttributeList, NewattributeValue) ->
    AttributeList ++ [NewattributeValue].

    
%% common delay function
sleep(T) ->
	receive
	after T -> ok
	end.


	
%% ====================================================================
%% Internal functions
%% ====================================================================
