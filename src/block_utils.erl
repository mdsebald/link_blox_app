%% @author Mark Sebald
%% @doc Common block utility functions


-module(block_utils).


%% ====================================================================
%% API functions
%% ====================================================================
-export([get_config_value/2, get_input_value/2, get_output_value/2, get_internal_value/2, get_value/2]).
-export([set_input_value/3, set_output_value/3, set_internal_value/3, set_value/3]).
-export([add_connection/3, set_input_link/3, set_input_link_value/5]).
-export([update_parameter_list/2, merge_parameter_lists/2]).
-export([sleep/1]). 

%% Get the Value of ValueName 
%% Return 'not_found', if ValueName is not found in the Values list
get_config_value(Configs, ValueName) ->
	case get_parameter_value(Configs, ValueName) of
		not_found ->
			io:format("get_value() Error: ~p not found in Config values~n", [ValueName]),
			not_found;
		{ValueName, Value} ->  Value
 	end.

get_input_value(Inputs, ValueName) ->
	case get_parameter_value(Inputs, ValueName) of
		not_found ->
			io:format("get_value() Error: ~p not found in Input values~n", [ValueName]),
			not_found;
		{ValueName, Value, _Link} -> Value
 	end.

get_output_value(Outputs, ValueName) ->
	case get_parameter_value(Outputs, ValueName) of
		not_found -> 
			io:format("get_value() Error: ~p not found in Output values~n", [ValueName]),
			not_found;
		{ValueName, Value, _Connections} -> Value
 	end.

get_internal_value(Internals, ValueName) ->
	case get_parameter_value(Internals, ValueName) of
		not_found -> 
			io:format("get_value() Error: ~p not found in Internal values~n", [ValueName]),
			not_found;
		{ValueName, Value} -> Value
 	end.
	
get_value(BlockValues, ValueName)->
	
	{BlockName, _BlockModule, Configs, Inputs, Outputs, Internals} = BlockValues,
	
	case get_parameter_value(Configs, ValueName) of
		not_found ->
			case get_parameter_value(Inputs, ValueName) of
				not_found ->
					case get_parameter_value(Outputs, ValueName) of
						not_found ->
							case get_parameter_value(Internals, ValueName) of
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
	case get_parameter_value(Inputs, ValueName) of
		not_found ->
			io:format("set_value() Error: ~p not found in Input values~n", [ValueName]),
			Inputs;
		{ValueName, _OldValue, Link} ->
			NewInput = {ValueName, NewValue, Link},
			NewInputs = replace_parameter_value(Inputs, ValueName, NewInput),
			NewInputs
	end.

set_output_value(Outputs, ValueName, NewValue) ->
	case get_parameter_value(Outputs, ValueName) of
		not_found ->
			io:format("set_value() Error: ~p not found in Output values~n", [ValueName]),
			Outputs;
		{ValueName, _OldValue, Connections} ->
			NewOutput = {ValueName, NewValue, Connections},
			NewOutputs = replace_parameter_value(Outputs, ValueName, NewOutput),
			NewOutputs
	end.	

set_internal_value(Internals, ValueName, NewValue) ->
	case get_parameter_value(Internals, ValueName) of
		not_found ->
			io:format("set_value() Error: ~p not found in Internal values~n", [ValueName]),
			Internals;
		{ValueName, _OldValue} ->
			NewInternal = {ValueName, NewValue},
			NewInternals = replace_parameter_value(Internals, ValueName, NewInternal),
			NewInternals
	end.	

set_value(BlockValues, ValueName, NewValue)->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Internals} = BlockValues,
	
	% Can't modify Congigs, don't bother checking those

	case get_parameter_value(Inputs, ValueName) of
		not_found ->
			case get_parameter_value(Outputs, ValueName) of
				not_found ->					
					case get_parameter_value(Internals, ValueName) of
						not_found ->
							io:format("~p set_value() Error. ~p not found in the BlockValues list~n", [BlockName, ValueName]),
							not_found;
						{ValueName, _OldValue1} ->
							NewInternal = {ValueName, NewValue},
							NewInternals = replace_parameter_value(Internals, ValueName, NewInternal),
							{BlockName, BlockModule, Configs, Inputs, Outputs, NewInternals}
					end;
				{ValueName, _OldValue2, Connections} -> 
					NewOutput = {ValueName, NewValue, Connections},
					NewOutputs = replace_parameter_value(Outputs, ValueName, NewOutput),
					{BlockName, BlockModule, Configs, Inputs, NewOutputs, Internals}
			end; 
		{ValueName, _OldValue3, Link} -> 
			NewInput = {ValueName, NewValue, Link},
			NewInputs = replace_parameter_value(Inputs, ValueName, NewInput),
			{BlockName, BlockModule, Configs, NewInputs, Outputs, Internals}
 	end.


%% Update the input Link for the input value 'ValueName'
set_input_link(BlockValues, ValueName, NewLink) ->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Internals} = BlockValues,
	
	case get_parameter_value(Inputs, ValueName) of
		not_found ->
			io:format("~p set_input_link() Error.  ~p not found in Input values.~n", [BlockName, ValueName]),
			% Input value not found, just return the BlockValues unchanged
			BlockValues;
		{ValueName, Value, _Link} ->
			NewInput = {ValueName, Value, NewLink},
			NewInputs = replace_parameter_value(Inputs, ValueName, NewInput),
			{BlockName, BlockModule, Configs, NewInputs, Outputs, Internals}
	end.


%% Update the value of every input in this block, linked to the 'ValueName:FromBlockName:NodeName' output value
%% Return the updated list of Input values
set_input_link_value(BlockValues, NewValueName, FromBlockName, NodeName, NewValue) ->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Internals} = BlockValues,

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
	
	{BlockName, BlockModule, Configs, NewInputs, Outputs, Internals}.


%% Add a connection 'ToBlockName' to the connection list of the value record of the given ValueName
add_connection(BlockValues, ValueName, ToBlockName) ->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Internals} = BlockValues,
	 
	case get_parameter_value(Outputs, ValueName) of
		
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
				NewOutputs = replace_parameter_value(Outputs, ValueName, NewOutput),
				{BlockName, BlockModule, Configs, Inputs, NewOutputs, Internals}
			end;
		Unknown ->
			io:format("~p add_connection() Error. Unknown output value record  ~p~n", [BlockName, Unknown]),
			BlockValues
	end.


%% Update parameter values in the Parameter List with the values in the NewParameterList 
%% and add any new parameters if they are not already in the ParameterList
%% This works on all types of paramter value lists, Configs, Inputs, Outputs, and Internals
merge_parameter_lists(ParameterList, []) -> ParameterList;

merge_parameter_lists(ParameterList, NewParameterList) ->
    [NewParameterValue | RemainingNewParameters] = NewParameterList,
    UpdatedParameterList = update_parameter_list(ParameterList, NewParameterValue),
    merge_parameter_lists(UpdatedParameterList, RemainingNewParameters).


%% Update the ParameterList with the new ParameterValue
%% This works on all types of paramter value lists, Configs, Inputs, Outputs, and Internals
update_parameter_list(ParameterList, NewParameterValue) ->
    % First element of any parameter value tuple is always the name 
    ParameterName = element(1, NewParameterValue),
 
    case get_parameter_value(ParameterList, ParameterName) of
        not_found -> add_parameter_value(ParameterList, NewParameterValue);
        _ParameterValue -> replace_parameter_value(ParameterList, ParameterName, NewParameterValue)
    end.

	
%% Get the parameter value record for the given ParameterName
%% This works on all types of paramter value lists, Configs, Inputs, Outputs, and Internals
get_parameter_value(ParameterList, ParameterName) ->
	% ValueName is always the first element in the tuple, regardless of the ValueRecord type
	case lists:keyfind(ParameterName, 1, ParameterList) of 
		        false -> not_found;
	   ParameterValue -> ParameterValue
 	end.


%% Replace the ParameterName record in the ParameterList with the NewParameterValue
%% Return the updated ParameterList
%% This works on all types of value parameter value lists, Configs, Inputs, Outputs, and Internals
replace_parameter_value(ParameterList, ParameterName, NewParameterValue) ->
	% ParameterName is always the first element in the tuple, regardless of the ParameterValue type
	lists:keyreplace(ParameterName, 1, ParameterList, NewParameterValue).


%% Add a new parameter value, {name, value} tuple, 
%% to the end of the given parameter list and return a new list
%% This works on all types of parameter value lists, Configs, Inputs, Outputs, and Internals
add_parameter_value(ParameterList, NewParameterValue) ->
    ParameterList ++ [NewParameterValue].

    
%% common delay function
sleep(T) ->
	receive
	after T -> ok
	end.


	
%% ====================================================================
%% Internal functions
%% ====================================================================
