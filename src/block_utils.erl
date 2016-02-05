%% @author Mark Sebald
%% @doc Common block utility functions


-module(block_utils).


%% ====================================================================
%% API functions
%% ====================================================================
-export([get_config_value/2, get_input_value/2, get_output_value/2, get_internal_value/2, get_value/2]).
-export([set_input_value/3, set_output_value/3, set_internal_value/3, set_value/3]).
-export([add_connection/3, set_input_pointer/3, set_input_pointer_value/5]).
-export([get_value_record/2, replace_value_record/3, add_parameter/2]).
-export([sleep/1]). 

%% Get the Value of ValueName 
%% Return 'not_found', if ValueName is not found in the Values list
get_config_value(Configs, ValueName) ->
	case get_value_record(Configs, ValueName) of
		not_found ->
			io:format("get_value() Error: ~p not found in Config values~n", [ValueName]),
			not_found;
		{ValueName, Value} ->  Value
 	end.

get_input_value(Inputs, ValueName) ->
	case get_value_record(Inputs, ValueName) of
		not_found ->
			io:format("get_value() Error: ~p not found in Input values~n", [ValueName]),
			not_found;
		{ValueName, Value, _Pointer} -> Value
 	end.

get_output_value(Outputs, ValueName) ->
	case get_value_record(Outputs, ValueName) of
		not_found -> 
			io:format("get_value() Error: ~p not found in Output values~n", [ValueName]),
			not_found;
		{ValueName, Value, _Connections} -> Value
 	end.

get_internal_value(Internals, ValueName) ->
	case get_value_record(Internals, ValueName) of
		not_found -> 
			io:format("get_value() Error: ~p not found in Internal values~n", [ValueName]),
			not_found;
		{ValueName, Value} -> Value
 	end.
	
get_value(BlockValues, ValueName)->
	
	{BlockName, _BlockModule, Configs, Inputs, Outputs, Internals} = BlockValues,
	
	case get_value_record(Configs, ValueName) of
		not_found ->
			case get_value_record(Inputs, ValueName) of
				not_found ->
					case get_value_record(Outputs, ValueName) of
						not_found ->
							case get_value_record(Internals, ValueName) of
								not_found ->
									io:format("~p get_value() Error: ~p not found in Block values~n", [BlockName, ValueName]),
									not_found;
								{ValueName, Value} -> Value	% Internal value
							end;
						{ValueName, Value, _Connections} -> Value	% Output value
					end;
				{ValueName, Value, _Pointer} -> Value	% Input value
			end; 
		{ValueName, Value} -> Value   % Config value
 	end.


% TODO: Update multiple values, with a list of ValueName, Value pairs

%% Return BlockValues list with the Value of ValueName updated 
%% Return original values list if value 'ValueName' not found 
set_input_value(Inputs, ValueName, NewValue) ->
	case get_value_record(Inputs, ValueName) of
		not_found ->
			io:format("set_value() Error: ~p not found in Input values", [ValueName]),
			Inputs;
		{ValueName, _OldValue, Pointer} ->
			NewInput = {ValueName, NewValue, Pointer},
			NewInputs = replace_value_record(Inputs, ValueName, NewInput),
			NewInputs
	end.

set_output_value(Outputs, ValueName, NewValue) ->
	case get_value_record(Outputs, ValueName) of
		not_found ->
			io:format("set_value() Error: ~p not found in Output values", [ValueName]),
			Outputs;
		{ValueName, _OldValue, Connections} ->
			NewOutput = {ValueName, NewValue, Connections},
			NewOutputs = replace_value_record(Outputs, ValueName, NewOutput),
			NewOutputs
	end.	

set_internal_value(Internals, ValueName, NewValue) ->
	case get_value_record(Internals, ValueName) of
		not_found ->
			io:format("set_value() Error: ~p not found in Internal values", [ValueName]),
			Internals;
		{ValueName, _OldValue} ->
			NewInternal = {ValueName, NewValue},
			NewInternals = replace_value_record(Internals, ValueName, NewInternal),
			NewInternals
	end.	

set_value(BlockValues, ValueName, NewValue)->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Internals} = BlockValues,
	
	% Can't modify Congigs, don't bother checking those

	case get_value_record(Inputs, ValueName) of
		not_found ->
			case get_value_record(Outputs, ValueName) of
				not_found ->					
					case get_value_record(Internals, ValueName) of
						not_found ->
							io:format("~p set_value() Error. ~p not found in the BlockValues list~n", [BlockName, ValueName]),
							not_found;
						{ValueName, _OldValue1} ->
							NewInternal = {ValueName, NewValue},
							NewInternals = replace_value_record(Internals, ValueName, NewInternal),
							{BlockName, BlockModule, Configs, Inputs, Outputs, NewInternals}
					end;
				{ValueName, _OldValue2, Connections} -> 
					NewOutput = {ValueName, NewValue, Connections},
					NewOutputs = replace_value_record(Outputs, ValueName, NewOutput),
					{BlockName, BlockModule, Configs, Inputs, NewOutputs, Internals}
			end; 
		{ValueName, _OldValue3, Pointer} -> 
			NewInput = {ValueName, NewValue, Pointer},
			NewInputs = replace_value_record(Inputs, ValueName, NewInput),
			{BlockName, BlockModule, Configs, NewInputs, Outputs, Internals}
 	end.


%% Update the input pointer for the input value 'ValueName'
set_input_pointer(BlockValues, ValueName, NewPointer) ->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Internals} = BlockValues,
	
	case get_value_record(Inputs, ValueName) of
		not_found ->
			io:format("~p set_input_pointer() Error.  ~p not found in Input values.~n", [BlockName, ValueName]),
			% Input value not found, just return the BlockValues unchanged
			BlockValues;
		{ValueName, Value, _Pointer} ->
			NewInput = {ValueName, Value, NewPointer},
			NewInputs = replace_value_record(Inputs, ValueName, NewInput),
			{BlockName, BlockModule, Configs, NewInputs, Outputs, Internals}
	end.


%% Update the value of every input in this block, pointing at the 'ValueName:FromBlockName:NodeName' output value
%% Return the updated list of Input values
set_input_pointer_value(BlockValues, NewValueName, FromBlockName, NodeName, NewValue) ->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Internals} = BlockValues,

	TargetPointer = {NewValueName, FromBlockName, NodeName},

	% Update the value of each input record pointing at the given value 
	NewInputs = lists:map(
		fun(Input) -> 
			{ValueName, _Value, Pointer} = Input,
			case Pointer =:= TargetPointer of
				true  -> {ValueName, NewValue, Pointer};
				false -> Input	% This block input is not pointing at the target block output, don't change the input value 
			end
		end, 
		Inputs),
	
	{BlockName, BlockModule, Configs, NewInputs, Outputs, Internals}.


%% Add a connection 'ToBlockName' to the connection list of the value record of the given ValueName
add_connection(BlockValues, ValueName, ToBlockName) ->
	
	{BlockName, BlockModule, Configs, Inputs, Outputs, Internals} = BlockValues,
	 
	case get_value_record(Outputs, ValueName) of
		
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
				NewOutputs = replace_value_record(Outputs, ValueName, NewOutput),
				{BlockName, BlockModule, Configs, Inputs, NewOutputs, Internals}
			end;
		Unknown ->
			io:format("~p add_connection() Error. Unknown output value record  ~p~n", [BlockName, Unknown]),
			BlockValues
	end.

	
%% Get the value record for the given ValueName
%% This works on all types of value record lists, Params, Inputs, and Outputs
get_value_record(ValueRecords, ValueName) ->
	% ValueName is always the first element in the tuple, regardless of the ValueRecord type
	case lists:keyfind(ValueName, 1, ValueRecords) of 
		      false -> not_found;
		ValueRecord -> ValueRecord
 	end.


%% Replace the ValueName record in the ValueRecords list with the NewValueRecord
%% Return the updated ValueRecords list
%% This works on all types of value record lists, Params, Inputs, and Outputs
replace_value_record(ValueRecords, ValueName, NewValueRecord) ->
	% ValueName is always the first element in the tuple, regardless of the ValueRecord type
	lists:keyreplace(ValueName, 1, ValueRecords, NewValueRecord).


%% Add a new parameter tuple {name, value} to the end of the given parameter list
%% Use this for Config, Inputs, Outputs, and Internal lists of block parmeters

add_parameter(ParameterList, NewParameter) ->
    lists:reverse([NewParameter | lists:reverse(ParameterList)]).

    
%% common delay function
sleep(T) ->
	receive
	after T -> ok
	end.


	
%% ====================================================================
%% Internal functions
%% ====================================================================
