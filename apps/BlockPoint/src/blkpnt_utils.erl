%% @author Mark Sebald
%% @doc Common block utility functions


-module(blkpnt_utils).


%% ====================================================================
%% API functions
%% ====================================================================
-export([get_param_value/2, get_input_value/2, get_output_value/2, get_interm_value/2, get_value/2]).
-export([set_input_value/3, set_output_value/3, set_interm_value/3, set_value/3]).
-export([add_connection/3, set_input_pointer/3, set_input_pointer_value/5]).
-export([update_common_outputs/3, get_value_record/2, replace_value_record/3]).
-export([sleep/1]). 

%% Get the Value of ValueName 
%% Return 'not_found', if ValueName is not found in the Values list
get_param_value(Params, ValueName) ->
	case get_value_record(Params, ValueName) of
		not_found ->
			io:format("get_value() Error: ~p not found in Param values~n", [ValueName]),
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

get_interm_value(Interms, ValueName) ->
	case get_value_record(Interms, ValueName) of
		not_found -> 
			io:format("get_value() Error: ~p not found in Interm values~n", [ValueName]),
			not_found;
		{ValueName, Value} -> Value
 	end.
	
get_value(BlockValues, ValueName)->
	
	{BlockName, _BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	
	case get_value_record(Params, ValueName) of
		not_found ->
			case get_value_record(Inputs, ValueName) of
				not_found ->
					case get_value_record(Outputs, ValueName) of
						not_found ->
							case get_value_record(Interms, ValueName) of
								not_found ->
									io:format("~p get_value() Error: ~p not found in Block values~n", [BlockName, ValueName]),
									not_found;
								{ValueName, Value} -> Value	% IntTerm value
							end;
						{ValueName, Value, _Connections} -> Value	% Output value
					end;
				{ValueName, Value, _Pointer} -> Value	% Input value
			end; 
		{ValueName, Value} -> Value   % Parameter value
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

set_interm_value(Interms, ValueName, NewValue) ->
	case get_value_record(Interms, ValueName) of
		not_found ->
			io:format("set_value() Error: ~p not found in Interm values", [ValueName]),
			Interms;
		{ValueName, _OldValue} ->
			NewIntTerm = {ValueName, NewValue},
			NewIntTerms = replace_value_record(Interms, ValueName, NewIntTerm),
			NewIntTerms
	end.	

set_value(BlockValues, ValueName, NewValue)->
	
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	
	% Can't modify Params, don't bother checking those

	case get_value_record(Inputs, ValueName) of
		not_found ->
			case get_value_record(Outputs, ValueName) of
				not_found ->					
					case get_value_record(Interms, ValueName) of
						not_found ->
							io:format("~p set_value() Error. ~p not found in the BlockValues list~n", [BlockName, ValueName]),
							not_found;
						{ValueName, _OldValue1} ->
							NewIntTerm = {ValueName, NewValue},
							NewIntTerms = replace_value_record(Interms, ValueName, NewIntTerm),
							{BlockName, BlockModule, Params, Inputs, Outputs, NewIntTerms}
					end;
				{ValueName, _OldValue2, Connections} -> 
					NewOutput = {ValueName, NewValue, Connections},
					NewOutputs = replace_value_record(Outputs, ValueName, NewOutput),
					{BlockName, BlockModule, Params, Inputs, NewOutputs, Interms}
			end; 
		{ValueName, _OldValue3, Pointer} -> 
			NewInput = {ValueName, NewValue, Pointer},
			NewInputs = replace_value_record(Inputs, ValueName, NewInput),
			{BlockName, BlockModule, Params, NewInputs, Outputs, Interms}
 	end.


%% Update the input pointer for the input value 'ValueName'
set_input_pointer(BlockValues, ValueName, NewPointer) ->
	
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	
	case get_value_record(Inputs, ValueName) of
		not_found ->
			io:format("~p set_input_pointer() Error.  ~p not found in Input values.~n", [BlockName, ValueName]),
			% Input value not found, just return the BlockValues unchanged
			BlockValues;
		{ValueName, Value, _Pointer} ->
			NewInput = {ValueName, Value, NewPointer},
			NewInputs = replace_value_record(Inputs, ValueName, NewInput),
			{BlockName, BlockModule, Params, NewInputs, Outputs, Interms}
	end.


%% Update the value of every input in this block, pointing at the 'ValueName:FromBlockName:NodeName' output value
%% Return the updated list of Input values
set_input_pointer_value(BlockValues, NewValueName, FromBlockName, NodeName, NewValue) ->
	
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,

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
	
	{BlockName, BlockModule, Params, NewInputs, Outputs, Interms}.


%% Add a connection 'ToBlockName' to the connection list of the value record of the given ValueName
add_connection(BlockValues, ValueName, ToBlockName) ->
	
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	 
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
				{BlockName, BlockModule, Params, Inputs, NewOutputs, Interms}
			end;
		Unknown ->
			io:format("~p add_connection() Error. Unknown output value record  ~p~n", [BlockName, Unknown]),
			BlockValues
	end.

%% Update the Output values common to all blocks, Value, Status, Execute Count, Last Executed Time
update_common_outputs(Outputs, Value, Status) ->	
	NewOutputs1 = set_output_value(Outputs, 'Value', Value),
	NewOutputs2 = set_output_value(NewOutputs1, 'Status', Status),
	NewOutputs3 = update_exec_count(NewOutputs2),
	set_output_value(NewOutputs3, 'LastExec', calendar:now_to_local_time(erlang:timestamp())).

update_exec_count(Outputs) ->
	% Arbitrarily roll over Execute Counter at 1,000,000,000
	case get_output_value(Outputs, 'ExecCount') + 1 of
		1000000000   -> set_output_value(Outputs, 'ExecCount', 0);
		NewExecCount -> set_output_value(Outputs, 'ExecCount', NewExecCount)
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


%% common delay function
sleep(T) ->
	receive
	after T -> ok
	end.


	
%% ====================================================================
%% Internal functions
%% ====================================================================
