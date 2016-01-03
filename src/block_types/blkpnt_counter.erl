%% @author Mark Sebald
%% @doc Counter block function


-module(blkpnt_counter).


%% ====================================================================
%% API functions
%% ====================================================================
-export([initiate/1, evaluate/1, terminate/1, create_values/1, type_name/0, version/0]).


initiate(BlockValues) ->
	{BlockName, BlockModule, Params, Inputs, Outputs, _Interms} = BlockValues,
	
	io:format("~p Type: Counter, initiate() ~n", [BlockName]),
	
	% Setup Intermediate Terms value storage here, use same {ValueName, Value} tuple form as other block value types.
	NewInterms = [{'LastTrigger', false}, {'LastReset', false}],
	
	{BlockName, BlockModule, Params, Inputs, Outputs, NewInterms}.


evaluate(BlockValues) ->
	
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,

	%io:format("~p Type: Counter, evaluate() ~n", [BlockName]),

	case blkpnt_utils:get_input_value(Inputs, 'Enable') of
		true ->
			Count = blkpnt_utils:get_output_value(Outputs, 'Value'),
	
			Reset = blkpnt_utils:get_input_value(Inputs, 'Reset'),
			LastReset = blkpnt_utils:get_interm_value(Interms, 'LastReset'),
	
			Trigger = blkpnt_utils:get_input_value(Inputs, 'Trigger'),
			LastTrigger = blkpnt_utils:get_interm_value(Interms, 'LastTrigger'),

			case (Reset == true) andalso (LastReset == false) of
				true ->
					% Reset input toggled, NewCount = 0, NewLastReset = true, NewLastTrigger = Trigger input val 
					update_values(BlockValues, 0, true, Trigger);
				false ->
					% Reset input not toggled
					case (Trigger == true) andalso (LastTrigger == false) of
						true ->
							% Trigger input toggled, NewCount = Count+1, NewLastReset = Reset input val, NewLastTrigger = Trigger input val 
							update_values(BlockValues, Count+1, Reset, true);
						false ->
							% Trigger input not toggled, NewCount = Count, NewLastReset = Reset input val, NewLastTrigger = Trigger input val 
							update_values(BlockValues, Count, Reset, Trigger)
					end
			end;
		false ->	% Block is Disabled
			NewOutputs = blkpnt_utils:update_common_outputs(Outputs, not_active, disabled),
			{BlockName, BlockModule, Params, Inputs, NewOutputs, Interms}
	end.

% Normal update values
update_values(BlockValues, NewCount, NewLastReset, NewLastTrigger) ->
	% TODO: Could do further checking to determine which values have not changed and do not need to be updated
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	NewInterms1 = blkpnt_utils:set_interm_value(Interms, 'LastReset', NewLastReset),
	NewInterms2 = blkpnt_utils:set_interm_value(NewInterms1, 'LastTrigger', NewLastTrigger),
	NewOutputs = blkpnt_utils:update_common_outputs(Outputs, NewCount, normal),
	{BlockName, BlockModule, Params, Inputs, NewOutputs, NewInterms2}.

	
terminate(BlockValues) ->
	{BlockName, _BlockModule, _Params, _Inputs, _Outputs, _IntTerms} = BlockValues,
	io:format("~p Type: Counter, terminate() ~n", [BlockName]).


create_values(BlockName)->
	BlockValues = blkpnt_setup:common_values(BlockName, ?MODULE),
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	
	NewInputs = [{'Trigger', false, {fixed, null, null}} | [{'Reset', false, {fixed, null, null}} | Inputs]],

	{BlockName, BlockModule, Params, NewInputs, Outputs, Interms}.
 


type_name()-> 'Counter'.

version() -> "0.1.0".

%% ====================================================================
%% Internal functions
%% ====================================================================
