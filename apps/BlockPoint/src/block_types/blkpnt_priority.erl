%% @author Mark Sebald
%% @doc Select Priority Input block function


-module(blkpnt_priority).


%% ====================================================================
%% API functions
%% ====================================================================
-export([initiate/1, evaluate/1, terminate/1, create_values/1, type_name/0, version/0]).

initiate(BlockValues) ->
	{BlockName, _BlockModule, _Params, _Inputs, _Outputs, _Interms} = BlockValues,
	io:format("~p Type Priority initiate() ~n", [BlockName]),
	BlockValues.

evaluate(BlockValues)->
	
	{_BlockName, _BlockModule, _Params, Inputs, _Outputs, _Interms} = BlockValues,
	
	%io:format("~p Type: block_priority, evaluate() ~n", [BlockName]),
	
	case blkpnt_utils:get_input_value(Inputs, 'Enable') of
		true ->
			InputValue1 = blkpnt_utils:get_input_value(Inputs, 'InputVal1'),
			InputValue2 = blkpnt_utils:get_input_value(Inputs, 'InputVal2'),

			% Set OutputVal to the first non-empty, non-not_active value of all of the inputs
			case InputValue1 of
				empty -> 
					case InputValue2 of
						empty -> update_values(BlockValues, not_active, normal);
						not_active -> update_values(BlockValues, not_active, normal);
						Other -> update_values(BlockValues, Other, normal)
					end;
				not_active ->
					case InputValue2 of
						empty -> update_values(BlockValues, not_active, normal);
						not_active -> update_values(BlockValues, not_active, normal);
						Other -> update_values(BlockValues, Other, normal)
					end;
				Other ->  
					update_values(BlockValues, Other, normal)
			end;			
		false ->  % Block is Disabled
			update_values(BlockValues, not_active, disabled)
	end.


update_values(BlockValues, NewOutputValue, NewStatus) ->
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	NewOutputs = blkpnt_utils:update_common_outputs(Outputs, NewOutputValue, NewStatus),
	{BlockName, BlockModule, Params, Inputs, NewOutputs, Interms}.

terminate(BlockValues) ->
	{BlockName, _BlockModule, _Params, _Inputs, _Outputs, _Interms} = BlockValues,
	io:format("~p Type: Priority, terminate() ~n", [BlockName]).


create_values(BlockName)->
	BlockValues = blkpnt_setup:common_values(BlockName, ?MODULE),
		
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	
	NewInputs = [{'InputVal1', empty, {null, null, null}} | [{'InputVal2', empty, {null, null, null}} | Inputs]],

	{BlockName, BlockModule, Params, NewInputs, Outputs, Interms}.


type_name()-> 'Priority'.

version() -> "0.1.0".

%% ====================================================================
%% Internal functions
%% ====================================================================


