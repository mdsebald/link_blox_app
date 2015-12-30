%% @author Mark Sebald
%% @doc Inverter block function


-module(blkpnt_inverter).


%% ====================================================================
%% API functions
%% ====================================================================
-export([initiate/1, evaluate/1, terminate/1, create_values/1, type_name/0, version/0]).

initiate(BlockValues) ->
	{BlockName, _BlockModule, _Params, _Inputs, _Outputs, _Interms} = BlockValues,
	io:format("~p Type: Inverter, initiate() ~n", [BlockName]),
	BlockValues.


evaluate(BlockValues)-> 
	
	{BlockName, _BlockModule, _Params, Inputs, _Outputs, _Interms} = BlockValues,

	%io:format("~p Type: Inverter, evaluate() ~n", [BlockName]),

	case blkpnt_utils:get_input_value(Inputs, 'Enable') of
		true ->
			InputValue = blkpnt_utils:get_input_value(Inputs, 'InputVal'),
	
			case InputValue of		
				true       -> update_values(BlockValues, false, normal);
				false      -> update_values(BlockValues, true, normal);
				empty      -> update_values(BlockValues, not_active, normal);
				not_active -> update_values(BlockValues, not_active, normal);
				not_found  ->  
					io:format("~p: Error. ~p value not found~n", [BlockName, 'InputVal']),
					update_values(BlockValues, not_active, error); 
				_Other     -> 
					io:format("~p: Error. Invalid input value: ~p~n", [BlockName, InputValue]),
					update_values(BlockValues, not_active, error)
			end;			
		false -> %Block is Disabled
			update_values(BlockValues, not_active, disabled)
	end.

update_values(BlockValues, NewOutputValue, NewStatus) ->
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	NewOutputs = blkpnt_utils:update_common_outputs(Outputs, NewOutputValue, NewStatus),
	{BlockName, BlockModule, Params, Inputs, NewOutputs, Interms}.

	
terminate(BlockValues) ->
	{BlockName, _BlockModule, _Params, _Inputs, _Outputs, _Interms} = BlockValues,
	io:format("~p Type: Inverter, terminate() ~n", [BlockName]).


create_values(BlockName)->
	BlockValues = blkpnt_setup:common_values(BlockName, ?MODULE),
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	
	NewInputs = [{'InputVal', empty, {null, null, null}} | Inputs],
	
	{BlockName, BlockModule, Params, NewInputs, Outputs, Interms}.


type_name()-> 'Inverter'.

version() -> "0.1.0".

%% ====================================================================
%% Internal functions
%% ====================================================================


