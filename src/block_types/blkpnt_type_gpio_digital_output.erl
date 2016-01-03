%% @author Mark Sebald
%% @doc Raspberry Pi GPIO Digital Output block function


-module(blkpnt_type_gpio_digital_output).


%% ====================================================================
%% API functions
%% ====================================================================
-export([initiate/1, evaluate/1, terminate/1, create_values/1, type_name/0, version/0]).

initiate(BlockValues) ->
	{BlockName, _BlockModule, Params, _Inputs, _Outputs, _IntTerms} = BlockValues,
	io:format("~p Type GPIO Digital Output initiate() ~n", [BlockName]),
	
	PinNumber = blkpnt_utils:get_param_value(Params, 'GpioPinNumber'),
	DefaultValue = blkpnt_utils:get_param_value(Params, 'DefaultValue'),
	
	PinNumberStr = integer_to_list(PinNumber),
	
	pi_gpio:set_pin_direction(PinNumberStr, "out"),
	
	case DefaultValue of
		true  -> pi_gpio:set_pin_value(PinNumberStr, "1");
		false -> pi_gpio:set_pin_value(PinNumberStr, "0")
	end,
	
	BlockValues.

evaluate(BlockValues)->
	
	{BlockName, _BlockModule, Params, Inputs, _Outputs, _Interms} = BlockValues,
	
	%io:format("~p Type: Pi GPIO Digital Output, evaluate() ~n", [BlockName]),
	
	case blkpnt_utils:get_input_value(Inputs, 'Enable') of
		true ->
			PinNumber = blkpnt_utils:get_param_value(Params, 'GpioPinNumber'),
			PinNumberStr = integer_to_list(PinNumber),
			
			Input = blkpnt_utils:get_input_value(Inputs, 'Input'),
	
			% Set Output Val to input and set the actual GPIO pin value too
			case Input of
				empty -> 
					pi_gpio:set_pin_value(PinNumberStr, "0"), % TODO: Set pin to default value or input? 
					update_values(BlockValues, not_active, normal);
					
				not_active ->
					pi_gpio:set_pin_value(PinNumberStr, "0"), % TODO: Set pin to default value or input? 
					update_values(BlockValues, not_active, normal);
					
				true ->  
					pi_gpio:set_pin_value(PinNumberStr, "1"),
					update_values(BlockValues, true, normal);
					
				false ->
					pi_gpio:set_pin_value(PinNumberStr, "0"),
					update_values(BlockValues, false, normal);
					
				Other -> 
					pi_gpio:set_pin_value(PinNumberStr, "0"), % TODO: Set pin to default value or input? 
					io:format("~p Error: Invalid input value: ~p~n", [BlockName, Other]),
					update_values(BlockValues, not_active, error)
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
	io:format("~p Type: GPIO Digital Output, terminate() ~n", [BlockName]).


create_values(BlockName)->
	BlockValues = blkpnt_setup:common_values(BlockName, ?MODULE),
	
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
		
	NewParams = [{'GpioPinNumber', 17} | [{'DefaultValue', true} | Params]],
	
	NewInputs = [{'Input', empty, {null, null, null}} | Inputs],
	
	{BlockName, BlockModule, NewParams, NewInputs, Outputs, Interms}.

	
type_name()-> 'PiGpioDigitalOutput'.

version() -> "0.1.0".

%% ====================================================================
%% Internal functions
%% ====================================================================


