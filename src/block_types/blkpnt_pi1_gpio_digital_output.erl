%% @author Mark Sebald
%% @doc Raspberry Pi1 GPIO Digital Output block function


-module(blkpnt_pi1_gpio_digital_output).


%% ====================================================================
%% API functions
%% ====================================================================
-export([initiate/1, evaluate/1, terminate/1, create_values/3, type_name/0, version/0]).

initiate(BlockValues) ->
	{BlockName, BlockModule, Params, Inputs, Outputs, _Interms} = BlockValues,
	io:format("~p Type Pi1 GPIO Digital Output initiate() ~n", [BlockName]),
	
	PinNumber = blkpnt_utils:get_param_value(Params, 'GpioPinNumber'),
	DefaultValue = blkpnt_utils:get_param_value(Params, 'DefaultValue'),
	    
    case gpio:start_link(PinNumber, output) of
        {ok, GpioPin} ->
            % Setup Intermediate Terms value storage here, use same {ValueName, Value} tuple form as other block value types.
	        NewInterms = [{'GpioPinRef', GpioPin}],
            set_pin_value_bool(GpioPin, DefaultValue);
            
        {error, ErrorResult} ->
            io:format("~p Error intitiating GPIO pin; ~p", [ErrorResult, PinNumber]),
            NewInterms = []
    end,
	
	{BlockName, BlockModule, Params, Inputs, Outputs, NewInterms}.


evaluate(BlockValues)->
	
	{BlockName, _BlockModule, Params, Inputs, _Outputs, Interms} = BlockValues,
	
	%io:format("~p Type: Pi GPIO Digital Output, evaluate() ~n", [BlockName]),
    GpioPin = blkpnt_utils:get_interm_value(Interms, 'GpioPinRef'),
	
	case blkpnt_utils:get_input_value(Inputs, 'Enable') of
		true ->
			Input = blkpnt_utils:get_input_value(Inputs, 'Input'),
            DefaultValue = blkpnt_utils:get_param_value(Params, 'DefaultValue'),
	
			% Set Output Val to input and set the actual GPIO pin value too
			case Input of
				empty -> 
					set_pin_value_bool(GpioPin, DefaultValue), % TODO: Set pin to default value or input? 
					update_values(BlockValues, not_active, normal);
					
				not_active ->
					set_pin_value_bool(GpioPin,DefaultValue), % TODO: Set pin to default value or input? 
					update_values(BlockValues, not_active, normal);
					
				true ->  
					set_pin_value_bool(GpioPin, true),
					update_values(BlockValues, true, normal);
					
				false ->
					set_pin_value_bool(GpioPin, false),
					update_values(BlockValues, false, normal);
					
				Other -> 
					set_pin_value_bool(GpioPin, DefaultValue), % TODO: Set pin to default value or input? 
					io:format("~p Error: Invalid input value: ~p~n", [BlockName, Other]),
					update_values(BlockValues, not_active, error)
			end;
		false ->  % Block is Disabled
			update_values(BlockValues, not_active, disabled)
	end.


set_pin_value_bool(GpioPin, BoolValue) ->
    case BoolValue of
        true  -> gpio:write(GpioPin, 1);
		false -> gpio:write(GpioPin, 0)
    end.


update_values(BlockValues, NewOutputValue, NewStatus) ->
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	NewOutputs = blkpnt_utils:update_common_outputs(Outputs, NewOutputValue, NewStatus),
	{BlockName, BlockModule, Params, Inputs, NewOutputs, Interms}.


terminate(BlockValues) ->
	{BlockName, _BlockModule, _Params, _Inputs, _Outputs, Interms} = BlockValues,
	io:format("~p Type: Pi1 GPIO Digital Output, terminate() ~n", [BlockName]),
    gpio:stop(blkpnt_utils:get_interm_value(Interms, 'GpioPinRef')).


create_values(BlockName, PinNumber, DefaultValue)->
	BlockValues = blkpnt_setup:common_values(BlockName, ?MODULE),
	
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
		
	NewParams = [{'GpioPinNumber', PinNumber} | [{'DefaultValue', DefaultValue} | Params]],
	
	NewInputs = [{'Input', empty, {null, null, null}} | Inputs],
	
	{BlockName, BlockModule, NewParams, NewInputs, Outputs, Interms}.

	
type_name()-> 'Pi1GpioDigitalOutput'.

version() -> "0.1.0".

%% ====================================================================
%% Internal functions
%% ====================================================================


