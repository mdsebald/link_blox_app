%% @author Mark Sebald
%% @doc Raspberry Pi1 GPIO Digital Input block function


-module(blkpnt_pi1_gpio_digital_input).


%% ====================================================================
%% API functions
%% ====================================================================
-export([initiate/1, evaluate/1, terminate/1, create_values/2, type_name/0, version/0]).

initiate(BlockValues) ->
	{BlockName, BlockModule, Params, Inputs, Outputs, _Interms} = BlockValues,
	io:format("~p Type Pi1 GPIO Digital Input initiate() ~n", [BlockName]),
	
	PinNumber = blkpnt_utils:get_param_value(Params, 'GpioPinNumber'),
	    
    case gpio:start_link(PinNumber, input) of
        {ok, GpioPin} ->
            % Setup Intermediate Terms value storage here, use same {ValueName, Value} tuple form as other block value types.
	        NewInterms = [{'GpioPinRef', GpioPin}],
            gpio:register_int(GpioPin);
            
        {error, ErrorResult} ->
            io:format("~p Error intitiating GPIO pin; ~p", [ErrorResult, PinNumber]),
            NewInterms = []
    end,
	
	{BlockName, BlockModule, Params, Inputs, Outputs, NewInterms}.


evaluate(BlockValues)->
	
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	
	%io:format("~p Type: Pi1 GPIO Digital Output, evaluate() ~n", [BlockName]),
    GpioPin = blkpnt_utils:get_interm_value(Interms, 'GpioPinRef'),
	
	case blkpnt_utils:get_input_value(Inputs, 'Enable') of
		true ->
            DigitalInputValue = read_pin_value_bool(GpioPin),
	
			% Set Output Value to the current digital input value
            NewOutputs = blkpnt_utils:update_common_outputs(Outputs, DigitalInputValue, normal);
            
		false ->  % Block is Disabled
			NewOutputs = blkpnt_utils:update_common_outputs(Outputs, not_active, disabled)
	end,
    
    {BlockName, BlockModule, Params, Inputs, NewOutputs, Interms}.
     

read_pin_value_bool(GpioPin) ->
    case gpio:read(GpioPin) of
        1  -> true;
		0 -> false
    end.


terminate(BlockValues) ->
	{BlockName, _BlockModule, _Params, _Inputs, _Outputs, Interms} = BlockValues,
	io:format("~p Type: Pi1 GPIO Digital Input, terminate() ~n", [BlockName]),
    gpio:stop(blkpnt_utils:get_interm_value(Interms, 'GpioPinRef')).


create_values(BlockName, PinNumber)->
	BlockValues = blkpnt_setup:common_values(BlockName, ?MODULE),
	
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
		
	NewParams = [{'GpioPinNumber', PinNumber} | Params],
	
	{BlockName, BlockModule, NewParams, Inputs, Outputs, Interms}.

	
type_name()-> 'Pi1GpioDigitalInput'.

version() -> "0.1.0".

%% ====================================================================
%% Internal functions
%% ====================================================================


