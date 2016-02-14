%%
%% @author Mark Sebald
%% @doc Block Type: Digital Outputs 
%% Description: Configure a Raspberry Pi 1 GPIO Pin as a Digital Output block
%% 

-module(block_pi1_gpio_digital_output).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/3, initialize/1, execute/1, delete/1]).


%%
%% Create a set of block values for this block type.  
%% Any Config, Input, Output, or Internal attributes 
%% not already defined in the set of common block values, 
%% will be created here and intialized to their default values.  
%% Initial Config and Input values are set here.
%%   
create(BlockName, InitConfigs, InitInputs)->

    % Create an initial set of common block values
	{CommonConfigs, CommonInputs, CommonOutputs, CommonInternals} = 
                             block_common:create(BlockName, type_name(), version()),
                             
    % TODO: Still need to create Config: default value, Input: input value, 
    % In case they are not created and set by the InitConfigs and InitInputs attributes
    
    Configs = block_utils:merge_attribute_lists(CommonConfigs, InitConfigs),
    Inputs = block_utils:merge_attribute_lists(CommonInputs, InitInputs), 
    Outputs = CommonOutputs,
    Internals = CommonInternals,

    % This is the block state, 
	{BlockName, ?MODULE, Configs, Inputs, Outputs, Internals}.


%% 
%% Initialize block values before starting execution
%%
initialize({BlockName, BlockModule, Configs, Inputs, Outputs, Internals}) ->

    % Perform common block initializations
    % Non-common Internal values are created here
    {InitOutputs, InitInternals} = block_common:initialize(Configs, Outputs, Internals),
	
	PinNumber = block_utils:get_config_value(Configs, 'GpioPinNumber'),
	DefaultValue = block_utils:get_config_value(Configs, 'DefaultValue'),
	    
    case gpio:start_link(PinNumber, output) of
        {ok, GpioPin} ->
 	        NewInternals = block_utils:merge_attribute_lists(InitInternals, [{'GpioPinRef', GpioPin}]),
            set_pin_value_bool(GpioPin, DefaultValue);
            
        {error, ErrorResult} ->
            io:format("~p Error intitiating GPIO pin; ~p", [ErrorResult, PinNumber]),
            NewInternals = Internals
    end,

	{BlockName, BlockModule, Configs, Inputs, InitOutputs, NewInternals}.

%%
%%  Execute the block specific functionality
%%
execute({BlockName, BlockModule, Configs, Inputs, Outputs, Internals}) ->

	%io:format("~p Type: Pi GPIO Digital Output, evaluate() ~n", [BlockName]),
    GpioPin = block_utils:get_internal_value(Internals, 'GpioPinRef'),
    DefaultValue = block_utils:get_config_value(Configs, 'DefaultValue'),

    % Always check if block is enabled first
	case block_utils:get_input_value(Inputs, 'Enable') of
		true ->
			Input = block_utils:get_input_value(Inputs, 'Input'),
 	
			% Set Output Val to input and set the actual GPIO pin value too
			case Input of
				empty -> 
					PinValue = DefaultValue, % TODO: Set pin to default value or input? 
					Value = not_active,
                    Status = normal;
					
				not_active ->
					PinValue = DefaultValue, % TODO: Set pin to default value or input? 
					Value = not_active,
                    Status = normal;
					
				true ->  
					PinValue = true, 
					Value = true,
                    Status = normal;
					
				false ->
					PinValue = false,
					Value = false,
                    Status = normal;

				Other -> 
					io:format("~p Error: Invalid input value: ~p~n", [BlockName, Other]),
					PinValue = DefaultValue, % TODO: Set pin to default value or input? 
					Value = not_active,
                    Status = error
			end,
            set_pin_value_bool(GpioPin, PinValue),
            % Perform common execute function for normally executing block
            {NewOutputs, NewInternals} = block_common:execute(Configs, Outputs, Internals, Value, Status); 

		false ->	% Block is Disabled, set GPIO pin to default value
            set_pin_value_bool(GpioPin, DefaultValue),
			{NewOutputs, NewInternals} = block_common:execute(Configs, Outputs, Internals, not_active, disabled) 
	end,
    
    {BlockName, BlockModule, Configs, Inputs, NewOutputs, NewInternals}.


%% 
%%  Delete the block
%%	
    
delete({_BlockName, _BlockModule, Configs, _Inputs, _Outputs, Internals}) ->
	block_common:delete(Configs, Internals).
    % Perform any other block type specific delete functionality here



%% ====================================================================
%% Internal functions
%% ====================================================================

set_pin_value_bool(GpioPin, BoolValue) ->
    case BoolValue of
        true  -> gpio:write(GpioPin, 1);
		false -> gpio:write(GpioPin, 0)
    end.

type_name()-> 'Pi1GpioDigitalOutput'.

version() -> "0.1.0".