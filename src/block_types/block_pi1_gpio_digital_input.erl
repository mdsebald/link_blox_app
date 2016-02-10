%%
%% @author Mark Sebald
%% @doc GPIO Digital Input Block Type 
%% 
%% 

-module(block_pi1_gpio_digital_input).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/3, initialize/1, execute/1, delete/1]).

%%
%% Create a set of block values for this block type.  
%% Any Config, Input, Output, or Internal parameters 
%% not already defined in the set of common block values, 
%% will be created here and intialized to their default values.  
%% Initial Config and Input values are set here.
%%   
create(BlockName, InitConfigs, InitInputs)->

    % Create an initial set of common block values
	{CommonConfigs, CommonInputs, CommonOutputs, CommonInternals} = 
                             block_common:create(BlockName, type_name(), version()),
	
    % Create any Config, Input, Output, and/or Internal parameters
    % specific for this block type and intialize them to their default values
    
    Configs = block_utils:merge_parameter_lists(CommonConfigs, InitConfigs),
    Inputs = block_utils:merge_parameter_lists(CommonInputs, InitInputs), 
    Outputs = CommonOutputs,
    Internals = CommonInternals,

    % This is the block state, 
	{BlockName, ?MODULE, Configs, Inputs, Outputs, Internals}.


%% 
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
%%
initialize({BlockName, BlockModule, Configs, Inputs, Outputs, Internals}) ->

    % Perform common block initializations
    % Non-Common Internal values are created here
    {InitOutputs, InitInternals} = block_common:initialize(Configs, Outputs, Internals),
    
	PinNumber = block_utils:get_config_value(Configs, 'GpioPinNumber'),

    % Perform block type specific initializations here, and update the state variables
    case gpio:start_link(PinNumber, input) of
        {ok, GpioPin} ->
	        NewInternals = block_utils:merge_parameter_lists(InitInternals, [{'GpioPinRef', GpioPin}]),
            gpio:register_int(GpioPin),
            gpio:set_int(GpioPin, both);  % TODO: Make interrupt type selectable via config value

        {error, ErrorResult} ->
            io:format("~p Error intitiating GPIO pin; ~p", [ErrorResult, PinNumber]),
            NewInternals = InitInternals
    end,
	
	{BlockName, BlockModule, Configs, Inputs, InitOutputs, NewInternals}.


%%
%%  Execute the block specific functionality
%%
execute({BlockName, BlockModule, Configs, Inputs, Outputs, Internals}) ->

    % Always check if block is enabled first
	case block_utils:get_input_value(Inputs, 'Enable') of
		true ->
		    % Perform block type specific actions here, calculate new outut value(s)
            GpioPin = block_utils:get_internal_value(Internals, 'GpioPinRef'),
            DigitalInputValue = read_pin_value_bool(GpioPin),

            % Perform common execute function for normally executing block
            {NewOutputs, NewInternals} = block_common:execute(Configs, Outputs, Internals, DigitalInputValue, normal); 

		false ->	% Block is Disabled, perform common execute function for a disabled block 
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

type_name()-> 'Template'.

version() -> "0.1.0".


read_pin_value_bool(GpioPin) ->
    case gpio:read(GpioPin) of
        1  -> true;
		0 -> false
    end.