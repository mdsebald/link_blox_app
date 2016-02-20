%%% @doc 
%%% Block Type: Pi 1 GPIO Digital Input 
%%% Description: Configure a Raspberry Pi 1 GPIO Pin as a Digital Input block
%%%               
%%% @end

-module(block_pi1_gpio_digital_input).

-author("Mark Sebald").

-include("../block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/3, initialize/1, execute/1, delete/1]).

%%
%% Create a set of block values for this block type.  
%% Any Config, Input, Output, or Private attributes 
%% not already defined in the set of common block values, 
%% will be created here and intialized to their default values.  
%% Initial Config and Input values are set here.
%%   
create(BlockName, InitConfig, InitInputs)->

    % Create an initial set of common block values
	{CommonConfig, CommonInputs, CommonOutputs, CommonPrivate} = 
                             block_common:create(BlockName, type_name(), version()),
	
    % Create any Config, Input, Output, and/or Private attributes
    % specific for this block type and intialize them to their default values
    
    Config = block_utils:merge_attribute_lists(CommonConfig, InitConfig),
    Inputs = block_utils:merge_attribute_lists(CommonInputs, InitInputs), 
    Outputs = CommonOutputs,
    Private = CommonPrivate,

    % This is the block state, 
	{BlockName, ?MODULE, Config, Inputs, Outputs, Private}.


%% 
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

    % Perform common block initializations
    % Non-Common Private values are created here
    {InitOutputs, InitPrivate} = block_common:initialize(Config, Outputs, Private),
    
	PinNumber = block_utils:get_config_value(Config, gpio_pin),

    % Perform block type specific initializations here, and update the state variables
    case gpio:start_link(PinNumber, input) of
        {ok, GpioPin} ->
	        NewPrivate = block_utils:merge_attribute_lists(InitPrivate, [{gpio_pin_ref, GpioPin}]),
            gpio:register_int(GpioPin),
            gpio:set_int(GpioPin, both);  % TODO: Make interrupt type selectable via config value

        {error, ErrorResult} ->
            io:format("~p Error intitiating GPIO pin; ~p", [ErrorResult, PinNumber]),
            NewPrivate = InitPrivate
    end,
	
	{BlockName, BlockModule, Config, Inputs, InitOutputs, NewPrivate}.


%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

    % Always check if block is enabled first
	case block_utils:get_input_value(Inputs, enable) of
		true ->
		    % Perform block type specific actions here, calculate new outut value(s)
            GpioPin = block_utils:get_private_value(Private, gpio_pin_ref),
            DigitalInputValue = read_pin_value_bool(GpioPin),

            % Perform common execute function for normally executing block
            {NewOutputs, NewPrivate} = block_common:execute(Config, Outputs, Private, DigitalInputValue, normal); 

		false ->	% Block is Disabled, perform common execute function for a disabled block 
			{NewOutputs, NewPrivate} = block_common:execute(Config, Outputs, Private, not_active, disabled) 
	end,
    
    {BlockName, BlockModule, Config, Inputs, NewOutputs, NewPrivate}.


%% 
%%  Delete the block
%%	
    
delete({_BlockName, _BlockModule, Config, _Inputs, _Outputs, Private}) ->
	block_common:delete(Config, Private).
    % Perform any other block type specific delete functionality here



%% ====================================================================
%% Internal functions
%% ====================================================================

type_name()-> pi1_gpio_digital_input.

version() -> "0.1.0".


read_pin_value_bool(GpioPin) ->
    case gpio:read(GpioPin) of
        1  -> true;
		0 -> false
    end.