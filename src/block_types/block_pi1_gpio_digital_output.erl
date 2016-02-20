%%% @doc 
%%% Block Type: Pi 1 GPIO Digital Output 
%%% Description: Configure a Raspberry Pi 1 GPIO Pin as a Digital Output block
%%%               
%%% @end 

-module(block_pi1_gpio_digital_output).

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
                             
    % TODO: Still need to create Config: default value, Input: input value, 
    % In case they are not created and set by the InitConfig and InitInputs attributes
    
    Config = block_utils:merge_attribute_lists(CommonConfig, InitConfig),
    Inputs = block_utils:merge_attribute_lists(CommonInputs, InitInputs), 
    Outputs = CommonOutputs,
    Private = CommonPrivate,

    % This is the block state, 
	{BlockName, ?MODULE, Config, Inputs, Outputs, Private}.


%% 
%% Initialize block values before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

    % Perform common block initializations
    % Non-common Private values are created here
    {InitOutputs, InitPrivate} = block_common:initialize(Config, Outputs, Private),
	
	PinNumber = block_utils:get_config_value(Config, gpio_pin),
	DefaultValue = block_utils:get_config_value(Config, default_value),
	    
    case gpio:start_link(PinNumber, output) of
        {ok, GpioPin} ->
 	        NewPrivate = block_utils:merge_attribute_lists(InitPrivate, [{gpio_pin_ref, GpioPin}]),
            set_pin_value_bool(GpioPin, DefaultValue);
            
        {error, ErrorResult} ->
            io:format("~p Error intitiating GPIO pin; ~p", [ErrorResult, PinNumber]),
            NewPrivate = Private
    end,

	{BlockName, BlockModule, Config, Inputs, InitOutputs, NewPrivate}.

%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

	%io:format("~p Type: Pi GPIO Digital Output, evaluate() ~n", [BlockName]),
    GpioPin = block_utils:get_private_value(Private, gpio_pin_ref),
    DefaultValue = block_utils:get_config_value(Config, default_value),

    % Always check if block is enabled first
	case block_utils:get_input_value(Inputs, enable) of
		true ->
			Input = block_utils:get_input_value(Inputs, input),
 	
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
            {NewOutputs, NewPrivate} = block_common:execute(Config, Outputs, Private, Value, Status); 

		false ->	% Block is Disabled, set GPIO pin to default value
            set_pin_value_bool(GpioPin, DefaultValue),
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

set_pin_value_bool(GpioPin, BoolValue) ->
    case BoolValue of
        true  -> gpio:write(GpioPin, 1);
		false -> gpio:write(GpioPin, 0)
    end.

type_name()-> pi1_gpio_digital_output.

version() -> "0.1.0".