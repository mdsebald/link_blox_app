%%% @doc 
%%% Block Type: Pi1 GPIO Digital Output
%%% Description: Configure a Raspberry Pi 1 GPIO Pin as a Digital Output block
%%%               
%%% @end 

-module(block_pi1_gpio_digital_output).

-author("Mark Sebald").

-include("../block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/1, create/3, create/5, initialize/1, execute/1, delete/1]).


type_name() -> pi1_gpio_digital_output.  

version() -> "0.1.0".   % Major.Minor.Patch, Major version change is a breaking change

  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
-spec create(BlockName :: atom()) -> block_state().

create(BlockName) -> create(BlockName, [], [], [], []).
   
create(BlockName, InitConfig, InitInputs) -> create(BlockName, InitConfig, InitInputs, [],[]).

create(BlockName, InitConfig, InitInputs, InitOutputs, InitPrivate)->
 
    error_logger:info_msg("Creating: ~p Type: ~p Version: ~s~n", [BlockName, type_name(), version()]),
    
    %% Update Default Config, Input, Output, and Private attribute values 
    %% with the initial values passed into this function.
    %%
    %% If any of the intial attributes do not already exist in the 
    %% default attribute lists, merge_attribute_lists() will create them.
    %% (This is useful for block types where the number of attributes is not fixed)
    
    Config = block_utils:merge_attribute_lists(default_configs(BlockName), InitConfig),
    Inputs = block_utils:merge_attribute_lists(default_inputs(), InitInputs), 
    Outputs = block_utils:merge_attribute_lists(default_outputs(), InitOutputs),
    Private = block_utils:merge_attribute_lists(default_private(), InitPrivate),

    % This is the block state, 
	{BlockName, ?MODULE, Config, Inputs, Outputs, Private}.
    
    
%%
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

    % Get the GPIO Pin number used for digital outputs 
	PinNumber = block_utils:get_value(Config, gpio_pin),
    % TODO: Check if Pin Number is an integer, and range
    
	DefaultValue = block_utils:get_value(Config, default_value),
    InvertOutput = block_utils:get_value(Config, invert_output),
	    
    case gpio:start_link(PinNumber, output) of
        {ok, GpioPinRef} ->
            Status = initialized,
            Value = DefaultValue,
 	        NewPrivate = block_utils:set_value(Private, gpio_pin_ref, GpioPinRef),
            set_pin_value_bool(GpioPinRef, DefaultValue, InvertOutput);
            
        {error, ErrorResult} ->
            error_logger:error_msg("~p Error: ~p intitiating GPIO pin; ~p~n", [BlockName, ErrorResult, PinNumber]),
            Status = process_error,
            Value = not_active,
            NewPrivate = Private
    end,	
  
    NewOutputs = block_utils:set_value_status(Outputs, Value, Status),
    
    {BlockName, BlockModule, Config, Inputs, NewOutputs, NewPrivate}.
    

%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->
    
    GpioPin = block_utils:get_value(Private, gpio_pin_ref),
    DefaultValue = block_utils:get_value(Config, default_value),
    InvertOutput = block_utils:get_value(Config, invert_output),
     
    Input = block_utils:get_value(Inputs, input),
 	
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
             error_logger:error_msg("~p Error: Invalid input value: ~p~n", [BlockName, Other]),
			 PinValue = DefaultValue, % TODO: Set pin to default value or input? 
		     Value = not_active,
             Status = input_error
	end,
    set_pin_value_bool(GpioPin, PinValue, InvertOutput),
 
    NewOutputs = block_utils:set_value_status(Outputs, Value, Status),     
 
    {BlockName, BlockModule, Config, Inputs, NewOutputs, Private}.


%% 
%%  Delete the block
%%	
-spec delete(block_state()) -> block_state().

delete({BlockName, BlockModule, Config, Inputs, Outputs, Private}) -> 
    % Perform any block type specific delete functionality here
    {BlockName, BlockModule, Config, Inputs, Outputs, Private}.


%% ====================================================================
%% Internal functions
%% ====================================================================

% Set the actual value of the GPIO pin here
set_pin_value_bool(GpioPin, Value, Invert) ->
    if Value -> % Value is true/on
        if Invert -> % Invert pin value 
            gpio:write(GpioPin, 0); % turn output off
        true ->      % Don't invert_output output value
            gpio:write(GpioPin, 1) % turn output on
        end;
    true -> % Value is false/off
        if Invert -> % Invert pin value
            gpio:write(GpioPin, 1); % turn output on
		true ->      % Don't invert_output output value
            gpio:write(GpioPin, 0)  % turn output off
        end
    end.


default_configs(BlockName) -> 
    block_utils:merge_attribute_lists(block_common:configs(BlockName, type_name(), version()), 
                            [ 
                              {gpio_pin, 0}, 
                              {default_value, false},
                              {invert_output, false}
                            ]).
 
default_inputs() -> 
     block_utils:merge_attribute_lists(block_common:inputs(),
                            [ 
                              {input, empty, {fixed, null, null}}
                            ]). 
                            
default_outputs() -> 
        block_utils:merge_attribute_lists(block_common:outputs(),
                            []). 
                            
default_private() -> 
        block_utils:merge_attribute_lists(block_common:private(),
                            [
                              {gpio_pin_ref, empty}
                            ]). 
