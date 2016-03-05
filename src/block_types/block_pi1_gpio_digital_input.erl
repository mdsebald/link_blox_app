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
-export([create/1, create/3, create/5, initialize/1, execute/1, delete/1]).


type_name()-> pi1_gpio_digital_input.  % atom, specifying the block type, usually the module name minus "block_"

version() -> "0.1.0".   % Major.Minor.Patch, Major version change is a breaking change

%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: atom()) -> block_state().

create(BlockName) -> create(BlockName, [], [], [], []).
   
create(BlockName, InitConfig, InitInputs) -> create(BlockName, InitConfig, InitInputs, [],[]).

create(BlockName, InitConfig, InitInputs, InitOutputs, InitPrivate)->

    io:format("Creating: ~p Type: ~p~n", [BlockName, type_name()]),
     
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
	
    % Get the GPIO pin number used by this block
    PinNumber = block_utils:get_value(Config, gpio_pin),
    % TODO: Check Pin Number is an integer in the right range

    % Initialize the GPIO pin as an input
    case gpio:start_link(PinNumber, input) of
        {ok, GpioPinRef} ->
            Status = initialized,
            Value = not_active,
	        NewPrivate = block_utils:set_value(Private, gpio_pin_ref, GpioPinRef),
            gpio:register_int(GpioPinRef),
            gpio:set_int(GpioPinRef, both);  % TODO: Make interrupt type selectable via config value

        {error, ErrorResult} ->
            io:format("~p Error: ~p intitiating GPIO pin; ~p~n", [BlockName, ErrorResult, PinNumber]),
            Status = process_error,
            Value = not_active,
            NewPrivate = Private
    end,
    
    NewOutputs = block_utils:set_values(Outputs, [{value, Value}, {status, Status}]),

    {BlockName, BlockModule, Config, Inputs, NewOutputs, NewPrivate}.
    

%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

    % Read the current value of the GPIO pin 
    GpioPinRef = block_utils:get_value(Private, gpio_pin_ref),
    Value = read_pin_value_bool(GpioPinRef),

    NewOutputs = block_utils:set_values(Outputs, [{value, Value}, {status, normal}]),
        
    {BlockName, BlockModule, Config, Inputs, NewOutputs, Private}.


%% 
%%  Delete the block
%%	
-spec delete(block_state()) -> block_state().

delete({BlockName, BlockModule, Config, Inputs, Outputs, Private}) -> 
    % Release the GPIO pin?
    {BlockName, BlockModule, Config, Inputs, Outputs, Private}.



%% ====================================================================
%% Internal functions
%% ====================================================================


read_pin_value_bool(GpioPin) ->
    case gpio:read(GpioPin) of
        1  -> true;
		0 -> false
    end.


default_configs(BlockName) -> 
    block_utils:merge_attribute_lists(block_common:configs(BlockName, type_name(), version()), 
                            [ 
                              {gpio_pin, 0}, 
                              {invert_output, false}
                            ]). 
 
default_inputs() -> 
     block_utils:merge_attribute_lists(block_common:inputs(),
                            []).
                            
default_outputs() -> 
        block_utils:merge_attribute_lists(block_common:outputs(),
                            []).
                            
default_private() -> 
        block_utils:merge_attribute_lists(block_common:private(),
                            [
                              {gpio_pin_ref, empty}
                            ]).
