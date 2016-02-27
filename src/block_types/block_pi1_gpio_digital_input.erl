%%% @doc 
%%% Block Type: Pi 1 GPIO Digital Input 
%%% Description: Configure a Raspberry Pi 1 GPIO Pin as a Digital Input block  
%%%               
%%% @end 

-module(block_pi1_gpio_digital_input).

-author("Mark Sebald").

-include("../block_state.hrl").  % Adjust path to hrl file as needed

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

    % Perform common block initializations
    InitPrivate = block_common:initialize(Config, Inputs, Private),
	
    % Perform block type specific initializations here, and update the state variables
    PinNumber = block_utils:get_value(Config, gpio_pin),
    % TODO: Check Pin Number is an integer in the right range

    % Perform block type specific initializations here, and update the state variables
    case gpio:start_link(PinNumber, input) of
        {ok, GpioPinRef} ->
            Status = initialized,
            Value = not_active,
	        NewPrivate = block_utils:merge_attribute_lists(InitPrivate, [{gpio_pin_ref, GpioPinRef}]),
            gpio:register_int(GpioPinRef),
            gpio:set_int(GpioPinRef, both);  % TODO: Make interrupt type selectable via config value

        {error, ErrorResult} ->
            io:format("~p Error: ~p intitiating GPIO pin; ~p~n", [BlockName, ErrorResult, PinNumber]),
            Status = process_error,
            Value = not_active,
            NewPrivate = InitPrivate
    end,
  
    NewOutputsX = block_utils:set_value(Outputs, value, Value),
    NewOutputs = block_utils:set_value(NewOutputsX, status, Status),
    
	{BlockName, BlockModule, Config, Inputs, NewOutputs, NewPrivate}.

%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

    % Perform block type specific actions here, 
    % read input value(s) calculate new outut value(s)
    % set block output status value
    % Perform block type specific actions here, calculate new outut value(s)
    
    GpioPinRef = block_utils:get_value(Private, gpio_pin_ref),
    Value = read_pin_value_bool(GpioPinRef),

    NewOutputsX = block_utils:set_value(Outputs, value, Value),
    NewOutputs = block_utils:set_value(NewOutputsX, status, normal), 
        
    {BlockName, BlockModule, Config, Inputs, NewOutputs, Private}.


%% 
%%  Delete the block
%%	
    
delete({_BlockName, _BlockModule, Config, _Inputs, _Outputs, Private}) ->
	block_common:delete(Config, Private).
    % Perform any other block type specific delete functionality here


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
                            ]).  % Insert block type specific config attributes here
 
default_inputs() -> 
     block_utils:merge_attribute_lists(block_common:inputs(),
                            []). % Insert block type specific input attributes here
                            
default_outputs() -> 
        block_utils:merge_attribute_lists(block_common:outputs(),
                            []). % Insert block type specific output attributes here
                            
default_private() -> 
        block_utils:merge_attribute_lists(block_common:private(),
                            [
                              {gpio_pin_ref, empty}
                            ]). % Insert block type specific private attributes here
