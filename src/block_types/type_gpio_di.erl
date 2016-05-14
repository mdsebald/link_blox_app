%%% @doc 
%%% Block Type: GPIO Digital Input 
%%% Description: Configure a GPIO Pin as a Digital Input block  
%%%               
%%% @end 

-module(type_gpio_di).

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([type_name/0, description/0, version/0]). 
-export([create/2, create/4, create/5, initialize/1, execute/1, delete/1]).


type_name() -> "gpio_di". 

description() -> "GPIO digital input". 

version() -> "0.1.0". 


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: atom(),
                      Description :: string()) -> list().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {gpio_pin, 0}, 
      {invert_output, false}
    ]). 
                            
                            
-spec default_inputs() -> list().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [

    ]).


-spec default_outputs() -> list().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [

    ]).

                        
%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: atom(),
             Description :: string()) -> block_defn().

create(BlockName, Description) -> 
  create(BlockName, Description, [], [], []).

-spec create(BlockName :: atom(),
             Description :: string(),  
             InitConfig :: list(), 
             InitInputs :: list()) -> block_defn().
   
create(BlockName, Description, InitConfig, InitInputs) -> 
  create(BlockName, Description, InitConfig, InitInputs, []).

-spec create(BlockName :: atom(),
             Description :: string(), 
             InitConfig :: list(), 
             InitInputs :: list(), 
             InitOutputs :: list()) -> block_defn().

create(BlockName, Description, InitConfig, InitInputs, InitOutputs)->

  %% Update Default Config, Input, Output, and Private attribute values 
  %% with the initial values passed into this function.
  %%
  %% If any of the intial attributes do not already exist in the 
  %% default attribute lists, merge_attribute_lists() will create them.
  %% (This is useful for block types where the number of attributes is not fixed)
    
  Config = attrib_utils:merge_attribute_lists(default_configs(BlockName, Description), InitConfig),
  Inputs = attrib_utils:merge_attribute_lists(default_inputs(), InitInputs), 
  Outputs = attrib_utils:merge_attribute_lists(default_outputs(), InitOutputs),

  % This is the block definition, 
  {Config, Inputs, Outputs}.


%%
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  Private1 = attrib_utils:add_attribute(Private, {gpio_pin_ref, empty}),
    
  % Get the GPIO pin number used by this block
  {ok, PinNumber} = attrib_utils:get_value(Config, gpio_pin),
  % TODO: Check Pin Number is an integer in the right range

  % Initialize the GPIO pin as an input
  case gpio:start_link(PinNumber, input) of
    {ok, GpioPinRef} ->
      Status = initialed,
      Value = not_active,
	    {ok, Private2} = attrib_utils:set_value(Private1, gpio_pin_ref, GpioPinRef),
      gpio:register_int(GpioPinRef),
      % TODO: Make interrupt type selectable via config value
      gpio:set_int(GpioPinRef, both);

    {error, ErrorResult} ->
      BlockName = config_utils:name(Config),
      error_logger:error_msg("~p Error: ~p intitiating GPIO pin; ~p~n", 
                              [BlockName, ErrorResult, PinNumber]),
      Status = process_error,
      Value = not_active,
      Private2 = Private1
  end,
    
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  {Config, Inputs, Outputs1, Private2}.
  

%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({Config, Inputs, Outputs, Private}) ->

  % Read the current value of the GPIO pin 
  {ok, GpioPinRef} = attrib_utils:get_value(Private, gpio_pin_ref),
  Value = read_pin_value_bool(GpioPinRef),

  Outputs1 = output_utils:set_value_status(Outputs, Value, normal),
        
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(block_state()) -> ok.

delete({_Config, _Inputs, _Outputs, _Private}) -> 
    % Release the GPIO pin?
    ok.


%% ====================================================================
%% Internal functions
%% ====================================================================

read_pin_value_bool(GpioPin) ->
  case gpio:read(GpioPin) of
    1  -> true;
    0 -> false
  end.
  