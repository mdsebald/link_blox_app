%%% @doc 
%%% Block Type:  4 Digit 7 Segment LED Display
%%% Description: LED Display with I2C Interface HTK1633 Driver   
%%%               
%%% @end 

-module(lblx_ht16k33_4digit_led).  

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([type_name/0, description/0, version/0]). 
-export([create/2, create/4, create/5, initialize/1, execute/1, delete/1]).


type_name() -> "ht16k33_4digit_led".

version() -> "0.1.0".

% INSTRUCTIONS String describing block function
description() -> "4 digit 7 segment LED display with I2C interface".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: atom(),
                      Comment :: string()) -> list().

default_configs(BlockName, Comment) -> 
  block_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, Comment, type_name(), version(), description()), 
    [
      {i2c_device, "i2c-1"},
      {i2c_addr, 16#70}                 
    ]). 


-spec default_inputs() -> list().

default_inputs() -> 
  block_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {blink_rate, 0, ?EMPTY_LINK},
      {brightness, 15, ?EMPTY_LINK},
      {digit_1, 1, ?EMPTY_LINK},
      {decimal_1, true, ?EMPTY_LINK},
      {digit_2, 2, ?EMPTY_LINK},
      {decimal_2, true, ?EMPTY_LINK},
      {colon, true, ?EMPTY_LINK},
      {digit_3, 3, ?EMPTY_LINK},
      {decimal_3, true, ?EMPTY_LINK},
      {digit_4, 4, ?EMPTY_LINK},
      {decimal_4, true, ?EMPTY_LINK}
    ]). 


-spec default_outputs() -> list().
                            
default_outputs() -> 
  block_utils:merge_attribute_lists(
    block_common:outputs(),
    [
     
    ]). 

%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: atom(),
             Comment :: string()) -> block_defn().

create(BlockName, Comment) -> 
  create(BlockName, Comment, [], [], []).

-spec create(BlockName :: atom(),
             Comment :: string(),  
             InitConfig :: list(), 
             InitInputs :: list()) -> block_defn().
   
create(BlockName, Comment, InitConfig, InitInputs) -> 
  create(BlockName, Comment, InitConfig, InitInputs, []).

-spec create(BlockName :: atom(),
             Comment :: string(), 
             InitConfig :: list(), 
             InitInputs :: list(), 
             InitOutputs :: list()) -> block_defn().

create(BlockName, Comment, InitConfig, InitInputs, InitOutputs)->

  %% Update Default Config, Input, Output, and Private attribute values 
  %% with the initial values passed into this function.
  %%
  %% If any of the intial attributes do not already exist in the 
  %% default attribute lists, merge_attribute_lists() will create them.
  %% (This is useful for block types where the number of attributes is not fixed)
    
  Config = block_utils:merge_attribute_lists(default_configs(BlockName, Comment), InitConfig),
  Inputs = block_utils:merge_attribute_lists(default_inputs(), InitInputs), 
  Outputs = block_utils:merge_attribute_lists(default_outputs(), InitOutputs),

  % This is the block definition, 
  {Config, Inputs, Outputs}.

%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  PrivateX = block_utils:add_attribute(Private, {i2c_ref, empty}),
  PrivateY = block_utils:add_attribute(PrivateX, {ht16k33_disp_buff, empty}),
  
  % Get the the I2C Address of the display 
  % TODO: Check for valid I2C Address
  I2cDevice = block_utils:get_value(Config, i2c_device),
  I2cAddr = block_utils:get_value(Config, i2c_addr),
	    
  case ht16k33_led_driver:init(I2cDevice, I2cAddr) of
    {ok, I2cRef, DisplayBuffer} ->
      Status = initialed,
      Value = 0, 
      PrivateZ = block_utils:set_values(PrivateY, 
                          [{i2c_ref, I2cRef}, {ht16k33_disp_buff, DisplayBuffer}]);      
    {error, Reason} ->
      error_logger:error_msg("Error: ~p intitiating I2C Address: ~p~n", 
                              [Reason, I2cAddr]),
      Status = proc_error,
      Value = not_active,
      PrivateZ = PrivateY
  end,	
   
  OutputsX = block_utils:set_value_status(Outputs, Value, Status),

  % This is the block state
  {Config, Inputs, OutputsX, PrivateZ}.

%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({Config, Inputs, Outputs, Private}) ->

  I2cRef = block_utils:get_value(Private, i2c_ref),
  Buffer = block_utils:get_value(Private, ht16k33_disp_buff),
  
  BlinkRate = block_utils:get_value(Inputs, blink_rate),
  ht16k33_led_driver:set_blink_rate(I2cRef, BlinkRate),
  
  Brightness = block_utils:get_value(Inputs, brightness),
  ht16k33_led_driver:set_blink_rate(I2cRef, Brightness),

  Digit1 = block_utils:get_value(Inputs, digit_1),
  Decimal1 = block_utils:get_value(Inputs, decimal_1),
  Buffer1 = ht16k33_4digit_led:write_digit(I2cRef, Buffer, 0, Digit1, Decimal1, false),
  
  Digit2 = block_utils:get_value(Inputs, digit_2),
  Decimal2 = block_utils:get_value(Inputs, decimal_2),
  Buffer2 = ht16k33_4digit_led:write_digit(I2cRef, Buffer1, 1, Digit2, Decimal2, false),
  
  Colon = block_utils:get_value(Inputs, colon),
  BufferC = ht16k33_4digit_led:set_colon(I2cRef, Buffer2, Colon, false),

  Digit3 = block_utils:get_value(Inputs, digit_3),
  Decimal3 = block_utils:get_value(Inputs, decimal_3),
  Buffer3 = ht16k33_4digit_led:write_digit(I2cRef, BufferC, 3, Digit3, Decimal3, false),
  
  Digit4 = block_utils:get_value(Inputs, digit_4),
  Decimal4 = block_utils:get_value(Inputs, decimal_4),
  % Update the display after updating the buffer on the last digit
  Buffer4 = ht16k33_4digit_led:write_digit(I2cRef, Buffer3, 4, Digit4, Decimal4, true),
  
  NewPrivate = block_utils:set_value(Private, ht16k33_disp_buff, Buffer4),
  
  % Block value output really doesn't have any useful info right now
  NewOutputs = block_utils:set_value_status(Outputs, 0, normal),
  
  % Return updated block state
  {Config, Inputs, NewOutputs, NewPrivate}.


%% 
%%  Delete the block
%%	
-spec delete(block_state()) -> ok.

delete({_Config, _Inputs, _Outputs, Private}) ->
  % Close the I2C Channel
  I2cRef = block_utils:get_value(Private, i2c_ref), 
  i2c:stop(I2cRef),
  ok.



%% ====================================================================
%% Internal functions
%% ====================================================================

