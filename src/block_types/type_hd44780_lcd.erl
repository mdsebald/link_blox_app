%%% @doc 
%%% Block Type:  LCD driver for display using HD44780 chip, with I2C interface
%%% Description:   
%%%               
%%% @end 

-module(type_hd44780_lcd).  

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([description/0, version/0]). 
-export([create/2, create/4, create/5, initialize/1, execute/1, delete/1]).

 
%   Major version change implies a breaking change, 
%   i.e. Block module code is not compatible with a 
%   block definition created with block code with a different major revison 
version() -> "0.1.0".

description() -> "HD44780 LCD display with I2C interface".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {i2c_device, {"i2c-1"}},
      {i2c_addr, {16#27}},
      {display_size, {twentyXfour}}  % sixteenXtwo, handle 20 col by 4 line, and 16 col by 2 line displays
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {display, {true, ?EMPTY_LINK}},
      {cursor, {true, ?EMPTY_LINK}},
      {blink_cursor, {true, ?EMPTY_LINK}},
      {backlight, {false, ?EMPTY_LINK}},
      {input, {"Hello World", ?EMPTY_LINK}}
    ]). 


-spec default_outputs() -> list(output_attr()).
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      % INTRUCTIONS: Insert block type specific output attribute tuples here
      % Output attribute tuples consist of a value name, a calculated value, 
      % and a list of blocks that reference (have links to) this output value
      % Output values are always set to 'not_actve' and empty reference list on creation
      % Example: {dwell, {not_active, []}}
      % Array Example: {digit, [{not_active, []}]}  
    ]). 


%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: block_name(),
             Description :: string()) -> block_defn().

create(BlockName, Description) -> 
  create(BlockName, Description, [], [], []).

-spec create(BlockName :: block_name(),
             Description :: string(),  
             InitConfig :: list(config_attr()), 
             InitInputs :: list(input_attr())) -> block_defn().
   
create(BlockName, Description, InitConfig, InitInputs) -> 
  create(BlockName, Description, InitConfig, InitInputs, []).

-spec create(BlockName :: block_name(),
             Description :: string(), 
             InitConfig :: list(config_attr()), 
             InitInputs :: list(input_attr()), 
             InitOutputs :: list(output_attr())) -> block_defn().

create(BlockName, Description, InitConfig, InitInputs, InitOutputs) ->

  % Update Default Config, Input, Output, and Private attribute values 
  % with the initial values passed into this function.
  %
  % If any of the intial attributes do not already exist in the 
  % default attribute lists, merge_attribute_lists() will create them.
    
  Config = attrib_utils:merge_attribute_lists(default_configs(BlockName, Description), InitConfig),
  Inputs = attrib_utils:merge_attribute_lists(default_inputs(), InitInputs), 
  Outputs = attrib_utils:merge_attribute_lists(default_outputs(), InitOutputs),

  % This is the block definition, 
  {Config, Inputs, Outputs}.


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
    
  Private1 = attrib_utils:add_attribute(Private, {i2c_ref, {empty}}),
  
  % Get the the I2C Address of the display 
  % TODO: Check for valid I2C Address
  {ok, I2cDevice} = attrib_utils:get_value(Config, i2c_device),
  {ok, I2cAddr} = attrib_utils:get_value(Config, i2c_addr),
	    
  case init_lcd_driver(I2cDevice, I2cAddr) of
    {ok, I2cRef} ->
      update_lcd_control(I2cRef, Inputs),
      Status = initialed,
      Value = 0, 
      {ok, Private2} = attrib_utils:set_value(Private1, i2c_ref, I2cRef);
      
    {error, Reason} ->
      error_logger:error_msg("Error: ~p intitializing LCD driver, I2C Address: ~p~n", 
                              [Reason, I2cAddr]),
      Status = proc_error,
      Value = not_active,
      Private2 = Private1
    end,
   
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  % This is the block state
  {Config, Inputs, Outputs1, Private2}.


%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({Config, Inputs, Outputs, Private}) ->

  {ok, I2cRef} = attrib_utils:get_value(Private, i2c_ref),

  Backlight = update_lcd_control(I2cRef, Inputs),

  write_data(I2cRef, Backlight, 65),

  Status = normal,
  Value = $A, 
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  % Return updated block state
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockValues :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  % INSTRUCTIONS: Perform any block type specific delete functionality here
  % Return block definition, (Block state - Private values)
  % in case calling function wants to reuse them.
  %
  % Private values are created in the block initialization routine
  % So they should be deleted here
  
  {Config, Inputs, Outputs}.



%% ====================================================================
%% Internal functions
%% ====================================================================

%
% Commands
%
-define(CLEAR_DISPLAY, 16#01).
-define(RETURN_HOME, 16#02).
-define(ENTRY_MODE_SET, 16#04).
-define(DISPLAY_CONTROL, 16#08).
-define(CURSOR_DISPLAY_SHIFT, 16#10).
-define(FUNCTION_SET, 16#20).
-define(SET_CGRAM_ADDR, 16#40).
-define(SET_DDRAM_ADDR, 16#80).

%
% Command Parameters
%
-define(NO_PARAMS, 16#00).

%
% Entry Mode Set command params
%
-define(INCREMENT, 16#01).  % DECREMENT = 0
-define(SHIFT, 16#02).  % No shift = 0

%
% Display Control Command params
%
-define(BLINK_CURSOR_ON, 16#01).
-define(BLINK_CURSOR_OFF, 16#00).
-define(CURSOR_ON, 16#02). 
-define(CURSOR_OFF, 16#00).
-define(DISPLAY_ON, 16#04).
-define(DISPLAY_OFF, 16#00).

%
% Function Set Command params
%
-define(DATA_LEN_8BITS, 16#10). % DL: 4 bits = 0
-define(NUM_LINES_2, 16#08). % N: 1 line = 0
-define(FONT_5X10, 16#04). % F: 5X8 dots = 0


%
% This code assumes the I2C Port Expander is wired to LCD Controller as follows:
%
%      Port  
% Expander Output: db7  db6  db5  db4  db3  db2  db1  db0
% ------------------|----|----|----|----|----|----|----|            
%     HD44780       V    V    V    V    V    V    V    V
% LCD Controller   db7  db6  db5  db4  bklt  EN  R/W   RS 
% 
% EN (Enable) must be set high (1) and then low (0) to write a value to the LCD
%   This requires 2 sequential writes of the same data.
% R/W must be set low (0) to write values and high (1) to read values from the LCD
% RS set low (0) for commands and high (1) for data (i.e. characters to display)
%

-define(RS_DATA, 16#01). % RS Command = 0
-define(RW_READ, 16#02). % R/~W Write = 0
-define(ENABLE_SET, 16#04).
-define(ENABLE_CLR, 16#FB).
-define(BACKLIGHT_ON, 16#08).  
-define(BACKLIGHT_OFF, 16#00).


%%
%% Initialize the LCD driver 
%%
-spec init_lcd_driver(I2cDevice :: string(),
                      I2cAddr :: integer()) -> {ok, pid()} | {error, atom()}.
                      
init_lcd_driver(I2cDevice, I2cAddr) ->
  case i2c:start_link(I2cDevice, I2cAddr) of
    {ok, I2cRef} ->
      
      % Reset the LCD, get its attention
      write_command_high(I2cRef, ?BACKLIGHT_OFF, 16#30),
      block_utils:sleep(5),

      write_command_high(I2cRef, ?BACKLIGHT_OFF, 16#30),
      block_utils:sleep(1),

      write_command_high(I2cRef, ?BACKLIGHT_OFF, 16#30),
      block_utils:sleep(1),

      write_command_high(I2cRef, ?BACKLIGHT_OFF, ?FUNCTION_SET),
      block_utils:sleep(1),

      % The LCD display should be reset and in 4 bit mode now
      % From this point use normal command and write data functions

      % Set Num of lines, Font, and keep Data Length 4 bits
      function_set(I2cRef, ?BACKLIGHT_OFF, (?NUM_LINES_2)),

      % Turn the display off
      display_control(I2cRef, ?BACKLIGHT_OFF, ?NO_PARAMS),

       % Clear the screen
      clear(I2cRef, ?BACKLIGHT_OFF),

      entry_mode_set(I2cRef, ?BACKLIGHT_OFF, (?INCREMENT)),

      {ok, I2cRef};
      
    {error, Reason} ->
      {error, Reason}
  end.


%%
%% Read control inputs and update the LCD control
%%
update_lcd_control(I2cRef, Inputs) ->

  case input_utils:get_boolean(Inputs, display) of
    {ok, true} -> Display = ?DISPLAY_ON;
            _  -> Display = ?DISPLAY_OFF
  end,

  case input_utils:get_boolean(Inputs, cursor) of
    {ok, true} -> Cursor = ?CURSOR_ON;
            _  -> Cursor = ?CURSOR_OFF
  end,

  case input_utils:get_boolean(Inputs, blink_cursor) of
    {ok, true} -> BlinkCursor = ?BLINK_CURSOR_ON;
            _  -> BlinkCursor = ?BLINK_CURSOR_OFF
  end,
 
  case input_utils:get_boolean(Inputs, backlight) of
    {ok, true} -> Backlight = ?BACKLIGHT_ON;
            _  -> Backlight = ?BACKLIGHT_OFF
  end,

  display_control(I2cRef, Backlight, ((Display bor Cursor) bor BlinkCursor)).



clear(I2cRef, Backlight) ->
  write_command(I2cRef, Backlight, ?CLEAR_DISPLAY, ?NO_PARAMS).


return_home(I2cRef, Backlight) ->
  write_command(I2cRef, Backlight, ?RETURN_HOME, ?NO_PARAMS).


function_set(I2cRef, Backlight, Params) ->
  write_command(I2cRef, Backlight, ?FUNCTION_SET, Params).


display_control(I2cRef, Backlight, Params) ->
  write_command(I2cRef, Backlight, ?DISPLAY_CONTROL, Params).


entry_mode_set(I2cRef, Backlight, Params) ->
  write_command(I2cRef, Backlight, ?ENTRY_MODE_SET, Params).


write_command(I2cRef, Backlight, Command, Params) ->
  CmdAndParams = (Command bor Params),
  % Always operate in 4 bit mode when using an I2C interface
  % Write high nibble (bits 7-4) first, then low nibble (bits 3-0)
  write_command_high(I2cRef, Backlight, CmdAndParams),
  write_command_low(I2cRef, Backlight, CmdAndParams).


% write high half of command byte value, R/W = 0, RS = 0
write_command_high(I2cRef, Backlight, Value) ->
  HighValue = ((Value band 16#F0) bor Backlight),
  write_value(I2cRef, HighValue).


% write low half of command byte value, R/W = 0, RS = 0
write_command_low(I2cRef, Backlight, Value) ->
  LowValue = ((Value bsl 4) bor Backlight),
  write_value(I2cRef, LowValue).


write_data(I2cRef, Backlight, Data) ->
  % Always operate in 4 bit mode when using an I2C interface
  % Write high nibble (bits 7-4) first, then low nibble (bits 3-0)
  write_data_high(I2cRef, Backlight, Data),
  write_data_low(I2cRef, Backlight, Data).


% write high half of data byte value, R/W = 0, RS = 1
write_data_high(I2cRef, Backlight, Value) ->
  HighValue = (((Value band 16#F0) bor Backlight) bor ?RS_DATA),
  write_value(I2cRef, HighValue).


% write low half of data byte value, R/W = 0, RS = 1
write_data_low(I2cRef, Backlight, Value) ->
  LowValue = (((Value bsl 4) bor Backlight) bor ?RS_DATA),
  write_value(I2cRef, LowValue).


write_value(I2cRef, Value) ->
  % Toggle Enable pin high
  ValueEnableSet = (Value bor ?ENABLE_SET),
  i2c:write(I2cRef, <<ValueEnableSet>>),
  % Toggle enable pin low, don't change the 4 data bits, Backlight, RS, or R/~W lines
  ValueEnableClr = (Value band ?ENABLE_CLR),
  i2c:write(I2cRef, <<ValueEnableClr>>).


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% INSTRUCTIONS: Create unit tests here

-endif.