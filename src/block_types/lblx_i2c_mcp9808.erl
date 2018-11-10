%%% @doc 
%%% BLOCKTYPE
%%% MCP9808 Temperature Sensor
%%% DESCRIPTION
%%% Microchip MCP9808 precision temperature sensor with I2C interface
%%% LINKS
%%% http://ww1.microchip.com/downloads/en/DeviceDoc/25095A.pdf
%%% @end 

-module(lblx_i2c_mcp9808).  

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [sensor, input, i2c_device].

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {i2c_bus, {"i2c-1"}},  %| string | "i2c-1" | N/A |
      {i2c_addr, {16#18}}, %| byte | 70h | 0..FFh |
      {deg_f, {true}}, %| bool | true | true, false |
      {temp_offset, {0.0}} %| float | 0.0 | +/- max float |
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
    ]). 


-spec default_outputs() -> output_attribs().
                            
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
-spec create(BlockName :: block_name(),
             Description :: string()) -> block_defn().

create(BlockName, Description) -> 
  create(BlockName, Description, [], [], []).

-spec create(BlockName :: block_name(),
             Description :: string(),  
             InitConfig :: config_attribs(), 
             InitInputs :: input_attribs()) -> block_defn().
   
create(BlockName, Description, InitConfig, InitInputs) -> 
  create(BlockName, Description, InitConfig, InitInputs, []).

-spec create(BlockName :: block_name(),
             Description :: string(), 
             InitConfig :: config_attribs(), 
             InitInputs :: input_attribs(), 
             InitOutputs :: list()) -> block_defn().

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
%% Upgrade block attribute values, when block code and block data versions are different
%% 
-spec upgrade(BlockDefn :: block_defn()) -> {ok, block_defn()} | {error, atom()}.

upgrade({Config, Inputs, Outputs}) ->
  ModuleVer = version(),
  {BlockName, BlockModule, ConfigVer} = config_utils:name_module_version(Config),
  BlockType = type_utils:type_name(BlockModule),

  case attrib_utils:set_value(Config, version, version()) of
    {ok, UpdConfig} ->
      m_logger:info(block_type_upgraded_from_ver_to, 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      m_logger:error(err_upgrading_block_type_from_ver_to, 
                            [Reason, BlockName, BlockType, ConfigVer, ModuleVer]),
      {error, Reason}
  end.


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  % Setup I2C comm channel of the sensor
  case config_utils:init_i2c(Config, Private) of
    {ok, Private1, I2cDevice} ->
      {ok, DegF} = attrib_utils:get_value(Config, deg_f),
      {ok, Offset} = attrib_utils:get_value(Config, temp_offset),
  
      % Read the ambient temperature
      case read_ambient(I2cDevice, DegF, Offset) of
       {ok, Value} ->
          Status = initialed;

       {error, Reason} ->
          m_logger:error(err_reading_temperature_sensor, [Reason]),
          Status = proc_err,
          Value = null
       end;
      
    {error, _Reason} ->
      Status = proc_err,
      Value = null,
      Private1 = Private
  end,	
   
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  % This is the block state
  {Config, Inputs, Outputs1, Private1}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, disable) ->
  Outputs1 = output_utils:update_all_outputs(Outputs, null, disabled),
  {Config, Inputs, Outputs1, Private};

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  % TODO: Check flag bits?
  %  Do we need to do this? Easy to implement in block code.
  % Critical temp trips hw interrupt on chip, may need to implement that
  % if ((UpperByte & 0x80) == 0x80){ //TA > TCRIT }
  % if ((UpperByte & 0x40) == 0x40){ //TA > TUPPER }
  % if ((UpperByte & 0x20) == 0x20){ //TA < TLOWER }
  
  {ok, I2cDevice} = attrib_utils:get_value(Private, i2c_dev),
  {ok, DegF} = attrib_utils:get_value(Config, deg_f),
  {ok, Offset} = attrib_utils:get_value(Config, temp_offset),
  
  % Read the ambient temperature
  case read_ambient(I2cDevice, DegF, Offset) of
    {ok, Value} ->
      Status = normal;

    {error, Reason} ->
      m_logger:error(err_reading_temperature_sensor, [Reason]),
      Status = proc_err,
      Value = null
   end,
   
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  % Return updated block state
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, Private}) -> 
  % Close the I2C Channel
  case attrib_utils:get_value(Private, i2c_dev) of
    {ok, {I2cRef, _I2cAddr}} -> i2c_utils:close(I2cRef);
    
    _ -> ok
  end,
  {Config, Inputs, Outputs}.


%% ====================================================================
%% Internal functions
%% ====================================================================

-define(AMBIENT_TEMP_REG, 16#05). 
-define(NEGATIVE_TEMP_FLAG, 16#10).
-define(LOW_TEMP_FLAG, 16#20).
-define(HIGH_TEMP_FLAG, 16#40).
-define(CRITICAL_TEMP_FLAG, 16#80).
-define(HIGH_BYTE_TEMP_MASK, 16#0F).

%
% Read the ambient temperature.
%
-spec read_ambient(I2cDevice :: lb_types:i2c_device(),
                   DegF :: boolean(),
                   Offset :: float()) -> {ok, float()} | {error, atom()}.
                   
read_ambient(I2cDevice, DegF, Offset) ->

  % Read two bytes from the ambient temperature register  
  case i2c_utils:write_read(I2cDevice, <<?AMBIENT_TEMP_REG>>, 2) of
    {error, Reason} -> {error, Reason};
  
    {ok, Result} ->
      RawBytes = binary:bin_to_list(Result),
      UpperByte = lists:nth(1, RawBytes),
      LowerByte = lists:nth(2, RawBytes),
  
      % Strip sign and alarm flags from upper byte
      UpperTemp = (UpperByte band ?HIGH_BYTE_TEMP_MASK),
       
      if (UpperByte band ?NEGATIVE_TEMP_FLAG) == ?NEGATIVE_TEMP_FLAG ->  
        % temp < 0
        TempDegC = 256 - (UpperTemp * 16 + LowerByte / 16);
      true -> 
        % temp >= 0 
        TempDegC = (UpperTemp * 16 + LowerByte / 16)
      end,

      case DegF of
        true  -> Temp = ((TempDegC * 9) / 5 + 32);
        false -> Temp = TempDegC
      end,
      {ok, Temp + Offset}
  end.


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("block_io_test_gen.hrl").

test_sets() ->
  [
    {[{status, normal}]}
  ].

-endif.