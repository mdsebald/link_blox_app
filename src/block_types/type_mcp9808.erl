%%% @doc 
%%% Block Type:  MCP9808 Temperature Sensor
%%% Description: Microchip MCP9808 precision temperature sensor with I2C interface   
%%%               
%%% @end 

-module(type_mcp9808).  

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([group/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/1, delete/1]).

group() -> [sensor, input].

description() -> "Precision temp sensor with I2C interface".

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {i2c_device, {"i2c-1"}},
      {i2c_addr, {16#18}},
      {deg_f, {true}},
      {offset, {0.0}}
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {input, {0, ?EMPTY_LINK}} % TODO: Delete, not used
    ]). 


-spec default_outputs() -> list(output_attr()).
                            
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
             InitConfig :: list(config_attr()), 
             InitInputs :: list(input_attr())) -> block_defn().
   
create(BlockName, Description, InitConfig, InitInputs) -> 
  create(BlockName, Description, InitConfig, InitInputs, []).

-spec create(BlockName :: block_name(),
             Description :: string(), 
             InitConfig :: list(config_attr()), 
             InitInputs :: list(input_attr()), 
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
      error_logger:info_msg("Block: ~p type: ~p upgraded from ver: ~s to: ~s~n", 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      error_logger:error_msg("Error: ~p upgrading block: ~p type: ~p from ver: ~s to: ~s~n", 
                            [Reason, BlockName, BlockType, ConfigVer, ModuleVer]),
      {error, Reason}
  end.


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  Private1 = attrib_utils:add_attribute(Private, {i2c_ref, {empty}}),
  
  % Get the the I2C Address of the sensor 
  % TODO: Check for valid I2C Address
  {ok, I2cDevice} = attrib_utils:get_value(Config, i2c_device),
  {ok, I2cAddr} = attrib_utils:get_value(Config, i2c_addr),
	    
  case i2c_utils:start_link(I2cDevice, I2cAddr) of
    {ok, I2cRef} ->
      {ok, Private2} = attrib_utils:set_value(Private1, i2c_ref, I2cRef),
      
      
      {ok, DegF} = attrib_utils:get_value(Config, deg_f),
      {ok, Offset} = attrib_utils:get_value(Config, offset),
  
      % Read the ambient temperature
      case read_ambient(I2cRef, DegF, Offset) of
       {ok, Value} ->
          Status = initialed;

       {error, Reason} ->
          error_logger:error_msg("Error: ~p reading temperature sensor~n", 
                              [Reason]),
          Status = proc_err,
          Value = not_active
       end;
      
    {error, Reason} ->
      error_logger:error_msg("Error: ~p intitiating I2C Address: ~p~n", 
                              [Reason, I2cAddr]),
      Status = proc_err,
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

  % TODO: Check flag bits?
  %  Do we need to do this? Easy to implement in block code.
  % Critical temp trips hw interrupt on chip, may need to implement that
  % if ((UpperByte & 0x80) == 0x80){ //TA > TCRIT }
  % if ((UpperByte & 0x40) == 0x40){ //TA > TUPPER }
  % if ((UpperByte & 0x20) == 0x20){ //TA < TLOWER }
  
  {ok, I2cRef} = attrib_utils:get_value(Private, i2c_ref),
  {ok, DegF} = attrib_utils:get_value(Config, deg_f),
  {ok, Offset} = attrib_utils:get_value(Config, offset),
  
  % Read the ambient temperature
  case read_ambient(I2cRef, DegF, Offset) of
    {ok, Value} ->
      Status = normal;

    {error, Reason} ->
      error_logger:error_msg("Error: ~p reading temperature sensor~n", 
                              [Reason]),
      Status = proc_err,
      Value = not_active
   end,
   
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  % Return updated block state
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockValues :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, Private}) -> 
  % Close the I2C Channel
  case attrib_utils:get_value(Private, i2c_ref) of
    {ok, I2cRef} -> i2c_utils:stop(I2cRef);
    
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
-spec read_ambient(I2cRef :: pid(),
                   DegF :: boolean(),
                   Offset :: float()) -> {ok, float()} | {error, atom()}.
                   
read_ambient(I2cRef, DegF, Offset) ->

  % Read two bytes from the ambient temperature register  
  case i2c_utils:write_read(I2cRef, <<?AMBIENT_TEMP_REG>>, 2) of
    {error, Reason} -> {error, Reason};
  
    Result ->
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

% At a minimum, call the block type's create(), upgrade(), initialize(), execute(), and delete() functions.

block_test() ->
  BlockDefn = create(create_test, "Unit Testing Block"),
  {ok, BlockDefn} = upgrade(BlockDefn),
  BlockState = block_common:initialize(BlockDefn),
  execute(BlockState),
  _BlockDefnFinal = delete(BlockState),
  ?assert(true).

-endif.