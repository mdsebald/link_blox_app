%%% @doc 
%%% Block Type:  Bosch BME280 Temperature, Pressure, Humidity Sensor
%%% Description:    
%%%               
%%% @end 

-module(lblx_bme280).  

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [input, sensor, i2c_device].

description() -> "Bosch temperature, pressure, and humidity sensor with I2C interface".

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
      {i2c_addr, {16#76}},
      {read_mode, {normal}},  % Valid values: normal, forced
      {filter_coeff, {0}},    % Valid values: 0,2,4,6,8,16
      {standby_time, {500}},  % Valid values: 0.5, 62.5, 125, 250, 500, 1000, 10, 20 Msecs 
      {temp_mode, {16}},       % Valid values: 0, (disabled), 1,2,4,8,16 (oversampling rate) 
      {press_mode, {16}},      % Valid values: 0, (disabled), 1,2,4,8,16 (oversampling rate) 
      {humid_mode, {16}},      % Valid values: 0, (disabled), 1,2,4,8,16 (oversampling rate)
      {deg_f, {true}},
      {inch_merc, {true}},
      {temp_offset, {0.0}},
      {press_offset, {0.0}},
      {humid_offset, {0.0}} 
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [

    ]). 


-spec default_outputs() -> list(output_attr()).
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {temp, {not_active, []}},
      {press, {not_active, []}},
      {humid, {not_active, []}}
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
      log_server:info(block_type_upgraded_from_ver_to, 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      log_server:error(err_upgrading_block_type_from_ver_to, 
                            [Reason, BlockName, BlockType, ConfigVer, ModuleVer]),
      {error, Reason}
  end.


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  % Create the private attributes needed
  Private1 = attrib_utils:merge_attribute_lists(Private, 
              [
                {i2c_ref, {empty}},
                {sensor_mode, {empty}},
                % Temperature calibration values
                {dig_T1, {empty}},
                {dig_T2, {empty}},
                {dig_T3, {empty}},
                {dig_T1, {empty}},
                % Pressure calibration values
                {dig_P1, {empty}},
                {dig_P2, {empty}},
                {dig_P3, {empty}},
                {dig_P4, {empty}},
                {dig_P5, {empty}},
                {dig_P6, {empty}},
                {dig_P7, {empty}},
                {dig_P8, {empty}},
                {dig_P9, {empty}},
                % Humidity calibration values
                {dig_H1, {empty}},
                {dig_H2, {empty}},
                {dig_H3, {empty}},
                {dig_H4, {empty}},
                {dig_H5, {empty}},
                {dig_H6, {empty}}
              ]),
  
  % Get the the I2C Address of the sensor 
  % TODO: Check for valid I2C Address
  {ok, I2cDevice} = attrib_utils:get_value(Config, i2c_device),
  {ok, I2cAddr} = attrib_utils:get_value(Config, i2c_addr),
      
  case i2c_utils:start_link(I2cDevice, I2cAddr) of
    {ok, I2cRef} ->
      {ok, Private2} = attrib_utils:set_value(Private1, i2c_ref, I2cRef),
      
      case configure_sensor(I2cRef, Config) of 
        {ok, SensorMode} ->
          % Need to save sensor mode, 
          % Read mode is part of sensor mode. If read mode is 'forced'
          % need to set Read mode every time we want to read the sensor
          {ok, Private3} = attrib_utils:set_value(Private2, sensor_mode, SensorMode),

          case get_calibration(I2cRef, Private3) of
            {ok, Private4} -> 
              case read_sensor(I2cRef, Private4) of
                {ok, InitTemp, InitPress, InitHumid} ->
                  case convert_readings(InitTemp, InitPress, InitHumid, Config) of
                    {ok, Temp, Press, Humid} ->
                      Status = initialed,
                      Value = Temp,
                      ok;
                    {error, Reason} ->
                      log_server:error(err_converting_sensor_values, 
                              [Reason]),
                      Status = config_err,
                      Value = not_active,
                      Temp = not_active,
                      Press = not_active,
                      Humid = not_active
                  end;    

                {error, Reason} ->
                  log_server:error(err_reading_sensor, 
                              [Reason]),
                  Status = proc_err,
                  Value = not_active,
                  Temp = not_active,
                  Press = not_active,
                  Humid = not_active
              end;

            {error, Reason} ->
              log_server:error(err_reading_sensor_calibration, [Reason]),
              Status = config_err,
              Value = not_active,
              Temp = not_active,
              Press = not_active,
              Humid = not_active,
              Private4 = Private3
          end;
  
        {error, Reason} ->
          log_server:error(err_configuring_sensor, [Reason]),
          Status = config_err,
          Value = not_active,
          Temp = not_active,
          Press = not_active,
          Humid = not_active,
          Private4 = Private1
      end;

    {error, Reason} ->
      log_server:error(err_initiating_I2C_address, 
                              [Reason, I2cAddr]),
      Status = proc_err,
      Value = not_active,
      Temp = not_active,
      Press = not_active,
      Humid = not_active,
      Private4 = Private1
  end,  
   
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),
  {ok, Outputs2} = attrib_utils:set_values(Outputs1, 
                                           [{value, Value},
                                            {temp, Temp},
                                            {press, Press},
                                            {humid, Humid}]),

  % This is the block state
  {Config, Inputs, Outputs2, Private4}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockValues :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->
  
  {ok, I2cRef} = attrib_utils:get_value(Private, i2c_ref),
  {ok, _DegF} = attrib_utils:get_value(Config, deg_f),
  {ok, _TempOffset} = attrib_utils:get_value(Config, temp_offset),

  case attrib_utils:get_value(Config, read_mode) of
    {ok, sleep}  ->
      % Don't read the sensor
      Status = no_input,
      Value = not_active,
      Temp = not_active,
      Press = not_active,
      Humid = not_active;

    {ok, forced} ->
      case read_sensor_forced(I2cRef, Private) of
        {ok, InitTemp, InitPress, InitHumid} ->
          case convert_readings(InitTemp, InitPress, InitHumid, Config) of
            {ok, Temp, Press, Humid} ->
              Status = normal,
              Value = Temp,
              ok;
            {error, Reason} ->
              log_server:error(err_converting_sensor_values, 
                                  [Reason]),
              Status = config_err,
              Value = not_active,
              Temp = not_active,
              Press = not_active,
              Humid = not_active
          end;

        {error, Reason} ->
          log_server:error(err_reading_sensor_forced_mode, 
                                  [Reason]),
          Status = proc_err,
          Value = not_active,
          Temp = not_active,
          Press = not_active,
          Humid = not_active
      end;

    {ok, normal} -> 
      case read_sensor(I2cRef, Private) of
        {ok, InitTemp, InitPress, InitHumid} ->
          case convert_readings(InitTemp, InitPress, InitHumid, Config) of
            {ok, Temp, Press, Humid} ->
              Status = normal,
              Value = Temp,
              ok;
            {error, Reason} ->
              log_server:error(err_converting_sensor_values, [Reason]),
              Status = config_err,
              Value = not_active,
              Temp = not_active,
              Press = not_active,
              Humid = not_active
          end;

        {error, Reason} ->
          log_server:error(err_reading_sensor, [Reason]),
          Status = proc_err,
          Value = not_active,
          Temp = not_active,
          Press = not_active,
          Humid = not_active
      end
  end,
   
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),
  {ok, Outputs2} = attrib_utils:set_values(Outputs1, 
                                           [{value, Value},
                                            {temp, Temp},
                                            {press, Press},
                                            {humid, Humid}]),

  % Return updated block state
  {Config, Inputs, Outputs2, Private}.


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

-define(RESET_REG, 16#E0).
-define(RESET_VALUE, 16#B6).

-define(CTRL_HUMID_REG, 16#F2).
-define(OSRS_HUMID_SKIP, 2#000).
-define(OSRS_HUMID_OVSAMPL_1X, 2#001).
-define(OSRS_HUMID_OVSAMPL_2X, 2#010).
-define(OSRS_HUMID_OVSAMPL_4X, 2#011).
-define(OSRS_HUMID_OVSAMPL_8X, 2#100).
-define(OSRS_HUMID_OVSAMPL_16X, 2#101).

-define(STATUS_REG, 16#F3).

-define(CTRL_MEAS_REG, 16#F4).  % Controls Temp, Pressure, and Sensor modes
% Bits 1,0 of ctrl_meas register
-define(SENSOR_MODE_SLEEP, 2#00).
-define(SENSOR_MODE_FORCED, 2#01). % 2#10 is also forced mode
-define(SENSOR_MODE_NORMAL, 2#11).
% Bits 4,3,2 of ctrl_meas register
-define(OSRS_PRESS_SKIP, 2#000).
-define(OSRS_PRESS_OVSAMPL_1X, 2#001).
-define(OSRS_PRESS_OVSAMPL_2X, 2#010).
-define(OSRS_PRESS_OVSAMPL_4X, 2#011).
-define(OSRS_PRESS_OVSAMPL_8X, 2#100).
-define(OSRS_PRESS_OVSAMPL_16X, 2#101).
% Bits 7,6,5 of ctrl_meas register
-define(OSRS_TEMP_SKIP, 2#000).
-define(OSRS_TEMP_OVSAMPL_1X, 2#001).
-define(OSRS_TEMP_OVSAMPL_2X, 2#010).
-define(OSRS_TEMP_OVSAMPL_4X, 2#011).
-define(OSRS_TEMP_OVSAMPL_8X, 2#100).
-define(OSRS_TEMP_OVSAMPL_16X, 2#101).

-define(CONFIG_REG, 16#F5).
% Bit 0 of config
-define(ENABLE_SPI, 2#1).  % Currently, code is only set up for I2C Interface, 
-define(ENABLE_I2C, 2#0).
% Bits 4,3,2 of config
-define(FILTER_COEFF_OFF, 2#000).
-define(FILTER_COEFF_2, 2#001).
-define(FILTER_COEFF_4, 2#010).
-define(FILTER_COEFF_8, 2#011).
-define(FILTER_COEFF_16, 2#100).
% Bits 7,6,5 of config, times are in milliseconds
-define(STANDBY_TIME_0p5, 2#000).
-define(STANDBY_TIME_62p5, 2#001).
-define(STANDBY_TIME_125, 2#010).
-define(STANDBY_TIME_250, 2#011).
-define(STANDBY_TIME_500, 2#100).
-define(STANDBY_TIME_1000, 2#101).
-define(STANDBY_TIME_10, 2#110).
-define(STANDBY_TIME_20, 2#1111).

-define(CALIB1_REG_BEGIN, 16#88).  % Temp and Pressure calibration
-define(CALIB1_LEN, 26).

-define(CALIB2_REG_BEGIN, 16#E1). % Humidity calibration parameters
-define(CALIB2_LEN, 7).

-define(SENSOR_DATA_REG_BEGIN, 16#F7).
-define(SENSOR_DATA_LEN, 8).


% The active measurement time depends on the selected values for 
% humidity, temperature and pressure oversampling. 
% See the BME280 Data sheet for formulas to calculate typical and maximum
 
-define(MAX_MEASUREMENT_TIME_MS, 100).


%
% Configure the sensor
%
-spec configure_sensor(I2cRef :: pid(),
                       Config :: list(config_attr())) -> {ok, byte()} | {error, atom()}.

configure_sensor(I2cRef, Config) -> 
  case reset_sensor(I2cRef) of
    ok ->

      case set_config_reg(I2cRef, Config) of
        ok ->

          case set_humid_mode(I2cRef, Config) of
            ok ->

              case set_temp_press_read_modes(I2cRef, Config) of
                {ok, SensorMode} -> {ok, SensorMode};

                {error, Reason} -> {error, Reason}
              end;

            {error, Reason} -> {error, Reason} 
          end;

        {error, Reason} -> {error, Reason} 
      end;

    {error, Reason} -> {error, Reason}  
  end.

%
% Reset sensor
% 
-spec reset_sensor(I2cRef :: pid()) -> ok | {error, atom()}.

reset_sensor(I2cRef) ->
  case i2c_utils:write(I2cRef, <<?RESET_REG, ?RESET_VALUE>>) of
    ok -> 
      block_utils:sleep(50),  % delay after reset
      ok;

    {error, Reason} ->
      log_server:error(err_resetting_sensor, [Reason]),
      {error, Reason}
  end. 

%
% Set config register. Must do this before leaving sleep mode
%
-spec set_config_reg(I2cRef :: pid(), 
                     Config :: list(config_attr())) -> ok | {error, atom()}.

set_config_reg(I2cRef, Config) ->
  case get_standby_time(Config) of
    {ok, StandbyTime} ->

      case get_filter_coeff(Config) of
        {ok, FilterCoeff} ->

          case i2c_utils:write(I2cRef, <<?CONFIG_REG, StandbyTime:3, FilterCoeff:3, ?ENABLE_I2C:2>>) of
            ok -> ok;

            {error, Reason} ->
              log_server:error(err_setting_sensor_config_register, [Reason]),
              {error, Reason}
          end;

        {error, Reason} -> {error, Reason}
      end;

    {error, Reason} -> {error, Reason}
  end. 

%
% Set Humidity sensor mode
%
-spec set_humid_mode(I2cRef :: pid(), 
                     Config :: list(config_attr())) -> ok | {error, atom()}.

set_humid_mode(I2cRef, Config) ->
  case get_humid_mode(Config) of
    {ok, HumidMode} ->

      case i2c_utils:write(I2cRef, <<?CTRL_HUMID_REG, HumidMode>>) of
        ok -> ok;

        {error, Reason} ->
          log_server:error(err_setting_humidity_mode, [Reason]),
          {error, Reason}
      end;

    {error, Reason} -> {error, Reason}
  end. 

%
% Set Temperature sensor mode, Pressure sensor mode, and Read mode
%
-spec set_temp_press_read_modes(I2cRef :: pid(), 
                                Config :: list(config_attr())) -> {ok, byte()} | {error, atom()}.

set_temp_press_read_modes(I2cRef, Config) ->
  
  case get_temp_mode(Config) of
    {ok, TempMode} ->

      case get_press_mode(Config) of
        {ok, PressMode} ->

          case get_read_mode(Config) of
            {ok, ReadMode} ->

              <<SensorMode>> = <<TempMode:3, PressMode:3, ReadMode:2>>,
              case i2c_utils:write(I2cRef, <<?CTRL_MEAS_REG, SensorMode>>) of
                ok -> {ok, SensorMode};

                {error, Reason} ->
                  log_server:error(err_setting_temperature_pressure_or_read_mode, [Reason]),
                  {error, Reason}
              end;

            {error, Reason} -> {error, Reason}         
          end;

        {error, Reason} -> {error, Reason}
      end;

    {error, Reason} -> {error, Reason}
  end. 



%
% Get standby time from config values, and convert to sensor bit format
%
-spec get_standby_time(Config :: list(config_attr())) -> {ok, non_neg_integer()} | {error, config_err}.

get_standby_time(Config) ->
  case attrib_utils:get_value(Config, standby_time) of
    {ok, 0.5}  -> {ok, ?STANDBY_TIME_0p5};
    {ok, 62.5} -> {ok, ?STANDBY_TIME_62p5};
    {ok, 125}  -> {ok, ?STANDBY_TIME_125};
    {ok, 250}  -> {ok, ?STANDBY_TIME_250};
    {ok, 500}  -> {ok, ?STANDBY_TIME_500};
    {ok, 1000} -> {ok, ?STANDBY_TIME_1000};
    {ok, 10.0} -> {ok, ?STANDBY_TIME_10};
    {ok, 20.0} -> {ok, ?STANDBY_TIME_20};
    {ok, InvalidVal} ->
      log_server:error(err_is_an_invalid_standby_time_value, [InvalidVal]),
      {error, config_err};
    {error, Reason} ->
      log_server:error(err_reading_standby_time_value, [Reason]),
      {error, config_err}
  end.

%
% Get filter coefficient from config values, and convert to sensor bit format
%
-spec get_filter_coeff(Config :: list(config_attr())) -> {ok, non_neg_integer()} | {error, config_err}.

get_filter_coeff(Config) ->
  case attrib_utils:get_value(Config, filter_coeff) of
    {ok, 0}  -> {ok, ?FILTER_COEFF_OFF};
    {ok, 2}  -> {ok, ?FILTER_COEFF_2};
    {ok, 4}  -> {ok, ?FILTER_COEFF_4};
    {ok, 8}  -> {ok, ?FILTER_COEFF_8};
    {ok, 16} -> {ok, ?FILTER_COEFF_16};
    {ok, InvalidVal} ->
      log_server:error(err_is_an_invalid_filter_coefficient_value, [InvalidVal]),
      {error, config_err};
    {error, Reason} ->
      log_server:error(err_reading_filter_coefficient_value, [Reason]),
      {error, config_err}
  end.

%
% Get humidity sensor mode from config values, and convert to sensor bit format
%
-spec get_humid_mode(Config :: list(config_attr())) -> {ok, non_neg_integer()} | {error, config_err}.

get_humid_mode(Config) ->
  case attrib_utils:get_value(Config, humid_mode) of
    {ok, 0}  -> {ok, ?OSRS_HUMID_SKIP};
    {ok, 1}  -> {ok, ?OSRS_HUMID_OVSAMPL_1X};
    {ok, 2}  -> {ok, ?OSRS_HUMID_OVSAMPL_2X};
    {ok, 4}  -> {ok, ?OSRS_HUMID_OVSAMPL_4X};
    {ok, 8}  -> {ok, ?OSRS_HUMID_OVSAMPL_8X};
    {ok, 16} -> {ok, ?OSRS_HUMID_OVSAMPL_16X};
    {ok, InvalidVal} ->
      log_server:error(err_is_an_invalid_humidity_mode_value, [InvalidVal]),
      {error, config_err};
    {error, Reason} ->
      log_server:error(err_reading_humidity_mode_value, [Reason]),
      {error, config_err}
  end.

%
% Get temperature sensor mode from config values, and convert to sensor bit format
%
-spec get_temp_mode(Config :: list(config_attr())) -> {ok, non_neg_integer()} | {error, config_err}.

get_temp_mode(Config) ->
  case attrib_utils:get_value(Config, temp_mode) of
    {ok, 0}  -> {ok, ?OSRS_TEMP_SKIP};
    {ok, 1}  -> {ok, ?OSRS_TEMP_OVSAMPL_1X};
    {ok, 2}  -> {ok, ?OSRS_TEMP_OVSAMPL_2X};
    {ok, 4}  -> {ok, ?OSRS_TEMP_OVSAMPL_4X};
    {ok, 8}  -> {ok, ?OSRS_TEMP_OVSAMPL_8X};
    {ok, 16} -> {ok, ?OSRS_TEMP_OVSAMPL_16X};
    {ok, InvalidVal} ->
      log_server:error(err_is_an_invalid_temperature_mode_value, [InvalidVal]),
      {error, config_err};
    {error, Reason} ->
      log_server:error(err_reading_temperature_mode_value, [Reason]),
      {error, config_err}
  end.

%
% Get pressure sensor mode from config values, and convert to sensor bit format
%
-spec get_press_mode(Config :: list(config_attr())) -> {ok, non_neg_integer()} | {error, config_err}.

get_press_mode(Config) ->
  case attrib_utils:get_value(Config, press_mode) of
    {ok, 0}  -> {ok, ?OSRS_PRESS_SKIP};
    {ok, 1}  -> {ok, ?OSRS_PRESS_OVSAMPL_1X};
    {ok, 2}  -> {ok, ?OSRS_PRESS_OVSAMPL_2X};
    {ok, 4}  -> {ok, ?OSRS_PRESS_OVSAMPL_4X};
    {ok, 8}  -> {ok, ?OSRS_PRESS_OVSAMPL_8X};
    {ok, 16} -> {ok, ?OSRS_PRESS_OVSAMPL_16X};
    {ok, InvalidVal} ->
      log_server:error(err_is_an_invalid_pressure_mode_value, [InvalidVal]),
      {error, config_err};
    {error, Reason} ->
      log_server:error(err_reading_pressure_mode_value, [Reason]),
      {error, config_err}
  end.

%
% Get sensor read mode from config values, and convert to sensor bit format
%
-spec get_read_mode(Config :: list(config_attr())) -> {ok, non_neg_integer()} | {error, config_err}.

get_read_mode(Config) ->
  case attrib_utils:get_value(Config, read_mode) of
    {ok, sleep}  -> {ok, ?SENSOR_MODE_SLEEP};
    {ok, forced} -> {ok, ?SENSOR_MODE_FORCED};
    {ok, normal} -> {ok, ?SENSOR_MODE_NORMAL};
    {ok, InvalidVal} ->
      log_server:error(err_is_an_invalid_read_mode_value_sleep_normal_forced, [InvalidVal]),
      {error, config_err};
    {error, Reason} ->
      log_server:error(err_reading_read_mode_value, [Reason]),
      {error, config_err}
  end.


%
% Get sensor calibration values
%
-spec get_calibration(I2cRef :: pid(),
                      Private :: list(private_attr())) -> {ok, list(private_attr())} | {error, atom()}.

get_calibration(I2cRef, Private) ->
  % Read the first set of calibration data
  case i2c_utils:write_read(I2cRef, <<?CALIB1_REG_BEGIN>>, ?CALIB1_LEN) of
    {error, Reason} -> {error, Reason};
  
    % Attempt to parse the first set of calibration data
    <<Dig_T1:2/little-unsigned-integer-unit:8,  % 0x88 / 0x89 | dig_T1 [7:0] / [15:8] | unsigned short
      Dig_T2:2/little-signed-integer-unit:8,    % 0x8A / 0x8B | dig_T2 [7:0] / [15:8] | signed short
      Dig_T3:2/little-signed-integer-unit:8,    % 0x8C / 0x8D | dig_T3 [7:0] / [15:8] | signed short

      Dig_P1:2/little-unsigned-integer-unit:8,  % 0x8E / 0x8F | dig_P1 [7:0] / [15:8] | unsigned short
      Dig_P2:2/little-signed-integer-unit:8,    % 0x90 / 0x91 | dig_P2 [7:0] / [15:8] | signed short
      Dig_P3:2/little-signed-integer-unit:8,    % 0x92 / 0x93 | dig_P3 [7:0] / [15:8] | signed short
      Dig_P4:2/little-signed-integer-unit:8,    % 0x94 / 0x95 | dig_P4 [7:0] / [15:8] | signed short
      Dig_P5:2/little-signed-integer-unit:8,    % 0x96 / 0x97 | dig_P5 [7:0] / [15:8] | signed short
      Dig_P6:2/little-signed-integer-unit:8,    % 0x98 / 0x99 | dig_P6 [7:0] / [15:8] | signed short
      Dig_P7:2/little-signed-integer-unit:8,    % 0x9A / 0x9B | dig_P7 [7:0] / [15:8] | signed short
      Dig_P8:2/little-signed-integer-unit:8,    % 0x9C / 0x9D | dig_P8 [7:0] / [15:8] | signed short
      Dig_P9:2/little-signed-integer-unit:8,    % 0x9E / 0x9F | dig_P9 [7:0] / [15:8] | signed short
      _Fill:1/integer-unit:8,                   % 0XA0  not used
      Dig_H1:1/unsigned-integer-unit:8>> ->     % 0xA1        | dig_H1 [7:0]          | unsigned char
      
      % Read the second set of calibration data
      case i2c_utils:write_read(I2cRef, <<?CALIB2_REG_BEGIN>>, ?CALIB2_LEN) of
        {error, Reason} -> {error, Reason};
  
        % Attempt to parse the second set of calibration data
        <<Dig_H2:2/little-signed-integer-unit:8,  % 0xE1 / 0xE2      | dig_H2 [7:0] / [15:8] | signed short
          Dig_H3:1/unsigned-integer-unit:8,       % 0xE3             | dig_H3 [7:0]          | unsigned char
          Dig_H4_E4:1/integer-unit:8,             % 0xE4 / 0xE5[3:0] | dig_H4 [11:4] / [3:0] | signed short
          Dig_H4_H5_E5:1/integer-unit:8,          % 0xE5[7:4] / 0xE6 | dig_H5 [3:0] / [11:4] | signed short
          Dig_H5_E6:1/integer-unit:8,
          Dig_H6:1/signed-integer-unit:8>> ->     % 0xE7             | dig_H6                | signed char

          % Registers E4, E5, & E6 are used to make H4 and H5 calibration constants
          Dig_H4 = (Dig_H4_E4 bsl 4) bor (Dig_H4_H5_E5 band 16#0F),
          Dig_H5 = (Dig_H5_E6 bsl 4) bor ((Dig_H4_H5_E5 bsr 4) band 16#0F),

          % Update the private attributes with the calibration values 
          attrib_utils:set_values(Private, 
               [
                 {dig_T1, Dig_T1},
                 {dig_T2, Dig_T2},
                 {dig_T3, Dig_T3},

                 {dig_P1, Dig_P1},
                 {dig_P2, Dig_P2},
                 {dig_P3, Dig_P3},
                 {dig_P4, Dig_P4},
                 {dig_P5, Dig_P5},
                 {dig_P6, Dig_P6},
                 {dig_P7, Dig_P7},
                 {dig_P8, Dig_P8},
                 {dig_P9, Dig_P9},

                 {dig_H1, Dig_H1},
                 {dig_H2, Dig_H2},
                 {dig_H3, Dig_H3},
                 {dig_H4, Dig_H4},
                 {dig_H5, Dig_H5},
                 {dig_H6, Dig_H6}
               ])
      end
  end.
  
  

%
% Read the sensor using forced mode.
%
-spec read_sensor_forced(I2cRef :: pid(),
                  Private :: list(private_attr())) -> {ok, float(), float(), float()} | {error, atom()}.
                   
read_sensor_forced(I2cRef, Private) ->

  % Read mode is 'forced'. Need to write the forced read mode before each read
  % and wait for sensor to return to sleep mode before reading the value
  {ok, SensorMode} = attrib_utils:get_value(Private, sensor_mode),

  case i2c_utils:write(I2cRef, <<?CTRL_MEAS_REG, SensorMode>>) of
    ok -> 

      case wait_for_sleep_mode(?MAX_MEASUREMENT_TIME_MS, I2cRef) of
        ok ->
          read_sensor(I2cRef, Private);

        {error, Reason} ->
          log_server:error(err_waiting_for_sleep_mode, [Reason]),
          {error, Reason}
      end;

    {error, Reason} ->
      log_server:error(err_setting_forced_read_mode, [Reason]),
      {error, Reason}
  end.

%
% Wait for sensor to return to sleep mode
%
-spec wait_for_sleep_mode(pos_integer(), pid()) -> ok | {error, atom()}.

wait_for_sleep_mode(TotalWait, I2cRef) ->
  MinDelay = 3,

  if (TotalWait > MinDelay) ->
    block_utils:sleep(MinDelay),

    case i2c_utils:write_read(I2cRef, <<?CTRL_MEAS_REG>>, 1) of  
      <<_TempPressMode:6, ReadMode:2>> ->
    
        case ReadMode of
          ?SENSOR_MODE_SLEEP ->
            % sensor is back in sleep mode ok to read
            ok;

          _ -> 
            % keep waiting
            wait_for_sleep_mode(TotalWait - MinDelay, I2cRef)
        end;
      {error, Reason} -> {error, Reason}
    end;
  true -> 
    {error, timeout}
  end.
    
    
%
% Read the sensor in normal mode.
% Sensor is continuously reading, with the "StandbyTime" delay between each read.
% We are just reading the last values.
%
-spec read_sensor(I2cRef :: pid(),
                  Private :: list(private_attr())) -> {ok, float(), float(), float()} | {error, atom()}.
                   
read_sensor(I2cRef, Private) ->

  % Data readout is done by starting a burst read from 0xF7 to 0xFC (temperature and pressure) 
  % or from 0xF7 to 0xFE (temperature, pressure and humidity). 
  % The data are read out in an unsigned 20-bit format both for pressure and for temperature 
  % and in an unsigned 16-bit format for humidity.  

  case i2c_utils:write_read(I2cRef, <<?SENSOR_DATA_REG_BEGIN>>, ?SENSOR_DATA_LEN) of
    {error, Reason} -> {error, Reason};
  
    <<Adc_Press:20, _Fill1:4, Adc_Temp:20, _Fill2:4, Adc_Humid:16>> ->
      {Temp, T_fine} = compensate_temp(Adc_Temp, Private),
      Press = compensate_press(Adc_Press, T_fine, Private),
      Humid = compensate_humid(Adc_Humid, T_fine, Private),

      {ok, Temp, Press, Humid}
  end.


%
% Convert readings to desired units and add user configured offset values
%
-spec convert_readings(Temp :: float(),
                       Press :: float(),
                       Humid :: float(), 
                       Config :: list(config_attr())) -> {ok, float(), float(), float()} | {error, atom()}.
                            
convert_readings(Temp, Press, Humid, Config) ->
  case convert_temp(Temp, Config) of
    {ok, ConvTemp} ->

      case convert_press(Press, Config) of
        {ok, ConvPress} ->

          case convert_humid(Humid, Config) of
            {ok, ConvHumid} ->
              {ok, ConvTemp, ConvPress, ConvHumid};
            
            {error, Reason} -> {error, Reason}
          end;
         {error, Reason} -> {error, Reason}
      end;
    {error, Reason} -> {error, Reason}
  end.


%
% Apply units conversion and user defined offset to temperature reading
% Temperature is originally calculated in Deg C.
%
-spec convert_temp(Temp :: float(),
                   Config :: list(config_attr())) -> {ok, float()} | {error, atom()}.

convert_temp(Temp, Config) ->
  case config_utils:get_float(Config, temp_offset) of
    {ok, TempOffset} ->

      case config_utils:get_boolean(Config, deg_f) of
        {ok, true} -> 
          ConvTemp = (((Temp * 9) / 5) + 32.0) + TempOffset,
          {ok, ConvTemp};
        
        {ok, false} -> 
          ConvTemp = Temp + TempOffset,
          {ok, ConvTemp};
        
        {error, Reason} ->
          log_server:error(err_reading_deg_f_config_value, [Reason]),
          {error, Reason}
      end;

    {error, Reason} ->
      log_server:error(err_reading_temp_offset_config_value, [Reason]),
      {error, Reason}
  end.


%
% Apply units conversion and user defined offset to pressure reading
% Pressure is originally calculated in Pa.
%
-spec convert_press(Press :: float(),
                    Config :: list(config_attr())) -> {ok, float()} | {error, atom()}.

convert_press(Press, Config) ->
  case config_utils:get_float(Config, press_offset) of
    {ok, PressOffset} ->

      case config_utils:get_boolean(Config, inch_merc) of
        {ok, true} -> 
          ConvPress = (Press * 0.0002953) + PressOffset,
          {ok, ConvPress};
        
        {ok, false} -> 
          ConvPress = Press + PressOffset,
          {ok, ConvPress};
        
        {error, Reason} ->
          log_server:error(err_reading_inch_merc_config_value, [Reason]),
          {error, Reason}
      end;

    {error, Reason} ->
      log_server:error(err_reading_press_offset_config_value, [Reason]),
      {error, Reason}
  end.

%
% Apply user defined offset to humidity reading
%
-spec convert_humid(Humid :: float(),
                    Config :: list(config_attr())) -> {ok, float()} | {error, atom()}.

convert_humid(Humid, Config) ->
  case config_utils:get_float(Config, humid_offset) of
    {ok, HumidOffset} ->
      ConvHumid = Humid + HumidOffset,
      {ok, ConvHumid};

    {error, Reason} ->
      log_server:error(err_reading_humid_offset_config_value, [Reason]),
      {error, Reason}
  end.

%
% Compensate the raw temperature sensor value 
% Use the following formula, found in the BME280 documentation:
%
% Returns temperature in DegC, resolution is 0.01 DegC. Output value of “5123” equals 51.23 DegC.
% t_fine carries fine temperature as global value
% BME280_S32_t t_fine;
% BME280_S32_t BME280_compensate_T_int32(BME280_S32_t adc_T)
% {
%   BME280_S32_t var1, var2, T;
%   var1 = ((((adc_T>>3) - ((BME280_S32_t)dig_T1<<1))) * ((BME280_S32_t)dig_T2)) >> 11;
%   var2 = (((((adc_T>>4) - ((BME280_S32_t)dig_T1)) * ((adc_T>>4) - ((BME280_S32_t)dig_T1))) >> 12) * 
%           ((BME280_S32_t)dig_T3)) >> 14;
%   t_fine = var1 + var2;
%   T = (t_fine * 5 + 128) >> 8;
%   return T;
% }
%
-spec compensate_temp(Adc_T :: integer(),
                      Private :: list(private_attr())) -> {float(), integer()}.

compensate_temp(Adc_T, Private) ->
  % Get the calibration values
  {ok, Dig_T1} = attrib_utils:get_value(Private, dig_T1), 
  {ok, Dig_T2} = attrib_utils:get_value(Private, dig_T2), 
  {ok, Dig_T3} = attrib_utils:get_value(Private, dig_T3),

  Var1 = ((((Adc_T bsr 3) - (Dig_T1 bsl 1))) * Dig_T2) bsr 11,
  Var2 = (((((Adc_T bsr 4) - Dig_T1) * ((Adc_T bsr 4) - Dig_T1)) bsr 12) * Dig_T3) bsr 14,

  T_fine = Var1 + Var2,
  T = (T_fine * 5 + 128) bsr 8,
  T_DegC = T / 100,

  {T_DegC, T_fine}.


%
% Compensate the raw pressure sensor value
% Use the following formula, found in the BME280 documentation:
%
% Returns pressure in Pa as unsigned 32 bit integer in Q24.8 format (24 integer bits and 8 fractional bits).
% Output value of “24674867” represents 24674867/256 = 96386.2 Pa = 963.862 hPa
% BME280_U32_t BME280_compensate_P_int64(BME280_S32_t adc_P)
% {
%   BME280_S64_t var1, var2, p;
%   var1 = ((BME280_S64_t)t_fine) - 128000;
%   var2 = var1 * var1 * (BME280_S64_t)dig_P6;
%   var2 = var2 + ((var1*(BME280_S64_t)dig_P5)<<17);
%   var2 = var2 + (((BME280_S64_t)dig_P4)<<35);
%   var1 = ((var1 * var1 * (BME280_S64_t)dig_P3)>>8) + ((var1 * (BME280_S64_t)dig_P2)<<12);
%   var1 = (((((BME280_S64_t)1)<<47)+var1))*((BME280_S64_t)dig_P1)>>33;
%   if (var1 == 0)
%   {
%     return 0; // avoid exception caused by division by zero
%   }
%   p = 1048576-adc_P;
%   p = (((p<<31)-var2)*3125)/var1;
%   var1 = (((BME280_S64_t)dig_P9) * (p>>13) * (p>>13)) >> 25;
%   var2 = (((BME280_S64_t)dig_P8) * p) >> 19;
%   p = ((p + var1 + var2) >> 8) + (((BME280_S64_t)dig_P7)<<4);
%   return (BME280_U32_t)p;
% }
%
-spec compensate_press(Adc_P :: integer(),
                       T_fine :: integer(),
                       Private :: list(private_attr())) -> float().

compensate_press(Adc_P, T_fine, Private) ->
  % Get the calibration values
  {ok, Dig_P1} = attrib_utils:get_value(Private, dig_P1), 
  {ok, Dig_P2} = attrib_utils:get_value(Private, dig_P2), 
  {ok, Dig_P3} = attrib_utils:get_value(Private, dig_P3),
  {ok, Dig_P4} = attrib_utils:get_value(Private, dig_P4), 
  {ok, Dig_P5} = attrib_utils:get_value(Private, dig_P5), 
  {ok, Dig_P6} = attrib_utils:get_value(Private, dig_P6),
  {ok, Dig_P7} = attrib_utils:get_value(Private, dig_P7), 
  {ok, Dig_P8} = attrib_utils:get_value(Private, dig_P8), 
  {ok, Dig_P9} = attrib_utils:get_value(Private, dig_P9),

  Var1 = T_fine - 128000,
  Var2 = Var1 * Var1 * Dig_P6,
  Var2_1 = Var2 + ((Var1 * Dig_P5) bsl 17),
  Var2_2 = Var2_1 + (Dig_P4 bsl 35),
  Var1_1 = ((Var1 * Var1 * Dig_P3) bsr 8) + ((Var1 * Dig_P2) bsl 12),
  Var1_2 = (((1 bsl 47) + Var1_1)) * Dig_P1 bsr 33,

  if (Var1_2 /= 0) ->
    P = 1048576 - Adc_P,
    P_1 = (((P bsl 31) - Var2_2) * 3125 ) div Var1_2,
    Var1_3 = (Dig_P9 * (P_1 bsr 13) * (P_1 bsr 13)) bsr 25,
    Var2_3 = (Dig_P8 * P_1) bsr 19,
    P_2 = ((P_1 + Var1_3 + Var2_3) bsr 8) + (Dig_P7 bsl 4),
    P_2 / 256; % division converts to floating point number
  true -> 
    0.0  % return 0, avoid exception caused by division by zero
  end.


%
% Compensate the raw humidity sensor value
% Use the following formula, found in the BME280 documentation:
%
% Returns humidity in %RH as unsigned 32 bit integer in Q22.10 format (22 integer and 10 fractional bits).
% Output value of “47445” represents 47445/1024 = 46.333 %RH
% BME280_U32_t bme280_compensate_H_int32(BME280_S32_t adc_H)
% {
%   BME280_S32_t v_x1_u32r;
%   v_x1_u32r = (t_fine - ((BME280_S32_t)76800));
%   v_x1_u32r = (((((adc_H << 14) - (((BME280_S32_t)dig_H4) << 20) - (((BME280_S32_t)dig_H5) * v_x1_u32r)) +
%   ((BME280_S32_t)16384)) >> 15) * (((((((v_x1_u32r * ((BME280_S32_t)dig_H6)) >> 10) * (((v_x1_u32r * 
%   ((BME280_S32_t)dig_H3)) >> 11) + ((BME280_S32_t)32768))) >> 10) + ((BME280_S32_t)2097152)) *
%   ((BME280_S32_t)dig_H2) + 8192) >> 14));
%   v_x1_u32r = (v_x1_u32r - (((((v_x1_u32r >> 15) * (v_x1_u32r >> 15)) >> 7) * ((BME280_S32_t)dig_H1)) >> 4));
%   v_x1_u32r = (v_x1_u32r < 0 ? 0 : v_x1_u32r);
%   v_x1_u32r = (v_x1_u32r > 419430400 ? 419430400 : v_x1_u32r);
%   return (BME280_U32_t)(v_x1_u32r>>12);
% }
%
-spec compensate_humid(Adc_H :: integer(),
                       T_fine :: integer(),
                       Private :: list(private_attr())) -> float().

compensate_humid(Adc_H, T_fine, Private) ->
  % Get the calibration values
  {ok, Dig_H1} = attrib_utils:get_value(Private, dig_H1), 
  {ok, Dig_H2} = attrib_utils:get_value(Private, dig_H2), 
  {ok, Dig_H3} = attrib_utils:get_value(Private, dig_H3),
  {ok, Dig_H4} = attrib_utils:get_value(Private, dig_H4), 
  {ok, Dig_H5} = attrib_utils:get_value(Private, dig_H5), 
  {ok, Dig_H6} = attrib_utils:get_value(Private, dig_H6),

  V1 = T_fine - 76800,
  V2 = (((((Adc_H bsl 14) - (Dig_H4 bsl 20) - (Dig_H5 * V1)) +
       16384) bsr 15) * (((((((V1 * Dig_H6) bsr 10) * (((V1 * 
       Dig_H3) bsr 11) + 32768)) bsr 10) + 2097152) *
       Dig_H2 + 8192) bsr 14)),
  V3 = (V2 - (((((V2 bsr 15) * (V2 bsr 15)) bsr 7) * Dig_H1) bsr 4)),

  % limit the calculated value
  if (V3 < 0) -> 
    V4 = 0;
  true ->
    if (V3 > 419430400) -> 
      V4 = 419430400;
    true ->
      V4 = V3
    end
  end,

  H = V4 bsr 12,
  H / 1024. % division converts to floating point number


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% At a minimum, call the block type's create(), upgrade(), initialize(), execute(), and delete() functions.

block_test() ->
  log_server:start(lang_en_us),
  BlockDefn = create(create_test, "Unit Testing Block"),
  {ok, BlockDefn} = upgrade(BlockDefn),
  BlockState = block_common:initialize(BlockDefn),
  execute(BlockState, input_cos),
  _BlockDefnFinal = delete(BlockState),
  ?assert(true).

-endif.
