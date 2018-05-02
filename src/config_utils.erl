%%% @doc 
%%% Get and Validate Block Config values   
%%%               
%%% @end 

-module(config_utils).

-author("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
          name/1, 
          module/1, 
          name_module/1, 
          name_module_version/1,
          get_any_type/2,
          get_integer/2,
          get_pos_integer/2,
          get_integer_greater_than/3,
          get_integer_range/4,
          get_float/2, 
          get_boolean/2,
          get_string/2,
          get_atom/2,
          get_node/2,
          get_value/3,
          init_i2c/2,
          init_gpio/4,
          resize_attribute_array_value/4,
          log_error/3
]).


%%
%%  Get block name from Config attribute values
%%
-spec name(Config :: config_attribs() |
           block_defn() | 
           block_state()) -> block_name().

name({Config, _Inputs, _Outputs, _Private}) ->
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  BlockName;
  
name({Config, _Inputs, _Outputs}) ->  
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  BlockName;
  
name(Config) ->
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  BlockName.

  
%%
%% Get block module from Config attribute values 
%%
-spec module(Config :: config_attribs() | 
             block_defn() | 
             block_state()) -> module().

module({Config, _Inputs, _Outputs, _Private}) ->
  {ok, BlockModule} = attrib_utils:get_value(Config, block_module),
  BlockModule;
  
module({Config, _Inputs, _Outputs}) ->
  {ok, BlockModule} = attrib_utils:get_value(Config, block_module),
  BlockModule;

module(Config) ->
  {ok, BlockModule} = attrib_utils:get_value(Config, block_module),
  BlockModule.


%%
%%  Get block name and module from Config attribute values
%%
-spec name_module(Config :: config_attribs() |
                  block_defn() | 
                  block_state()) -> {block_name(), module()}.

name_module({Config, _Inputs, _Outputs, _Private}) ->
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  {ok, BlockModule} = attrib_utils:get_value(Config, block_module),
  {BlockName, BlockModule};
  
name_module({Config, _Inputs, _Outputs}) ->  
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  {ok, BlockModule} =  attrib_utils:get_value(Config, block_module),
  {BlockName, BlockModule};
                       
name_module(Config) ->
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  {ok, BlockModule} =  attrib_utils:get_value(Config, block_module),
  {BlockName, BlockModule}.
  
  
%%
%%  Get block name, module, and version from Config attribute values
%%
-spec name_module_version(Config :: config_attribs() |
                          block_defn() | 
                          block_state()) -> {block_name(), module(), string()}.

name_module_version({Config, _Inputs, _Outputs, _Private}) ->
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  {ok, BlockModule} = attrib_utils:get_value(Config, block_module),
  {ok, Version} = attrib_utils:get_value(Config, version),
  {BlockName, BlockModule, Version};
  
name_module_version({Config, _Inputs, _Outputs}) ->  
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  {ok, BlockModule} =  attrib_utils:get_value(Config, block_module),
  {ok, Version} = attrib_utils:get_value(Config, version),
  {BlockName, BlockModule, Version};
                       
name_module_version(Config) ->
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  {ok, BlockModule} =  attrib_utils:get_value(Config, block_module),
  {ok, Version} = attrib_utils:get_value(Config, version),
  {BlockName, BlockModule, Version}.


%%
%% Get configuration value of any type and check for errors.
%%
-spec get_any_type(Config :: config_attribs(),
                   ValueId :: value_id()) -> generic_config_value().

get_any_type(Config, ValueId) ->
  % Return true for every value
  CheckType = fun(_Value) -> true end,
  get_value(Config, ValueId, CheckType).


%%
%% Get an integer configuration value and check for errors.
%%
-spec get_integer(Config :: config_attribs(), 
                  ValueId :: value_id()) -> integer_config_value().

get_integer(Config, ValueId) ->
  CheckType = fun is_integer/1,
  get_value(Config, ValueId, CheckType).


%%
%% Get a postitive (1...) integer configuration value and check for errors.
%%
-spec get_pos_integer(Config :: config_attribs(), 
                      ValueId :: value_id()) -> integer_config_value().

get_pos_integer(Config, ValueId) ->
  CheckType = fun is_integer/1,
  case get_value(Config, ValueId, CheckType) of
    {error, Reason} -> 
      {error, Reason};
    {ok, Value} ->
      if (Value < 1 ) ->
        {error, range};
      true -> 
        {ok, Value}
      end
  end.


%%
%% Get an integer config value greater than minimum and check for errors.
%%
-spec get_integer_greater_than(Config :: config_attribs(), 
                               ValueId :: value_id(),
                               Min :: integer()) -> integer_config_value().

get_integer_greater_than(Config, ValueId, Min) ->
  case get_integer(Config, ValueId) of
    {error, Reason} ->
      {error, Reason};
    {ok, Value} ->
      if (Value < Min) ->
        {error, range};
      true -> 
        {ok, Value}
      end
  end.


%%
%% Get an integer configuration value within specified range and check for errors.
%%
-spec get_integer_range(Config :: config_attribs(), 
                        ValueId :: value_id(),
                        Min :: integer(),
                        Max :: integer()) -> integer_config_value().

get_integer_range(Config, ValueId, Min, Max) ->
  CheckType = fun is_integer/1,
  case get_value(Config, ValueId, CheckType) of
    {error, Reason} -> 
      {error, Reason};
    {ok, Value} ->
      if (Value < Min ) orelse (Max < Value) ->
        {error, range};
      true -> 
        {ok, Value}
      end
  end.


%%
%% Get a floating point configuration value and check for errors.
%%
-spec get_float(Config :: config_attribs(), 
                ValueId :: value_id()) -> float_config_value().

get_float(Config, ValueId) ->
  CheckType = fun is_float/1,
  get_value(Config, ValueId, CheckType).
  
  
%%
%% Get a boolean configuration value and check for errors
%%
-spec get_boolean(Config :: config_attribs(), 
                  ValueId :: value_id()) -> boolean_config_value().

get_boolean(Config, ValueId) ->
  CheckType = fun is_boolean/1,
  get_value(Config, ValueId, CheckType).


%%
%% Get a string configuration value and check for errors
%%
-spec get_string(Config :: config_attribs(), 
                ValueId :: value_id()) -> string_config_value().

get_string(Config, ValueId) ->
  CheckType = fun block_utils:is_string/1,  
  get_value(Config, ValueId, CheckType).


%%
%% Get an atom configuration value and check for errors
%%
-spec get_atom(Config :: config_attribs(), 
               ValueId :: value_id()) -> string_config_value().

get_atom(Config, ValueId) ->
  CheckType = fun is_atom/1,  
  get_value(Config, ValueId, CheckType).


%%
%% Get a node configuration value and check for errors.
%%
-spec get_node(Config :: config_attribs(),
               ValueId :: value_id()) -> {ok, node()} | {error, atom()}.

get_node(Config, ValueId) ->
  % TODO: Create CheckType to check for node value types
  % currently any value is acceptable
  CheckType = fun(_Value) -> true end,
  get_value(Config, ValueId, CheckType).


%%
%% Generic get configuration value, check for errors.
%%
-spec get_value(Config :: config_attribs(),
                ValueId :: value_id(),
                CheckType :: fun()) -> term().
                
get_value(Config, ValueId, CheckType) ->
   % get the whole attribute, not just the value
  case attrib_utils:get_attribute(Config, ValueId) of
    {error, not_found}  -> {error, not_found};
    
    % If this is a non-array value, check it
    {ok, {_ValueName, {Value}}} ->
      case CheckType(Value) of
        true  -> {ok, Value};
        false -> {error, bad_type}   
      end;

    % if this is an array value, check the index, and get the nth element
    {ok, {ValueName, ArrayValue}} ->
      % if this is an array value, the ValueName from get_attribute()
      % will match ValueName in the ValueId tuple
      case ValueId of
        {ValueName, ArrayIndex} ->
          if (0 < ArrayIndex) andalso (ArrayIndex =< length(ArrayValue)) ->
            {Value} = lists:nth(ArrayIndex, ArrayValue),
            case CheckType(Value) of
              true  -> {ok, Value};
              false -> {error, bad_type}   
            end;
          true->  % array index is out of bounds
            {error, invalid_index}
          end;
        _ -> {error, invalid_value}
      end
  end.


%%
%% Initialize I2C Bus Connection
%%
-spec init_i2c(Config :: config_attribs(),
               Private :: private_attribs()) -> {ok, private_attribs(), pid()} | {error, atom()}.

init_i2c(Config, Private) ->
  case get_string(Config, i2c_device) of

    {ok, I2cDevice} ->
      case i2c_utils:is_installed(I2cDevice) of
        true ->
          case get_integer_range(Config, i2c_addr, 2, 127) of

            {ok, I2cAddr} ->
              case i2c_utils:start_link(I2cDevice, I2cAddr) of

                {ok, I2cRef} ->
                  {ok, attrib_utils:add_attribute(Private, {i2c_ref, {I2cRef}}), I2cRef};

                {error, Reason} ->
                  logger:error(err_initiating_I2C_address, [Reason, I2cAddr]),
                  {error, Reason}
              end;
            {error, Reason} ->
              log_error(Config, i2c_addr, Reason),
              {error, Reason}
          end;
        false ->
          logger:error(err_i2c_not_installed, [I2cDevice]),
          {error, i2c_not_installed}
      end;
    {error, Reason} ->
      log_error(Config, i2c_device, Reason),
      {error, Reason}
  end.


%%
%% Initialize a GPIO pin before use
%%
-spec init_gpio(Config :: config_attribs(),
                Private :: private_attribs(),
                Name :: value_name(),
                Direction :: atom()) -> {ok, private_attribs(), pid()} | {error, atom()}.
              
init_gpio(Config, Private, Name, Direction) ->

  case config_utils:get_integer_range(Config, Name, 1, 40) of
    {ok, PinNumber} ->
      case gpio_utils:start_link(PinNumber, Direction) of
        {ok, GpioPinRef} ->
          {ok, attrib_utils:add_attribute(Private, {gpio_pin_ref, {GpioPinRef}}), GpioPinRef};
        
        {error, Reason} ->
          BlockName = config_utils:name(Config),
          logger:error(err_initiating_GPIO_pin, [BlockName, Reason, PinNumber]),
          {error, Reason}
      end;

    {error, Reason} ->
      log_error(Config, Name, Reason),
      {error, Reason}
  end.


%%
%% Resize an array value in the Config attribute list
%% to match the target quantity
%% Returns updated Config attribute list
%%
-spec resize_attribute_array_value(Config :: config_attribs(),
                                   ArrayValueName :: value_name(),
                                   TargQuant :: pos_integer(),
                                   DefaultValue :: config_value()) -> config_attribs().
                             
resize_attribute_array_value(Config, ArrayValueName, TargQuant, DefaultValue)->
  attrib_utils:resize_attribute_array_value(Config, ArrayValueName, TargQuant, DefaultValue).


%%
%% Log config value error
%%
-spec log_error(Config :: config_attribs(),
                ValueId :: value_id(),
                Reason :: atom()) -> {null, config_err}.
                  
log_error(Config, ValueId, Reason) ->
  BlockName = name(Config),
  ValueIdStr = attrib_utils:value_id_to_str(ValueId),
  logger:error(err_invalid_config_value, [BlockName, ValueIdStr, Reason]),
  {null, config_err}.
  
  
%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


% get_integer/2,
% get_pos_integer/2,
% get_integer_greater_than/3,
% get_integer_range/4,
% get_float/2, 
% get_boolean/2,
% get_string/2,
% get_atom/2,
% get_node/2,
% get_value/3,
% init_i2c/2,
% init_gpio/4,
% resize_attribute_array_value/4,
% log_error/3

% ====================================================================
% Test name()
% 
name_config_test()->
  Config = test_data:config_utils_config_attribs1(),
  Actual = name(Config),
  Expected = test_config_utils,
  ?assertEqual(Actual, Expected).
  
name_block_defn_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  BlockDefn = {Config, Inputs, Outputs},
  Actual = name(BlockDefn),  
  Expected = test_config_utils,
  ?assertEqual(Actual, Expected).
  
name_block_state_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  Private = [],
  BlockState = {Config, Inputs, Outputs, Private},
  Actual = name(BlockState),  
  Expected = test_config_utils,
  ?assertEqual(Actual, Expected).
% ====================================================================

% ====================================================================
% Test module()
% 
module_config_test()->
  Config = test_data:config_utils_config_attribs1(),
  Actual = module(Config),
  Expected = type_test,
  ?assertEqual(Actual, Expected).
  
module_block_defn_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  BlockDefn = {Config, Inputs, Outputs},
  Actual = module(BlockDefn),  
  Expected = type_test,
  ?assertEqual(Actual, Expected).
  
module_block_state_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  Private = [],
  BlockState = {Config, Inputs, Outputs, Private},
  Actual = module(BlockState),  
  Expected = type_test,
  ?assertEqual(Actual, Expected).
% ====================================================================

% ====================================================================
% Test name_module()
% 
name_module_config_test()->
  Config = test_data:config_utils_config_attribs1(),
  Actual = name_module(Config),  
  Expected = {test_config_utils, type_test},
  ?assertEqual(Actual, Expected).
  
name_module_block_defn_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  BlockDefn = {Config, Inputs, Outputs},
  Actual = name_module(BlockDefn),  
  Expected = {test_config_utils, type_test},
  ?assertEqual(Actual, Expected).
  
name_module_block_state_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  Private = [],
  BlockState = {Config, Inputs, Outputs, Private},
  Actual = name_module(BlockState),  
  Expected = {test_config_utils, type_test},
  ?assertEqual(Actual, Expected).
% ====================================================================

% ====================================================================
% Test name_module_version()
% 
name_module_version_config_test()->
  Config = test_data:config_utils_config_attribs1(),
  Actual = name_module_version(Config), 
  Expected = {test_config_utils, type_test, "0.0.0"},
  ?assertEqual(Actual, Expected).
  
name_module_version_block_defn_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  BlockDefn = {Config, Inputs, Outputs},
  Actual = name_module_version(BlockDefn),  
  Expected = {test_config_utils, type_test, "0.0.0"},
  ?assertEqual(Actual, Expected).
  
name_module_version_block_state_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  Private = [],
  BlockState = {Config, Inputs, Outputs, Private},
  Actual = name_module_version(BlockState), 
  Expected = {test_config_utils, type_test, "0.0.0"},
  ?assertEqual(Actual, Expected).
% ====================================================================

% ====================================================================
% Test get_any_type()
% 
get_any_type_not_found_test()->
  Config = test_data:config_utils_config_attribs1(),
  ValueName = doesnt_exist,
  Actual = get_any_type(Config, ValueName),  
  Expected = {error, not_found},
  ?assertEqual(Actual, Expected).

get_any_type_ok_test()->
  Config = test_data:config_utils_config_attribs1(),
  ValueName = string1,
  Actual = get_any_type(Config, ValueName),  
  Expected = {ok, "Testing"},
  ?assertEqual(Actual, Expected).

% ====================================================================

% ====================================================================
% Test get_integer_range()
% 
get_integer_range_test()->
  Config = test_data:config_utils_config_attribs1(),
  ValueName = integer_good,
  Actual = get_integer_range(Config, ValueName, 1, 100),  
  Expected = {error, range},
  ?assertEqual(Actual, Expected).
% ====================================================================
  


get_value_test() ->
  _Config = test_data:config_utils_config_attribs1().


% ====================================================================
% Test log_error()
%     
log_error_test() ->
  Config = test_data:config_utils_config_attribs1(),
  Actual = log_error(Config, value_name, bad_value),  
  Expected =  {null, config_err},
  ?assertEqual(Actual, Expected).
% ====================================================================

-endif.