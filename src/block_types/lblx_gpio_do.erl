%%% @doc 
%%% Block Type: GPIO Digital Output
%%% Description: Configure a GPIO Pin as a Digital Output block
%%%               
%%% @end 

-module(lblx_gpio_do).

-author("Mark Sebald").

-include("../block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [digital, output].

description() -> "GPIO digital output". 

version() -> "0.1.0". 

%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {gpio_pin, {0}}, 
      {default_value, {false}},
      {invert_output, {false}}
    ]).


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {input, {empty, ?EMPTY_LINK}}
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
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  Private1 = attrib_utils:add_attribute(Private, {gpio_pin_ref, {empty}}),
  
  % Get the GPIO Pin number used for digital outputs  
  case config_utils:get_integer_range(Config, gpio_pin, 1, 40) of
    {ok, PinNumber} ->
      case config_utils:get_boolean(Config, default_value) of
        {ok, DefaultValue} ->
          case config_utils:get_boolean(Config, invert_output) of
            {ok, InvertOutput} -> 
              case gpio_utils:start_link(PinNumber, output) of
                {ok, GpioPinRef} ->
                  Status = initialed,
                  Value = DefaultValue,
                   {ok, Private2} = 
                    attrib_utils:set_value(Private1, gpio_pin_ref, GpioPinRef),
                  set_pin_value_bool(GpioPinRef, DefaultValue, InvertOutput);
            
                {error, ErrorResult} ->
                  BlockName = config_utils:name(Config),
                  log_server:error(err_initiating_GPIO_pin, [BlockName, ErrorResult, PinNumber]),
                  Status = proc_err,
                  Value = null,
                  Private2 = Private1
              end;
            {error, Reason} ->
              log_server:error(err_reading_invert_output_value, [Reason]),
              Status = config_err,
              Value = null,
              Private2 = Private1
          end;
        {error, Reason} ->
          log_server:error(err_reading_default_value, [Reason]),
          Status = config_err,
          Value = null,
          Private2 = Private1
      end;
    {error, Reason} ->
      BlockName = config_utils:name(Config),
      log_server:error(err_reading_GPIO_pin_number, [BlockName, Reason]),
      Status = config_err,
      Value = null,
      Private2 = Private1
  end,

  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),
    
  {Config, Inputs, Outputs1, Private2}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  
  {ok, GpioPinRef} = attrib_utils:get_value(Private, gpio_pin_ref),
  {ok, DefaultValue} = attrib_utils:get_value(Config, default_value),
  {ok, InvertOutput} = attrib_utils:get_value(Config, invert_output),
     
  % Set Output Val to input and set the actual GPIO pin value too
  case input_utils:get_boolean(Inputs, input) of
    {ok, null} -> 
      PinValue = DefaultValue, 
      Value = null,
      Status = ok;

    {ok, Value} ->  
      PinValue = Value, 
      Status = normal;

     {error, Reason} ->
      PinValue = DefaultValue,
      {Value, Status} = input_utils:log_error(Config, input, Reason)
  end,

  set_pin_value_bool(GpioPinRef, PinValue, InvertOutput),
 
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) ->
  % TODO: Release the GPIO pin?
  {Config, Inputs, Outputs}.


%% ====================================================================
%% Internal functions
%% ====================================================================

% Set the actual value of the GPIO pin here
set_pin_value_bool(GpioPinRef, Value, Invert) ->
  if Value -> % Value is true/on
    if Invert -> % Invert pin value 
      gpio_utils:write(GpioPinRef, 0); % turn output off
    true ->      % Don't invert_output output value
      gpio_utils:write(GpioPinRef, 1) % turn output on
    end;
  true -> % Value is false/off
    if Invert -> % Invert pin value
      gpio_utils:write(GpioPinRef, 1); % turn output on
    true ->      % Don't invert_output output value
      gpio_utils:write(GpioPinRef, 0)  % turn output off
    end
  end.


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% Perform minimum block unit test

block_test() ->
  unit_test_utils:min_block_test(?MODULE).


-endif.