%%% @doc 
%%% BLOCKTYPE
%%% GPIO Digital Output
%%% DESCRIPTION
%%% Configure a GPIO Pin as a Digital Output block
%%% LINKS
%%% https://www.raspberrypi.org/documentation/usage/gpio-plus-and-raspi2/README.md
%%% @end 

-module(lblx_gpio_do).

-author("Mark Sebald").

-include("../block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [digital, output].

version() -> "0.1.1". 

%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {gpio_pin, {0}}, %| int | 0 | 0..40 |
      {default_value, {false}}, %| bool | false | true, false |
      {invert_output, {false}} %| bool | false | true, false |
    ]).


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {input, {empty, {empty}}} %| bool | empty | true, false |
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
             InitOutputs :: output_attribs()) -> block_defn().

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
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
%%
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  case config_utils:get_boolean(Config, default_value) of
    {ok, DefaultValue} ->
      case config_utils:get_boolean(Config, invert_output) of
        {ok, InvertOutput} ->
          case config_utils:init_gpio(Config, gpio_pin, output) of
            {ok, GpioPinRef} ->
              Private1 = attrib_utils:add_attribute(Private, {gpio_pin_ref, {GpioPinRef}}),
              Status = initialed,
              Value = DefaultValue,
              set_pin_value_bool(GpioPinRef, DefaultValue, InvertOutput);
        
            {error, _Reason} ->
              Status = proc_err,
              Value = null,
              Private1 = Private
          end;
        {error, Reason} ->
          {Value, Status} = config_utils:log_error(Config, invert_output, Reason),
          Private1 = Private
      end;
    {error, Reason} ->
      {Value, Status} = config_utils:log_error(Config, default_value, Reason),
      Private1 = Private
  end,

  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),
    
  {Config, Inputs, Outputs1, Private1}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, disable) ->
  % TODO: Set Default state
  Outputs1 = output_utils:update_all_outputs(Outputs, null, disabled),
  {Config, Inputs, Outputs1, Private};

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  {ok, GpioPinRef} = attrib_utils:get_value(Private, gpio_pin_ref),
  {ok, DefaultValue} = attrib_utils:get_value(Config, default_value),
  {ok, InvertOutput} = attrib_utils:get_value(Config, invert_output),
     
  % Set Output Val to input and set the actual GPIO pin value too
  case input_utils:get_boolean(Inputs, input) of
    {ok, null} -> 
      PinValue = DefaultValue, 
      Value = null,
      Status = normal;

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
  % Nothing to close, stop, or release for a GPIO pin
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

-include("block_io_test_gen.hrl").

test_sets() ->
  [
    {[{status, proc_err}]}
  ].

-endif.