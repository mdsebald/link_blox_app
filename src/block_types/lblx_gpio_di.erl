%%% @doc 
%%% Block Type: GPIO Digital Input 
%%% Description: Configure a GPIO Pin as a Digital Input block  
%%%               
%%% @end 

-module(lblx_gpio_di).

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).
-export([handle_info/2]).

groups() -> [digital, input].

description() -> "GPIO digital input". 

version() -> "0.1.0". 


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {gpio_pin, {0}}, 
      {invert_output, {false}}
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
      {active_true, {empty, []}},
      {active_false, {empty, []}}
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
      logger:info(block_type_upgraded_from_ver_to, 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      logger:error(err_upgrading_block_type_from_ver_to, 
                            [Reason, BlockName, BlockType, ConfigVer, ModuleVer]),
      {error, Reason}
  end.


%%
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
%%
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  Private1 = attrib_utils:add_attribute(Private, {gpio_pin_ref, {empty}}),
    
  % Get the GPIO pin number used by this block
  case config_utils:get_integer_range(Config, gpio_pin, 1, 40) of
    {ok, PinNumber} ->
      % Initialize the GPIO pin as an input
      case gpio_utils:start_link(PinNumber, input) of
        {ok, GpioPinRef} ->
          Status = initialed,
          Value = null,
          {ok, Private2} = attrib_utils:set_value(Private1, gpio_pin_ref, GpioPinRef),
          % TODO: Make interrupt type selectable via config value, check return value
          gpio_utils:register_int(GpioPinRef),
          gpio_utils:set_int(GpioPinRef, both);

        {error, ErrorResult} ->
          BlockName = config_utils:name(Config),
          logger:error(err_initiating_GPIO_pin, 
                              [BlockName, ErrorResult, PinNumber]),
          Status = proc_err,
          Value = null,
          Private2 = Private1
      end;
    {error, Reason} ->
      {Value, Status} = config_utils:log_error(Config, gpio_pin, Reason),
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

  % Read the current value of the GPIO pin 
  {ok, GpioPinRef} = attrib_utils:get_value(Private, gpio_pin_ref),

  Value = read_pin_value_bool(GpioPinRef),

  Outputs1 = output_utils:set_tristate_outputs(input, {ok, Value}, Config, Outputs),
        
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%  
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, Private}) -> 
  % Release the GPIO pin, if used
  case attrib_utils:get_value(Private, gpio_pin_ref) of
    {ok, GpioRef} -> gpio_utils:stop(GpioRef);

    _DoNothing -> ok
  end,
  {Config, Inputs, Outputs}.


%% 
%% GPIO Interupt message from Erlang ALE library, 
%% Execute this block
%% 
-spec handle_info(Info :: term(), 
                  BlockState :: block_state()) -> {noreply, block_state()}.

handle_info({gpio_interrupt, Pin, Condition}, BlockState) ->
  logger:debug("Rx interrupt: ~p from GPIO pin: ~p ~n", [Condition, Pin]),
  NewBlockState = block_common:execute(BlockState, hardware),
  {noreply, NewBlockState};

%%
%% Unknown Info message, just log a warning
%% 
handle_info(Info, BlockState) ->
  {BlockName, BlockModule} = config_utils:name_module(BlockState),
  logger:warning(block_type_name_unknown_info_msg, [BlockModule, BlockName, Info]),
  {noreply, BlockState}.  


%% ====================================================================
%% Internal functions
%% ====================================================================

read_pin_value_bool(GpioPinRef) ->
  case gpio_utils:read(GpioPinRef) of
    1  -> true;
    0 -> false
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
  