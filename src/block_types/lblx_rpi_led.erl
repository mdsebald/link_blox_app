%%% @doc 
%%% BLOCKTYPE
%%% Raspberry Pi on-board LED
%%% DESCRIPTION
%%% Control Raspberry Pi on-board LED 
%%% LINKS              
%%% @end 

-module(lblx_rpi_led).  

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [display, output].

version() -> "0.2.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {led_id, {"led0"}},  %| string | "led0" | "led0", "led1" |
      {default_value, {false}}, %| bool | false | true, false |
      {invert_output, {false}}  %| bool | false | true, false |
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {input, {empty, {empty}}}, %| bool | empty | true, false |
      {trigger, {"none", {"none"}}}, %| string | "none" | string representing led trigger mode |
      {delay_on, {250, {250}}}, %| integer | 250 | 1..Max Integer |
      {delay_off, {250, {250}}} %| integer | 250 | 1..Max Integer |
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
      logger:info(block_type_upgraded_from_ver_to, 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      logger:error(err_upgrading_block_type_from_ver_to, 
                            [Reason, BlockName, BlockType, ConfigVer, ModuleVer]),
      {error, Reason}
  end.


-define(LED_FILE_PATH, "/sys/class/leds/" ).

%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
  Private1 = attrib_utils:merge_attribute_lists(Private, 
    [
      {last_state, {empty}},
      {last_trigger, {empty}},
      {last_delay_on, {empty}},
      {last_delay_off, {empty}}
    ]),

  case config_utils:get_string(Config, led_id) of
    {ok, LedId} ->
      case config_utils:get_boolean(Config, default_value) of
        {ok, DefaultValue} ->
          case config_utils:get_boolean(Config, invert_output) of
            {ok, InvertOutput} -> 
              case filelib:is_file(?LED_FILE_PATH ++ LedId) of
                true ->
                  Status = initialed,
                  Value = DefaultValue,
                  {ok, Private2} = set_state(Private1, LedId, DefaultValue, InvertOutput);

                false ->
                  logger:error(err_LED_file_does_not_exist, [?LED_FILE_PATH ++ LedId]),
                  Private2 = Private1,
                  Status = proc_err,
                  Value = null
              end;
            {error, Reason} ->
              Private2 = Private1,
              {Value, Status} = config_utils:log_error(Config, invert_output, Reason)
          end;
        {error, Reason} ->
          Private2 = Private1,
          {Value, Status} = config_utils:log_error(Config, default_value, Reason)
      end;
    {error, Reason} ->
      Private2 = Private1,
      {Value, Status} = config_utils:log_error(Config, led_id, Reason)
  end,

  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),
    
  % This is the block state
  {Config, Inputs, Outputs1, Private2}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  {ok, LedId} = config_utils:get_string(Config, led_id),
  {ok, DefaultValue} = config_utils:get_boolean(Config, default_value),
  {ok, InvertOutput} = config_utils:get_boolean(Config, invert_output),
     
  % Set Output Val to input and set the actual LED file value too
  case input_utils:get_boolean(Inputs, input) of
    {ok, InValue} ->
      case input_utils:get_string(Inputs, trigger) of
        {ok, InTrigger} ->
          if (InTrigger == "timer") ->
            case input_utils:get_integer_greater_than(Inputs, delay_on, 0) of
              {ok, DelayOn} ->
                case input_utils:get_integer_greater_than(Inputs, delay_off, 0) of
                  {ok, DelayOff} ->
                    if (InValue == null) ->
                      LedValue = DefaultValue;
                    true ->
                      LedValue = InValue
                    end,
                    Trigger = InTrigger,
                    Value = InValue,
                    Status = normal;

                  {error, Reason} ->
                    {LedValue, Trigger, DelayOn, DelayOff, Value, Status} =
                      error_result(Config, delay_off, Reason, DefaultValue)
                end;

              {error, Reason} ->
                {LedValue, Trigger, DelayOn, DelayOff, Value, Status} =
                  error_result(Config, delay_on, Reason, DefaultValue)
            end;

          true -> % Trigger != "timer", don't care about on/off delay times
            if (InValue == null) ->
              LedValue = DefaultValue;
            true ->
              LedValue = InValue
            end,
            Trigger = InTrigger,
            DelayOn = DelayOff = 0,
            Value = InValue,
            Status = normal
          end;
        {error, Reason} ->
          {LedValue, Trigger, DelayOn, DelayOff, Value, Status} =
            error_result(Config, trigger, Reason, DefaultValue)
      end;
    {error, Reason} ->
      {LedValue, Trigger, DelayOn, DelayOff, Value, Status} =
        error_result(Config, input, Reason, DefaultValue)
  end,

  {ok, Private1} = set_state(Private, LedId, LedValue, InvertOutput),
  {ok, Private2} = set_trigger(Private1, LedId, Trigger),
    
  % Only set delay_on and delay_off, if trigger is "timer"
  if (Trigger == "timer") ->
    {ok, Private3} = set_delay_on(Private2, LedId, DelayOn),
    {ok, Private4} = set_delay_off(Private3, LedId, DelayOff);
      
  true ->
    Private4 = Private2
  end,

  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  {Config, Inputs, Outputs1, Private4}.


%% 
%%  Delete the block
%%	
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  %
  % Private values are created in the block initialization routine
  % So they should be deleted here
  
  {Config, Inputs, Outputs}.


%% ====================================================================
%% Internal functions
%% ====================================================================

% TODO: May switch to using nerves_led project
%set_led(LedId, Trigger, DelayOn, DelayOff) ->
%  Elixir.Nerves.Leds:set( LedId, [ {trigger, Trigger}, {delay_on, DelayOn}, {delay_off, DelayOff}]).

% Found some input value error
% Log error, and return appropriate values
error_result(Config, ErrorInput, Reason, DefaultValue) ->
  LedValue = DefaultValue,
  Trigger = "none",
  DelayOn = DelayOff = 0,
  {Value, Status} = input_utils:log_error(Config, ErrorInput, Reason),
  {LedValue, Trigger, DelayOn, DelayOff, Value, Status}.


% Set the actual value of the LED file here
set_state(Private, LedId, Value, Invert) ->
  
  if Value -> % Value is true/on
    if Invert -> % Invert pin value 
      State = "0"; % turn output off
    true ->      % Don't invert_output output value
      State = "1" % turn output on
    end;
  true -> % Value is false/off
    if Invert -> % Invert pin value
      State = "1"; % turn output on
    true ->      % Don't invert_output output value
      State = "0"  % turn output off
    end
  end,

  {ok, LastState} = attrib_utils:get_value(Private, last_state),
  if (State /= LastState) ->
    FileId = ?LED_FILE_PATH ++ LedId ++ "/brightness",
    write_file(FileId, State),
    attrib_utils:set_value(Private, last_state, State);

  true -> % Last state value same as new state, do nothing
    {ok, Private}
  end.


% Set the LED trigger file
set_trigger(Private, LedId, Trigger) ->
  {ok, LastTrigger} = attrib_utils:get_value(Private, last_trigger),
  if (Trigger /= LastTrigger) ->
    FileId = ?LED_FILE_PATH ++ LedId ++ "/trigger",
    write_file(FileId, Trigger),
    attrib_utils:set_value(Private, last_trigger, Trigger);

  true -> % Last trigger value same as new trigger, do nothing
    {ok, Private}
  end.


% Set the LED delay_on file
set_delay_on(Private, LedId, DelayOn) ->
  {ok, LastOnDelay} = attrib_utils:get_value(Private, last_delay_on),
  if (DelayOn /= LastOnDelay) ->
    FileId = ?LED_FILE_PATH ++ LedId ++ "/delay_on",
    write_file(FileId, integer_to_list(DelayOn)),
    attrib_utils:set_value(Private, last_delay_on, DelayOn);

  true -> % Last delay_on value same as new delay_on, do nothing
    {ok, Private}
  end.


% Set the LED delay_off file
set_delay_off(Private, LedId, DelayOff) ->
  {ok, LastOffDelay} = attrib_utils:get_value(Private, last_delay_off),
  if (DelayOff /= LastOffDelay) -> 
    FileId = ?LED_FILE_PATH ++ LedId ++ "/delay_off",
    write_file(FileId, integer_to_list(DelayOff)),
    attrib_utils:set_value(Private, last_delay_off, DelayOff);

  true -> % Last delay_off value same as new delay_off, do nothing
    {ok, Private}
  end.


% Write a value to one of the LED control files
write_file(FileId, Value) ->
    file:write_file(FileId, Value).


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