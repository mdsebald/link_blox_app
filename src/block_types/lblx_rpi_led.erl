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
      {default_trigger, {"none"}}, %| string | "none" | string representing led trigger mode |
      {default_delay_on, {250}}, %| integer | 250 | 1..Max Integer |
      {default_delay_off, {250}}, %| integer | 250 | 1..Max Integer |
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

  case config_utils:get_string(Config, led_id) of
    {ok, LedId} ->
      case config_utils:get_boolean(Config, default_value) of
        {ok, DefaultValue} ->
          case config_utils:get_string(Config, default_trigger) of
            {ok, DefaultTrigger} ->
              case config_utils:get_pos_integer(Config, default_delay_on) of
                {ok, DefaultDelayOn} ->
                  case config_utils:get_pos_integer(Config, default_delay_off) of
                    {ok, DefaultDelayOff} ->
                      case config_utils:get_boolean(Config, invert_output) of
                        {ok, InvertOutput} -> 
                          case filelib:is_file(?LED_FILE_PATH ++ LedId) of
                            true ->
                              Status = initialed,
                              Value = DefaultValue,
                              set_led_state(LedId, DefaultValue, DefaultTrigger, DefaultDelayOn, 
                                            DefaultDelayOff, InvertOutput);
                            false ->
                              logger:error(err_LED_file_does_not_exist, [?LED_FILE_PATH ++ LedId]),
                              Status = proc_err,
                              Value = null
                            end;
                        {error, Reason} ->
                          {Value, Status} = config_utils:log_error(Config, invert_output, Reason)
                      end;
                    {error, Reason} ->
                      {Value, Status} = config_utils:log_error(Config, default_delay_off, Reason)
                  end;
                {error, Reason} ->
                  {Value, Status} = config_utils:log_error(Config, default_delay_on, Reason)
              end;                                                      
            {error, Reason} ->
              {Value, Status} = config_utils:log_error(Config, invert_output, Reason)
          end;
        {error, Reason} ->
          {Value, Status} = config_utils:log_error(Config, default_value, Reason)
      end;
    {error, Reason} ->
      {Value, Status} = config_utils:log_error(Config, led_id, Reason)
  end,

  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),
    
  % This is the block state
  {Config, Inputs, Outputs1, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, disable) ->
  set_led_default_state(Config),
  Outputs1 = output_utils:update_all_outputs(Outputs, null, disabled),
  {Config, Inputs, Outputs1, Private};

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  {ok, LedId} = config_utils:get_string(Config, led_id),
  {ok, DefaultValue} = config_utils:get_boolean(Config, default_value),
  {ok, DefaultTrigger} = config_utils:get_string(Config, default_trigger),
  {ok, DefaultDelayOn} = config_utils:get_integer(Config, default_delay_on),
  {ok, DefaultDelayOff} = config_utils:get_integer(Config, default_delay_on),
  {ok, InvertOutput} = config_utils:get_boolean(Config, invert_output),
     
  % Set Output Val to input and set the actual LED file value too
  case input_utils:get_boolean(Inputs, input) of
    {ok, InValue} ->
      case input_utils:get_string(Inputs, trigger) of
        {ok, InTrigger} ->
          if (InTrigger == "timer") ->
            case input_utils:get_pos_integer(Inputs, delay_on) of
              {ok, InDelayOn} ->
                case input_utils:get_pos_integer(Inputs, delay_off) of
                  {ok, InDelayOff} ->
                    if (InValue == null) ->
                      LedValue = DefaultValue;
                    true ->
                      LedValue = InValue
                    end,

                    if (InDelayOn == null) ->
                      DelayOn = DefaultDelayOn;
                    true ->
                      DelayOn = InDelayOn
                    end,

                    if (InDelayOff == null) ->
                      DelayOff = DefaultDelayOff;
                    true ->
                      DelayOff = InDelayOff
                    end,

                    set_led_state(LedId, LedValue, InTrigger, DelayOn, DelayOff, InvertOutput),
                    Value = InValue,
                    Status = normal;

                  {error, Reason} ->
                    {Value, Status} = input_utils:log_error(Config, delay_off, Reason),
                    set_led_state(LedId, DefaultValue, DefaultTrigger, DefaultDelayOn, DefaultDelayOff, InvertOutput)
                end;

              {error, Reason} ->
                {Value, Status} = input_utils:log_error(Config, delay_on, Reason),
                set_led_state(LedId, DefaultValue, DefaultTrigger, DefaultDelayOn, DefaultDelayOff, InvertOutput)
            end;

          true -> % Trigger != "timer", don't care about on/off delay times
            if (InValue == null) ->
              LedValue = DefaultValue;
            true ->
              LedValue = InValue
            end,

            if (InTrigger == null) ->
              Trigger = DefaultTrigger;
            true ->
              Trigger = InTrigger
            end,
            set_led_state(LedId, LedValue, Trigger, 0, 0, InvertOutput),
            Value = InValue,
            Status = normal
          end;
        {error, Reason} ->
          {Value, Status} = input_utils:log_error(Config, trigger, Reason),
          set_led_state(LedId, DefaultValue, DefaultTrigger, DefaultDelayOn, DefaultDelayOff, InvertOutput)
      end;
    {error, Reason} ->
      {Value, Status} = input_utils:log_error(Config, input, Reason),
      set_led_state(LedId, DefaultValue, DefaultTrigger, DefaultDelayOn, DefaultDelayOff, InvertOutput)
  end,

  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  % Leave LED in default state on block delete
  set_led_default_state(Config),

  % Private values are created in the block initialization routine
  % So they should be deleted here
  {Config, Inputs, Outputs}.


%% ====================================================================
%% Internal functions
%% ====================================================================

% TODO: May switch to using nerves_led project
%set_led(LedId, Trigger, DelayOn, DelayOff) ->
%  Elixir.Nerves.Leds:set( LedId, [ {trigger, Trigger}, {delay_on, DelayOn}, {delay_off, DelayOff}]).


set_led_default_state(Config) ->
  {ok, LedId} = config_utils:get_string(Config, led_id),
  {ok, DefaultValue} = config_utils:get_boolean(Config, default_value),
  {ok, DefaultTrigger} = config_utils:get_string(Config, default_trigger),
  {ok, DefaultDelayOn} = config_utils:get_integer(Config, default_delay_on),
  {ok, DefaultDelayOff} = config_utils:get_integer(Config, default_delay_on),
  {ok, InvertOutput} = config_utils:get_boolean(Config, invert_output),
  set_led_state(LedId, DefaultValue, DefaultTrigger, DefaultDelayOn, DefaultDelayOff, InvertOutput).


% Set the actual values of the LED files here
set_led_state(LedId, Value, Trigger, DelayOn, DelayOff, Invert) ->
  FilePath = ?LED_FILE_PATH ++ LedId,

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

  write_file(FilePath ++ "/brightness", State),

  % Set the LED trigger file
  write_file(FilePath ++  "/trigger", Trigger),

  % Only set Delay On/Off times if trigger is "timer"
  if (Trigger == "timer") ->
    % Set the LED delay_on file
    write_file(FilePath ++  "/delay_on", integer_to_list(DelayOn)),

    % Set the LED delay_off file
    write_file(FilePath ++ "/delay_off", integer_to_list(DelayOff));
  true -> 
    ok
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