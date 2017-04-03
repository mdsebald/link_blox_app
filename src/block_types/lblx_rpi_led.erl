%%% @doc 
%%% Block Type:   Raspberry Pi on-board LED
%%% Description:  Control Raspberry Pi on-board LED 
%%%               
%%% @end 

-module(lblx_rpi_led).  

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([group/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/1, delete/1]).

group() -> [display, output].

description() -> "Control Raspi on-board LED".

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {led_id, {"led0"}},  % Default to green LED, "led1" is red LED
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
      error_logger:info_msg("Block: ~p type: ~p upgraded from ver: ~s to: ~s~n", 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      error_logger:error_msg("Error: ~p upgrading block: ~p type: ~p from ver: ~s to: ~s~n", 
                            [Reason, BlockName, BlockType, ConfigVer, ModuleVer]),
      {error, Reason}
  end.


-define(LED_FILE_PATH, "/sys/class/leds/" ).

%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
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
                  take_led_control(LedId),
                  set_led_value(LedId, DefaultValue, InvertOutput);
            
                false ->
                  error_logger:error_msg("Error: LED file: ~p does not exist~n", 
                              [?LED_FILE_PATH ++ LedId]),
                  Status = proc_err,
                  Value = not_active
              end;
            {error, Reason} ->
              error_logger:error_msg("Error: ~p Error reading invert_output value~n", 
                              [Reason]),
              Status = config_err,
              Value = not_active
          end;
        {error, Reason} ->
          error_logger:error_msg("Error: ~p Error reading default_value~n", 
                              [Reason]),
          Status = config_err,
          Value = not_active
      end;
    {error, Reason} ->
      error_logger:error_msg("Error: ~p Error reading LED ID~n", 
                              [Reason]),
      Status = config_err,
      Value = not_active
  end,

  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),
    
  % This is the block state
  {Config, Inputs, Outputs1, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({Config, Inputs, Outputs, Private}) ->

  {ok, LedId} = config_utils:get_string(Config, led_id),
  {ok, DefaultValue} = attrib_utils:get_value(Config, default_value),
  {ok, InvertOutput} = attrib_utils:get_value(Config, invert_output),
     
  {ok, Input} = attrib_utils:get_value(Inputs, input),

  % Set Output Val to input and set the actual LED file value too
  case Input of
    empty -> 
      LedValue = DefaultValue, % TODO: Set pin to default value or input? 
      Value = not_active,
      Status = no_input;

    not_active ->
      LedValue = DefaultValue, % TODO: Set pin to default value or input? 
      Value = not_active,
      Status = normal;

    true ->  
      LedValue = true, 
      Value = true,
      Status = normal;

    false ->
      LedValue = false,
      Value = false,
      Status = normal;

    Other ->
      BlockName = config_utils:name(Config),
      error_logger:error_msg("~p Error: Invalid input value: ~p~n", 
                               [BlockName, Other]),
      LedValue = DefaultValue, % TODO: Set pin to default value or input? 
      Value = not_active,
      Status = input_err
  end,
  set_led_value(LedId, LedValue, InvertOutput),
 
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockValues :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  %
  % Private values are created in the block initialization routine
  % So they should be deleted here
  
  {Config, Inputs, Outputs}.



%% ====================================================================
%% Internal functions
%% ====================================================================
% Set the actual value of the LED file here
set_led_value(LedId, Value, Invert) ->
  FileId = ?LED_FILE_PATH ++ LedId ++ "/brightness",
  
  if Value -> % Value is true/on
    if Invert -> % Invert pin value 
      file:write_file(FileId, "0"); % turn output off
    true ->      % Don't invert_output output value
      file:write_file(FileId, "1") % turn output on
    end;
  true -> % Value is false/off
    if Invert -> % Invert pin value
      file:write_file(FileId, "1"); % turn output on
    true ->      % Don't invert_output output value
      file:write_file(FileId, "0")  % turn output off
    end
  end.

  % Take control of LED, from trigger
  take_led_control(LedId) ->
    FileId = ?LED_FILE_PATH ++ LedId ++ "/trigger",
    file:write_file(FileId, "none").


  


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