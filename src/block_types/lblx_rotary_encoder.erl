%%% @doc 
%%% Block Type:  Rotorary encoder with switch
%%% Description:   Rotary encoder outputs 2 phase digital signal
%%%              Channels A & B to indicate rotation and direction
%%%              Can also include a switch activated by pressing the 
%%%              knob of the encoder.
%%%               
%%% @end 

-module(lblx_rotary_encoder).  

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1, handle_info/2]).

groups() -> [digital, input].

description() -> "Rotary encoder with optional switch".

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {gpio_pin_phase_A, {21}},   % TODO: Final block set to zero
      {gpio_pin_phase_B, {20}},
      {phase_int_edge, {both}},  % Valid values are: falling, rising, or both
      {gpio_pin_switch, {19}},
      {switch_int_edge, {falling}}  
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
      {switch, {null, []}}
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
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
  
  % Set up the private values needed  
  Private1 = attrib_utils:merge_attribute_lists(Private, 
                            [{gpio_pin_A_ref, {empty}},
                             {last_A_value, {empty}},
                             {gpio_pin_B_ref, {empty}},
                             {last_B_value, {empty}},
                             {gpio_pin_sw_ref, {empty}},
                             {last_sw_value, {empty}}]),
    
  % Get the GPIO pin numbers and interrupt edge directions used by this block
  {ok, PhaseA_Pin} = attrib_utils:get_value(Config, gpio_pin_phase_A),
  {ok, PhaseB_Pin} = attrib_utils:get_value(Config, gpio_pin_phase_B),
  {ok, PhaseIntEdge} = attrib_utils:get_value(Config, phase_int_edge),
  {ok, SwitchPin} = attrib_utils:get_value(Config, gpio_pin_switch),
  {ok, SwitchIntEdge} = attrib_utils:get_value(Config, switch_int_edge),
  
  % TODO: Check Pin Numbers are an integer in the right range

  % Initialize the GPIO pins as inputs
  case gpio_utils:start_link(PhaseA_Pin, input) of
    {ok, GpioPinA_Ref} ->
      gpio_utils:register_int(GpioPinA_Ref),
      gpio_utils:set_int(GpioPinA_Ref, PhaseIntEdge),
      LastA_Value = read_pin_value_bool(GpioPinA_Ref),
      {ok, Private2} = attrib_utils:set_values(Private1, 
                                        [{gpio_pin_A_ref, GpioPinA_Ref},
                                         {last_A_value, LastA_Value}]),

      case gpio_utils:start_link(PhaseB_Pin, input) of
        {ok, GpioPinB_Ref} ->
          gpio_utils:register_int(GpioPinB_Ref),
          gpio_utils:set_int(GpioPinB_Ref, PhaseIntEdge),
          LastB_Value = read_pin_value_bool(GpioPinB_Ref),
          {ok, Private3} = attrib_utils:set_values(Private2, 
                                        [{gpio_pin_B_ref, GpioPinB_Ref},
                                         {last_B_value, LastB_Value}]),
      
          case gpio_utils:start_link(SwitchPin, input) of
            {ok, GpioPinSwRef} ->
              gpio_utils:register_int(GpioPinSwRef),
              gpio_utils:set_int(GpioPinSwRef, SwitchIntEdge),
              LastSwValue = read_pin_value_bool(GpioPinSwRef),
              {ok, Private4} = attrib_utils:set_values(Private3, 
                                        [{gpio_pin_sw_ref, GpioPinSwRef},
                                         {last_sw_value, LastSwValue}]),

              Value = null,
              Status = initialed;
              
            {error, SwReason} ->
              {Value, Status} = log_gpio_error(Config, SwReason, SwitchPin),
              Private4 = Private3
          end;

        {error, B_Reason} ->
          {Value, Status} = log_gpio_error(Config, B_Reason, PhaseB_Pin), 
          Private4 = Private2
      end;

    {error, A_Reason} ->
      {Value, Status} = log_gpio_error(Config, A_Reason, PhaseA_Pin), 
      Private4 = Private1
  end,
  
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),
  
  % This is the block state
  {Config, Inputs, Outputs1, Private4}.

%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

 % Read the current values of the GPIO pins 
  {ok, GpioPinA_Ref} = attrib_utils:get_value(Private, gpio_pin_A_ref),
  PhaseA = read_pin_value_bool(GpioPinA_Ref),
  {ok, LastA_Value} = attrib_utils:get_value(Private, last_A_value),
  
  {ok, GpioPinB_Ref} = attrib_utils:get_value(Private, gpio_pin_B_ref),
  PhaseB = read_pin_value_bool(GpioPinB_Ref),
  {ok, _LastB_Value} = attrib_utils:get_value(Private, last_B_value),

  {ok, GpioPinSwRef} = attrib_utils:get_value(Private, gpio_pin_sw_ref),
  SwValue = read_pin_value_bool(GpioPinSwRef),
  {ok, _LastSwValue} = attrib_utils:get_value(Private, last_sw_value),
  
  case attrib_utils:get_value(Outputs, value) of
    {ok, null} -> Count = 0;
    {ok, Count}      -> Count
  end,
 
  if (LastA_Value) andalso (not PhaseA) ->
    % A has gone from high to low 
    if PhaseB ->
      % B is high so clockwise (increase count)
      NewCount = Count + 1;            
    true ->
      % B is low so counter-clockwise (decrease count)     
      NewCount = Count -1
    end;
  true ->
    NewCount = Count
  end,   

  
  {ok, Outputs1} = attrib_utils:set_values(Outputs, [{value, NewCount},
                                              {switch, SwValue},
                                              {status, normal}]),
                                              
  {ok, Private1} = attrib_utils:set_values(Private, [{last_A_value, PhaseA},
                                              {last_B_value, PhaseB},
                                              {last_sw_value, SwValue}]),

  % Return updated block state
  {Config, Inputs, Outputs1, Private1}.


%% 
%%  Delete the block
%%  
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, Private}) -> 
  % Release the GPIO pins, if used
  case attrib_utils:get_value(Private, gpio_pin_A_ref) of
      {ok, GpioRefA} -> gpio_utils:stop(GpioRefA);
  
      _DoNothingA -> ok
  end,

  case attrib_utils:get_value(Private, gpio_pin_B_ref) of
      {ok, GpioRefB} -> gpio_utils:stop(GpioRefB);
  
      _DoNothingB -> ok
  end,

  case attrib_utils:get_value(Private, gpio_pin_sw_ref) of
      {ok, GpioRefSw} -> gpio_utils:stop(GpioRefSw);
  
      _DoNothingSw -> ok
  end,
  {Config, Inputs, Outputs}.


%% 
%% Unknown Info message, just log a warning
%% 
-spec handle_info(Info :: term(), 
                  BlockState :: block_state()) -> {noreply, block_state()}.

handle_info(Info, BlockState) ->
  log_server:warning(block_server_unknown_info_msg, [Info]),
  {noreply, BlockState}.


%% ====================================================================
%% Internal functions
%% ====================================================================

-spec log_gpio_error(Config :: list(), 
                     Reason :: atom(), 
                     PinNumber :: integer()) -> {null, proc_err}.

log_gpio_error(Config, Reason, PinNumber) ->
  BlockName = config_utils:name(Config),
  log_server:error(err_initiating_GPIO_pin, [BlockName, Reason, PinNumber]),
  
  {null, proc_err}.


% TODO: Create common GPIO helper module
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
