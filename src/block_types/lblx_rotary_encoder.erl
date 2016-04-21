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

 % INSTRUCTIONS: Adjust path to hrl file as needed
-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([type_name/0, description/0, version/0]). 
-export([create/2, create/4, create/5, initialize/1, execute/1, delete/1]).


type_name() -> "rotary_encoder".

version() -> "0.1.0".

description() -> "Rotary encoder with optional switch".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: atom(),
                      Description :: string()) -> list().

default_configs(BlockName, Description) -> 
  block_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {gpio_pin_phase_A, 21},   % TODO: Final block set to zero
      {gpio_pin_phase_B, 20},
      {phase_int_edge, both},  % Valid values are: falling, rising, or both
      {gpio_pin_switch, 19},
      {switch_int_edge, falling}  
    ]). 


-spec default_inputs() -> list().

default_inputs() -> 
  block_utils:merge_attribute_lists(
    block_common:inputs(),
    [
     
    ]). 


-spec default_outputs() -> list().
                            
default_outputs() -> 
  block_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {switch, not_active, []}
    ]). 

%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: atom(),
             Description :: string()) -> block_defn().

create(BlockName, Description) -> 
  create(BlockName, Description, [], [], []).

-spec create(BlockName :: atom(),
             Description :: string(),  
             InitConfig :: list(), 
             InitInputs :: list()) -> block_defn().
   
create(BlockName, Description, InitConfig, InitInputs) -> 
  create(BlockName, Description, InitConfig, InitInputs, []).

-spec create(BlockName :: atom(),
             Description :: string(), 
             InitConfig :: list(), 
             InitInputs :: list(), 
             InitOutputs :: list()) -> block_defn().

create(BlockName, Description, InitConfig, InitInputs, InitOutputs)->
    
  Config = block_utils:merge_attribute_lists(default_configs(BlockName, Description), InitConfig),
  Inputs = block_utils:merge_attribute_lists(default_inputs(), InitInputs), 
  Outputs = block_utils:merge_attribute_lists(default_outputs(), InitOutputs),

  % This is the block definition, 
  {Config, Inputs, Outputs}.


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
  
  % Set up the private values needed  
  Private1 = block_utils:merge_attribute_lists(Private, 
                            [{gpio_pin_A_ref, empty},
                             {last_A_value, empty},
                             {gpio_pin_B_ref, empty},
                             {last_B_value, empty},
                             {gpio_pin_sw_ref, empty},
                             {last_sw_value, empty}]),
    
  % Get the GPIO pin numbers and interrupt edge directions used by this block
  PhaseA_Pin = block_utils:get_value(Config, gpio_pin_phase_A),
  PhaseB_Pin = block_utils:get_value(Config, gpio_pin_phase_B),
  PhaseIntEdge = block_utils:get_value(Config, phase_int_edge),
  SwitchPin = block_utils:get_value(Config, gpio_pin_switch),
  SwitchIntEdge = block_utils:get_value(Config, switch_int_edge),
  
  % TODO: Check Pin Numbers are an integer in the right range

  % Initialize the GPIO pins as inputs
  case gpio:start_link(PhaseA_Pin, input) of
    {ok, GpioPinA_Ref} ->
	    gpio:register_int(GpioPinA_Ref),
      gpio:set_int(GpioPinA_Ref, PhaseIntEdge),
      LastA_Value = read_pin_value_bool(GpioPinA_Ref),
      Private2 = block_utils:set_values(Private1, 
                                        [{gpio_pin_A_ref, GpioPinA_Ref},
                                         {last_A_value, LastA_Value}]),

      case gpio:start_link(PhaseB_Pin, input) of
        {ok, GpioPinB_Ref} ->
	        gpio:register_int(GpioPinB_Ref),
          gpio:set_int(GpioPinB_Ref, PhaseIntEdge),
          LastB_Value = read_pin_value_bool(GpioPinB_Ref),
          Private3 = block_utils:set_values(Private2, 
                                        [{gpio_pin_B_ref, GpioPinB_Ref},
                                         {last_B_value, LastB_Value}]),
      
          case gpio:start_link(SwitchPin, input) of
            {ok, GpioPinSwRef} ->
	            gpio:register_int(GpioPinSwRef),
              gpio:set_int(GpioPinSwRef, SwitchIntEdge),
              LastSwValue = read_pin_value_bool(GpioPinSwRef),
              Private4 = block_utils:set_values(Private3, 
                                        [{gpio_pin_sw_ref, GpioPinSwRef},
                                         {last_sw_value, LastSwValue}]),

              Value = not_active,
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
  
  Outputs1 = block_utils:set_value_status(Outputs, Value, Status),
  
  % This is the block state
  {Config, Inputs, Outputs1, Private4}.

%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({Config, Inputs, Outputs, Private}) ->

 % Read the current values of the GPIO pins 
  GpioPinA_Ref = block_utils:get_value(Private, gpio_pin_A_ref),
  PhaseA = read_pin_value_bool(GpioPinA_Ref),
  LastA_Value = block_utils:get_value(Private, last_A_value),
  
  GpioPinB_Ref = block_utils:get_value(Private, gpio_pin_B_ref),
  PhaseB = read_pin_value_bool(GpioPinB_Ref),
  _LastB_Value = block_utils:get_value(Private, last_B_value),

  GpioPinSwRef = block_utils:get_value(Private, gpio_pin_sw_ref),
  SwValue = read_pin_value_bool(GpioPinSwRef),
  _LastSwValue = block_utils:get_value(Private, last_sw_value),
  
  case block_utils:get_value(Outputs, value) of
    not_active -> 
      Count = 0;
    Count -> Count  
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

  
  Outputs1 = block_utils:set_values(Outputs, [{value, NewCount},
                                              {switch, SwValue},
                                              {status, normal}]),
                                              
  Private1 = block_utils:set_values(Private, [{last_A_value, PhaseA},
                                              {last_B_value, PhaseB},
                                              {last_sw_value, SwValue}]),

  % Return updated block state
  {Config, Inputs, Outputs1, Private1}.


%% 
%%  Delete the block
%%	
-spec delete(block_state()) -> ok.

delete({_Config, _Inputs, _Outputs, _Private}) -> 
  % INSTRUCTIONS: Perform any block type specific delete functionality here
  ok.



%% ====================================================================
%% Internal functions
%% ====================================================================

-spec log_gpio_error(Config :: list(), 
                     Reason :: atom(), 
                     PinNumber :: integer()) -> {not_active, proc_error}.

log_gpio_error(Config, Reason, PinNumber) ->
  BlockName = block_utils:name(Config),
  error_logger:error_msg("~p Error: ~p intitiating GPIO pin; ~p~n", 
                              [BlockName, Reason, PinNumber]),
  
  {not_active, proc_error}.


% TODO: Create common GPIO helper module
read_pin_value_bool(GpioPinRef) ->
  case gpio:read(GpioPinRef) of
    1  -> true;
    0 -> false
  end.