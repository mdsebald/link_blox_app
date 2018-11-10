%%% @doc 
%%% BLOCKTYPE
%%% Rotorary encoder with switch
%%% DESCRIPTION
%%% Rotary encoder outputs 2 phase digital signal
%%% Channels A & B to indicate rotation and direction
%%% Can also read a switch activated by pressing the knob of the encoder.
%%% LINKS
%%% https://www.bourns.com/docs/Product-Datasheets/pec11R.pdf
%%% @end 

-module(lblx_rotary_encoder).  

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).
-export([handle_info/2]).

groups() -> [digital, input].

version() -> "0.2.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {gpio_pin_phase_A, {0}}, %| int | 0 | 0..40 |
      {gpio_pin_phase_B, {0}}, %| int | 0 | 0..40 |
      {phase_int_edge, {both}} %| enum | both | falling, rising, both |
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
      m_logger:info(block_type_upgraded_from_ver_to, 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      m_logger:error(err_upgrading_block_type_from_ver_to, 
                            [Reason, BlockName, BlockType, ConfigVer, ModuleVer]),
      {error, Reason}
  end.


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
    
  % Get the GPIO interrupt edge directions used by this block
  {ok, PhaseIntEdge} = attrib_utils:get_value(Config, phase_int_edge),
  
  % Initialize the GPIO pins
  case config_utils:init_gpio(Config, gpio_pin_phase_A, input) of
    {ok, GpioPinA_Ref} ->
      Private1 = attrib_utils:add_attribute(Private, {gpio_pin_A_ref,{GpioPinA_Ref}}),
      gpio_utils:set_edge_mode(GpioPinA_Ref, PhaseIntEdge),
      LastA_Value = gpio_utils:read_bool(GpioPinA_Ref),
      Private2 = attrib_utils:add_attribute(Private1, {last_A_value, {LastA_Value}}),

      case config_utils:init_gpio(Config, gpio_pin_phase_B, input) of
        {ok, GpioPinB_Ref} ->
          Private3 = attrib_utils:add_attribute(Private2, {gpio_pin_B_ref, {GpioPinB_Ref}}),
          gpio_utils:set_edge_mode(GpioPinB_Ref, PhaseIntEdge),
          LastB_Value = gpio_utils:read_bool(GpioPinB_Ref),
          Private4 = attrib_utils:add_attribute(Private3, {last_B_value, {LastB_Value}}),
          Value = null,
          Status = initialed;

        {error, _Reason} ->
          Value = null, 
          Status = proc_err,
          Private4 = Private2
      end;

    {error, _Reason} ->
      Value = null, 
      Status = proc_err,
      Private4 = Private
  end,
  
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),
  
  % This is the block state
  {Config, Inputs, Outputs1, Private4}.

%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, disable) ->
  Outputs1 = output_utils:update_all_outputs(Outputs, null, disabled),
  {Config, Inputs, Outputs1, Private};

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

 % Read the current values of the GPIO pins 
  {ok, GpioPinA_Ref} = attrib_utils:get_value(Private, gpio_pin_A_ref),
  PhaseA = gpio_utils:read_bool(GpioPinA_Ref),
  {ok, LastA_Value} = attrib_utils:get_value(Private, last_A_value),
  
  {ok, GpioPinB_Ref} = attrib_utils:get_value(Private, gpio_pin_B_ref),
  PhaseB = gpio_utils:read_bool(GpioPinB_Ref),
  {ok, _LastB_Value} = attrib_utils:get_value(Private, last_B_value),

  case attrib_utils:get_value(Outputs, value) of
    {ok, null}  -> Count = 0;
    {ok, Count} -> Count
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
                                              {status, normal}]),
                                              
  {ok, Private1} = attrib_utils:set_values(Private, [{last_A_value, PhaseA},
                                              {last_B_value, PhaseB}]),

  % Return updated block state
  {Config, Inputs, Outputs1, Private1}.


%% 
%%  Delete the block
%%  
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  % Nothing to close, stop, or release for the GPIO pins
  {Config, Inputs, Outputs}.


%% 
%% GPIO Interupt message from Circuits GPIO library, 
%% Execute this block
%% 
-spec handle_info(Info :: term(), 
                  BlockState :: block_state()) -> {noreply, block_state()}.

handle_info({gpio, Pin, _Timestamp, Condition}, BlockState) ->
  m_logger:debug("Rx interrupt: ~p from GPIO pin: ~p ~n", [Condition, Pin]),
  NewBlockState = block_common:execute(BlockState, hardware),
  {noreply, NewBlockState};

%%
%% Unknown Info message, just log a warning
%% 
handle_info(Info, BlockState) ->
  {BlockName, BlockModule} = config_utils:name_module(BlockState),
  m_logger:warning(block_type_name_unknown_info_msg, [BlockModule, BlockName, Info]),
  {noreply, BlockState}.  


%% ====================================================================
%% Internal functions
%% ====================================================================


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
