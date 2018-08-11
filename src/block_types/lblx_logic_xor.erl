%%% @doc 
%%% BLOCKTYPE
%%% Logic Exclusive OR
%%% DESCRIPTION
%%% Set the block output value to boolean Exclusive OR of the inputs
%%% LINKS              
%%% @end 

-module(lblx_logic_xor).  

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [logic].

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {input_a, {empty, {empty}}}, %| bool | empty | true, false |
      {input_b, {empty, {empty}}}  %| bool | empty | true, false |
  ]). 


-spec default_outputs() -> output_attribs().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {active_true, {empty, []}}, %| bool | empty | true, null |
      {active_false, {empty, []}} %| bool | empty | false, null |
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
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
  
  % No config values to check
  
  Outputs1 = output_utils:set_value_status(Outputs, null, initialed),  

  % This is the block state
  {Config, Inputs, Outputs1, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, disable) ->
  Outputs1 = output_utils:update_all_outputs(Outputs, null, disabled),
  {Config, Inputs, Outputs1, Private};

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  case input_utils:get_boolean(Inputs, input_a) of
    {ok, null} ->
      InputId = input_a,
      OutputVal = {ok, null};
    
    {ok, InputA} ->
      case input_utils:get_boolean(Inputs, input_b) of
        {ok, null} ->
          InputId = input_b,
          OutputVal = {ok, null};
    
        {ok, InputB} ->
          InputId = input, % Don't care, input is not in error state
          % Exclusive OR, if one or the other, but not both are true, output is true
          Value = (InputA andalso (not InputB)) orelse ((not InputA) andalso InputB),
          OutputVal = {ok, Value};
          
        {error, Reason} ->
          InputId = input_b,
          OutputVal = {error, Reason}
      end;

    {error, Reason} ->
      InputId = input_a,
      OutputVal = {error, Reason}
  end,

  Outputs1 = output_utils:set_tristate_outputs(InputId, OutputVal, Config, Outputs),

  % Return updated block state
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  {Config, Inputs, Outputs}.


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
    {[{status, no_input}, {value, null}]},
    {[{input_a, null}, {input_b, false}], [{status, no_input}, {value, null}]},
    {[{input_a, true}, {input_b, null}], [{status, no_input}, {value, null}]},
    {[{input_a, bad_value}, {input_b, false}], [{status, input_err}, {value, null}]},
    {[{input_a, true}, {input_b, bad_value}], [{status, input_err}, {value, null}]},
    {[{input_a, false}, {input_b, false}], [{status, normal}, {value, false}]},
    {[{input_a, true}, {input_b, false}], [{status, normal}, {value, true}]},
    {[{input_a, false}, {input_b, true}], [{status, normal}, {value, true}]},
    {[{input_a, true}, {input_b, true}], [{status, normal}, {value, false}]}
  ].


-endif.