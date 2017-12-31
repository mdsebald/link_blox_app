%%% @doc 
%%% Block Type: Toggle Output
%%% Description: Toggle binary output value each time block is executed  
%%%               
%%% @end 

-module(lblx_logic_toggle).

-author("Mark Sebald").

-include("../block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [logic].

description() -> "Toggle binary output value on block execution".

version() -> "0.1.0". 


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
        {initial_state, {false}}  % This is the initial value when transitioning from a null state
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
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
%%
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  case config_utils:get_boolean(Config, initial_state) of
    {ok, _InitState} -> 
      Value = null, Status = initialed;
    
    {error, Reason} ->
      {Value, Status} = config_utils:log_error(Config, initial_state, Reason)
  end,

  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),
 
  % This is the block state  
  {Config, Inputs, Outputs1, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  % Toggle output everytime block is executed
  case attrib_utils:get_value(Outputs, value) of
    {ok, true}  -> OutputVal = {ok, false};
    
    {ok, false} -> OutputVal = {ok, true};
    
    {ok, null} -> 
      OutputVal = config_utils:get_boolean(Config, initial_state)
  end,
	
  Outputs1 = output_utils:set_tristate_outputs(input, OutputVal, Config, Outputs),
     
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

block_test_() ->
  {"Input to Output tests for: " ++ atom_to_list(?MODULE),
   {setup, 
      fun setup/0, 
      fun cleanup/1,
      fun (BlockState) -> 
        {inorder,
        [
          test_io(BlockState)
        ]}
      end} 
  }.

setup() ->
  unit_test_utils:block_setup(?MODULE).

cleanup(BlockState) ->
  unit_test_utils:block_cleanup(?MODULE, BlockState).

test_io(BlockState) ->
  unit_test_utils:create_io_tests(?MODULE, input_cos, BlockState, test_sets()).

test_sets() ->
  [
    {[], [{status, normal}, {value, false}]},
    {[], [{status, normal}, {value, true}]},
    {[], [{status, normal}, {value, false}]},
    {[], [{status, normal}, {value, true}]}
  ].

block_init_state_true_test_() ->
  {"Input to Output tests for: " ++ atom_to_list(?MODULE),
   {setup, 
      fun init_state_true_setup/0, 
      fun init_state_true_cleanup/1,
      fun (BlockState) -> 
        {inorder,
        [
          init_state_true_io(BlockState)
        ]}
      end} 
  }.

init_state_true_setup() ->
  InitConfigVals = [{initial_state, true}],
  unit_test_utils:block_setup(?MODULE, InitConfigVals).

init_state_true_cleanup(BlockState) ->
  unit_test_utils:block_cleanup(?MODULE, BlockState).

init_state_true_io(BlockState) ->
  unit_test_utils:create_io_tests(?MODULE, input_cos, BlockState, init_state_true_test_sets()).

init_state_true_test_sets() ->
  [
    {[], [{status, normal}, {value, true}]},
    {[], [{status, normal}, {value, false}]}
  ].


-endif.