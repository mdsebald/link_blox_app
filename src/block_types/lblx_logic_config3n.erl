%%% @doc 
%%% Block Type:  3 Input Configurable Logic Gate, Null Values Allowed
%%% Description:  Output is set to the config value corresponding to the combination of binary and null input values
%%%               
%%% @end 


-module(lblx_logic_config3n).
  
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
      {'0_0_0_out', {null}}, % Output value for input 3 = false & 2 = false & 1 = false
      {'0_0_1_out', {null}}, % Output value for input 3 = false & 2 = false & 1 = true
      {'0_0_X_out', {null}}, % Output value for input 3 = false & 2 = false & 1 = null
      {'0_1_0_out', {null}}, % Output value for input 3 = false & 2 = true & 1 = false
      {'0_1_1_out', {null}}, % Output value for input 3 = false & 2 = true & 1 = true
      {'0_1_X_out', {null}}, % Output value for input 3 = false & 2 = true & 1 = null
      {'0_X_0_out', {null}}, % Output value for input 3 = false & 2 = null & 1 = false
      {'0_X_1_out', {null}}, % Output value for input 3 = false & 2 = null & 1 = true
      {'0_X_X_out', {null}}, % Output value for input 3 = false & 2 = null & 1 = null

      {'1_0_0_out', {null}}, % Output value for input 3 = true & 2 = false & 1 = false
      {'1_0_1_out', {null}}, % Output value for input 3 = true & 2 = false & 1 = true
      {'1_0_X_out', {null}}, % Output value for input 3 = true & 2 = false & 1 = null
      {'1_1_0_out', {null}}, % Output value for input 3 = true & 2 = true & 1 = false
      {'1_1_1_out', {null}}, % Output value for input 3 = true & 2 = true & 1 = true
      {'1_1_X_out', {null}}, % Output value for input 3 = true & 2 = true & 1 = null
      {'1_X_0_out', {null}}, % Output value for input 3 = true & 2 = null & 1 = false
      {'1_X_1_out', {null}}, % Output value for input 3 = true & 2 = null & 1 = true
      {'1_X_X_out', {null}}, % Output value for input 3 = true & 2 = null & 1 = null

      {'X_0_0_out', {null}}, % Output value for input 3 = null & 2 = false & 1 = false
      {'X_0_1_out', {null}}, % Output value for input 3 = null & 2 = false & 1 = true
      {'X_0_X_out', {null}}, % Output value for input 3 = null & 2 = false & 1 = null
      {'X_1_0_out', {null}}, % Output value for input 3 = null & 2 = true & 1 = false
      {'X_1_1_out', {null}}, % Output value for input 3 = null & 2 = true & 1 = true
      {'X_1_X_out', {null}}, % Output value for input 3 = null & 2 = true & 1 = null
      {'X_X_0_out', {null}}, % Output value for input 3 = null & 2 = null & 1 = false
      {'X_X_1_out', {null}}, % Output value for input 3 = null & 2 = null & 1 = true
      {'X_X_X_out', {null}}  % Output value for input 3 = null & 2 = null & 1 = null
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {input3, {empty, {empty}}},
      {input2, {empty, {empty}}},
      {input1, {empty, {empty}}}
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


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
    
  % No config values to check,
  
  Outputs1 = output_utils:set_value_status(Outputs, null, initialed),
  
  % Return updated block state
  {Config, Inputs, Outputs1, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  {Value, Status} = get_output_value(Config, Inputs),
 
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

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

-spec get_output_value(Config :: config_attribs(),
                       Inputs :: input_attribs()) -> {value(), block_status()} | {error, atom()}.

get_output_value(Config, Inputs) ->

  case input_utils:get_boolean(Inputs, input3) of

    {ok, Input3} ->
      case input_utils:get_boolean(Inputs, input2) of

        {ok, Input2} ->
          case input_utils:get_boolean(Inputs, input1) of

            {ok, Input1} ->
              ValueName = maps:get({Input3, Input2, Input1}, in_out_value_map()),
              % Set the output value to the config value corresponding to the input state
              {ok, Value} = config_utils:get_any_type(Config, ValueName),
              {Value, normal};

            {error, Reason} ->
              input_utils:log_error(Config, input1, Reason)
          end;

        {error, Reason} ->
          input_utils:log_error(Config, input2, Reason)
      end;
 
    {error, Reason} ->
      input_utils:log_error(Config, input3, Reason)
  end.
 

in_out_value_map() ->
  #{
    {false, false, false} => '0_0_0_out',
    {false, false, true} => '0_0_1_out',
    {false, false, null} => '0_0_X_out',
    {false, true, false} => '0_1_0_out',
    {false, true, true} => '0_1_1_out',
    {false, true, null} => '0_1_X_out',
    {false, null, false} => '0_X_0_out',
    {false, null, true} => '0_X_1_out',
    {false, null, null} => '0_X_X_out',
    
    {true, false, false} => '1_0_0_out',
    {true, false, true} => '1_0_1_out',
    {true, false, null} => '1_0_X_out',
    {true, true, false} => '1_1_0_out',
    {true, true, true} => '1_1_1_out',
    {true, true, null} => '1_1_X_out',
    {true, null, false} => '1_X_0_out',
    {true, null, true} => '1_X_1_out',
    {true, null, null} => '1_X_X_out',

    {null, false, false} => 'X_0_0_out',
    {null, false, true} => 'X_0_1_out',
    {null, false, null} => 'X_0_X_out',
    {null, true, false} => 'X_1_0_out',
    {null, true, true} => 'X_1_1_out',
    {null, true, null} => 'X_1_X_out',
    {null, null, false} => 'X_X_0_out',
    {null, null, true} => 'X_X_1_out',
    {null, null, null} => 'X_X_X_out'
  }.


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
  InitConfigVals = [{'0_0_X_out', "Input 1 Null"}, {'0_X_0_out', "Input 2 Null"}, {'X_0_0_out', "Input 3 Null"}, {'X_X_X_out', "All Null"}, 
                    {'0_0_0_out', 0}, {'0_0_1_out', 1}, {'0_1_0_out', 2}, {'1_0_0_out', 4}, {'1_1_1_out', 7}],
  unit_test_utils:block_setup(?MODULE, InitConfigVals).

cleanup(BlockState) ->
  unit_test_utils:block_cleanup(?MODULE, BlockState).

test_io(BlockState) ->
  unit_test_utils:create_io_tests(?MODULE, input_cos, BlockState, test_sets()).

test_sets()->
  [
    % Test null/empty input values
    {[{input3, false}, {input2, false}, {input1, null}],  [{status, normal}, {value, "Input 1 Null"}]},
    {[{input3, false}, {input2, empty}, {input1, false}], [{status, normal}, {value, "Input 2 Null"}]},
    {[{input3, null},  {input2, false}, {input1, false}], [{status, normal}, {value, "Input 3 Null"}]},
    {[{input3, null},  {input2, null},  {input1, empty}], [{status, normal}, {value, "All Null"}]},
    % Test bad input values
    {[{input3, false}, {input2, true},  {input1, "bad"}], [{status, input_err}, {value, null}]},
    {[{input3, true},  {input2, "bad"}, {input1, true}],  [{status, input_err}, {value, null}]},
    {[{input3, "bad"}, {input2, true},  {input1, false}], [{status, input_err}, {value, null}]},
    % Test normal input values
    {[{input3, false}, {input2, false}, {input1, false}], [{status, normal}, {value, 0}]},
    {[{input3, false}, {input2, false}, {input1, true}],  [{status, normal}, {value, 1}]},
    {[{input3, false}, {input2, true},  {input1, false}], [{status, normal}, {value, 2}]},
    {[{input3, true},  {input2, false}, {input1, false}], [{status, normal}, {value, 4}]},
    {[{input3, true},  {input2, true},  {input1, true}],  [{status, normal}, {value, 7}]}
 ].


-endif.
