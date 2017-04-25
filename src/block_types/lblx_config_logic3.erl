%%% @doc 
%%% Block Type:  
%%% Description:  3 Input Configurable Logic Gate
%%%               
%%% @end 


-module(lblx_config_logic3).
  
-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).


groups() -> [logic].

description() -> "3 Input Configurable Logic Gate".

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {'0_0_0_out', {null}}, % Output value for input 3 = false & 2 = false & 1 = false
      {'0_0_1_out', {null}}, % Output value for input 3 = false & 2 = false & 1 = true
      {'0_1_0_out', {null}}, % Output value for input 3 = false & 2 = true & 1 = false
      {'0_1_1_out', {null}}, % Output value for input 3 = false & 2 = true & 1 = true
      {'1_0_0_out', {null}}, % Output value for input 3 = true & 2 = false & 1 = false
      {'1_0_1_out', {null}}, % Output value for input 3 = true & 2 = false & 1 = true
      {'1_1_0_out', {null}}, % Output value for input 3 = true & 2 = true & 1 = false
      {'1_1_1_out', {null}}  % Output value for input 3 = true & 2 = true & 1 = true
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {inputs_3, {empty, ?EMPTY_LINK}},
      {inputs_2, {empty, ?EMPTY_LINK}},
      {inputs_1, {empty, ?EMPTY_LINK}}
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
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
    
  {Value, Status} = get_output_value(Config, Inputs),
 
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),
  
  % Return updated block state
  {Config, Inputs, Outputs1, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockValues :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  {Value, Status} = get_output_value(Config, Inputs),
 
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  % Return updated block state
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockValues :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  
  {Config, Inputs, Outputs}.



%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_output_value(Config :: list(config_attr()),
                       Inputs :: list(input_attr())) -> {value(), block_status()}.

get_output_value(Config, Inputs) ->

  case input_utils:get_boolean(Inputs, inputs_3) of
    {ok, null} ->
      % input value null, set output value null
      {null, normal};

    {ok, Input3} ->
      case input_utils:get_boolean(Inputs, inputs_2) of
        {ok, null} ->
          % input value null, set output value null
          {null, normal};

        {ok, Input2} ->
          case input_utils:get_boolean(Inputs, inputs_1) of
            {ok, null} ->
              % input value null, set output value null
              {null, normal};

            {ok, Input1} ->
              ValueName = maps:get({Input3, Input2, Input1}, in_out_value_map()),
              % Set the output value to the config value corresponding to the input state
              {ok, Value} = config_utils:get_any_type(Config, ValueName),
              {Value, normal};

            {error, Reason} ->
              input_utils:log_error(Config, inputs_1, Reason)
          end;

        {error, Reason} ->
          input_utils:log_error(Config, inputs_2, Reason)
      end;
 
    {error, Reason} ->
      input_utils:log_error(Config, inputs_3, Reason)
  end.
 

% Return Config value ID for given input values
-spec in_out_value_map() -> value_name().

in_out_value_map() ->
  #{
    {false, false, false} => '0_0_0_out',
    {false, false, true} => '0_0_1_out',
    {false, true, false} => '0_1_0_out',
    {false, true, true} => '0_1_1_out',
    {true, false, false} => '1_0_0_out',
    {true, false, true} => '1_0_1_out',
    {true, true, false} => '1_1_0_out',
    {true, true, true} => '1_1_1_out'
  }.


%% ====================================================================
%% Tests
%% ====================================================================

% INSTRUCTIONS:  Create unit tests here.  

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
  InitConfigVals = [{'0_0_0_out', 0}, {'0_0_1_out', 1}, {'0_1_0_out', 2}, {'1_0_0_out', 4}, {'1_1_1_out', 7}],
  unit_test_utils:block_setup(?MODULE, InitConfigVals).

cleanup(BlockState) ->
  unit_test_utils:block_cleanup(?MODULE, BlockState).

test_io(BlockState) ->
  unit_test_utils:create_io_tests(?MODULE, input_cos, BlockState, test_sets()).

test_sets()->
  [
    % Test null/empty input values
    {[{inputs_3, false}, {inputs_2, true},  {inputs_1, null}], [{status, normal}, {value, null}]},
    {[{inputs_3, false}, {inputs_2, empty}, {inputs_1, true}], [{status, normal}, {value, null}]},
    {[{inputs_3, null},  {inputs_2, false}, {inputs_1, true}], [{status, normal}, {value, null}]},
    % Test bad input values
    {[{inputs_3, false}, {inputs_2, true},  {inputs_1, "bad"}], [{status, input_err}, {value, null}]},
    {[{inputs_3, true},  {inputs_2, "bad"}, {inputs_1, true}],  [{status, input_err}, {value, null}]},
    {[{inputs_3, "bad"}, {inputs_2, true},  {inputs_1, false}], [{status, input_err}, {value, null}]},
    % Test normal input values
    {[{inputs_3, false}, {inputs_2, false}, {inputs_1, false}], [{status, normal}, {value, 0}]},
    {[{inputs_3, false}, {inputs_2, false}, {inputs_1, true}],  [{status, normal}, {value, 1}]},
    {[{inputs_3, false}, {inputs_2, true},  {inputs_1, false}], [{status, normal}, {value, 2}]},
    {[{inputs_3, true},  {inputs_2, false}, {inputs_1, false}], [{status, normal}, {value, 4}]},
    {[{inputs_3, true},  {inputs_2, true},  {inputs_1, true}],  [{status, normal}, {value, 7}]}
 ].

-endif.
