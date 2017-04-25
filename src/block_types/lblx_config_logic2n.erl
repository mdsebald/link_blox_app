%%% @doc 
%%% Block Type:  
%%% Description:  2 Input Configurable Logic Gate, Null Values Allowed
%%%               
%%% @end 


-module(lblx_config_logic2n).
  
-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).


groups() -> [logic].

description() -> "2 Input Configurable Logic Gate, Null Values Allowed".

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {'0_0_out', {null}}, % Output value for input 2 = false & 1 = false
      {'0_1_out', {null}}, % Output value for input 2 = false & 1 = true
      {'0_X_out', {null}}, % Output value for input 2 = false & 1 = null
      {'1_0_out', {null}}, % Output value for input 2 = true & 1 = false
      {'1_1_out', {null}}, % Output value for input 2 = true & 1 = true
      {'1_X_out', {null}}, % Output value for input 2 = true & 1 = null
      {'X_0_out', {null}}, % Output value for input 2 = null & 1 = false
      {'X_1_out', {null}}, % Output value for input 2 = null & 1 = true
      {'X_X_out', {null}}  % Output value for input 2 = null & 1 = null            
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
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

 case input_utils:get_boolean(Inputs, inputs_2) of
    {ok, Input2} ->

      case input_utils:get_boolean(Inputs, inputs_1) of
        {ok, Input1} ->
          ValueName = maps:get({Input2, Input1}, in_out_value_map()),
          % Set the output value to the config value corresponding to the input state
          {ok, Value} = config_utils:get_any_type(Config, ValueName),
          {Value, normal};

        {error, Reason} ->
          input_utils:log_error(Config, inputs_1, Reason)
      end;

    {error, Reason} ->
      input_utils:log_error(Config, inputs_2, Reason)
  end.

% Return Config value ID for given input values
-spec in_out_value_map() -> value_name().

in_out_value_map() ->
  #{
    {false, false} => '0_0_out',
    {false, true} => '0_1_out',
    {false, null} => '0_X_out',
    {true, false} => '1_0_out',
    {true, true} => '1_1_out',
    {true, null} => '1_X_out',
    {null, false} => 'X_0_out',
    {null, true} => 'X_1_out',
    {null, null} => 'X_X_out'
  }.


%% ====================================================================
%% Tests
%% ====================================================================

% INSTRUCTIONS:  Create unit tests here.  

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% Perform minimum block unit test

block_test() ->
  unit_test_utils:min_block_test(?MODULE).


-endif.
