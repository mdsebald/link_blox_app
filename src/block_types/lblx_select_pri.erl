%%% @doc 
%%% Block Type: Select Higest Priority Active Input Value
%%% Description:  Set the block output value to the highest priority actvive input value 
%%%               
%%% @end 

-module(lblx_select_pri).  

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [select].

description() -> "Select Highest Priority Active Input Value".

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
       {num_of_inputs, {3}}  % Default number of selectable inputs to 3                
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {inputs, [{empty, {empty}}]} % Array attribute
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
  % Check the config values
  case config_utils:get_integer_range(Config, num_of_inputs, 1, 99) of
    {ok, NumOfInputs} ->      
      % All config values are OK
              
      % Create N inputs
      BlockName = config_utils:name(Config),
      Inputs1 = input_utils:resize_attribute_array_value(BlockName, Inputs, 
                                  inputs, NumOfInputs, {empty, {empty}}),

      % Initialize output values
      Value = null,
      Status = initialed;
    
    {error, Reason} ->
      Inputs1 = Inputs,
      {Value, Status} = config_utils:log_error(Config, num_of_inputs, Reason)
 end,

  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),  

  % This is the block state
  {Config, Inputs1, Outputs1, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  {ok, NumOfInputs} = attrib_utils:get_value(Config, num_of_inputs),

  case highest_priority_input(Inputs, 1, NumOfInputs) of
    {ok, null} ->
      Value = null,
      Status = no_input;
    
    {ok, Value} -> 
      Status = normal;
  
    {error, Reason} ->
      {Value, Status} = input_utils:log_error(Config, inputs, Reason)
  end,
   
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

% Find the value of the highest priority (lowest index) input value

highest_priority_input(Inputs, Index, NumOfInputs) when Index =< NumOfInputs ->

  case input_utils:get_any_type(Inputs, {inputs, Index}) of
    % Input value is null, get next input value
    {ok, null} -> highest_priority_input(Inputs, Index+1, NumOfInputs);
    
    % Got an active input value, return it
    {ok, Value} -> {ok, Value};
  
    % Error input value, stop looking, put block in error state
    {error, Reason} -> {error, Reason}
  end;

% None of the inputs have active value, return null
highest_priority_input(_Inputs, _Index, _NumOfInputs) -> {ok, null}.


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
  InitConfigVals = [{num_of_inputs, 10}],
  unit_test_utils:block_setup(?MODULE, InitConfigVals).

cleanup(BlockState) ->
  unit_test_utils:block_cleanup(?MODULE, BlockState).

test_io(BlockState) ->
  unit_test_utils:create_io_tests(?MODULE, input_cos, BlockState, test_sets()).

test_sets() ->
  [
    {[], [{status, no_input}, {value, null}]},
    {[{{inputs, 1}, null}, {{inputs, 2}, 2}, {{inputs, 10}, 10}], [{status, normal}, {value, 2}]},
    {[{{inputs, 1}, 1}, {{inputs, 2}, 2}, {{inputs, 10}, 10}], [{status, normal}, {value, 1}]},
    {[{{inputs, 1}, null}, {{inputs, 2}, null}], [{status, normal}, {value, 10}]}
 ].

-endif.