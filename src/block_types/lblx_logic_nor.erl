%%% @doc 
%%% BLOCKTYPE
%%% Logic NOR
%%% DESCRIPTION
%%% Set the block output value to boolean NOT OR of all of the inputs
%%% LINKS              
%%% @end 

-module(lblx_logic_nor).  

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
       {num_of_inputs, {2}}, %| int | 2 | 2..99 |
       {ignore_nulls, {false}} %| bool | false | true, false |
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {inputs, [{empty, {empty}}]} %| bool array | empty | true, false |
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
      % Create N inputs
      BlockName = config_utils:name(Config),
      Inputs1 = input_utils:resize_attribute_array_value(BlockName, Inputs, 
                                  inputs, NumOfInputs, {empty, {empty}}),

      case config_utils:get_boolean(Config, ignore_nulls) of
        {ok, _IgnoreNulls} ->
          % All config values are OK      
          % Initialize output value
          Value = null,
          Status = initialed;
        
        {error, Reason} ->
           {Value, Status} = config_utils:log_error(Config, ignore_nulls, Reason)
      end;
    
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

execute({Config, Inputs, Outputs, Private}, disable) ->
  Outputs1 = output_utils:update_all_outputs(Outputs, null, disabled),
  {Config, Inputs, Outputs1, Private};

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  {ok, NumOfInputs} = attrib_utils:get_value(Config, num_of_inputs),
  {ok, IgnoreNulls} = attrib_utils:get_value(Config, ignore_nulls),

  % Use the logic from the OR block and invert the output value
  case lblx_logic_or:or_inputs(Inputs, 1, NumOfInputs, IgnoreNulls, []) of
     % At least one input value is null, and ignore_nulls config value is false
    {ok, null} ->  OutputVal = {ok, null};
    
    {ok, true} ->  OutputVal = {ok, false};
    
    {ok, false} -> OutputVal = {ok, true};
  
    {error, Reason} -> OutputVal = {error, Reason}
  end,

  ok, Outputs1 = output_utils:set_tristate_outputs(inputs, OutputVal, Config, Outputs),

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
    {[{num_of_inputs, 5}], [], [{status, no_input}, {value, null}]},
    {[{{inputs, 1}, false}, {{inputs, 2}, true}, {{inputs, 3}, null}, {{inputs, 4}, false}, {{inputs, 5}, bad_input_value}], [{status, input_err}, {value, null}]},
    {[{{inputs, 1}, true}, {{inputs, 2}, false}, {{inputs, 3}, null}, {{inputs, 4}, false}, {{inputs, 5}, false}], [{status, no_input}, {value, null}]},
    {[{{inputs, 1}, false}, {{inputs, 2}, false}, {{inputs, 3}, false}, {{inputs, 4}, false}, {{inputs, 5}, false}], [{status, normal}, {value, true}]},
    {[{{inputs, 1}, true}, {{inputs, 2}, false}, {{inputs, 3}, false}, {{inputs, 4}, false}, {{inputs, 5}, false}], [{status, normal}, {value, false}]},
    {[{{inputs, 1}, true}, {{inputs, 2}, true}, {{inputs, 3}, true}, {{inputs, 4}, true}, {{inputs, 5}, true}], [{status, normal}, {value, false}]},

    {[{ignore_nulls, true}], [], [{status, normal}, {value, false}]},
    {[{{inputs, 1}, false}, {{inputs, 2}, false}, {{inputs, 3}, null}, {{inputs, 4}, false}, {{inputs, 5}, bad_input_value}], [{status, input_err}, {value, null}]},
    {[{{inputs, 1}, false}, {{inputs, 2}, false}, {{inputs, 3}, null}, {{inputs, 4}, false}, {{inputs, 5}, false}], [{status, normal}, {value, true}]},
    {[{{inputs, 1}, false}, {{inputs, 2}, false}, {{inputs, 3}, null}, {{inputs, 4}, false}, {{inputs, 5}, true}], [{status, normal}, {value, false}]},
    {[{{inputs, 1}, true}, {{inputs, 2}, false}, {{inputs, 3}, false}, {{inputs, 4}, false}, {{inputs, 5}, false}], [{status, normal}, {value, false}]},
    {[{{inputs, 1}, true}, {{inputs, 2}, true}, {{inputs, 3}, null}, {{inputs, 4}, true}, {{inputs, 5}, true}], [{status, normal}, {value, false}]}
 ].

-endif.