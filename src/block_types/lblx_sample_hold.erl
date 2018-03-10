%%% @doc 
%%% BLOCKTYPE
%%% Sample and Hold input values
%%% DESCRIPTION
%%% Output values will equal corresponding input values as long as hold input is false
%%% When hold input value is true, outputs will be held at the last value
%%% regardless of the input values.
%%% LINKS
%%% @end 

-module(lblx_sample_hold).  
  
-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================

-export([groups/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [control].

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
       {num_of_values, {1}} %| int | 1 | 1..99 |
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {hold, {false, {false}}}, %| bool | false | true, false |
      {inputs, [{empty, {empty}}]} %| array of any | empty | N/A |
    ]). 


-spec default_outputs() -> output_attribs().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {outputs, [{null, []}]} %| array of any | null | N/A |
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

  case config_utils:get_integer_range(Config, num_of_values, 1, 99) of
    {ok, NumOfValues} ->      
      % Create N inputs and outputs
      BlockName = config_utils:name(Config),
      Inputs1 = input_utils:resize_attribute_array_value(BlockName, Inputs, 
                                  inputs, NumOfValues, {empty, {empty}}),

      Outputs1 = output_utils:resize_attribute_array_value(Outputs, 
                                  outputs, NumOfValues, {null, []}),
      
      Value = null,
      Status = initialed;

    {error, Reason} ->
      Inputs1 = Inputs,
      Outputs1 = Outputs,
      {Value, Status} = config_utils:log_error(Config, num_of_values, Reason)
  end,
  Outputs2 = output_utils:set_value_status(Outputs1, Value, Status),
  
  % This is the block state
  {Config, Inputs1, Outputs2, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  case input_utils:get_boolean(Inputs, hold) of
    {ok, true} ->
      % Outputs are held at last value, don't update
      Value = true, Status = normal,
      Outputs1 = Outputs;

    {ok, Value} -> % hold input value is false or null, 
      Status = normal,

      % Pass input values through to outputs. 
      % Quantity of input and output values are the same
      {ok, {inputs, InputVals}} = attrib_utils:get_attribute(Inputs, inputs),
      OutputVals = lists:map(fun({InputVal, {_DefVal}}) -> InputVal end, InputVals),
      Outputs1 = output_utils:set_array_values(Outputs, outputs, OutputVals);

    {error, Reason} ->
      {Value, Status} = input_utils:log_error(Config, hold, Reason),
      {ok, NumOfValues} = config_utils:get_integer(Config, num_of_values),
      % Set array of output values to null
      NullVals = lists:duplicate(NumOfValues, null),
      Outputs1 = output_utils:set_array_values(Outputs, outputs, NullVals)
  end,

  Outputs2 = output_utils:set_value_status(Outputs1, Value, Status),

  % Return updated block state
  {Config, Inputs, Outputs2, Private}.


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
    % Test bad config values
    {[{num_of_values, -1}],[{hold, "Bad Value"}], [{status, config_err}, {value, null}, {{outputs, 1}, null}]},
    {[{num_of_values, 100}],[], [{status, config_err}, {value, null}, {{outputs, 1}, null}]},

    % Test bad input values
    {[{num_of_values, 1}], [], [{status, input_err}, {value, null}, {{outputs, 1}, null}]},

    {[{hold, true}, {{inputs, 1}, "something"}], [{status, normal}, {value, true}, {{outputs, 1}, null}]},
    {[{hold, false}], [{status, normal}, {value, false}, {{outputs, 1}, "something"}]},
    {[{hold, false}, {{inputs, 1}, "something else"}], [{status, normal}, {value, false}, {{outputs, 1}, "something else"}]},
    {[{num_of_values, 10}], [{hold, true}, {{inputs, 1}, 1},{{inputs, 5}, 5}, {{inputs, 10}, 10}], 
                            [{status, normal}, {value, true}, {{outputs, 1}, "something else"}, {{outputs, 5}, null}, {{outputs, 10}, null}]},
    {[{hold, false}], [{status, normal}, {value, false}, {{outputs, 1}, 1}, {{outputs, 5}, 5}, {{outputs, 10}, 10}]}
  ].

-endif.
