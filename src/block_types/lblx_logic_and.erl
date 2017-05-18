%%% @doc 
%%% Block Type: Logic AND 
%%% Description:  Set the block output value to boolean AND of all of the inputs
%%%               
%%% @end 

-module(lblx_logic_and).  

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1, handle_info/2]).
-export([and_inputs/5]). % Special for NAND block type

groups() -> [logic].

description() -> "AND of All of the Binary Inputs".

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
       {num_of_inputs, {2}},  % Default number of inputs to 2
       {ignore_nulls, {false}} % Ignore NULL input values when calculating output value
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {inputs, [{empty, ?EMPTY_LINK}]} % Array attribute
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
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
  % Check the config values
  case config_utils:get_integer_range(Config, num_of_inputs, 1, 99) of
    {ok, NumOfInputs} ->              
      % Create N inputs
      BlockName = config_utils:name(Config),
      Inputs1 = input_utils:resize_attribute_array_value(BlockName, Inputs, 
                                  inputs, NumOfInputs, {empty, ?EMPTY_LINK}),

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

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  {ok, NumOfInputs} = attrib_utils:get_value(Config, num_of_inputs),
  {ok, IgnoreNulls} = attrib_utils:get_value(Config, ignore_nulls),

  case and_inputs(Inputs, 1, NumOfInputs, IgnoreNulls, []) of
    % At least one input value is null, and ignore_nulls config value is false
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


%% 
%% Unknown Info message, just log a warning
%% 
-spec handle_info(Info :: term(), 
                  BlockState :: block_state()) -> {noreply, block_state()}.

handle_info(Info, BlockState) ->
  log_server:warning(block_server_unknown_info_msg, [Info]),
  {noreply, BlockState}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%
% AND all of the binary input values together
%
and_inputs(Inputs, Index, NumOfInputs, IgnoreNulls, Values) when Index =< NumOfInputs ->

  case input_utils:get_boolean(Inputs, {inputs, Index}) of
     
    % Build a list of input values. Value is true, false, or null.
    {ok, Value} -> and_inputs(Inputs, Index+1, NumOfInputs, IgnoreNulls, [Value | Values]);

    % Error input value, stop looking, put block in error state
    {error, Reason} -> {error, Reason}
  end;

% Determine final block value
and_inputs(_Inputs, _Index, _NumOfInputs, IgnoreNulls, Values) -> 
  case IgnoreNulls of
    true -> % Ignoring null input values
      case lists:member(false, Values) of
        true  -> % At least one input was false, output value is false
          {ok, false};
        false -> 
          case lists:member(true, Values) of
            true -> 
              {ok, true};
            false -> % No false or true values, all inputs must be null, 
                     % Even though we are ignoring nulls, output is null
              {ok, null}
          end
      end;

    false -> % Not ignoring null input values
      case lists:member(null, Values) of
        true -> % At least one input was null, return null
          {ok, null};
      
        false -> % No inputs were null
          % If at least one input was false, then output value is false, otherwise true.
          case lists:member(false, Values) of
            true  -> {ok, false};
            false -> {ok, true}
          end
      end
  end.

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
  InitConfigVals = [{num_of_inputs, 5}],
  unit_test_utils:block_setup(?MODULE, InitConfigVals).

cleanup(BlockState) ->
  unit_test_utils:block_cleanup(?MODULE, BlockState).

test_io(BlockState) ->
  unit_test_utils:create_io_tests(?MODULE, input_cos, BlockState, test_sets()).

test_sets() ->
  [
    {[], [{status, no_input}, {value, null}]},
    {[{{inputs, 1}, false}, {{inputs, 2}, false}, {{inputs, 3}, null}, {{inputs, 4}, false}, {{inputs, 5}, bad_input_value}], [{status, input_err}, {value, null}]},
    {[{{inputs, 1}, false}, {{inputs, 2}, false}, {{inputs, 3}, null}, {{inputs, 4}, false}, {{inputs, 5}, false}], [{status, no_input}, {value, null}]},
    {[{{inputs, 1}, false}, {{inputs, 2}, false}, {{inputs, 3}, false}, {{inputs, 4}, false}, {{inputs, 5}, false}], [{status, normal}, {value, false}]},
    {[{{inputs, 1}, true}, {{inputs, 2}, false}, {{inputs, 3}, false}, {{inputs, 4}, false}, {{inputs, 5}, false}], [{status, normal}, {value, false}]},
    {[{{inputs, 1}, true}, {{inputs, 2}, true}, {{inputs, 3}, true}, {{inputs, 4}, true}, {{inputs, 5}, true}], [{status, normal}, {value, true}]}
 ].


block_ignore_nulls_test_() ->
  {"Input to Output tests (Ignore Nulls) for: " ++ atom_to_list(?MODULE),
   {setup, 
      fun setup_ignore_nulls/0, 
      fun cleanup_ignore_nulls/1,
      fun (BlockState) -> 
        {inorder,
        [
          test_io_ignore_nulls(BlockState)
        ]}
      end} 
  }.

setup_ignore_nulls() ->
  InitConfigVals = [{num_of_inputs, 5}, {ignore_nulls, true}],
  unit_test_utils:block_setup(?MODULE, InitConfigVals).

cleanup_ignore_nulls(BlockState) ->
  unit_test_utils:block_cleanup(?MODULE, BlockState).

test_io_ignore_nulls(BlockState) ->
  unit_test_utils:create_io_tests(?MODULE, input_cos, BlockState, test_sets_ignore_nulls()).

test_sets_ignore_nulls() ->
  [
    {[], [{status, no_input}, {value, null}]},
    {[{{inputs, 1}, false}, {{inputs, 2}, false}, {{inputs, 3}, null}, {{inputs, 4}, false}, {{inputs, 5}, bad_input_value}], [{status, input_err}, {value, null}]},
    {[{{inputs, 1}, false}, {{inputs, 2}, false}, {{inputs, 3}, null}, {{inputs, 4}, false}, {{inputs, 5}, false}], [{status, normal}, {value, false}]},
    {[{{inputs, 1}, false}, {{inputs, 2}, false}, {{inputs, 3}, false}, {{inputs, 4}, false}, {{inputs, 5}, false}], [{status, normal}, {value, false}]},
    {[{{inputs, 1}, true}, {{inputs, 2}, false}, {{inputs, 3}, false}, {{inputs, 4}, false}, {{inputs, 5}, false}], [{status, normal}, {value, false}]},
    {[{{inputs, 1}, true}, {{inputs, 2}, true}, {{inputs, 3}, null}, {{inputs, 4}, true}, {{inputs, 5}, true}], [{status, normal}, {value, true}]}
 ].

-endif.