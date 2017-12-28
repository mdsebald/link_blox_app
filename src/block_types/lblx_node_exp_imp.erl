
%%% @doc 
%%% Block Type:  Node Export/Import
%%% Description: Export values to and import values from other LinkBlox Nodes  
%%%               
%%% @end 


-module(lblx_node_exp_imp).  
  
-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1, handle_info/2]).

groups() -> [input, output, comm].

description() -> "Export and import node values".

version() -> "0.1.0".


-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {num_of_nodes, {1}},
      {export_nodes, [{null}]},
      {num_of_exports, {1}},
      {num_of_imports, {1}}
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {export_values, [{empty, {empty}}]}
    ]). 


-spec default_outputs() -> output_attribs().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {exp_nodes_state, [{null, []}]},
      {import_values, [{null, []}]}
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

  % Add a private attribute, to hold values published by other node_exp_imp block types.
  Private1 = attrib_utils:add_attribute(Private, {publish_values, {[]}}),

  BlockName = config_utils:name(Config),
  % Check the config values
  case config_utils:get_integer_range(Config, num_of_nodes, 1, 99) of
    {ok, NumOfNodes} ->              
      % Create array of node names
      Config1 = config_utils:resize_attribute_array_value(Config, 
                                      export_nodes, NumOfNodes, {null}),
      
      Outputs1 = output_utils:resize_attribute_array_value(Outputs, 
                                      exp_nodes_state, NumOfNodes, {null, []}),

      case config_utils:get_integer_range(Config1, num_of_exports, 1, 99) of
        {ok, NumOfExports} ->              
          % Create array of inputs to export values to other nodes
          Inputs1 = input_utils:resize_attribute_array_value(BlockName, Inputs, 
                                            export_values, NumOfExports, {empty, {empty}}),
          
          case config_utils:get_integer_range(Config1, num_of_imports, 1, 99) of
            {ok, NumOfImports} ->              
              % Create array of outputs to import values from other nodes
              Outputs2 = output_utils:resize_attribute_array_value(Outputs1, 
                                            import_values, NumOfImports, {null, []}),
              % All config values are OK      
              % Initialize output value
              Value = null, Status = initialed;

            {error, Reason} ->
              {Value, Status} = config_utils:log_error(Config1, num_of_imports, Reason),
              Outputs2 = Outputs
          end;
        {error, Reason} ->
          {Value, Status} = config_utils:log_error(Config1, num_of_exports, Reason),
          Inputs1 = Inputs,
          Outputs2 = Outputs
      end;
    {error, Reason} ->
      {Value, Status} = config_utils:log_error(Config, num_ofnodes, Reason),
      Config1 = Config,
      Inputs1 = Inputs,
      Outputs2 = Outputs
  end,

  Outputs3 = output_utils:set_value_status(Outputs2, Value, Status),

  % This is the block state
  {Config1, Inputs1, Outputs3, Private1}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, ExecMethod) ->

  case ExecMethod of
    message ->
      % Received a publish message from another node_exp_imp block type
      % Write the values to the import_values[x] outputs
      {ok, Values} = attrib_utils:get_value(Private, publish_values),
      {ok, Private1} = attrib_utils:set_value(Private, publish_values, []),

      case length(Values) of
        0 ->  % Nothing to write
          Outputs2 = Outputs;

        ValuesQty ->
          {ok, ImportQty} = config_utils:get_pos_integer(Config, num_of_imports),
      
          case ImportQty < ValuesQty of
            true -> % Too many values, truncate Values list
              {TruncValues, _Remainder} = lists:split(ImportQty, Values), 
              % Update the output values
              case attrib_utils:set_values(Outputs, TruncValues) of
                {ok, Outputs1} -> 
                  % Update the status and main output
                  Outputs2 = output_utils:set_value_status(Outputs1, true, normal);

                {error, Reason} ->
                  attrib_utils:log_error(Config, import_values, invalid, Reason),
                  Outputs2 = output_utils:set_value_status(Outputs, null, proc_err)
              end;
              
            false ->
              % Update the output values
              case attrib_utils:set_values(Outputs, Values) of
                {ok, Outputs1} -> 
                  % Update the status and main output
                  Outputs2 = output_utils:set_value_status(Outputs1, true, normal);

                {error, Reason} ->
                    attrib_utils:log_error(Config, import_values, invalid, Reason),
                    Outputs2 = output_utils:set_value_status(Outputs, null, proc_err)
              end
          end
      end,
      % Return updated block state
      {Config, Inputs, Outputs2, Private1};
    
    _DontCare ->
      % For all other exec methods, read the input values and publish to other nodes

      % Number of nodes to export values to
      {ok, NumOfNodes} = attrib_utils:get_value(Config, num_of_nodes),

      % Number of input values to export to other nodes
      {ok, NumOfExports} = attrib_utils:get_value(Config, num_of_exports),

      % Read the inputs
      case read_inputs(Config, Inputs, 1, NumOfExports, []) of
        {ok, Values} ->
          % The name of the block on the other node(s) 
          % is the same as the name of the block on this node
          BlockName = config_utils:name(Config),
          case publish_values(Config, 1, NumOfNodes, BlockName, Values) of
            ok -> Value = true, Status = normal;

            {error, Reason} ->
              attrib_utils:log_error(Config, export_nodes, Reason),
              Value = null, Status = config_err
          end;

        {error, Reason} ->
          {Value, Status} = input_utils:log_error(Config, export_values, Reason)
      end,

      Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

      % Return updated block state
      {Config, Inputs, Outputs1, Private}
  end.


%% 
%%  Delete the block
%%	
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  
  {Config, Inputs, Outputs}.


%% 
%% Handle Info messages 
%%
-spec handle_info(Info :: term(), 
                  BlockState :: block_state()) -> {noreply, block_state()}.

%% 
%% LinkBlox publish message from another node_exp_imp block type.
%%
handle_info({linkblox_publish, Values}, BlockState) ->
  {Config, Inputs, Outputs, Private} = BlockState,
  BlockName = config_utils:name(Config),
  log_server:debug("Rx LinkBlox publish message: ~p Values: ~p~n", 
                      [BlockName, Values]),

  % Save values in private attribute space and execute the block 
  %   to update the block import_values[x] output attributes
  {ok, NewPrivate} = attrib_utils:set_value(Private, publish_values, Values),
  NewBlockState = block_common:execute({Config, Inputs, Outputs, NewPrivate}, message),

  {noreply, NewBlockState};

%%
%% Unknown Info message, just log a warning
%%
handle_info(Info, BlockState) ->
  log_server:warning(block_server_unknown_info_msg, [Info]),
  {noreply, BlockState}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%
% Read all of block export values inputs
%
read_inputs(Config, Inputs, Index, NumOfExports, Values) when Index =< NumOfExports ->

  case input_utils:get_any_type(Inputs, {export_values, Index}) of
     
    % Build a list of input values to export.
    {ok, Value} -> 
      % Rename Value ID to import_values, to match the output attributes of the destination node block
      read_inputs(Config, Inputs, Index+1, NumOfExports, [{{import_values, Index}, Value} | Values]);

    % Error input value, stop looking, put block in error state
    {error, Reason} -> 
      input_utils:log_error(Config, {export_values, Index}, Reason),
      {error, Reason}
  end;

read_inputs(_Config, _Inputs, _Index, _NumOfExports, Values) ->
  {ok, Values}.


%
% Publish the export values inputs
%
publish_values(Config, Index, NumOfNodes, BlockName, Values) when Index =< NumOfNodes ->

  ValueId = {export_nodes, Index},

  case config_utils:get_node(Config, ValueId) of
    {ok, null} -> % Ignore null export node values
      publish_values(Config, Index+1, NumOfNodes, BlockName, Values);

    {ok, Node} ->
      % Check if already connected to this node, nodes() is Erlang BIF
      case lists:member(Node, nodes()) of
        true -> % Already connected to node, send values
          block_server:linkblox_publish(Node, BlockName, Values);
        false -> % Not connected to node yet, see if we can connect
          case net_adm:ping(Node) of
            pong -> % Connected, send values
              block_server:linkblox_publish(Node, BlockName, Values);
            pang -> % Unable to connect, do nothing
              ok
          end
      end,
      publish_values(Config, Index+1, NumOfNodes, BlockName, Values);

    % Error reading node values, stop sending and put block in error state  
    {error, Reason} -> 
      attrib_utils:log_error(Config, ValueId, Reason),
      {error, Reason}
  end;

publish_values(_Config, _Index, _NumOfNodes, _BlockName, _Values) -> 
  ok.


%% ====================================================================
%% Tests
%% ====================================================================


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% INSTRUCTIONS: At the very least,  invoke a minimal block test, 
%   which calls the block's create(), upgrade(), initialize(), execute(), and delete() functions

block_test() ->
  unit_test_utils:min_block_test(?MODULE).


% INSTRUCTIONS: Use this test fixture to exercise the block's input to output functionality
block_test_() ->
  {"Input to Output tests for: " ++ atom_to_list(?MODULE),
   {setup, 
      fun setup/0, 
      fun cleanup/1,
      fun (BlockState) -> 
        {inorder,
        [
          test_io(BlockState)
          % test_io_x(BlockState)
        ]}
      end} 
  }.

setup() ->
  % INSTRUCTIONS: Call one of the following 3 block_setup() functions:

  % INSTRUCTIONS: Use this setup if default block config values do not need to be set before running I/O tests
  unit_test_utils:block_setup(?MODULE).

  % INSTRUCTIONS: Use this setup if default block config values need to be set before running I/O tests
  % PreInitConfigVals = [{config1, "config1 value"}, {config2, 2}],  % Example config values
  % unit_test_utils:block_setup(?MODULE, PreInitConfigVals).

  % INSTRUCTIONS: Use this setup if block config values need to be set before and after initialization
  % PreInitConfigVals = [{num_of_configs, 24} ],  % Example pre-init config values  
  % PostInitConfigVals = [{{config, 24}, "24th Config Value"}]  % Example post-init config values 
  % unit_test_utils:block_setup(?MODULE, PreInitConfigVals, PostInitConfigVals).

cleanup(BlockState) ->
  unit_test_utils:block_cleanup(?MODULE, BlockState).

test_io(BlockState) ->
  unit_test_utils:create_io_tests(?MODULE, input_cos, BlockState, test_sets()).

% INSTRUCTIONS: 
%  test_sets() is a list of tuples.  
%  Each tuple defines one I/O test, and contains 2 lists.
%  The first list contains {input value id, value} tuples, that the inputs of the block 
%    will be set to before executing the block.  
%  The second list contains {output value id, value} tuples, that represent 
%    the expected output values after executing the block
%  Each set of input / output values represents a test.
%  A test consists of setting the input values to the input values of the list,
%    executing the block, and comparing the actual output values to the expected output values list
%  The block state is preserved between each test, and used in the subsequent test.
%  Any input value IDs not specified in the input list, will not be modified before the test
%  Any output value IDs not specified in the output list, will not be compared after the test

test_sets() ->
  [
    {[{input1, "I/O Unit Test 1"}], [{status, normal}, {value, "I/O Unit Test 1"}]},
    {[{input1, "I/O Unit Test 2"}], [{status, normal}, {value, "I/O Unit Test 2"}]}
  ].

-endif.
