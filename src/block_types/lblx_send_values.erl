
%%% @doc 
%%% @doc 
%%% BLOCKTYPE
%%% Send Input values
%%% DESCRIPTION
%%% Export values to receive_value block(s) on other LinkBlox Nodes.
%%% Send block and Receive block must have the same name.
%%% LINKS              
%%% @end 

-module(lblx_send_values).  
  
-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [output, comm].

version() -> "0.1.0".


-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {num_of_nodes, {1}}, %| int | 1 | 1..99 |
      {nodes, [{null}]}, %| atom array | null | N/A |
      {num_of_values, {1}} %| int | 1 | 1..99 |
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {send_values, [{empty, {empty}}]} %| any array | empty | N/A |
    ]). 


-spec default_outputs() -> output_attribs().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {nodes_state, [{null, []}]} %| bool array | null | true, false |
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

  BlockName = config_utils:name(Config),
  % Check the config values
  case config_utils:get_integer_range(Config, num_of_nodes, 1, 99) of
    {ok, NumOfNodes} ->              
      % Create array of node names
      Config1 = config_utils:resize_attribute_array_value(Config, 
                                      nodes, NumOfNodes, {null}),
      
      Outputs1 = output_utils:resize_attribute_array_value(Outputs, 
                                      nodes_state, NumOfNodes, {null, []}),

      case config_utils:get_integer_range(Config1, num_of_values, 1, 99) of
        {ok, NumOfValues} ->              
          % Create array of input values to send to other nodes
          Inputs1 = input_utils:resize_attribute_array_value(BlockName, Inputs, 
                                            send_values, NumOfValues, {empty, {empty}}),
          % All config values are OK      
          % Initialize output value
          Value = null, Status = initialed;
          
        {error, Reason} ->
          {Value, Status} = config_utils:log_error(Config1, num_of_exports, Reason),
          Inputs1 = Inputs
      end;
    {error, Reason} ->
      {Value, Status} = config_utils:log_error(Config, num_ofnodes, Reason),
      Config1 = Config,
      Inputs1 = Inputs,
      Outputs1 = Outputs
  end,

  Outputs2 = output_utils:set_value_status(Outputs1, Value, Status),

  % This is the block state
  {Config1, Inputs1, Outputs2, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, disable) ->
  Outputs1 = output_utils:update_all_outputs(Outputs, null, disabled),
  {Config, Inputs, Outputs1, Private};

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  % Read the input values and publish to other nodes

  % Number of nodes to export values to
  {ok, NumOfNodes} = attrib_utils:get_value(Config, num_of_nodes),

  % Number of input values to export to other nodes
  {ok, NumOfValues} = attrib_utils:get_value(Config, num_of_values),

  % Read the inputs
  case read_inputs(Config, Inputs, 1, NumOfValues, []) of
    {ok, Values} ->
      % The name of the block receiving values on the other node(s) 
      %   is the same as the name of the block on this node
      BlockName = config_utils:name(Config),
      case publish_values(Config, Outputs, 1, NumOfNodes, BlockName, Values) of
        {ok, Outputs1} -> 
          Value = true, Status = normal;

        {error, Reason, Outputs1} ->
          attrib_utils:log_error(Config, export_nodes, Reason),
          Value = null, Status = config_err
      end;

    {error, Reason} ->
      Outputs1 = Outputs,
      {Value, Status} = input_utils:log_error(Config, send_values, Reason)
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

%
% Read all of block export values inputs
%
-spec read_inputs(Config :: config_attribs(), 
                  Inputs :: input_attribs(), 
                  Index :: pos_integer(), 
                  NumOfValues :: pos_integer(), 
                  Values :: list()) -> {ok, list()} | {error, atom()}.

read_inputs(Config, Inputs, Index, NumOfValues, Values) when Index =< NumOfValues ->

  case input_utils:get_any_type(Inputs, {send_values, Index}) of
     
    % Build a list of input values to export.
    {ok, Value} -> 
      % Rename Value ID to receive_values, to match the output attributes of the destination node block
      read_inputs(Config, Inputs, Index+1, NumOfValues, [{{receive_values, Index}, Value} | Values]);

    % Error input value, stop looking, put block in error state
    {error, Reason} -> 
      input_utils:log_error(Config, {send_values, Index}, Reason),
      {error, Reason}
  end;

read_inputs(_Config, _Inputs, _Index, _NumOfValues, Values) ->
  {ok, Values}.


%
% Publish the export values inputs
%
-spec publish_values(Config :: config_attribs(),
                     Outputs :: output_attribs(),
                     Index :: pos_integer(),
                     NumOfNodes :: pos_integer(),
                     BlockName :: block_name(),
                     Values :: list()) -> {ok, output_attribs()} | {error, atom(), output_attribs()}.

publish_values(Config, Outputs, Index, NumOfNodes, BlockName, Values) when Index =< NumOfNodes ->

  ValueId = {nodes, Index},

  case config_utils:get_node(Config, ValueId) of
    {ok, null} -> % Ignore null export node values
      {ok, NewOutputs} = attrib_utils:set_value(Outputs, {nodes_state, Index}, null),
      publish_values(Config, NewOutputs, Index+1, NumOfNodes, BlockName, Values);

    {ok, Node} ->
      % Check if already connected to this node, nodes() is an Erlang BIF
      case lists:member(Node, nodes()) of
        true -> % Already connected to node, send values
          block_server:publish_values(Node, BlockName, Values),
          {ok, NewOutputs} = attrib_utils:set_value(Outputs, {nodes_state, Index}, connected);

        false -> % Not connected to node yet, see if we can connect
          case net_adm:ping(Node) of
            pong -> % Connected, send values
              block_server:publish_values(Node, BlockName, Values),
              {ok, NewOutputs} = attrib_utils:set_value(Outputs, {nodes_state, Index}, connected);

            pang -> % Unable to connect, do nothing
              {ok, NewOutputs} = attrib_utils:set_value(Outputs, {nodes_state, Index}, not_connected)
          end
      end,
      publish_values(Config, NewOutputs, Index+1, NumOfNodes, BlockName, Values);

    % Error reading node values, stop sending and put block in error state  
    {error, Reason} -> 
      attrib_utils:log_error(Config, ValueId, Reason),
      {error, Reason, Outputs}
  end;

publish_values(_Config, Outputs, _Index, _NumOfNodes, _BlockName, _Values) -> 
  {ok, Outputs}.


%% ====================================================================
%% Tests
%% ====================================================================


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("block_io_test_gen.hrl").

test_sets() ->
  [
    {[{status, normal}]}
  ].

-endif.
