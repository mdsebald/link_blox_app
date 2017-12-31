
%%% @doc 
%%% Block Type:  Receive block values from other nodes
%%% Description: Receive block values from other LinkBlox nodes,
%%%              transmitted by send_values block types
%%%               
%%% @end 


-module(lblx_receive_values).  
  
-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).
-export([handle_cast/2]).

groups() -> [input, comm].

description() -> "Receive values from other nodes".

version() -> "0.1.0".


-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {num_of_values, {1}}
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
       {receive_values, [{null, []}]}
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

  % Add a private attribute, to hold values published by send_values block types.
  Private1 = attrib_utils:add_attribute(Private, {publish_values, {[]}}),

  % Check the config values
  case config_utils:get_integer_range(Config, num_of_values, 1, 99) of
    {ok, NumOfValues} ->              
      % Create array of outputs to import values from other nodes
      Outputs1 = output_utils:resize_attribute_array_value(Outputs, 
                                    receive_values, NumOfValues, {null, []}),
      Value = true, Status = normal;
      
    {error, Reason} ->
      {Value, Status} = config_utils:log_error(Config, num_of_values, Reason),
      Outputs1 = Outputs
  end,
       
  Outputs2 = output_utils:set_value_status(Outputs1, Value, Status),

  % This is the block state
  {Config, Inputs, Outputs2, Private1}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, ExecMethod) ->
  % Only update outputs if we get a publish_values message
  case ExecMethod of
    message ->
      % Received updated values, reset the exec_interval timer, if set
      BlockName = config_utils:name(Config),
      {_Status, Private1} = block_common:update_execution_timer(BlockName, Inputs, Private), 
    
      % Received a publish message from another node_exp_imp block type
      % Write the values to the import_values[x] outputs
      {ok, Values} = attrib_utils:get_value(Private, publish_values),
      {ok, Private2} = attrib_utils:set_value(Private1, publish_values, []),

      case length(Values) of
        0 ->  % Nothing to write
          Outputs2 = Outputs;

        ValuesQty ->
          {ok, BlockQty} = config_utils:get_pos_integer(Config, num_of_values),
      
          case BlockQty < ValuesQty of
            true -> % Too many values, truncate Values list
              {TruncValues, _Remainder} = lists:split(BlockQty, Values), 
              % Update the output values
              case attrib_utils:set_values(Outputs, TruncValues) of
                {ok, Outputs1} -> 
                  % Update the status and main output
                  Outputs2 = output_utils:set_value_status(Outputs1, true, normal);

                {error, Reason} ->
                  attrib_utils:log_error(Config, receive_values, invalid, Reason),
                  Outputs2 = output_utils:set_value_status(Outputs, null, proc_err)
              end;
              
            false ->
              % Update the output values
              case attrib_utils:set_values(Outputs, Values) of
                {ok, Outputs1} -> 
                  % Update the status and main output
                  Outputs2 = output_utils:set_value_status(Outputs1, true, normal);

                {error, Reason} ->
                  attrib_utils:log_error(Config, receive_values, invalid, Reason),
                  Outputs2 = output_utils:set_value_status(Outputs, null, proc_err)
              end
          end
      end,
      % Return updated block state
      {Config, Inputs, Outputs2, Private2};

    % Exec timer timed out before receiving updated values, set output values to null
    timer ->
      case attrib_utils:set_array_value(Outputs, receive_values, null) of
        {ok, Outputs1} -> 
          % Update the status and main output
          Outputs2 = output_utils:set_value_status(Outputs1, false, timeout);

        {error, Reason} ->
          attrib_utils:log_error(Config, receive_values, invalid, Reason),
          Outputs2 = output_utils:set_value_status(Outputs, null, proc_err)
      end,
      % Return updated block state
      {Config, Inputs, Outputs2, Private};

    % Nothing to do, if block is executed for any other reason
    _DontCare ->
      % Return block state unchanged
      {Config, Inputs, Outputs, Private}
  end.


%% 
%%  Delete the block
%%	
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  
  {Config, Inputs, Outputs}.


%% 
%% Handle cast messages 
%%
-spec handle_cast(Msg :: term(), 
                  BlockState :: block_state()) -> {noreply, block_state()}.

%% 
%% Handle publish_values message from a send_values block type.
%%
handle_cast({publish_values, Values}, BlockState) ->
  {Config, Inputs, Outputs, Private} = BlockState,
  BlockName = config_utils:name(Config),
  log_server:debug("Rx publish_values message: ~p Values: ~p~n", 
                      [BlockName, Values]),

  % Save values in private attribute space and execute the block 
  %   to update the block receive_values[x] output attributes
  {ok, NewPrivate} = attrib_utils:set_value(Private, publish_values, Values),
  NewBlockState = block_common:execute({Config, Inputs, Outputs, NewPrivate}, message),

  {noreply, NewBlockState};

%%
%% Unkown cast message
%%
handle_cast(Msg, BlockState) ->

  {BlockName, BlockModule} = config_utils:name_module(BlockState),
  log_server:warning(block_type_name_unknown_cast_msg, [BlockModule, BlockName, Msg]),
  {noreply, BlockState}.
    

%% ====================================================================
%% Internal functions
%% ====================================================================




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
