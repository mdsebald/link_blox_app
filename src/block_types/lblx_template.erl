% INSTRUCTIONS: Copy this module and modify as appropriate 
%               for the function this block will perform.
%               Comments marked "INSTRUCTIONS:" may be deleted 

%%% @doc 
%%% Block Type:  
%%% Description:   
%%%               
%%% @end 

% INSTRUCTIONS: Modify to match new block type module name
% All block type module names must begin with "lblx_" 
% to uniquely identify them as LinkBlox block type modules.
-module(lblx_template).  
  
-author("Your Name").

 % INSTRUCTIONS: Adjust path to hrl file as needed
-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================

% INSTRUCTIONS: The following 10 functions must be implemented and exported by all block types
-export([groups/0, description/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

% INSTRUCTIONS: Optional custom message handling functions.
% If the block type needs to handle custom messages, not currently handled by block_server(),
% export the necessary function(s) here, and create the function(s) below. 
% Otherwise, delete this line.
-export([handle_call/3, handle_cast/2, handle_info/2]).


% INSTRUCTIONS: Classify block type, by assigning it to one or more groups
groups() -> [none].

% INSTRUCTIONS: String describing block function
description() -> "Short description of block function".

% INSTRUCTIONS: Set block type version number.
% Use pattern: Major.Minor.Patch
% When a block is created, the Config version attribute value 
% is set to this version.
% When a block is loaded from a config file, the version attribute value
% is compared to this.  
% If the versions are different, the upgrade() function is called.
version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      % INTRUCTIONS: Insert block type specific config attribute tuples here
      % Config attribute tuples consist of a value name and a value
      % Example: {gpio_pin, {0}}
      % Array Example: {start_rows, [{1}, {2}]}
      % The block is (re) initialized, when any config value is modified.
      {config1, {"Example Value"}}               
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      % INTRUCTIONS: Insert block type specific input attribute tuples here
      % Input attribute tuples consist of a value name, a value, and a default value
      % Example: {hi_limit, {100, {100}}}
      % Array Example: {inputs, [{empty, {empty}}, {empty, {empty}}]}
      {input1, {"Example Input Value", {"Example Input Value"}}}
    ]). 


-spec default_outputs() -> output_attribs().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      % INTRUCTIONS: Insert block type specific output attribute tuples here
      % Output attribute tuples consist of a value name, a calculated value, 
      % and a list of links to block input values
      % Output values are always set to 'null' and empty link list on creation
      % Example: {dwell, {null, []}}
      % Array Example: {digit, [{null, []}, {null, []}]} 
      {output1, {null, []}} 
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

  % INSTRUCTIONS:  This function is called, on block creation, when the
  % module's version does not match the version in the block's config data.
  % Depending on the version(s) perform any necessary adjustments to the 
  % block's attributes, to make it compatible with the current block type's code.
  % If upgrading the attributes is not possible, return an error and reason.

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
    
  % INSTRUCTIONS: Perform block type specific initializations here
  % Add and intialize private attributes here
  Outputs1 = Outputs,
  Private1 = Private,

  % This is the block state
  {Config, Inputs, Outputs1, Private1}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  % INSTRUCTIONS: Perform block type specific actions here, 
  % read input value(s) calculate new output value(s)
  % set block output status and value

  % Example block execution: 
  %  read input1 and set value output to same, and set status output to normal, 
  %  unless error encountered reading input1
  case input_utils:get_any_type(Inputs, input1) of
    {ok, InputVal} ->
      Outputs1 = output_utils:set_value_normal(Outputs, InputVal);
    {error, _Reason} ->
      Outputs1 = output_utils:set_value_status(Outputs, null, input_err)
  end,

  % Return updated block state
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  % INSTRUCTIONS: Perform any block type specific delete functionality here
  % Return block definition, (Block state less Private values)
  % in case calling function wants to reuse them.
  %
  % Private values are created in the block initialization routine
  % So they should be deleted here
  
  {Config, Inputs, Outputs}.


% INSTRUCTIONS: If needed, implement block type specific message handling functions here.
% These function(s) are typically requrired for block types that 
%   interact with 3rd party libraries, external hardware, blocks on other nodes, or timers.
% See: lblx_mqtt_pub_sub and lblx_gpio_di block types for examples
% If all of the messages are already handled in the block_server module,
%   these handle_*() functions are not required, and may be deleted.

%% 
%% Handle block type specific call message(s)
%%
-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()}, 
                  BlockState :: block_state()) -> {reply, ok, block_state()}.

handle_call(Request, From, BlockState) ->

  {BlockName, BlockModule} = config_utils:name_module(BlockState),
  logger:warning(block_type_name_unknown_call_msg_from, [BlockModule, BlockName, Request, From]),
  {reply, ok, BlockState}.


%% 
%% Handle block type specific cast message(s)
%%
-spec handle_cast(Msg :: term(), 
                  BlockState :: block_state()) -> {noreply, block_state()}.
 
handle_cast(Msg, BlockState) ->

  {BlockName, BlockModule} = config_utils:name_module(BlockState),
  logger:warning(block_type_name_unknown_cast_msg, [BlockModule, BlockName, Msg]),
  {noreply, BlockState}.


%% 
%% Handle block type specific info message(s)
%% 
-spec handle_info(Info :: term(), 
                  BlockState :: block_state()) -> {noreply, block_state()}.

handle_info(Info, BlockState) ->
  
  {BlockName, BlockModule} = config_utils:name_module(BlockState),
  logger:warning(block_type_name_unknown_info_msg, [BlockModule, BlockName, Info]),
  {noreply, BlockState}.


%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Tests
%% ====================================================================

% INSTRUCTIONS:  Create unit tests here.  

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
