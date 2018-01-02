%%% @doc 
%%% Unit Test Utillity Functions
%%%               
%%% @end 

-module(unit_test_utils).

-author("Mark Sebald").

-ifdef(TEST).
-include_lib("../block_state.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
          min_block_test/1,
          block_setup/1,
          block_setup/2,
          block_setup/3,
          block_cleanup/2,
          create_io_test/6,
          create_io_tests/4
]).


%
% Minimal block type, unit test. 
% Call the block type's create(), upgrade(), initialize(), execute(), and delete() functions.
% Verify it does not crash
%
min_block_test(Module) ->
  block_cleanup(Module, Module:execute(block_setup(Module), input_cos)),
  ?assert(true).


%
% Common block unit test setup.
%

% No Config Values to setup, just initialize the block
block_setup(Module) ->
  Module:initialize(common_setup(Module)).

% Set Config values before initializing the block
block_setup(Module, PreInitConfigVals) ->
  Module:initialize(set_test_config(common_setup(Module), PreInitConfigVals)).

% Set Config values before and after initializing the block
% Example: Setting one config value, could change the quantity of other config values.
%  The affected config attributes, will not exist until after the block is initialized
block_setup(Module, PreInitConfigVals, PostInitConfigVals) ->
  set_test_config(block_setup(Module, PreInitConfigVals), PostInitConfigVals).


%
% Common test setup. 
% Call block's create() and upgrade() functions
% Create the initial BlockState i.e. {Config, Inputs, Outputs, Private}   
%
common_setup(Module) ->
  logger:start(lang_en_us),
  TestBlockTypeStr = atom_to_list(Module),
  TestBlockName = list_to_atom("test_" ++ TestBlockTypeStr),
  TestBlockDescription = "Unit testing block type: " ++ TestBlockTypeStr,

  % Creates the block with default values
  BlockDefn = Module:create(TestBlockName, TestBlockDescription),

  % Merely tests the existence of the upgrade function.
  % Since block code and block data are the same version, 
  % upgrade() does nothing here.
  {ok, {Config, Inputs, Outputs}} = Module:upgrade(BlockDefn),

  % Add an empty list to hold Private values.  This makes the complete BlockState
  {Config, Inputs, Outputs, []}.


%
% Common block test cleanup.  Call blocks's delete() function
%
block_cleanup(Module, BlockState) -> 
  Module:delete(BlockState),
  logger:stop().

%
%  Create a test that sets Config and Inputs test values, Executes the block, 
%  and then checks if updated Output values match expected output values
%
create_io_test(Module, ExecMethod, BlockState, TestConfigVals, TestInputVals, ExpOutputVals) ->
  ?_assert(get_block_outputs(Module, ExecMethod, BlockState, TestConfigVals, 
                             TestInputVals, ExpOutputVals) == ExpOutputVals).


%
% Update the block input and config values with the test values.
% Execute the block, and return a list of the current values,
% corresponding to the list of expected output values  
%
get_block_outputs(Module, ExecMethod, BlockState, TestConfigVals, TestInputVals, ExpOutputVals) ->
  % Update the Config and Inputs values to desired test state
  BlockState1 = set_test_config(BlockState, TestConfigVals),

  % By rule, block should be reinitialized after updating config value(s)
  BlockState2 = Module:initialize(BlockState1),
  
  BlockState3 = set_test_inputs(BlockState2, TestInputVals),

  % Execute the block, to update output values
  {_Config, _Inputs, CurrOutputs, _Private} = Module:execute(BlockState3, ExecMethod),

  % Create list of current block output values, to compare with expected block output values
  CurrOutputVals = lists:map(fun({ValueId, _ExpValue}) ->
                               {ok, CurrValue} = attrib_utils:get_value(CurrOutputs, ValueId),
                               {ValueId, CurrValue}
                             end,
                             ExpOutputVals),
  ?debugFmt("~n Exp Values: ~p~nCurr Values: ~p~n", [ExpOutputVals, CurrOutputVals]),
  CurrOutputVals.



%
% Create a test for each set of Input and Expected Output values
%
create_io_tests(Module, ExecMethod, BlockState, TestSets) ->
  lists:map(fun({TestInputVals, ExpOutputVals}) ->
                  ?_assert(get_block_outputs(Module, ExecMethod, BlockState, 
                                            TestInputVals, ExpOutputVals) == ExpOutputVals)
                end, TestSets).

%
% Update the block input and config values with the test values.
% Execute the block, and return a list of the current values,
% corresponding to the list of expected output values
% Preserve the BlockState for subsequent tests.  
% Some tests may depend on result of previous block execution
%
get_block_outputs(Module, ExecMethod, BlockState, TestInputVals, ExpOutputVals) ->
  % Get the BlockState from the previous test, in this set, if it exists
  case block_state() of
    undefined ->  LastBlockState = BlockState;
    LastBlockState -> ok
  end,

  BlockState1 = set_test_inputs(LastBlockState, TestInputVals),

  % Execute the block, to update output values
  BlockState2 = Module:execute(BlockState1, ExecMethod),

  % Save the BlockState for the next test in this set
  block_state(BlockState2),

  {_Config, _Inputs, CurrOutputs, _Private} = BlockState2,

  % Create list of current block output values, to compare with expected block output values
  CurrOutputVals = lists:map(fun({ValueId, _ExpValue}) ->
                               {ok, CurrValue} = attrib_utils:get_value(CurrOutputs, ValueId),
                               {ValueId, CurrValue}
                             end,
                             ExpOutputVals),
  ?debugFmt("~n Exp Values: ~p~nCurr Values: ~p~n", [ExpOutputVals, CurrOutputVals]),
  CurrOutputVals.

%
% Preserve the block state between consecutive block executions
%
block_state() ->
  get(block_state).

block_state(BlockState) ->
  put(block_state, BlockState).


%
% Set config test values
%
set_test_config(BlockState, TestConfigVals) ->
  {Config, Inputs, Outputs, Private} = BlockState,
  {ok, Config1} = attrib_utils:set_values(Config, TestConfigVals),
  {Config1, Inputs, Outputs, Private}.

%
% Set inputs test values
%
set_test_inputs(BlockState, TestInputVals) ->
  {Config, Inputs, Outputs, Private} = BlockState,
  {ok, Inputs1} = attrib_utils:set_values(Inputs, TestInputVals),
  {Config, Inputs1, Outputs, Private}.

-endif.
