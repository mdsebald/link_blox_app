%%% @doc 
%%% Block Input/Output Unit Test Utilities
%%%               
%%% @end 

-module(block_io_test_utils).

-author("Mark Sebald").

-ifdef(TEST).
-include_lib("../block_state.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
          block_setup/1,
          block_cleanup/1,
          create_io_tests/2
]).

%
% Common block unit test setup.
%
block_setup(Module) ->
  TestBlockTypeStr = atom_to_list(Module),
  TestBlockName = list_to_atom("test_" ++ TestBlockTypeStr),
  TestBlockDescription = "Unit testing block type: " ++ TestBlockTypeStr,

  % Creates the block with default values
  BlockDefn = Module:create(TestBlockName, TestBlockDescription),

  % Merely tests the existence of the upgrade function.
  % Since block code and block data are the same version, 
  % upgrade() does nothing here.
  {ok, {Configs, Inputs, Outputs}} = Module:upgrade(BlockDefn),

  Module:initialize({Configs, Inputs, Outputs, []}).


%
% Common block test cleanup.  Call blocks's delete() function
%
block_cleanup(BlockState) ->
  Module = config_utils:module(BlockState),
  Module:delete(BlockState),
  block_state(undefined).


%
% Create a test for each set of Input and Expected Output values
%
create_io_tests(BlockState, TestSets) ->
  lists:map(fun(TestSet) ->
              {CurrOutputVals, ExpOutputVals} = set_inputs_get_outputs(BlockState, TestSet),
              ?_assert(CurrOutputVals == ExpOutputVals)
            end, TestSets).

%
% Update the block config and input attributes with the test values.
% Initiaze the block if the config values list is not empty
% Execute the block, and return a list of the current values,
% corresponding to the list of expected output values
% Preserve the BlockState for subsequent tests.  
% Some tests may depend on result of previous block execution
%
set_inputs_get_outputs(BlockState, TestSet) ->
  % Get the BlockState from the previous test, in this set, if it exists
  case block_state() of
    undefined ->  LastBlockState = BlockState;
    LastBlockState -> ok
  end,

  Module = config_utils:module(BlockState),

  % ?debugFmt("~n~p~n", [LastBlockState]),

  % ?debugFmt("~nTest Set: ~p~n", [TestSet]),

  case TestSet of

    {ExpOutputVals} ->
      ExecMethod = input_cos,
      TestConfigVals = [],
      TestInputVals = [],
      {ExecMethod, TestConfigVals, TestInputVals, ExpOutputVals};

    {ExecMethod, ExpOutputVals} when is_atom(ExecMethod) -> 
        TestConfigVals = [],
        TestInputVals = [],
        {ExecMethod, TestConfigVals, TestInputVals, ExpOutputVals};
  
    {TestInputVals, ExpOutputVals} -> 
        ExecMethod = input_cos,
        TestConfigVals = [],
        {ExecMethod, TestConfigVals, TestInputVals, ExpOutputVals};
    
    {ExecMethod, TestInputVals, ExpOutputVals} when is_atom(ExecMethod) ->
        TestConfigVals = [],
        {ExecMethod, TestConfigVals, TestInputVals, ExpOutputVals};
      
    {TestConfigVals, TestInputVals, ExpOutputVals} ->
        ExecMethod = input_cos,
        {ExecMethod, TestConfigVals, TestInputVals, ExpOutputVals};
  
    {ExecMethod, TestConfigVals, TestInputVals, ExpOutputVals} ->
        {ExecMethod, TestConfigVals, TestInputVals, ExpOutputVals}
  end,

   % ?debugFmt("~nExc: ~p, Cfg: ~p, Inp: ~p, Out: ~p~n", [ExecMethod, TestConfigVals, TestInputVals, ExpOutputVals]),

  case TestConfigVals of
    [] -> 
        BlockState1 = LastBlockState;

    TestConfigVals ->
      % By rule, block should be reinitialized after updating config value(s)
      % Clear private values, initialize() recreates private values
      {Configs, Inputs, Outputs, _Private} = set_test_config(LastBlockState, TestConfigVals),
      BlockState1 = Module:initialize({Configs, Inputs, Outputs, []})
  end,

  BlockState2 = set_test_inputs(BlockState1, TestInputVals),

  {_Config2, _Inputs2, Outputs2, _Private2} = BlockState2,
  BlockStatus = output_utils:get_status(Outputs2),

  % Execute the block, to update output values
  case block_common:ok_to_execute(BlockStatus, ExecMethod) of
    true  -> BlockState3 = Module:execute(BlockState2, ExecMethod);
    false -> BlockState3 = BlockState2
  end,

  % Save the BlockState for the next test in this set
  block_state(BlockState3),

  {_Config3, _Inputs3, Outputs3, _Private3} = BlockState3,

  % Create list of current block output values, to compare with expected block output values
  CurrOutputVals = lists:map(fun({ValueId, _ExpValue}) ->
                               {ok, CurrValue} = attrib_utils:get_value(Outputs3, ValueId),
                               {ValueId, CurrValue}
                             end,
                             ExpOutputVals),
  ?debugFmt("~n Exp Values: ~p~nCurr Values: ~p~n", [ExpOutputVals, CurrOutputVals]),
  {CurrOutputVals, ExpOutputVals}.


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
