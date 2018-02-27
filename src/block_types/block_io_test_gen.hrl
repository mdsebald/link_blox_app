%%% @doc 
%%% Unit Test Generator, for block type code.
%%%
%%% We put this in an include file, so we don't have to copy this to every block type code module
%%% This Generates a set of unit tests.
%%%  test_sets() is defined in the block type code module Test section.
%%%  It is a list of tuples where each tuple defines a set of config values, inputs, and expected output values
%%% @end 

block_io_test_() ->
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
  unit_test_utils:block_setup(?MODULE).

cleanup(BlockState) ->
  unit_test_utils:block_cleanup(?MODULE, BlockState).

test_io(BlockState) ->
  unit_test_utils:create_io_tests(?MODULE, BlockState, test_sets()).