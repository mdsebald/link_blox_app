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
  block_io_test_utils:block_setup(?MODULE).

cleanup(BlockState) ->
  block_io_test_utils:block_cleanup(BlockState).

test_io(BlockState) ->
  block_io_test_utils:create_io_tests(BlockState, test_sets()).