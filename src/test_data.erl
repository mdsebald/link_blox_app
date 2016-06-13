%%% @doc 
%%% Unit Test Data   
%%%               
%%% @end 

-module(test_data).

-author("Mark Sebald").

-ifdef(TEST).
-include_lib("block_state.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([attrib_utils_config_attribs1/0,
         attrib_utils_config_attribs2/0,
         attrib_utils_config_attribs3/0,
         attrib_utils_input_attribs1/0,
         config_utils_config_attribs1/0,
         input_utils_config_attribs1/0,
         input_utils_input_attribs1/0,
         input_utils_input_attribs2/0,
         output_utils_config_attribs1/0,
         output_utils_input_attribs1/0,
         output_utils_input_attribs2/0,
         output_utils_output_attribs1/0,
         output_utils_output_attribs2/0,
         output_utils_output_attribs3/0,
         output_utils_output_attribs4/0
]).

% ====================================================================
% Test data
%
attrib_utils_config_attribs1() ->
  [ {block_name, {test_config}},
    {block_module, {type_template}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}},
    {number1, {123.45}},
    {string1, {"Testing"}},
    {bool_array, [{true}, {false}]},
    {integer1, {123}},
    {integer_array, [{234}, {456}, {-123}]}
  ].
  
attrib_utils_config_attribs2() ->
  [ {block_name, {test_config}},
    {block_module, {type_template}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}},
    {number1, {123.45}},
    {string1, {"Testing"}},
    {bool_array, [{true}, {false}, {true}, {true}, {true}, {true}, {true}, {true}, {true}, {true}]},
    {integer1, {123}},
    {integer_array, [{234}, {456}, {-123}]}
  ].
  
attrib_utils_config_attribs3() ->
  [ {block_name, {test_config}},
    {block_module, {type_template}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}},
    {number1, {123.45}},
    {string1, {"Testing"}},
    {bool_array, [{true}, {false}]},
    {integer1, {123}},
    {integer_array, [{234}]}
  ].
  
attrib_utils_input_attribs1() ->
  [ {block_name, {test_config}},
    {block_module, {type_template}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}},
    {number_in, {123.45, {}}},
    {string_in, {"Testing", {}}},
    {bool_array_in, [{true,{}}, {false,{}}]},
    {integer_in, {123}},
    {integer_array_in, [{234,{}}, {456,{}}, {-123,{}}]}
  ].

config_utils_config_attribs1() ->
  [ {block_name, {test_config_utils}},
    {block_module, {type_test}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}},
    {number1, {123.45}},
    {string1, {"Testing"}},
    {bool_array, [{true}, {false}]},
    {integer1, {123}},
    {integer_array, [{234}, {456}, {-123}]},
    {float_good, {123.45}},
    {float_bad, {xyz}},
    {integer_good, {12345}},
    {integer_bad, {"bad"}},
    {boolean_good, {true}},
    {boolean_bad, {0.0}},
    {not_active_good, {not_active}},
    {empty_good, {empty}},
    {empty_bad, {empty, {knot, empty, link}}},
    {not_config, {123, [test1,test2]}}
  ].

input_utils_config_attribs1() ->
  [ {block_name, {test_input_utils}},
    {block_module, {type_test}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}}
  ].

input_utils_input_attribs1() ->
  [ {disable, {true, ?EMPTY_LINK}},
    {freeze, {false, ?EMPTY_LINK}},
    {exec_in, {empty, ?EMPTY_LINK}},
    {exec_interval, {0, ?EMPTY_LINK}},
    {float_good, {123.45, {null, block1, value}}},
    {float_bad, {xyz, ?EMPTY_LINK}},
    {integer_good, {12345, {null, block2, value}}},
    {integer_bad, {"bad", ?EMPTY_LINK}},
    {boolean_good, {true, ?EMPTY_LINK}},
    {boolean_bad, {0.0, ?EMPTY_LINK}},
    {not_active_good, {not_active, ?EMPTY_LINK}},
    {empty_good, {empty, ?EMPTY_LINK}},
    {empty_bad, {empty, {knot, empty, link}}},
    {not_input, {123, [test1,test2]}},
    {integer_array, [{123, {}}, {789, {null, test_block, test_output}}]}
  ].
  
input_utils_input_attribs2() ->
  InputList = input_utils_input_attribs1(),
  ModifiedAttribute = {integer_array, 
                       [{123, {}}, 
                        {789, {null, test_block, test_output}},
                        {empty, ?EMPTY_LINK},
                        {empty, ?EMPTY_LINK}]},
   attrib_utils:replace_attribute(InputList, integer_array, 
                            ModifiedAttribute).


output_utils_config_attribs1() ->
  [ {block_name, {test_input_utils}},
    {block_module, {type_test}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}}
  ].

output_utils_input_attribs1() ->
  [ {disable, {true, ?EMPTY_LINK}},
    {freeze, {false, ?EMPTY_LINK}},
    {exec_in, {empty, ?EMPTY_LINK}},
    {exec_interval, {0, ?EMPTY_LINK}},
    {float_good, {123.45, {null, block1, value}}},
    {float_bad, {xyz, ?EMPTY_LINK}},
    {integer_good, {12345, {null, block2, value}}},
    {integer_bad, {"bad", ?EMPTY_LINK}},
    {boolean_good, {true, ?EMPTY_LINK}},
    {boolean_bad, {0.0, ?EMPTY_LINK}},
    {not_active_good, {not_active, ?EMPTY_LINK}},
    {empty_good, {empty, ?EMPTY_LINK}},
    {empty_bad, {empty, {knot, empty, link}}},
    {not_input, {123, [test1,test2]}},
    {integer_array, [{123, {}}, {789, {null, test_block, test_output}}]}
  ].
  
output_utils_input_attribs2() ->
  InputList = output_utils_input_attribs1(),
  ModifiedAttribute = {integer_array, 
                       [{123, {}}, 
                        {789, {null, test_block, test_output}},
                        {empty, ?EMPTY_LINK},
                        {empty, ?EMPTY_LINK}]},
   attrib_utils:replace_attribute(InputList, integer_array, 
                            ModifiedAttribute).
output_utils_output_attribs1() ->
  [
    {exec_out, {false, []}},                         
    {status, {created, []}},     
    {exec_method, {empty, []}},
    {last_exec, {empty, []}},
    {value, {not_active, []}},
    {integer_array_out, [{0, []}, {1,[]}, {2, []}]}
  ].
 
  output_utils_output_attribs2() ->
    OutputList = output_utils_output_attribs1(),
    ModifiedAttribute = {integer_array_out, 
    [{0, []}, {1,[]}, {2, []}, {not_active, []}, {not_active, []}, {not_active, []}]},
    
    attrib_utils:replace_attribute(OutputList, integer_array_out, 
                            ModifiedAttribute).
 
 output_utils_output_attribs3() ->
    OutputList = output_utils_output_attribs1(),
    ModifiedAttribute = {integer_array_out, 
    [{6, []}, {7,[]}, {8, []}]},
    
    attrib_utils:replace_attribute(OutputList, integer_array_out, 
                            ModifiedAttribute).
                            
output_utils_output_attribs4() ->
  [
    {exec_out, {not_active, []}},                         
    {status, {input_err, []}},     
    {exec_method, {not_active, []}},
    {last_exec, {not_active, []}},
    {value, {not_active, []}},
    {integer_array_out, [{not_active, []}, {not_active,[]}, {not_active, []}]}
  ].
  
-endif.