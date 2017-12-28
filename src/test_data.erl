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
         output_attribs1/0,
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
  [ {number_in, {123.45, {empty}}},
    {string_in, {"Testing", {empty}}},
    {bool_array_in, [{true,{empty}}, {false, {empty}}]},
    {integer_in, {123, {0}}},
    {integer_array_in, [{234,{0}}, {456,{0}}, {-123,{0}}]}
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
    {not_active_good, {null}},
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
  [ {disable, {true, {true}}},
    {freeze, {false, {false}}},
    {exec_in, {[], {[]}}},
    {exec_interval, {0, {0}}},
    {float_good, {123.45, {123.45}}},
    {float_bad, {xyz, {xyz}}},
    {integer_good, {12345, {12345}}},
    {integer_bad, {"bad", {"bad"}}},
    {boolean_good, {true, {true}}},
    {boolean_bad, {0.0, {0.0}}},
    {not_active_good, {null, {empty}}},
    {empty_good, {empty, {empty}}},
    {empty_bad, {empty, {empty}}},
    {not_input, {123, [test1,test2]}},
    {integer_array, [{123, {0}}, {789, {0}}]}
  ].
  
input_utils_input_attribs2() ->
  InputList = input_utils_input_attribs1(),
  ModifiedAttribute = {integer_array, 
                       [{123, {0}}, 
                        {789, {0}},
                        {empty, {empty}},
                        {empty, {empty}}]},
   attrib_utils:replace_attribute(InputList, integer_array, 
                            ModifiedAttribute).


output_utils_config_attribs1() ->
  [ {block_name, {test_input_utils}},
    {block_module, {type_test}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}}
  ].

output_utils_input_attribs1() ->
  [ {disable, {true, {true}}},
    {freeze, {false, {false}}},
    {exec_in, {[], {[]}}},
    {exec_interval, {0, {0}}},
    {float_good, {123.45, {0.0}}},
    {float_bad, {xyz, {0.0}}},
    {integer_good, {12345, {0}}},
    {integer_bad, {"bad", {"bad"}}},
    {boolean_good, {true, {true}}},
    {boolean_bad, {0.0, {false}}},
    {not_active_good, {null, {empty}}},
    {empty_good, {empty, {empty}}},
    {empty_bad, {empty, {empty}}},
    {not_input, {123, 123}},
    {integer_array, [{123, {0}}, {789, {0}}]}].
  
output_utils_input_attribs2() ->
  InputList = output_utils_input_attribs1(),
  ModifiedAttribute = {integer_array, 
                       [{123, {}}, 
                        {789, {null, test_block, test_output}},
                        {empty, {empty}},
                        {empty, {empty}}]},
   attrib_utils:replace_attribute(InputList, integer_array, 
                            ModifiedAttribute).

output_attribs1() ->
  [
    {exec_out, {false, []}},                         
    {status, {created, []}},     
    {exec_method, {empty, []}},
    {last_exec, {empty, []}},
    {value, {null, []}},
    {integer_array_out, [{0, []}, {1,[]}, {2, []}]}
  ].
 
  output_utils_output_attribs2() ->
    OutputList = output_attribs1(),
    ModifiedAttribute = {integer_array_out, 
    [{0, []}, {1,[]}, {2, []}, {null, []}, {null, []}, {null, []}]},
    
    attrib_utils:replace_attribute(OutputList, integer_array_out, 
                            ModifiedAttribute).
 
 output_utils_output_attribs3() ->
    OutputList = output_attribs1(),
    ModifiedAttribute = {integer_array_out, 
    [{6, []}, {7,[]}, {8, []}]},
    
    attrib_utils:replace_attribute(OutputList, integer_array_out, 
                            ModifiedAttribute).
                            
output_utils_output_attribs4() ->
  [
    {exec_out, {null, []}},                         
    {status, {input_err, []}},     
    {exec_method, {null, []}},
    {last_exec, {null, []}},
    {value, {null, []}},
    {integer_array_out, [{null, []}, {null,[]}, {null, []}]}
  ].
  
-endif.