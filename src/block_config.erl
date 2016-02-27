%% @author Mark
%% @doc Create initial block configuration


-module(block_config).

%% ====================================================================
%% API functions
%% ====================================================================
-export([read_config/1, write_config/2, create_demo_config/0]).

create_demo_config() ->
  
    Led17DigitalOutput = block_pi1_gpio_digital_output:create(led_17, [{gpio_pin, 17}, {invert_output, true}], 
                                [{input, empty, {value, toggle_led, null}}]),
                                    
    PbSwDigitalOutput = block_pi1_gpio_digital_output:create(led_22, [{gpio_pin, 22}], 
                                [{input, empty, {value, switch_27, null}}]),
                                
    PbSwDigitalInput = block_pi1_gpio_digital_input:create(switch_27, [{gpio_pin, 27}], []),
   
    ToggleBlockValues = block_toggle:create(toggle_led, [], [{execute_interval, 250, {fixed, null, null}}]),
    
    [ToggleBlockValues, Led17DigitalOutput, PbSwDigitalOutput, PbSwDigitalInput].


%
%create_demo1_config() ->
%	BlockValues1 = blkpnt_inverter:create_values('Inverter1'),
%	BlockValues1a = blkpnt_utils:set_input_pointer(BlockValues1, 'InputVal', {'Value', 'Priority1', null}),
%	
%	BlockValues2 = blkpnt_inverter:create_values('Inverter2'),
%	BlockValues2a = blkpnt_utils:set_input_pointer(BlockValues2, 'InputVal', {'Value', 'Inverter1', null}),
%
%	BlockValues3 = blkpnt_inverter:create_values('Inverter3'),
%	BlockValues3a = blkpnt_utils:set_input_pointer(BlockValues3, 'InputVal', {'Value', 'Inverter2', null}),
%
%	BlockValues4 = blkpnt_priority:create_values('Priority1'),
%	BlockValues4a = blkpnt_utils:set_input_pointer(BlockValues4, 'InputVal1', {'Value', 'Inverter3', null}),
%	BlockValues4b = blkpnt_utils:set_input_pointer(BlockValues4a, 'InputVal2', {fixed, null, null}),
%	BlockValues4c = blkpnt_utils:set_value(BlockValues4b, 'InputVal2', true),
%	
%	BlockValues5 = blkpnt_type_gpio_digital_output:create_values('Red_LED_17'),
%	
%	BlockValuesList = [BlockValues1a, BlockValues2a, BlockValues3a, BlockValues4c, BlockValues5],
%	
%	write_config("/vagrant/BlockPoint/TestConfig.bpt", BlockValuesList).

%% Read a set of block values from a config file
% TODO: Check for existence and validity
read_config(FileName) ->
	file:consult(FileName).
	
	
%% Write the block values to a configuration file
% TODO:  Add BlockPoint specific header?
write_config(FileName, BlockValuesList) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = lists:map(Format, BlockValuesList),
    file:write_file(FileName, Text).
