%%%
%%%  @doc 
%%% 
%%%  Create initial block configuration
%%%
%%%  @ndd


-module(block_config).

-author("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([read_config/1, write_config/2, create_demo_config/0]).

 
 
 create_demo_config() ->
 
    PbSwDigitalInput = block_pi1_gpio_digital_input:create(switch_27, [{gpio_pin, 27}], []),

    TimedCount = block_exec_counter:create(timed_count,
                                 [
                                     {execute_interval, 1000}
                                 ],
                                 [
                                     {reset, empty, {value, switch_27, null}}
                                 ]),
 
    DispCount = block_7_segment:create(disp_count,
                                 [],
                                 [
                                    {input, empty, {value, timed_count, null}}
                                 ]),
                                 
    SegA = block_pi1_gpio_digital_output:create(seg_a, 
                                 [{gpio_pin, 17}, {invert_output, true}], 
                                 [{input, empty, {seg_a, disp_count, null}}]
                                ),

    SegB = block_pi1_gpio_digital_output:create(seg_b, 
                                 [{gpio_pin, 23}, {invert_output, true}], 
                                 [{input, empty, {seg_b, disp_count, null}}]
                                ),

    SegC = block_pi1_gpio_digital_output:create(seg_c, 
                                 [{gpio_pin, 25}, {invert_output, true}], 
                                 [{input, empty, {seg_c, disp_count, null}}]
                                ),

    SegD = block_pi1_gpio_digital_output:create(seg_d, 
                                 [{gpio_pin, 16}, {invert_output, true}], 
                                 [{input, empty, {seg_d, disp_count, null}}]
                                ),

    SegE = block_pi1_gpio_digital_output:create(seg_e, 
                                 [{gpio_pin, 26}, {invert_output, true}], 
                                 [{input, empty, {seg_e, disp_count, null}}]
                                ),
        
    SegF = block_pi1_gpio_digital_output:create(seg_f, 
                                 [{gpio_pin, 22}, {invert_output, true}], 
                                 [{input, empty, {seg_f, disp_count, null}}]
                                ),
        
    SegG = block_pi1_gpio_digital_output:create(seg_g, 
                                 [{gpio_pin, 24}, {invert_output, true}], 
                                 [{input, empty, {seg_g, disp_count, null}}]
                                ),

    [PbSwDigitalInput, TimedCount, DispCount, SegA, SegB, SegC, SegD, SegE, SegF, SegG].                         

create_demo_config1() ->
  
    Led17DigitalOutput = block_pi1_gpio_digital_output:create(led_17, [{gpio_pin, 17}, {invert_output, true}], 
                                [{input, empty, {value, toggle_led, null}}]),
                                    
    PbSwDigitalOutput = block_pi1_gpio_digital_output:create(led_22, [{gpio_pin, 22}, {invert_output, true}], 
                                [{input, empty, {value, switch_27, null}}]),
                                
    PbSwDigitalInput = block_pi1_gpio_digital_input:create(switch_27, [{gpio_pin, 27}], []),
   
    ToggleBlockValues = block_toggle:create(toggle_led, [{execute_interval, 2000}], []),
    
    Led26DigitalOutput = block_pi1_gpio_digital_output:create(led_26, 
                                [
                                    {gpio_pin, 26},
                                    {invert_output, true}
                                ], 
                                [
                                    {input, empty, {value, switch_27, null}}, 
                                    {execute_in, empty, {execute_out, toggle_led, null}}
                                ]),
                                
    [ToggleBlockValues, Led17DigitalOutput, PbSwDigitalOutput, PbSwDigitalInput, Led26DigitalOutput].


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
