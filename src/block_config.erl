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
 
    PbSwitch = lblx_pi_gpio_di:create(switch_27, "Reset Counter",
                                 [
                                     {gpio_pin, 27}
                                 ], 
                                 [
                                     {disable, false, ?EMPTY_LINK}
                                 ]),

    Counter = lblx_exec_count:create(counter, "Count to 9 and rollover",
                                 [
                                    
                                 ],
                                 [
                                     {disable, false, ?EMPTY_LINK},
                                     {reset, empty, {null, switch_27, value}},
                                     {exec_interval, 1000, ?EMPTY_LINK}
                                 ]),
 
    Display = lblx_seven_seg:create(display, "Decode count to seven-segments",
                                 [],
                                 [
                                     {disable, false, ?EMPTY_LINK},
                                     {input, empty, {null, counter, value}}
                                 ]),
                                 
    SegA = lblx_pi_gpio_do:create(seg_a, "LED Segment A",
                                 [{gpio_pin, 17}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {null, display, seg_a}}]
                                ),

    SegB = lblx_pi_gpio_do:create(seg_b, "LED Segment B",
                                 [{gpio_pin, 23}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {null, display, seg_b}}]
                                ),

    SegC = lblx_pi_gpio_do:create(seg_c, "LED Segment C",
                                 [{gpio_pin, 25}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {null, display, seg_c}}]
                                ),

    SegD = lblx_pi_gpio_do:create(seg_d, "LED Segment D",
                                 [{gpio_pin, 16}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {null, display, seg_d}}]
                                ),

    SegE = lblx_pi_gpio_do:create(seg_e, "LED Segment E",
                                 [{gpio_pin, 26}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {null, display, seg_e}}]
                                ),
        
    SegF = lblx_pi_gpio_do:create(seg_f, "LED Segment F",
                                 [{gpio_pin, 22}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {null, display, seg_f}}]
                                ),
        
    SegG = lblx_pi_gpio_do:create(seg_g, "LED Segment G",
                                 [{gpio_pin, 24}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {null, display, seg_g}}]
                                ),

    [PbSwitch, Counter, Display, SegA, SegB, SegC, SegD, SegE, SegF, SegG].                         

create_demo_config1() ->
  
    Led17DigitalOutput = lblx_pi_gpio_do:create(led_17, "", [{gpio_pin, 17}, {invert_output, true}], 
                                [{disable, false, ?EMPTY_LINK}, {input, empty, {null, toggle_led, value}}]),
                                    
    PbSwDigitalOutput = lblx_pi_gpio_do:create(led_22, "", [{gpio_pin, 22}, {invert_output, true}], 
                                [{disable, false, ?EMPTY_LINK}, {input, empty, {null, switch_27, value}}]),
                                
    PbSwDigitalInput = lblx_pi_gpio_di:create(switch_27, "", [{gpio_pin, 27}], [{disable, false, ?EMPTY_LINK}]),
   
    ToggleBlockValues = lblx_toggle:create(toggle_led, "", [], [{disable, false, ?EMPTY_LINK},{exec_interval, 2000, ?EMPTY_LINK}]),
    
    Led26DigitalOutput = lblx_pi_gpio_do:create(led_26, "LED",
                                [
                                    {gpio_pin, 26},
                                    {invert_output, true}
                                ], 
                                [
                                    {disable, false, ?EMPTY_LINK},
                                    {input, empty, {null, switch_27, value}}, 
                                    {exec_in, empty, {null, toggle_led, exec_out}}
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
