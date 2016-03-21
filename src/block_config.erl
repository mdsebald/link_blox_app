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
 
    PbSwitch = lblx_pi_gpio_di:create(switch_27, 
                                 [
                                     {gpio_pin, 27}
                                 ], 
                                 [
                                     {disable, false, ?EMPTY_LINK}
                                 ]),

    Counter = lblx_exec_count:create(counter,
                                 [
                                    
                                 ],
                                 [
                                     {disable, false, ?EMPTY_LINK},
                                     {reset, empty, {value, switch_27, null}},
                                     {exec_interval, 1000, ?EMPTY_LINK}
                                 ]),
 
    Display = lblx_seven_seg:create(display,
                                 [],
                                 [
                                     {disable, false, ?EMPTY_LINK},
                                     {input, empty, {value, counter, null}}
                                 ]),
                                 
    SegA = lblx_pi_gpio_do:create(seg_a, 
                                 [{gpio_pin, 17}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {seg_a, display, null}}]
                                ),

    SegB = lblx_pi_gpio_do:create(seg_b, 
                                 [{gpio_pin, 23}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {seg_b, display, null}}]
                                ),

    SegC = lblx_pi_gpio_do:create(seg_c, 
                                 [{gpio_pin, 25}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {seg_c, display, null}}]
                                ),

    SegD = lblx_pi_gpio_do:create(seg_d, 
                                 [{gpio_pin, 16}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {seg_d, display, null}}]
                                ),

    SegE = lblx_pi_gpio_do:create(seg_e, 
                                 [{gpio_pin, 26}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {seg_e, display, null}}]
                                ),
        
    SegF = lblx_pi_gpio_do:create(seg_f, 
                                 [{gpio_pin, 22}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {seg_f, display, null}}]
                                ),
        
    SegG = lblx_pi_gpio_do:create(seg_g, 
                                 [{gpio_pin, 24}, {invert_output, true}], 
                                 [{disable, false, ?EMPTY_LINK},
                                  {input, empty, {seg_g, display, null}}]
                                ),

    [PbSwitch, Counter, Display, SegA, SegB, SegC, SegD, SegE, SegF, SegG].                         

create_demo_config1() ->
  
    Led17DigitalOutput = lblx_pi_gpio_do:create(led_17, [{gpio_pin, 17}, {invert_output, true}], 
                                [{disable, false, ?EMPTY_LINK}, {input, empty, {value, toggle_led, null}}]),
                                    
    PbSwDigitalOutput = lblx_pi_gpio_do:create(led_22, [{gpio_pin, 22}, {invert_output, true}], 
                                [{disable, false, ?EMPTY_LINK}, {input, empty, {value, switch_27, null}}]),
                                
    PbSwDigitalInput = lblx_pi_gpio_di:create(switch_27, [{gpio_pin, 27}], [{disable, false, ?EMPTY_LINK}]),
   
    ToggleBlockValues = lblx_toggle:create(toggle_led, [], [{disable, false, ?EMPTY_LINK},{exec_interval, 2000, ?EMPTY_LINK}]),
    
    Led26DigitalOutput = lblx_pi_gpio_do:create(led_26, 
                                [
                                    {gpio_pin, 26},
                                    {invert_output, true}
                                ], 
                                [
                                    {disable, false, ?EMPTY_LINK},
                                    {input, empty, {value, switch_27, null}}, 
                                    {exec_in, empty, {exec_out, toggle_led, null}}
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
