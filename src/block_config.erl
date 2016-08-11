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
    Test7Seg = type_int_to_7seg:create(test_7seg, "Testing Seven Segment Decoder",
                                 [
                                     {num_of_digits, {4}}
                                 ],
                                 [
                                     {disable, {false, ?EMPTY_LINK}},
                                     {exec_interval, {0, ?EMPTY_LINK}},
                                     {input, {empty, {counter, value}}}
                                 ]),
    
    RotEncoder = type_rotary_encoder:create(rotary_encode, "Rotary Encoder with Switch",
                                [],
                                 [
                                     {disable, {false, ?EMPTY_LINK}}
                                 ]),
                                                        
    RmTemp = type_mcp9808_temp:create(room_temp, "Room Temp Sensor",
                                 [],
                                 [
                                     {disable, {false, ?EMPTY_LINK}},
                                     {exec_interval, {1000, ?EMPTY_LINK}}
                                 ]),
                                 
    PbSwitch = type_gpio_di:create(switch_27, "Reset Counter",
                                 [
                                     {gpio_pin, {27}}
                                 ], 
                                 [
                                     {disable, {false, ?EMPTY_LINK}}
                                 ]),

    Display = type_one_digit_7seg:create(display, "Decode count to seven-segments",
                                 [],
                                 [
                                     {disable, {false, ?EMPTY_LINK}},
                                     {segments, {empty, {rotary_encode, value}}}
                                 ]),
                                 
    SegA = type_gpio_do:create(seg_a, "LED Segment A",
                                 [{gpio_pin, {23}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_a}}}]
                                ),

    SegB = type_gpio_do:create(seg_b, "LED Segment B",
                                 [{gpio_pin, {24}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_b}}}]
                                ),

    SegC = type_gpio_do:create(seg_c, "LED Segment C",
                                 [{gpio_pin, {25}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_c}}}]
                                ),

    SegD = type_gpio_do:create(seg_d, "LED Segment D",
                                 [{gpio_pin, {26}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_d}}}]
                                ),

    SegE = type_gpio_do:create(seg_e, "LED Segment E",
                                 [{gpio_pin, {16}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_e}}}]
                                ),
        
    SegF = type_gpio_do:create(seg_f, "LED Segment F",
                                 [{gpio_pin, {22}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_f}}}]
                                ),
        
    SegG = type_gpio_do:create(seg_g, "LED Segment G",
                                 [{gpio_pin, {17}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_g}}}]
                                ),

    Counter = type_exec_count:create(counter, "Count to 3 and rollover",
                                 [
                                    
                                 ],
                                 [
                                     {disable, {false, ?EMPTY_LINK}},
                                     {reset, {empty, {switch_27, value}}},
                                     {exec_interval, {2000, ?EMPTY_LINK}},
                                     {initial_value, {1, ?EMPTY_LINK}},
                                     {final_value, {3, ?EMPTY_LINK}}
                                 ]),

    EnvSensor = type_bme280_env_sensor:create(env_sensor, "Temp, Press, Humid Sensor",
                                 [

                                 ], 
                                 [
                                    {disable, {false, ?EMPTY_LINK}},
                                    {exec_interval, {1000, ?EMPTY_LINK}}
                                ]),

   SelectVal = type_n_select:create(select_sensor, "Select Temp, Press, or Humid Sensor",
                                [

                                ],
                                [
                                  {disable, {false, ?EMPTY_LINK}},
                                  {select, {empty, {counter, value}}}, 
                                  {inputs, [{empty, {env_sensor, temp}}, {empty, {env_sensor, press}}, {empty, {env_sensor, humid}}]}
                                ]),

    Decoder = type_float_to_7seg:create(decoder, "Decode Number to 7 Segment Digits",
                                [

                                ], 
                                [
                                  {disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {select_sensor, value}}}
                                ]),

    LedDisp = type_ht16k33_4digit_led:create(led_disp, "4 Digit LED Display",
                                 [

                                 ],
                                 [
                                    {disable, {false, ?EMPTY_LINK}},
                                    {segs_digit_1, {empty, {decoder, {digit,1}}}},
                                    {segs_digit_2, {empty, {decoder, {digit,2}}}},
                                    {segs_digit_3, {empty, {decoder, {digit,3}}}},
                                    {segs_digit_4, {empty, {decoder, {digit,4}}}},
                                    {colon, {false, ?EMPTY_LINK}}
                                 ]),

    [Test7Seg, RotEncoder, RmTemp, PbSwitch, Display, SegA, SegB, SegC, SegD, SegE, SegF, SegG, 
     Counter, EnvSensor, SelectVal, Decoder, LedDisp].                         



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
