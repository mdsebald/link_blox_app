%%%
%%%  @doc 
%%% 
%%%  Create a demo configuration in code instead of a config file
%%%
%%%  @ndd


-module(demo_config).

-author("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([create_demo_config/0]).

% Load this demo configuration, in case default LinkBloxConfig file is not available.
create_demo_config() ->

  GreenLed = lblx_rpi_led:create(green_led, "On-Board Green LED",
                                 [],  % Default Config values are good 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {toggle, value}}}]
                                ),
  Toggle = lblx_toggle:create(toggle, "Toggle LED on / off",
                              [],  % Default Config values are good 
                              [{disable, {false, ?EMPTY_LINK}},
                               {exec_interval, {250, ?EMPTY_LINK}}]
                               ),
  [GreenLed, Toggle].


 % Don't use this demo_config, relies too much on I2C harware.
 create_demo_config1() ->
    Test7Seg = lblx_int_to_7seg:create(test_7seg, "Testing Seven Segment Decoder",
                                 [
                                     {num_of_digits, {4}}
                                 ],
                                 [
                                     {disable, {false, ?EMPTY_LINK}},
                                     {exec_interval, {0, ?EMPTY_LINK}},
                                     {input, {empty, {counter, value}}}
                                 ]),
    
    RotEncoder = lblx_rotary_encoder:create(rotary_encode, "Rotary Encoder with Switch",
                                [],
                                 [
                                     {disable, {false, ?EMPTY_LINK}}
                                 ]),
                                                        
    RmTemp = lblx_mcp9808:create(room_temp, "Room Temp Sensor",
                                 [],
                                 [
                                     {disable, {false, ?EMPTY_LINK}},
                                     {exec_interval, {1000, ?EMPTY_LINK}}
                                 ]),
                                 
    PbSwitch = lblx_gpio_di:create(switch_27, "Reset Counter",
                                 [
                                     {gpio_pin, {27}}
                                 ], 
                                 [
                                     {disable, {false, ?EMPTY_LINK}}
                                 ]),

    Display = lblx_one_digit_7seg:create(display, "Decode count to seven-segments",
                                 [],
                                 [
                                     {disable, {false, ?EMPTY_LINK}},
                                     {segments, {empty, {rotary_encode, value}}}
                                 ]),
                                 
    SegA = lblx_gpio_do:create(seg_a, "LED Segment A",
                                 [{gpio_pin, {23}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_a}}}]
                                ),

    SegB = lblx_gpio_do:create(seg_b, "LED Segment B",
                                 [{gpio_pin, {24}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_b}}}]
                                ),

    SegC = lblx_gpio_do:create(seg_c, "LED Segment C",
                                 [{gpio_pin, {25}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_c}}}]
                                ),

    SegD = lblx_gpio_do:create(seg_d, "LED Segment D",
                                 [{gpio_pin, {26}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_d}}}]
                                ),

    SegE = lblx_gpio_do:create(seg_e, "LED Segment E",
                                 [{gpio_pin, {16}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_e}}}]
                                ),
        
    SegF = lblx_gpio_do:create(seg_f, "LED Segment F",
                                 [{gpio_pin, {22}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_f}}}]
                                ),
        
    SegG = lblx_gpio_do:create(seg_g, "LED Segment G",
                                 [{gpio_pin, {17}}, {invert_output, {false}}], 
                                 [{disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {display, seg_g}}}]
                                ),

    Counter = lblx_exec_count:create(counter, "Count to 3 and rollover",
                                 [
                                    
                                 ],
                                 [
                                     {disable, {false, ?EMPTY_LINK}},
                                     {reset, {empty, {switch_27, value}}},
                                     {exec_interval, {2000, ?EMPTY_LINK}},
                                     {initial_value, {1, ?EMPTY_LINK}},
                                     {final_value, {3, ?EMPTY_LINK}}
                                 ]),

    EnvSensor = lblx_bme280:create(env_sensor, "Temp, Press, Humid Sensor",
                                 [

                                 ], 
                                 [
                                    {disable, {false, ?EMPTY_LINK}},
                                    {exec_interval, {1000, ?EMPTY_LINK}}
                                ]),

   SelectVal = lblx_n_select:create(select_sensor, "Select Temp, Press, or Humid Sensor",
                                [

                                ],
                                [
                                  {disable, {false, ?EMPTY_LINK}},
                                  {select, {empty, {counter, value}}}, 
                                  {inputs, [{empty, {env_sensor, temp}}, {empty, {env_sensor, press}}, {empty, {env_sensor, humid}}]}
                                ]),

    Decoder = lblx_float_to_7seg:create(decoder, "Decode Number to 7 Segment Digits",
                                [

                                ], 
                                [
                                  {disable, {false, ?EMPTY_LINK}},
                                  {input, {empty, {select_sensor, value}}}
                                ]),

    LedDisp = lblx_ht16k33:create(led_disp, "4 Digit LED Display",
                                 [

                                 ],
                                 [
                                    {disable, {false, ?EMPTY_LINK}},
                                    {digit_segs_1, {empty, {decoder, {digits,1}}}},
                                    {digit_segs_2, {empty, {decoder, {digits,2}}}},
                                    {digit_segs_3, {empty, {decoder, {digits,3}}}},
                                    {digit_segs_4, {empty, {decoder, {digits,4}}}},
                                    {colon, {false, ?EMPTY_LINK}}
                                 ]),

    [Test7Seg, RotEncoder, RmTemp, PbSwitch, Display, SegA, SegB, SegC, SegD, SegE, SegF, SegG, 
     Counter, EnvSensor, SelectVal, Decoder, LedDisp].                         

