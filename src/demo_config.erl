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
                                 [{disable, {false, {true}}},
                                  {input, {true, {false}}},
                                  {trigger, {"heartbeat", {"none"}}}]  % Just setup the LED with "fastblink" mode
                                ),

  [GreenLed].


             

