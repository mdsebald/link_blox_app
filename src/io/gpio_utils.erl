%%% @doc 
%%% GPIO pins interface utility functions 
%%% Currently utilizes erlang_ale library
%%% This indirection allows other I/O libraries to be inserted if needed
%%%               
%%% @end 


-module(gpio_utils).

-author("Mark Sebald").


%% ====================================================================
%% API functions
%% ====================================================================
-export([
          open/2,
          read_bool/1,
          read/1,
          write/2,
          set_edge_mode/2,
          set_pull_mode/2,
          close/1
]).

% TODO: Combine functions for higher level of abstraction
%       Example;  start_link() and set_int() may be combined.
%       Also check if GPIO subsystem exists, before calling start_link().

%
% Replace the real functions with test functions when performing unit Tests
%
-ifndef(TEST).

%%
%% Start GPIO Channel
%%
-spec open(PinNumber :: non_neg_integer(),
           Direction :: input | output) -> {ok, reference()} | {error, atom()}.

open(PinNumber, Direction) -> circuits_gpio:open(PinNumber, Direction).
 

%%
%% Read the value of the gpio pin, return a boolean value
%%
-spec read_bool(GpioPinRef :: reference()) -> true | false.

read_bool(GpioPinRef) ->
  case read(GpioPinRef) of
    1  -> true;
    0 -> false
  end.


%%
%% Read the value of the GPIO pin
%%
-spec read(GpioRef :: reference()) -> 0 | 1 | {error, atom()}.

read(GpioRef) -> circuits_gpio:read(GpioRef).


%%
%% Write the value of the GPIO pin
%%
-spec write(GpioRef :: reference(),
            PinState :: 0 | 1) -> ok | {error, atom()}.

write(GpioRef, PinState) -> circuits_gpio:write(GpioRef, PinState).


%%
%% Set interrupt trigger condition
%%
-spec set_edge_mode(GpioRef :: reference(),
                    Condition :: enabled | summerize | both | rising | falling | none ) -> 
                    'ok' | {'error', atom()}.

set_edge_mode(GpioRef, Condition) -> circuits_gpio:set_edge_mode(GpioRef, Condition).


%%
%% Set internal pull up/down resistor on GPIO pin
%%
-spec set_pull_mode(GpioRef :: reference(),
                    Condition :: not_set | none | pullup | pulldown) -> 
                    'ok' | {'error', atom()}.

set_pull_mode(GpioRef, Condition) -> circuits_gpio:set_pull_mode(GpioRef, Condition).


%%
%% close GPIO Channel
%%
-spec close(GpioRef :: reference()) -> ok. 

close(GpioRef) -> circuits_gpio:close(GpioRef).

-endif.

%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Unit Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%
%% Test open
%%
-spec open(PinNumber :: non_neg_integer(),
           Direction :: input | output) -> {ok, reference()} | {error, atom()}.

open(_PinNumber, _Direction) -> {ok, make_ref()}.


%%
%% Test read the value of the gpio pin, return a boolean value
%%
-spec read_bool(GpioPinRef :: reference()) -> true | false.

read_bool(GpioPinRef) ->
  case read(GpioPinRef) of
    1  -> true;
    0 -> false
  end.


%%
%% Test Read the value of the GPIO pin
%%
-spec read(GpioRef :: reference()) -> 0 | 1 | {error, atom()}.

read(_GpioRef) -> 1.


%%
%% Test write the value of the GPIO pin
%%
-spec write(GpioRef :: reference(), 
            PinState :: 0 | 1) -> ok | {error, atom()}.

write(_GpioRef, _PinState) -> ok.


%%
%% Test set interrupt trigger condition
%%
-spec set_edge_mode(GpioRef :: reference(), 
                    Condition :: enabled | summerize | both | rising | falling | none ) -> 
                    'ok' | {'error', atom()}.

set_edge_mode(_GpioRef, _Condition) -> ok.


%%
%% Test set internal pull up/down resistor on GPIO pin
%%
-spec set_pull_mode(GpioRef :: reference(),
                    Condition :: not_set | none | pullup | pulldown) -> 
                    'ok' | {'error', atom()}.

set_pull_mode(_GpioRef, _Condition) -> ok.


%%
%% Test Close
%%
-spec close(GpioRef :: reference()) -> ok. 

close(_GpioRef) -> ok.


-endif.
