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
          start_link/2,
          register_int/1,
          set_int/2,
          read/1,
          write/2,
          stop/1
]).

% TODO: Combine functions for higher level of abstraction
%       Example;  start_link(), register_int(), and set_int() may be combined.
%       Also check if GPIO subsystem exists, before calling start_link().

%
% Replace the real functions with test functions when performing unit Tests
%
-ifndef(TEST).

%%
%% Start GPIO Channel
%%
-spec start_link(PinNumber :: non_neg_integer(),
                 Direction :: input | output) -> {ok, pid()} | {error, atom()}.

start_link(PinNumber, Direction) -> gpio:start_link(PinNumber, Direction).
 

%%
%% Register a channel to receive interrupts
%%
-spec register_int(GpioRef :: pid()) -> ok | {error, atom()}.

register_int(GpioRef) -> gpio:register_int(GpioRef).


%%
%% Set interrupt trigger condition
%%
-spec set_int(GpioRef :: pid(),
              Condition :: enabled | summerize | both | rising | falling | none ) -> 
                    'ok' | {'error', atom()}.

set_int(GpioRef, Condition) -> gpio:set_int(GpioRef, Condition).


%%
%% Read the value of the GPIO pin
%%
-spec read(GpioRef :: pid()) -> 0 | 1 | {error, atom()}.

read(GpioRef) -> gpio:read(GpioRef).


%%
%% Write the value of the GPIO pin
%%
-spec write(GpioRef :: pid(),
            PinState :: 0 | 1) -> ok | {error, atom()}.

write(GpioRef, PinState) -> gpio:write(GpioRef, PinState).


%%
%% Stop GPIO Channel
%%
-spec stop(GpioRef :: pid()) -> ok. 

stop(GpioRef) ->gpio:stop(GpioRef).

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
%% Test Start link
%%
-spec start_link(PinNumber :: string(), 
                 Direction :: non_neg_integer()) -> {ok, pid()} | {error, atom()}.

start_link(_PinNumber, _Direction) -> {ok, make_ref()}.


%%
%% Test Register a channel to receive interrupts
%%
-spec register_int(GpioRef :: pid()) -> ok | {error, atom()}.

register_int(_GpioRef) -> ok.


%%
%% Test Set interrupt trigger condition
%%
-spec set_int(GpioRef :: pid(), 
              Condition :: enabled | summerize | both | rising | falling | none ) -> 
                    'ok' | {'error', atom()}.

set_int(_GpioRef, _Condition) -> ok.


%%
%% Test Read the value of the GPIO pin
%%
-spec read(GpioRef :: pid()) -> 0 | 1 | {error, atom()}.

read(_GpioRef) -> 1.


%%
%% Test Write the value of the GPIO pin
%%
-spec write(GpioRef :: pid(), 
            PinState :: 0 | 1) -> ok | {error, atom()}.

write(_GpioRef, _PinState) -> ok.


%%
%% Test Stop
%%
-spec stop(GpioRef :: pid()) -> ok. 

stop(_GpioRef) -> ok.


-endif.
