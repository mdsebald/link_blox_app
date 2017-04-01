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
          start_link_mult/2,
          register_int/0,
          register_int/1,
          set_int/1,
          set_int/2,
          read/0,
          read/1,
          write/1,
          write/2,
          stop/0,
          stop/1
]).



%
% Replace the real functions with test functions when performing unit Tests
%
-ifndef(TEST).

%%
%% Start GPIO Channel
%%
-spec start_link(PinNumber :: non_neg_integer(),
                 Direction :: input | output) -> {ok, pid()} | {error, atom()}.

start_link(PinNumber, Direction) ->
  case gpio:start_link(PinNumber, Direction) of
    {ok, GpioRef} ->
      gpio_ref(GpioRef),
      {ok, GpioRef};

    {error, Reason} ->
      gpio_ref(undefined),
      {error, Reason}
  end.

%% If a block uses multiple GPIO pins, need to track GpioRef's separately, 
%% Don't use process dictionary to store GpioRef's
-spec start_link_mult(PinNumber :: non_neg_integer(),
                      Direction :: input | output) -> {ok, pid()} | {error, atom()}.

start_link(PinNumber, Direction) -> gpio:start_link(PinNumber, Direction).
 

%%
%% Register a channel to receive interrupts
%%
-spec register_int() -> ok | {error, atom()}.

register_int() ->
  case gpio_ref() of
    undefined -> {error, undefined};
  
    GpioRef ->
      gpio:register_int(GpioRef)
  end.

%% For blocks that manipulate multiple GPIO pins, don't use GpioRef from process dictionary
-spec register_int(GpioRef :: pid()) -> ok | {error, atom()}.

register_int(GpioRef) -> gpio:register_int(GpioRef).


%%
%% Set interrupt trigger condition
%%
-spec set_int(Condition :: enabled | summerize | both | rising | falling | none ) -> 
                    'ok' | {'error', atom()}.

set_int(Condition) ->
  case gpio_ref() of 
    undefined -> {error, undefined};
  
    GpioRef ->
      gpio:set_int(GpioRef, Condition)
  end.

%% For blocks that manipulate multiple GPIO pins, don't use GpioRef from process dictionary
-spec set_int(GpioRef :: pid(),
              Condition :: enabled | summerize | both | rising | falling | none ) -> 
                    'ok' | {'error', atom()}.

set_int(GpioRef, Condition) -> gpio:set_int(GpioRef, Condition).


%%
%% Read the value of the GPIO pin
%%
-spec read() -> 0 | 1 | {error, atom()}.

read() ->
  case gpio_ref() of 
    undefined -> {error, undefined};
  
    GpioRef -> gpio:read(GpioRef)
  end.

%% For blocks that manipulate multiple GPIO pins, don't use GpioRef from process dictionary
-spec read(GpioRef :: pid()) -> 0 | 1 | {error, atom()}.

read(GpioRef) -> gpio:read(GpioRef).


%%
%% Write the value of the GPIO pin
%%
-spec write(PinState :: 0 | 1) -> ok | {error, atom()}.

write(PinState) ->
  case gpio_ref() of 
    undefined -> {error, undefined};
  
    GpioRef -> gpio:write(GpioRef, PinState)
  end.

%% For blocks that manipulate multiple GPIO pins, don't use GpioRef from process dictionary
-spec write(GpioRef :: pid(),
            PinState :: 0 | 1) -> ok | {error, atom()}.

write(GpioRef, PinState) -> gpio:write(GpioRef, PinState).


%%
%% Stop GPIO Channel
%%
-spec stop() -> ok. 

stop() ->
  case gpio_ref() of
    undefined -> ok;

    GpioRef -> 
      gpio:stop(GpioRef), 
      gpio_ref(undefined), 
      ok
  end.

%% For blocks that manipulate multiple GPIO pins, don't use GpioRef from process dictionary
-spec stop(GpioRef :: pid()) -> ok. 

stop() ->gpio:stop(GpioRef).


-endif.

%% ====================================================================
%% Internal functions
%% ====================================================================

%
% Get and set the GPIO reference for the block controling the GPIO pin
% Use the process dictionary to store the GPIO references,
% so we don't have to pass it in as a parameter everywhere
%

gpio_ref() ->
  get(gpio_ref).

gpio_ref(GpioRef) ->
  put(gpio_ref, GpioRef).


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

start_link(_PinNumber, _Direction) ->
  GpioRef = make_ref(), 
  gpio_ref(GpioRef),
  {ok, GpioRef}.

%% Test with GPIO Reference pid
-spec start_link_mult(PinNumber :: string(), 
                 Direction :: non_neg_integer()) -> {ok, pid()} | {error, atom()}.

start_link_mult(_PinNumber, _Direction) -> {ok, make_ref()}.


%%
%% Test Register a channel to receive interrupts
%%
-spec register_int() -> ok | {error, atom()}.

register_int() ->
  case gpio_ref() of
    undefined -> {error, undefined};
  
    _GpioRef -> ok
  end.

%% Test with GPIO Reference pid
-spec register_int(GpioRef :: pid()) -> ok | {error, atom()}.

register_int(_GpioRef) -> ok.


%%
%% Test Set interrupt trigger condition
%%
-spec set_int(Condition :: enabled | summerize | both | rising | falling | none ) -> 
                    'ok' | {'error', atom()}.

set_int(_Condition) ->
  case gpio_ref() of 
    undefined -> {error, undefined};
  
    _GpioRef -> ok

  end.

%% Test with GPIO Reference pid
-spec set_int(GpioRef :: pid(), 
              Condition :: enabled | summerize | both | rising | falling | none ) -> 
                    'ok' | {'error', atom()}.

set_int(_GpioRef, _Condition) -> ok.


%%
%% Test Read the value of the GPIO pin
%%
-spec read() -> 0 | 1 | {error, atom()}.

read() ->
  case gpio_ref() of 
    undefined -> {error, undefined};
  
    _GpioRef -> 1
  end.

%% Test with GPIO Reference pid
-spec read(GpioRef :: pid()) -> 0 | 1 | {error, atom()}.

read(_GpioRef) -> 1.


%%
%% Test Write the value of the GPIO pin
%%
-spec write(PinState :: 0 | 1) -> ok | {error, atom()}.

write(_PinState) ->
  case gpio_ref() of 
    undefined -> {error, undefined};
  
    _GpioRef -> ok
  end.

%% Test with GPIO Reference pid
-spec write(GpioRef :: pid(), 
            PinState :: 0 | 1) -> ok | {error, atom()}.

write(_GpioRef, _PinState) -> ok.


%%
%% Test Stop
%%
-spec stop() -> ok. 

stop() ->
  case gpio_ref() of
    undefined -> ok;

    _GpioRef -> 
      gpio_ref(undefined), 
      ok
  end.

%% Test with GPIO Reference pid
-spec stop(GpioRef :: pid()) -> ok. 

stop(_GpioRef) -> ok.


-endif.
