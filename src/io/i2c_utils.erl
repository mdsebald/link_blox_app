%%% @doc 
%%% I2C bus interface utility functions 
%%% Currently utilizes erlang_ale library
%%% This indirection allows other I/O libraries to be inserted if needed
%%%               
%%% @end 

-module(i2c_utils).

-author("Mark Sebald").


%% ====================================================================
%% API functions
%% ====================================================================
-export([
          start_link/2,
          write/1,
          write_read/2,
          stop/0
]).

%
% Replace the real functions with test functions when performing unit Tests
%
-ifndef(TEST).

%%
%% Start I2C channel
%%
-spec start_link(I2cDevice :: string(), 
                 I2cAddr :: non_neg_integer()) -> {ok, pid()} | {error, atom()}.

start_link(I2cDevice, I2cAddr) ->
  case i2c:start_link(I2cDevice, I2cAddr) of
    {ok, I2cRef} ->
      i2c_ref(I2cRef),
      {ok, I2cRef};

    {error, Reason} ->
      i2c_ref(undefined),
      {error, Reason}
  end.


%%
%% Write data to I2C channel
%%
-spec write(Data :: binary()) -> ok | {error, atom()}.

write(Data) ->
  case i2c_ref() of
    undefined -> {error, undefined};

    I2cRef ->
      case i2c:write(I2cRef, Data) of
        ok -> ok;

        {error, Reason} -> {error, Reason}
      end
  end.


%%
%% Write and read data, to and from I2C channel
%%
-spec write_read(WriteData :: binary(),
                 ReadCnt :: pos_integer()) -> binary() | {error, atom()}.

write_read(WriteData, ReadCnt) ->
  case i2c_ref() of
    undefined -> {error, undefined};

    I2cRef ->
      case i2c:write_read(I2cRef, WriteData, ReadCnt) of
        {error, Reason} -> {error, Reason};

        ReadData -> ReadData
      end
  end.

  
%%
%% Stop I2C channel
%%
-spec stop() -> ok. 

stop() ->
  case i2c_ref() of
    undefined -> ok;

    I2cRef -> 
      i2c:stop(I2cRef), 
      i2c_ref(undefined), 
      ok
  end.

-endif.

%% ====================================================================
%% Internal functions
%% ====================================================================

%
% Get and set the I2C reference for the block controlling this I2C channel
% Use the process dictionary to store the I2C reference,
% so we don't have to pass it in as a parameter everywhere
%

i2c_ref() ->
  get(i2c_ref).

i2c_ref(I2cRef) ->
  put(i2c_ref, I2cRef).


%% ====================================================================
%% Unit Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%
%% Test Start link
%%
-spec start_link(I2cDevice :: string(), 
                I2cAddr :: non_neg_integer()) -> {ok, pid()} | {error, atom()}.

start_link(_I2cDevice, _I2cAddr) ->
  I2cRef = make_ref(), 
  i2c_ref(I2cRef),
  {ok, I2cRef}.

%%
%% Test Write data 
%%
-spec write(Data :: binary()) -> ok | {error, atom()}.

write(_Data) ->
  case i2c_ref() of
    undefined -> {error, undefined};

    _I2cRef -> ok
  end.


%%
%% Test Write and read data
%%
-spec write_read(WriteData :: binary(),
                 ReadCnt :: pos_integer()) -> binary() | {error, atom()}.

write_read(_WriteData, ReadCnt) ->
  case i2c_ref() of
    undefined -> {error, undefined};

    _I2cRef ->
      % make up a list of bytes, ReadCnt size, and return a binary
      create_binary_data(ReadCnt)
  end.

% Create binary data of N bytes length for testing
-spec create_binary_data(N :: pos_integer()) -> binary().

create_binary_data(N) -> create_binary(create_list(N)).

create_list(N) -> create_list(N, []).

create_list(0, Acc) -> Acc;
create_list(N, Acc) -> create_list(N-1, [N|Acc]).

create_binary(List) -> create_binary(List, <<>>).

create_binary([], Acc) -> Acc;
create_binary([H|T], Acc) -> create_binary(T, <<Acc/binary,H>>).


%%
%% Test Stop
%%
-spec stop() -> ok. 

stop() ->
  case i2c_ref() of
    undefined -> ok;

    _I2cRef -> 
      i2c_ref(undefined), 
      ok
  end.

-endif.