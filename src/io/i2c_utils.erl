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
          is_installed/1,
          open/1,
          write/2,
          write_read/3,
          close/1
]).


%
% Replace the real functions with test functions when performing unit Tests
%
-ifndef(TEST).

%%
%% Check if i2c Channel exists
%%
-spec is_installed(I2cBus :: string()) -> boolean().

is_installed(I2cBus) ->

  case file:read_file_info("/dev/" ++ I2cBus) of
    {ok, _FileInfo} -> true;
    {error, _Reason} -> false
  end.


%%
%% Open I2C channel/bus
%%
-spec open(I2cBus :: string()) -> {ok, reference()} | {error, atom()}.

open(I2cBus) -> circuits_i2c:open(I2cBus).


%%
%% Write data to I2C channel
%%
-spec write(I2cDevice :: lb_types:i2c_device(),
            Data :: binary()) -> ok | {error, atom()}.

write({I2cRef, I2cAddr}, Data) -> circuits_i2c:write(I2cRef, I2cAddr, Data).


%%
%% Write and read data, to and from I2C channel
%%
-spec write_read(I2cDevice :: lb_types:i2c_device(),
                 WriteData :: binary(),
                 ReadCnt :: pos_integer()) -> binary() | {error, atom()}.

write_read({I2cRef, I2cAddr}, WriteData, ReadCnt) -> 
  circuits_i2c:write_read(I2cRef, I2cAddr, WriteData, ReadCnt).

  
%%
%% Close I2C channel
%%
-spec close(I2cRef :: reference()) -> ok. 

close(I2cRef) -> circuits_i2c:close(I2cRef).

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
%% Test if i2c Channel exists
%%
-spec is_installed(I2cDevice :: string()) -> boolean().

is_installed(_I2cDevice) -> true.


%%
%% Test Open
%%
-spec open(I2cBus :: string()) -> {ok, reference()} | {error, atom()}.

open(_I2cDevice) -> {ok, make_ref()}.


%%
%% Test Write data 
%%
-spec write(I2cDevice :: lb_types:i2c_device(),
            Data :: binary()) -> ok | {error, atom()}.

write(_I2cDevice, _Data) -> ok.


%%
%% Test Write and read data
%%
-spec write_read(I2cDevice :: lb_types:i2c_device(),
                 WriteData :: binary(),
                 ReadCnt :: pos_integer()) -> binary() | {error, atom()}.

write_read(_I2cDevice, _WriteData, ReadCnt) ->
      % make up a list of bytes, ReadCnt size, and return a binary
      create_binary_data(ReadCnt).

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
%% Test Close
%%
-spec close(I2cRef :: reference()) -> ok. 

close(_I2cRef) -> ok.


-endif.