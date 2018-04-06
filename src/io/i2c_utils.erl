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
          start_link/2,
          write/2,
          write_read/3,
          stop/1
]).


% TODO: Check if I2C system exists before calling start_link().

%
% Replace the real functions with test functions when performing unit Tests
%
-ifndef(TEST).

%%
%% Check if i2c Channel exists
%%
-spec is_installed(I2cDevice :: string()) -> boolean().

is_installed(I2cDevice) ->
  case file:read_file_info("/dev/" ++ I2cDevice) of
    {ok, _FileInfo} -> true;
    {error, _Reason} -> false
  end.



%%
%% Start I2C channel
%%
-spec start_link(I2cDevice :: string(), 
                 I2cAddr :: non_neg_integer()) -> {ok, pid()} | {error, atom()}.

start_link(I2cDevice, I2cAddr) -> i2c:start_link(I2cDevice, I2cAddr).


%%
%% Write data to I2C channel
%%
-spec write(I2cRef :: pid(),
            Data :: binary()) -> ok | {error, atom()}.

write(I2cRef, Data) -> i2c:write(I2cRef, Data).


%%
%% Write and read data, to and from I2C channel
%%
-spec write_read(I2cRef :: pid(),
                 WriteData :: binary(),
                 ReadCnt :: pos_integer()) -> binary() | {error, atom()}.

write_read(I2cRef, WriteData, ReadCnt) -> i2c:write_read(I2cRef, WriteData, ReadCnt).

  
%%
%% Stop I2C channel
%%
-spec stop(I2cRef :: pid()) -> ok. 

stop(I2cRef) -> i2c:stop(I2cRef).

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
%% Test Start link
%%
-spec start_link(I2cDevice :: string(), 
                 I2cAddr :: non_neg_integer()) -> {ok, pid()} | {error, atom()}.

start_link(_I2cDevice, _I2cAddr) -> {ok, make_ref()}.


%%
%% Test Write data 
%%
-spec write(I2cRef :: pid(),
            Data :: binary()) -> ok | {error, atom()}.

write(_I2cRef, _Data) -> ok.


%%
%% Test Write and read data
%%
-spec write_read(I2cRef :: pid(),
                 WriteData :: binary(),
                 ReadCnt :: pos_integer()) -> binary() | {error, atom()}.

write_read(_I2cRef, _WriteData, ReadCnt) ->
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
%% Test Stop
%%
-spec stop(I2cRef :: pid()) -> ok. 

stop(_I2cRef) -> ok.


-endif.