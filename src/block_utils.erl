%%% @doc 
%%% Common Block utility functions     
%%%               
%%% @end 

-module(block_utils).

-author("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([sleep/1]). 


%% common delay function
sleep(T) ->
  receive
  after T -> ok
  end.


      

%% ====================================================================
%% Internal functions
%% ====================================================================
