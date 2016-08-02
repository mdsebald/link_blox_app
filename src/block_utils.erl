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

-export([
          sleep/1,
          char_to_segments/2
]). 


%% common delay function
-spec sleep(T :: pos_integer()) -> ok.

sleep(T) ->
  receive
  after T -> ok
  end.


%%
%% Convert a character to a byte indicating which segments
%% of a 7 segment display should be turned on.
%% Set the 0x80 bit of the segments byte, 
%% if the decimal point should be turned on. 
%%
-spec char_to_segments(Char:: char(), 
                       DecPnt :: boolean()) -> byte().

char_to_segments(Char, DecPnt) ->

  % -------------------------------------------------------
  % LED Segment ON:  a  |  b |  c | d  |  e |  f |  g | dp  
  % Segments Value: 0x01|0x02|0x04|0x08|0x10|0x20|0x40|0x80
  % --------------------------------------------------------
  
  CharToSegs = 
   [{$0,16#3F}, {$1,16#06}, {$2,16#5B}, {$3,16#4F}, {$4,16#66}, {$5,16#6D}, 
    {$6,16#7D}, {$7,16#07}, {$8,16#7F}, {$9,16#6F}, 
    {$A,16#77}, {$b,16#7C}, {$C,16#39}, {$d,16#5E}, {$E,16#79}, {$F,16#71},
    {32,16#00}, {$-,16#40}],
  
  case DecPnt of
    true -> DecPntSeg = 16#80;
    false -> DecPntSeg = 16#00
  end,
  
  case lists:keyfind(Char, 1, CharToSegs) of
    false -> % No character match found, just return the decimal point segment
      DecPntSeg; 
    {Char, Segments} -> % Combine the 7 segments with the decimal point segment
      (Segments bor DecPntSeg)
  end.

%% ====================================================================
%% Internal functions
%% ====================================================================
