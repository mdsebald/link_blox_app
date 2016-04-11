%%% @doc 
%%% Block Type: Number to Seven Segment Decoder
%%% Description: Convert an input number to a set of bytes.
%%%              one per digit, indicating which segments of a 
%%%              seven segment display digit should be turned on.    
%%%               
%%% @end 

-module(lblx_seven_seg_decoder). 

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([type_name/0, description/0, version/0]). 
-export([create/2, create/4, create/5, initialize/1, execute/1, delete/1]).


type_name() -> "seven_seg_decoder".

version() -> "0.1.0".

description() -> "Convert numerical input to 7 segment digits representation".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: atom(),
                      Comment :: string()) -> list().

default_configs(BlockName, Comment) -> 
  block_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, Comment, type_name(), version(), description()), 
    [
     
    ]). 


-spec default_inputs() -> list().

default_inputs() -> 
  block_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {input, empty, ?EMPTY_LINK}
    ]). 


-spec default_outputs() -> list().
                            
default_outputs() -> 
  block_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {digit_1, not_active, []},
      {digit_2, not_active, []},
      {digit_3, not_active, []},
      {digit_4, not_active, []}
    ]). 


%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: atom(),
             Comment :: string()) -> block_defn().

create(BlockName, Comment) -> 
  create(BlockName, Comment, [], [], []).

-spec create(BlockName :: atom(),
             Comment :: string(),  
             InitConfig :: list(), 
             InitInputs :: list()) -> block_defn().
   
create(BlockName, Comment, InitConfig, InitInputs) -> 
  create(BlockName, Comment, InitConfig, InitInputs, []).

-spec create(BlockName :: atom(),
             Comment :: string(), 
             InitConfig :: list(), 
             InitInputs :: list(), 
             InitOutputs :: list()) -> block_defn().

create(BlockName, Comment, InitConfig, InitInputs, InitOutputs)->

  %% Update Default Config, Input, Output, and Private attribute values 
  %% with the initial values passed into this function.
  %%
  %% If any of the intial attributes do not already exist in the 
  %% default attribute lists, merge_attribute_lists() will create them.
  %% (This is useful for block types where the number of attributes is not fixed)
    
  Config = block_utils:merge_attribute_lists(default_configs(BlockName, Comment), InitConfig),
  Inputs = block_utils:merge_attribute_lists(default_inputs(), InitInputs), 
  Outputs = block_utils:merge_attribute_lists(default_outputs(), InitOutputs),

  % This is the block definition, 
  {Config, Inputs, Outputs}.


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
    
  % Perform block type specific initializations here
  % Add and intialize private attributes here
  Outputs1 = Outputs, 
  Private1 = Private,

  % This is the block state
  {Config, Inputs, Outputs1, Private1}.


%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({Config, Inputs, Outputs, Private}) ->

  case lblx_inputs:get_float(Inputs, input) of
    {error, Reason} ->
      lblx_inputs:log_error(Config, input, Reason),
      Value = not_active, Status = input_err,
      Digit1 = not_active,
      Digit2 = not_active,
      Digit3 = not_active,
      Digit4 = not_active;

    {ok, not_active} ->
      Value = not_active, Status = normal,
      Digit1 = not_active,
      Digit2 = not_active,
      Digit3 = not_active,
      Digit4 = not_active;
   
    {ok, Value} ->  
      NumberStr = io_lib:format("~.2f", [Value]),
      Status = normal,
      
      % Convert formatted number string into list of bytes
      FlatNumberStr = lists:flatten(NumberStr),
      
      Digit1 = char_to_segments(lists:nth(1, FlatNumberStr), false),
      Digit2 = char_to_segments(lists:nth(2, FlatNumberStr), true),
      Digit3 = char_to_segments(lists:nth(4, FlatNumberStr), false),
      Digit4 = char_to_segments(lists:nth(5, FlatNumberStr), false)
  end,
  
  Outputs1 = block_utils:set_values(Outputs, 
  [
    {value, Value}, {status, Status},  
    {digit_1, Digit1}, {digit_2, Digit2}, {digit_3, Digit3}, {digit_4, Digit4}
  ]),


  % Return updated block state
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(block_state()) -> ok.

delete({_Config, _Inputs, _Outputs, _Private}) -> 
  ok.


%% ====================================================================
%% Internal functions
%% ====================================================================


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