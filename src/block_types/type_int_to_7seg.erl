%%% @doc 
%%% Block Type: Integer to Seven Segment Variable Digits Decoder
%%% Description: Convert an input integer number to a set of bytes.
%%%              one per digit, indicating which segments of a 
%%%              seven segment display digit should be turned on.    
%%%               
%%% @end 

-module(type_int_to_7seg). 

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([type_name/0, description/0, version/0]). 
-export([create/2, create/4, create/5, initialize/1, execute/1, delete/1]).


type_name() -> "int_to_7seg".

version() -> "0.1.0".

description() -> "Convert integer input to multiple 7 segment digits outputs".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {num_of_digits, {1}},
      {number_base, {10}},
      {leading_zeros, {false}}
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {input, {empty, ?EMPTY_LINK}},
      {dec_pnt, [{false, ?EMPTY_LINK}]}  % Array attribute
    ]). 


-spec default_outputs() -> list().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {digit, [{not_active, []}]}  % Array attribute 
    ]).


%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: block_name(),
             Description :: string()) -> block_defn().

create(BlockName, Description) -> 
  create(BlockName, Description, [], [], []).

-spec create(BlockName :: block_name(),
             Description :: string(),  
             InitConfig :: list(config_attr()), 
             InitInputs :: list(input_attr())) -> block_defn().
   
create(BlockName, Description, InitConfig, InitInputs) -> 
  create(BlockName, Description, InitConfig, InitInputs, []).

-spec create(BlockName :: block_name(),
             Description :: string(), 
             InitConfig :: list(config_attr()), 
             InitInputs :: list(input_attr()), 
             InitOutputs :: list()) -> block_defn().

create(BlockName, Description, InitConfig, InitInputs, InitOutputs)->

  %% Update Default Config, Input, Output, and Private attribute values 
  %% with the initial values passed into this function.
  %%
  %% If any of the intial attributes do not already exist in the 
  %% default attribute lists, merge_attribute_lists() will create them.
     
  Config = attrib_utils:merge_attribute_lists(default_configs(BlockName, Description), InitConfig),
  Inputs = attrib_utils:merge_attribute_lists(default_inputs(), InitInputs), 
  Outputs = attrib_utils:merge_attribute_lists(default_outputs(), InitOutputs),

  % This is the block definition, 
  {Config, Inputs, Outputs}.


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
  % Check the config values
  case config_utils:get_integer_range(Config, num_of_digits, 1, 99) of
    {error, Reason} ->
      Inputs1 = Inputs,
      Outputs1 = Outputs,
      {Value, Status} = config_utils:log_error(Config, num_of_digits, Reason);
       
    {ok, NumOfDigits} ->
    
      case config_utils:get_integer_range(Config, number_base, 2, 16) of
        {error, Reason} ->
          Inputs1 = Inputs,
          Outputs1 = Outputs,
          {Value, Status} = config_utils:log_error(Config, number_base, Reason);
             
        {ok, _NumberBase} ->
        
          case config_utils:get_boolean(Config, leading_zeros) of
            {error, Reason} ->
              Inputs1 = Inputs,
              Outputs1 = Outputs,
              {Value, Status} = config_utils:log_error(Config, number_base, Reason);

            {ok, _LeadingZeros} ->
      
              % All config values are OK
              
              % Create a decimal point input for each digit
              BlockName = config_utils:name(Config),
              Inputs1 = input_utils:resize_attribute_array_value(BlockName, Inputs, 
                                  dec_pnt, NumOfDigits, {false, ?EMPTY_LINK}),

              % Create a digit output for each digit
              Outputs1 = 
                output_utils:resize_attribute_array_value(BlockName, Outputs, 
                                       digit, NumOfDigits, {not_active, []}),
              Value = not_active,
              Status = initialed
          end                    
      end
  end,
  Outputs2 = output_utils:set_value_status(Outputs1, Value, Status),
  
  % This is the block state
  {Config, Inputs1, Outputs2, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({Config, Inputs, Outputs, Private}) ->
  
  % Config values are validated in initialize function, just read them here   
  {ok, NumOfDigits} = attrib_utils:get_value(Config, num_of_digits),
  {ok, NumberBase} = attrib_utils:get_value(Config, number_base),
  {ok, LeadingZeros} = attrib_utils:get_value(Config, leading_zeros),
      
  case input_utils:get_integer(Inputs, input) of
    {error, Reason} ->
      {Value, Status} = input_utils:log_error(Config, input, Reason),
      Digits7Seg = lists:duplicate(NumOfDigits, not_active);

    {ok, not_active} ->
      Value = not_active, Status = normal,
      Digits7Seg = lists:duplicate(NumOfDigits, not_active);
   
    {ok, InValue} ->  
      InValueStr = integer_to_list(InValue, NumberBase),
      
      % Set the main output value to the formated input value
      % This should be the same as the 7 segment display is showing
      Value = InValueStr, Status = normal, 
      LenInValueStr = length(InValueStr),
          
      if LenInValueStr > NumOfDigits ->
        % The magnitude of the input value exceeds the number of digits
        % Set the digits outputs to display "---" 
        Digits = lists:duplicate(NumOfDigits, $-);
            
      true ->  % Input value will fit into the digits
        % Determine if leading digits should be zero or blank
        NumBlankDigits = NumOfDigits - LenInValueStr,
        if LeadingZeros ->
            LeadDigits = lists:duplicate(NumBlankDigits, $0);
        true ->
            LeadDigits = lists:duplicate(NumBlankDigits, 32)
        end,
        Digits = LeadDigits ++ InValueStr  
      end,
     
      % Convert the digits to 7 segment representations
      % TODO: Read decimal point inputs, and set decimal points also
      Digits7Seg = lists:map(fun(Digit) -> char_to_segments(Digit, false) end, Digits)
  end,
  
  Outputs1 = output_utils:set_array_value(Outputs, digit, Digits7Seg),
  Outputs2 = output_utils:set_value_status(Outputs1, Value, Status),

  % Return updated block state
  {Config, Inputs, Outputs2, Private}.


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
  

%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
  

 create_test() ->
    create(create_test, "Testing Block Creation").
  %{Config, Inputs, Outputs} = BlockDefn.

initialize_test() ->
  BlockDefn = create(initialize_test, "Testing Block Initialization"),
  _BlockState = block_common:initialize(BlockDefn).


-endif.