%%% @doc 
%%% Block Type: Floating point value to Seven Segment Variable Digits Decoder
%%% Description: Convert an input floating point number to a set of bytes.
%%%              one per digit, indicating which segments of a 
%%%              seven segment display digit should be turned on.    
%%%               
%%% @end 

-module(type_float_to_7seg). 

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([description/0, version/0]). 
-export([create/2, create/4, create/5, initialize/1, execute/1, delete/1]).


version() -> "0.1.0".

description() -> "Convert floating point value input to multiple 7 segment digits outputs".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {num_of_digits, {4}}
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      % Number of digits to the right of the decimal point for positive values
      {pos_precision, {2, ?EMPTY_LINK}}, 
      % Number of digits to the right of the decimal point for negative values
      {neg_precision, {1, ?EMPTY_LINK}},
      % The value to display.
      {input, {empty, ?EMPTY_LINK}}
     ]). 


-spec default_outputs() -> list().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {digit, [{not_active, []}]},  % Array attribute
      {pos_overflow, {not_active, []}},  % Insufficient digits to display positive value
      {neg_overflow, {not_active, []}}   % Insufficient digits to display negative value
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
  case config_utils:get_integer_range(Config, num_of_digits, 2, 99) of
    {error, Reason} ->
      Outputs1 = Outputs,
      {Value, Status} = config_utils:log_error(Config, num_of_digits, Reason);
       
    {ok, NumOfDigits} ->
      % Create a digit output for each digit
      BlockName = config_utils:name(Config),
      Outputs1 = output_utils:resize_attribute_array_value(BlockName, Outputs, 
                                       digit, NumOfDigits, {not_active, []}),
      Value = not_active,
      Status = initialed                   
  end,
  Outputs2 = output_utils:set_value_status(Outputs1, Value, Status),
  
  % This is the block state
  {Config, Inputs, Outputs2, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({Config, Inputs, Outputs, Private}) ->
  
  % Config values are validated in initialize function, just read them here   
  {ok, NumOfDigits} = attrib_utils:get_value(Config, num_of_digits),

  case input_utils:get_float(Inputs, input) of
    {ok, not_active} ->
      Value = not_active, Status = normal,
      Digits7Seg = lists:duplicate(NumOfDigits, not_active),
      PosOverflow = not_active,
      NegOverflow = not_active;
   
    {ok, InValue} ->
      % In normal status, set the main Value output equal to the input value
      % Value output is not used to drive 7 segement displays
      case format_number(InValue, NumOfDigits, Inputs) of
        {ok, DigitsToDisp, Precision} ->
          Value = InValue, Status = normal,
          
          Digits7SegNoDecPnt = lists:map(fun(Digit) ->
                              block_utils:char_to_segments(Digit, false)
                              end,
                              DigitsToDisp),
                            
          DecPntDigitIndex = NumOfDigits - Precision,

          DigitWithDecPnt = lists:nth(DecPntDigitIndex, Digits7SegNoDecPnt) bor 16#80,
          Digits7Seg = list_replace(Digits7SegNoDecPnt, DigitWithDecPnt, DecPntDigitIndex),

          PosOverflow = false,
          NegOverflow = false;

        {error, too_big, IsNegative} -> 
          Value = InValue, Status = normal,
          % TODO: create config attributes for user selectable overflow segments to enable
          % Set all digits to '-' to indicate overflow condition
          Overflow = block_utils:char_to_segments($-, false),
          Digits7Seg = lists:duplicate(NumOfDigits, Overflow),

          if IsNegative ->
            PosOverflow = false,
            NegOverflow = true;
          true ->
            PosOverflow = true,
            NegOverflow = false
          end;
      
        {error, Reason} ->
          {Value, Status} = input_utils:log_error(Config, precision, Reason),
          Digits7Seg = lists:duplicate(NumOfDigits, not_active),
          PosOverflow = not_active,
          NegOverflow = not_active
      end;

    {error, Reason} ->
      {Value, Status} = input_utils:log_error(Config, input, Reason),
      Digits7Seg = lists:duplicate(NumOfDigits, not_active),
      PosOverflow = not_active,
      NegOverflow = not_active
  end,

  {ok, Outputs1} = attrib_utils:set_value(Outputs, pos_overflow, PosOverflow),
  {ok, Outputs2} = attrib_utils:set_value(Outputs1, neg_overflow, NegOverflow),
  Outputs3 = output_utils:set_array_value(Outputs2, digit, Digits7Seg),
  Outputs4 = output_utils:set_value_status(Outputs3, Value, Status),

  % Return updated block state
  {Config, Inputs, Outputs4, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockValues :: block_state()) -> block_state().

delete({Config, Inputs, Outputs, Private}) -> 
  {Config, Inputs, Outputs, Private}.


%% ====================================================================
%% Internal functions
%% ====================================================================

format_number(InValue, NumOfDigits, Inputs) ->

  if ((-1.0 < InValue) andalso (InValue < 1.0)) ->
    IsLessThanOne = true;
  true ->
    IsLessThanOne = false
  end,

  if (InValue >= 0.0) ->
    % Max precision is NumOfDigits-1, for leading zero
    PrecResult = input_utils:get_integer_range(Inputs, pos_precision, 0, 
                                               (NumOfDigits-1)),
    IsNegative = false; 
  true ->
    % Max precision is NumOfDigits-2, for minus sign and leading zero
    PrecResult = input_utils:get_integer_range(Inputs, neg_precision, 0, 
                                               (NumOfDigits-2)),
    IsNegative = true 
  end,

  case PrecResult of
    {ok, not_active} ->
      % Don't care about precision 
      % Format number with the maximum possible precision
      if IsNegative ->
        MaxPrecision = NumOfDigits - 2;
      true ->
        MaxPrecision = NumOfDigits - 1
      end,
      
      max_precision(InValue, NumOfDigits, MaxPrecision, IsNegative, IsLessThanOne);

    {ok, Precision} ->
      DigitsToDisp = digits_to_display(InValue, Precision, IsNegative, IsLessThanOne),
      
      if (length(DigitsToDisp) > NumOfDigits) ->
        {error, too_big, IsNegative};
      true ->
        {ok, DigitsToDisp, Precision}
      end;

    {error, Reason} -> {error, Reason}
  end.


%
% Find the maximum precision that fits in the number of digits allowed
% Starting precision is NumOfDigits -1 or -2 (for negative numbers)
%
-spec max_precision(InValue :: float(),
                    NumOfDigits :: pos_integer(),
                    Precision :: pos_integer(),
                    IsNegative :: boolean(),
                    IsLessThanOne :: boolean()) -> {ok, list(), pos_integer()} |
                                                   {error, too_big, boolean()}. 

max_precision(InValue, NumOfDigits, 0, IsNegative, IsLessThanOne) ->
  DigitsToDisp = 
    digits_to_display(InValue, 0, IsNegative, IsLessThanOne),

  if (length(DigitsToDisp) > NumOfDigits) ->
    % Number doesn't fit using least (zero) precision, return error
    {error, too_big, IsNegative};
  true ->
    {ok, DigitsToDisp, 0}
  end;

max_precision(InValue, NumOfDigits, Precision, IsNegative, IsLessThanOne) ->
  DigitsToDisp = digits_to_display(InValue, Precision, 
                                    IsNegative, IsLessThanOne),

  if (length(DigitsToDisp) > NumOfDigits) ->
    % Number doesn't fit, try formatting at a lower precision
    max_precision(InValue, NumOfDigits, Precision-1, 
                  IsNegative, IsLessThanOne);

  true ->
    % Number fits in the display, use this.
    {ok, DigitsToDisp, Precision}
  end.


%
% Get the list of digits to display, without a decimal point
% Add the minus sign and leading zero where appropriate
%
-spec digits_to_display(InValue :: float(),
                        Precision :: pos_integer(),
                        IsNegative :: boolean(),
                        IsLessThanOne :: boolean()) -> list().

digits_to_display(InValue, Precision, IsNegative, IsLessThanOne) ->
  % Example: 
  %   InValue 12.3456, Precision 3
  %   InValueNoDecPnt = 12346
  InValueNoDecPnt = round(math:pow(10, Precision) * InValue),

  RawDigits = integer_to_list(InValueNoDecPnt, 10),

  if IsLessThanOne ->
    % Add leading zero
    if IsNegative ->
      % Add minus sign
      [$- | [$0 | RawDigits]];
    true ->
      [$0 | RawDigits]
    end;
  true -> 
    if IsNegative ->
      [$- | RawDigits];
    true ->
      RawDigits
    end
  end.


%%
%% Replace the element at Position in the List, with Element
%% Lists are indexed from 1
%%
-spec list_replace(List :: list(),
                   Element :: term(),
                   Position :: pos_integer()) -> list(). 
                           
list_replace(List, Element, Position) ->
  lists:sublist(List, Position - 1) 
         ++ [Element] 
         ++ lists:nthtail(Position, List).

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