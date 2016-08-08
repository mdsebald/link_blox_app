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
      {num_of_digits, {2}}
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      % Number of digits to the right of the decimal point for positive values
      {pos_precision, {empty, ?EMPTY_LINK}}, 
      % Number of digits to the right of the decimal point for negative values
      {neg_precision, {empty, ?EMPTY_LINK}},
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
  case config_utils:get_integer_range(Config, num_of_digits, 1, 99) of
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
      case get_display_limits(InValue, NumOfDigits, Inputs) of
        {ok, Precision} ->
          Value = InValue, Status = normal,
          Digits7Seg = convert_number(InValue, NumOfDigits, Precision),
          PosOverflow = false,
          NegOverflow = false;

        too_big -> 
          Value = InValue, Status = normal,
          % Set all digits to '-' to indicate overflow condition
          Overflow = block_utils:char_to_segments($-, false),
          Digits7Seg = lists:duplicate(NumOfDigits, Overflow),
          PosOverflow = true,
          NegOverflow = false;

        too_small ->
          Value = InValue, Status = normal,
          % Set all digits to '-' to indicate underflow condition
          Underflow = block_utils:char_to_segments($-, false),
          Digits7Seg = lists:duplicate(NumOfDigits, Underflow),
          PosOverflow = false,
          NegOverflow = true;
      
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

% Get the limits on what is displayable
-spec get_display_limits(InValue :: float(),
                         NumOfDigits :: pos_integer(),
                         Inputs :: list(input_attr())) -> 
                         {ok, pos_integer()} | too_big | too_small | {error, atom()}.
                  
get_display_limits(InValue, NumOfDigits, Inputs) ->
  if (InValue >= 0.0) ->
    case get_pos_limit(NumOfDigits, Inputs) of
      {ok, MaxDispVal, PosPrec} ->
        if (InValue =< MaxDispVal) ->
          {ok, PosPrec};
        true ->
          too_big
        end;

      {error, Reason} -> {error, Reason}
    end;

  true -> % Input value is negative
    case get_neg_limit(NumOfDigits, Inputs) of
      {ok, MinDispVal, NegPrec} ->
        if (MinDispVal =< InValue ) ->
          {ok, NegPrec};
        true ->
          too_small
        end;

      {error, Reason} -> {error, Reason}
    end
  end.

% Get the largest positive number that can be displayed
% Leave one digit for leading zero
% So maximum number of digits to the right of the decimal point is: NumOfDigits-1
-spec get_pos_limit(NumOfDigits :: pos_integer(),
                    Inputs :: list(input_attr())) -> {ok, float(), pos_integer()} | {error, atom()}.

get_pos_limit(NumOfDigits, Inputs) ->
  case input_utils:get_integer_range(Inputs, pos_precision, 0, (NumOfDigits-1)) of
    {ok, not_active} ->
      % Don't care about precision 
      % display the maximum number possible
      % with maximum precision possible
      % i.e. 4 digits, MaxDisplayValue = 9999
      % MinDisplayPrecision = 0.001 

      MaxDispVal = math:pow(10, NumOfDigits) - 1.0,
      {ok, MaxDispVal, undefined};

    {ok, PosPrec} ->
      % i.e. NumOfDigits = 4, PosPrec = 2, 
      % MaxDispVal = 99.99, MinDispPrec = 0.01

      Max = math:pow(10, (NumOfDigits - PosPrec)),
      MinDispPrec = (1.0 / Max),
      MaxDispVal = Max - MinDispPrec,
      {ok, MaxDispVal, PosPrec};
    
    {error, Reason} -> {error, Reason}
  end.

% Get the largest negative number that can be displayed
% First digit will always be negative sign
% Leave one digit for leading zero
% So maximum number of digits to the right of the decimal point is: NumOfDigits-2
-spec get_neg_limit(NumOfDigits :: pos_integer(),
                    Inputs :: list(input_attr())) -> {ok, float(), pos_integer()} | {error, atom()}.

get_neg_limit(NumOfDigits, Inputs) ->

  case input_utils:get_integer_range(Inputs, neg_precision, 0, (NumOfDigits-2)) of
    {ok, not_active} ->
      % Don't care about precision 
      MaxDispVal = -(math:pow(10, (NumOfDigits-1))) + 1.0,
      {ok, MaxDispVal, undefined};

    {ok, NegPrec} ->
      Max = math:pow(10, ((NumOfDigits-1) - NegPrec)),
      MinDispPrec = (1.0 / Max),
      MaxDispVal = (-Max) + MinDispPrec,
      {ok, MaxDispVal, NegPrec};
    
    {error, Reason} -> {error, Reason}
  end.  

% Convert floating point value to a list of bytes
% One byte per digit. Each byte specifying which digit segment(s) to turn on
-spec convert_number(Number :: float(), 
                     NumOfDigits :: pos_integer(),
                     Precision :: pos_integer() | undefined) -> list(byte()).

convert_number(Number, NumOfDigits, Precision) ->
  case Precision of
    undefined ->
      FormatString = io_lib:format("~c~wf", [$~, NumOfDigits]);

    Precision ->
      FormatString = io_lib:format("~c~w.~wf", [$~, NumOfDigits+1, Precision])
  end,

  FormattedNumber = io_lib:format(FormatString, [Number]),
  Digits7Seg = 
    lists:map(fun(Digit) ->
                block_utils:char_to_segments(Digit)
              end,
              FormattedNumber).

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