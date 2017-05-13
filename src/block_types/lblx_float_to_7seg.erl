%%% @doc 
%%% Block Type: Floating point value to Seven Segment Variable Digits Decoder
%%% Description: Convert an input floating point number to a set of bytes.
%%%              one per digit, indicating which segments of a 
%%%              seven segment display digit should be turned on.    
%%%               
%%% @end 

-module(lblx_float_to_7seg). 

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [conversion].

description() -> "Convert floating point value input to multiple 7 segment digits outputs".

version() -> "0.1.0".


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


-spec default_outputs() -> list(output_attr()).
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {digits, [{null, []}]},  % Array attribute
      {pos_overflow, {null, []}},  % Insufficient digits to display positive value
      {neg_overflow, {null, []}}   % Insufficient digits to display negative value
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

create(BlockName, Description, InitConfig, InitInputs, InitOutputs) ->

  % Update Default Config, Input, Output, and Private attribute values 
  % with the initial values passed into this function.
  %
  % If any of the intial attributes do not already exist in the 
  % default attribute lists, merge_attribute_lists() will create them.
     
  Config = attrib_utils:merge_attribute_lists(default_configs(BlockName, Description), InitConfig),
  Inputs = attrib_utils:merge_attribute_lists(default_inputs(), InitInputs), 
  Outputs = attrib_utils:merge_attribute_lists(default_outputs(), InitOutputs),

  % This is the block definition, 
  {Config, Inputs, Outputs}.


%%
%% Upgrade block attribute values, when block code and block data versions are different
%% 
-spec upgrade(BlockDefn :: block_defn()) -> {ok, block_defn()} | {error, atom()}.

upgrade({Config, Inputs, Outputs}) ->
  ModuleVer = version(),
  {BlockName, BlockModule, ConfigVer} = config_utils:name_module_version(Config),
  BlockType = type_utils:type_name(BlockModule),

  case attrib_utils:set_value(Config, version, version()) of
    {ok, UpdConfig} ->
      log_server:info(block_type_upgraded_from_ver_to, 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      log_server:error(err_upgrading_block_type_from_ver_to, 
                            [Reason, BlockName, BlockType, ConfigVer, ModuleVer]),
      {error, Reason}
  end.


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
  % Check the config values
  case config_utils:get_integer_range(Config, num_of_digits, 2, 99) of
    {ok, NumOfDigits} ->
      % Create a digit output for each digit
      BlockName = config_utils:name(Config),
      Outputs1 = output_utils:resize_attribute_array_value(BlockName, Outputs, 
                                       digits, NumOfDigits, {null, []}),
      Value = null,
      Status = initialed;

    {error, Reason} ->
      Outputs1 = Outputs,
      {Value, Status} = config_utils:log_error(Config, num_of_digits, Reason)          
  end,

  Outputs2 = output_utils:set_value_status(Outputs1, Value, Status),
  
  % This is the block state
  {Config, Inputs, Outputs2, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->
  
  % Config values are validated in initialize function, just read them here   
  {ok, NumOfDigits} = attrib_utils:get_value(Config, num_of_digits),

  case input_utils:get_float(Inputs, input) of
    {ok, null} ->
      Value = null, Status = normal,
      Digits7Seg = lists:duplicate(NumOfDigits, null),
      PosOverflow = null,
      NegOverflow = null;
   
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
      
        {error, Reason, false} ->
          {Value, Status} = input_utils:log_error(Config, precision, Reason),
          Digits7Seg = lists:duplicate(NumOfDigits, null),
          PosOverflow = null,
          NegOverflow = null
      end;

    {error, Reason} ->
      {Value, Status} = input_utils:log_error(Config, input, Reason),
      Digits7Seg = lists:duplicate(NumOfDigits, null),
      PosOverflow = null,
      NegOverflow = null
  end,

  {ok, Outputs1} = attrib_utils:set_value(Outputs, pos_overflow, PosOverflow),
  {ok, Outputs2} = attrib_utils:set_value(Outputs1, neg_overflow, NegOverflow),
  Outputs3 = output_utils:set_array_value(Outputs2, digits, Digits7Seg),
  Outputs4 = output_utils:set_value_status(Outputs3, Value, Status),

  % Return updated block state
  {Config, Inputs, Outputs4, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  {Config, Inputs, Outputs}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%
% Format the input number given the number of digits and desired precision
%
-spec format_number(InValue :: float(),
                    NumOfDigits :: integer(),
                    Inputs :: list(input_attr())) -> 
                       {ok, list(byte()), integer()} | {error, too_big | input_errors(), boolean()}.

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
    {ok, null} ->
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

    {error, Reason} -> {error, Reason, false}
  end.


%
% Find the maximum precision that fits in the number of digits allowed
% Starting precision is NumOfDigits -1 or -2 (for negative numbers)
%
-spec max_precision(InValue :: float(),
                    NumOfDigits :: pos_integer(),
                    Precision :: non_neg_integer(),
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
                        Precision :: non_neg_integer(),
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


%
% Replace the element at Position in the List, with Element
% Lists are indexed from 1
%
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

block_test_() ->
  {"Input to Output tests for: " ++ atom_to_list(?MODULE),
   {setup, 
      fun setup/0, 
      fun cleanup/1,
      fun (BlockState) -> 
        {inorder,
        [
          test_io(BlockState)
        ]}
      end} 
  }.

setup() ->
  unit_test_utils:block_setup(?MODULE).

cleanup(BlockState) ->
  unit_test_utils:block_cleanup(?MODULE, BlockState).

test_io(BlockState) ->
  unit_test_utils:create_io_tests(?MODULE, input_cos, BlockState, test_states()).

test_states() ->
  [
    {[{input, "bad"}], [{status, input_err}, {value, null}, {pos_overflow, null}, {neg_overflow, null}]},
    {[{input, 88.0}], [{status, normal}, {value, 88.0}, {{digits, 1}, 16#7F}, {{digits, 2}, 16#FF}, {pos_overflow, false}, {neg_overflow, false}]},
    {[{input, -100.0}], [{status, normal}, {value, -100.0}, {{digits, 1}, 16#40}, {{digits, 2}, 16#40}, {pos_overflow, false}, {neg_overflow, true}]},
    {[{input, 100.0}], [{status, normal}, {value, 100.0}, {{digits, 1}, 16#40}, {{digits, 2}, 16#40}, {pos_overflow, true}, {neg_overflow, false}]}
  ].


-endif.