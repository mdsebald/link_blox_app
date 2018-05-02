%%% @doc 
%%% BLOCKTYPE 
%%% Floating point value to Seven Segment Variable Digits Decoder
%%% DESCRIPTION
%%% Convert an input floating point number to a set of bytes.
%%% one per digit, indicating which segments of a 
%%% seven segment display digit should be turned on.    
%%% LINKS              
%%% @end 

-module(lblx_float_to_7seg). 

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [conversion].

version() -> "0.2.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {num_of_digits, {4}}, %| int | 4 | 1..99 |
      {pos_precision, {2}}, %| int | 2 | 0..num of digits |
      {neg_precision, {1}} %| int | 1 | 1..num of digits |
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {input, {empty, {empty}}} %| float | empty | +/- max float |
    ]). 


-spec default_outputs() -> output_attribs().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {digits, [{null, []}]}, %| byte arrary | null | 0..FFh |
      {pos_overflow, {null, []}}, %| bool | null | true, false |
      {neg_overflow, {null, []}}  %| bool | null | true, false |
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
             InitConfig :: config_attribs(), 
             InitInputs :: input_attribs()) -> block_defn().
   
create(BlockName, Description, InitConfig, InitInputs) -> 
  create(BlockName, Description, InitConfig, InitInputs, []).

-spec create(BlockName :: block_name(),
             Description :: string(), 
             InitConfig :: config_attribs(), 
             InitInputs :: input_attribs(), 
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
      logger:info(block_type_upgraded_from_ver_to, 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      logger:error(err_upgrading_block_type_from_ver_to, 
                            [Reason, BlockName, BlockType, ConfigVer, ModuleVer]),
      {error, Reason}
  end.


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
  % Check the config values
  case config_utils:get_integer_range(Config, num_of_digits, 1, 99) of
    {ok, NumOfDigits} ->
      % Create a digit output for each digit
      Outputs1 = output_utils:resize_attribute_array_value(Outputs, 
                                       digits, NumOfDigits, {null, []}),
      case config_utils:get_integer_range(Config, pos_precision, 0, NumOfDigits) of
        {ok, _PosPrecision} ->

          case config_utils:get_integer_range(Config, neg_precision, 0, NumOfDigits) of
            {ok, _NegPrecision} ->
              Value = null, 
              Status = initialed;              
            
            {error, Reason} ->
              {Value, Status} = config_utils:log_error(Config, neg_precision, Reason)
          end;
              
        {error, Reason} ->
          {Value, Status} = config_utils:log_error(Config, pos_precision, Reason)
      end;
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

execute({Config, Inputs, Outputs, Private}, disable) ->
  Outputs1 = output_utils:update_all_outputs(Outputs, null, disabled),
  {Config, Inputs, Outputs1, Private};

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
      case InValue >= 0.0 of
        true -> 
          {ok, Precision} = attrib_utils:get_value(Config, pos_precision),
          IsPositive = true;
    
        false -> 
          {ok, Precision} = attrib_utils:get_value(Config, neg_precision),
          IsPositive = false
      end,
      
      % Just set the output value equal to the string of the input value
      % Value is not used to drive 7-Segment display
      Value = float_to_list(InValue, [{decimals, Precision}]),
      Status = normal,

      case check_length(Value, NumOfDigits) of
        true ->
          % Remove the decimal point, and Pad out the string to the size of the display
          InValueStr = unicode:characters_to_nfc_list(string:pad(lists:delete($., Value), NumOfDigits, leading)),
          Digits7SegNoDecPnt = lists:map(fun(Digit) ->
                              block_utils:char_to_segments(Digit, false)
                              end,
                              InValueStr),
          
          if (Precision > 0) ->
            DecPntDigitIndex = NumOfDigits - Precision,
            DigitWithDecPnt = lists:nth(DecPntDigitIndex, Digits7SegNoDecPnt) bor 16#80,
            Digits7Seg = list_replace(Digits7SegNoDecPnt, DigitWithDecPnt, DecPntDigitIndex);
          true ->
            Digits7Seg = Digits7SegNoDecPnt
          end,

          PosOverflow = false,
          NegOverflow = false;

        false -> % number too big
          Overflow = block_utils:char_to_segments($-, false),
          Digits7Seg = lists:duplicate(NumOfDigits, Overflow),

          if IsPositive ->
            PosOverflow = true,
            NegOverflow = false;
          true ->
            PosOverflow = false,
            NegOverflow = true
          end
      end;

    {error, Reason} ->
      {Value, Status} = input_utils:log_error(Config, input, Reason),
      Digits7Seg = lists:duplicate(NumOfDigits, null),
      PosOverflow = null,
      NegOverflow = null
  end,

  {ok, Outputs1} = attrib_utils:set_value(Outputs, pos_overflow, PosOverflow),
  {ok, Outputs2} = attrib_utils:set_value(Outputs1, neg_overflow, NegOverflow),
  Outputs3 = output_utils:set_array_values(Outputs2, digits, Digits7Seg),
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
% Check if if value fits into the display
%
-spec check_length(InValueStr :: string(),
                   NumOfDigits :: integer()) -> boolean().

check_length(InValueStr, NumOfDigits) ->

  case lists:member($., InValueStr) of
    true -> % don't count decimal point when determining if digits fit into display
      length(InValueStr) =< (NumOfDigits + 1);

    false -> % No decimal point in string
      length(InValueStr) =< NumOfDigits
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

-include("block_io_test_gen.hrl").

test_sets() ->
  [
    % Test bad config inputs
    {[{num_of_digits, -1}], [], [{status, config_err}, {value, null}, {pos_overflow, null}, {neg_overflow, null}]},
    {[{num_of_digits, 4}, {pos_precision, -1}], [], [{status, config_err}, {value, null}, {pos_overflow, null}, {neg_overflow, null}]},
    {[{pos_precision, 1}, {neg_precision, 5}], [], [{status, config_err}, {value, null}, {pos_overflow, null}, {neg_overflow, null}]},


    % Test bad inputs
    {[{neg_precision, 1}], [{input, "bad"}], [{status, input_err}, {value, null}, {pos_overflow, null}, {neg_overflow, null}]},

    {[{pos_precision, 0}], [{input, 8.0}], [{status, normal}, {value, "8"}, 
                                               {{digits, 1}, 16#00}, {{digits, 2}, 16#00}, {{digits, 3}, 16#00}, {{digits, 4}, 16#7F}, 
                                               {pos_overflow, false}, {neg_overflow, false}]},

    {[{input, 8888.0}], [{status, normal}, {value, "8888"}, 
                                               {{digits, 1}, 16#7F}, {{digits, 2}, 16#7F}, {{digits, 3}, 16#7F}, {{digits, 4}, 16#7F}, 
                                               {pos_overflow, false}, {neg_overflow, false}]},

    {[{pos_precision, 1}], [], [{status, normal}, {value, "8888.0"}, 
                                               {{digits, 1}, 16#40}, {{digits, 2}, 16#40}, {{digits, 3}, 16#40}, {{digits, 4}, 16#40}, 
                                               {pos_overflow, true}, {neg_overflow, false}]},

    {[{input, 888.8}], [{status, normal}, {value, "888.8"}, 
                                               {{digits, 1}, 16#7F}, {{digits, 2}, 16#7F}, {{digits, 3}, 16#FF}, {{digits, 4}, 16#7F}, 
                                               {pos_overflow, false}, {neg_overflow, false}]},

    {[{pos_precision, 2}], [], [{status, normal}, {value, "888.80"}, 
                                               {{digits, 1}, 16#40}, {{digits, 2}, 16#40}, {{digits, 3}, 16#40}, {{digits, 4}, 16#40}, 
                                               {pos_overflow, true}, {neg_overflow, false}]},

    {[{input, 88.88}], [{status, normal}, {value, "88.88"}, 
                                               {{digits, 1}, 16#7F}, {{digits, 2}, 16#FF}, {{digits, 3}, 16#7F}, {{digits, 4}, 16#7F}, 
                                               {pos_overflow, false}, {neg_overflow, false}]},

    {[{pos_precision, 3}], [], [{status, normal}, {value, "88.880"}, 
                                               {{digits, 1}, 16#40}, {{digits, 2}, 16#40}, {{digits, 3}, 16#40}, {{digits, 4}, 16#40}, 
                                               {pos_overflow, true}, {neg_overflow, false}]},



    {[{neg_precision, 0}], [{input, -8.0}], [{status, normal}, {value, "-8"}, 
                                               {{digits, 1}, 16#00}, {{digits, 2}, 16#00}, {{digits, 3}, 16#40}, {{digits, 4}, 16#7F}, 
                                               {pos_overflow, false}, {neg_overflow, false}]},

    {[{input, -888.0}], [{status, normal}, {value, "-888"}, 
                                               {{digits, 1}, 16#40}, {{digits, 2}, 16#7F}, {{digits, 3}, 16#7F}, {{digits, 4}, 16#7F}, 
                                               {pos_overflow, false}, {neg_overflow, false}]},

    {[{neg_precision, 1}], [], [{status, normal}, {value, "-888.0"}, 
                                               {{digits, 1}, 16#40}, {{digits, 2}, 16#40}, {{digits, 3}, 16#40}, {{digits, 4}, 16#40}, 
                                               {pos_overflow, false}, {neg_overflow, true}]},

    {[{input, -88.8}], [{status, normal}, {value, "-88.8"}, 
                                               {{digits, 1}, 16#40}, {{digits, 2}, 16#7F}, {{digits, 3}, 16#FF}, {{digits, 4}, 16#7F}, 
                                               {pos_overflow, false}, {neg_overflow, false}]},

    {[{neg_precision, 2}], [], [{status, normal}, {value, "-88.80"}, 
                                               {{digits, 1}, 16#40}, {{digits, 2}, 16#40}, {{digits, 3}, 16#40}, {{digits, 4}, 16#40}, 
                                               {pos_overflow, false}, {neg_overflow, true}]},

    {[{input, -8.88}], [{status, normal}, {value, "-8.88"}, 
                                               {{digits, 1}, 16#40}, {{digits, 2}, 16#FF}, {{digits, 3}, 16#7F}, {{digits, 4}, 16#7F}, 
                                               {pos_overflow, false}, {neg_overflow, false}]},

    {[{neg_precision, 3}], [], [{status, normal}, {value, "-8.880"}, 
                                               {{digits, 1}, 16#40}, {{digits, 2}, 16#40}, {{digits, 3}, 16#40}, {{digits, 4}, 16#40},
                                               {pos_overflow, false}, {neg_overflow, true}]},

    {[{pos_precision, 4}], [{input, 0.88888}], [{status, normal}, {value, "0.8889"}, 
                                               {{digits, 1}, 16#40}, {{digits, 2}, 16#40}, {{digits, 3}, 16#40}, {{digits, 4}, 16#40}, 
                                               {pos_overflow, true}, {neg_overflow, false}]},

    {[{pos_precision, 3}], [], [{status, normal}, {value, "0.889"}, 
                                               {{digits, 1}, 16#BF}, {{digits, 2}, 16#7F}, {{digits, 3}, 16#7f}, {{digits, 4}, 16#6F}, 
                                               {pos_overflow, false}, {neg_overflow, false}]},
  
    {[{neg_precision, 3}], [{input, -0.88888}], [{status, normal}, {value, "-0.889"}, 
                                               {{digits, 1}, 16#40}, {{digits, 2}, 16#40}, {{digits, 3}, 16#40}, {{digits, 4}, 16#40}, 
                                               {pos_overflow, false}, {neg_overflow, true}]},

    {[{neg_precision, 2}], [], [{status, normal}, {value, "-0.89"}, 
                                               {{digits, 1}, 16#40}, {{digits, 2}, 16#BF}, {{digits, 3}, 16#7F}, {{digits, 4}, 16#6F}, 
                                               {pos_overflow, false}, {neg_overflow, false}]}

  ].


-endif.