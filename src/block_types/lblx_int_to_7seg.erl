 %%% @doc 
%%% BLOCKTYPE
%%% Integer to Seven Segment Variable Digits Decoder
%%% DESCRIPTION
%%% Convert an input integer number to a set of bytes.
%%% one per digit, indicating which segments of a
%%% seven segment display digit should be turned on.
%%% LINKS              
%%% @end 

-module(lblx_int_to_7seg). 

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
      {num_of_digits, {1}}, %| int | 1 | 1..99 |
      {number_base, {10}}, %| int | 10 | 2..16 |
      {leading_zeros, {false}} %| bool | false | true, false |
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {input, {empty, {empty}}}, %| int | empty | 0..max int |
      {dec_pnts, [{false, {false}}]} %| bool array | false | true, false |
    ]). 


-spec default_outputs() -> output_attribs().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {digits, [{null, []}]} %| byte array | null | 00.FFh |
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
      m_logger:info(block_type_upgraded_from_ver_to, 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      m_logger:error(err_upgrading_block_type_from_ver_to, 
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

      case config_utils:get_integer_range(Config, number_base, 2, 16) of   
        {ok, _NumberBase} ->
        
          case config_utils:get_boolean(Config, leading_zeros) of
            {ok, _LeadingZeros} ->
      
              % All config values are OK
              
              % Create a decimal point input for each digit
              BlockName = config_utils:name(Config),
              Inputs1 = input_utils:resize_attribute_array_value(BlockName, Inputs, 
                                  dec_pnts, NumOfDigits, {false, {false}}),

              % Create a digit output for each digit
              Outputs1 = 
                output_utils:resize_attribute_array_value(Outputs, 
                                       digits, NumOfDigits, {null, []}),
              Value = null,
              Status = initialed;

            {error, Reason} ->
              Inputs1 = Inputs,
              Outputs1 = Outputs,
              {Value, Status} = config_utils:log_error(Config, number_base, Reason)
          end;

        {error, Reason} ->
          Inputs1 = Inputs,
          Outputs1 = Outputs,
          {Value, Status} = config_utils:log_error(Config, number_base, Reason)
      end;

    {error, Reason} ->
      Inputs1 = Inputs,
      Outputs1 = Outputs,
      {Value, Status} = config_utils:log_error(Config, num_of_digits, Reason)
  end,

  Outputs2 = output_utils:set_value_status(Outputs1, Value, Status),
  
  % This is the block state
  {Config, Inputs1, Outputs2, Private}.


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
  {ok, NumberBase} = attrib_utils:get_value(Config, number_base),
  {ok, LeadingZeros} = attrib_utils:get_value(Config, leading_zeros),
      
  case input_utils:get_integer(Inputs, input) of
  
    {ok, null} ->
      Value = null, Status = normal,
      Digits7Seg = lists:duplicate(NumOfDigits, null);
   
    {ok, InValue} ->  
      InValueStr = integer_to_list(InValue, NumberBase),
      LenInValueStr = length(InValueStr),
          
      case LenInValueStr > NumOfDigits of
        true ->
          % The magnitude of the input value exceeds the number of digits
          % Set the digits outputs to display "---" 
          Digits = lists:duplicate(NumOfDigits, $-);
            
        false ->  % Input value will fit into the digits
          % Determine if leading digits should be zero or blank
          NumBlankDigits = NumOfDigits - LenInValueStr,
          case LeadingZeros of
            true ->
              LeadDigits = lists:duplicate(NumBlankDigits, $0);
            false ->
              LeadDigits = lists:duplicate(NumBlankDigits, 32)
          end,
          Digits = LeadDigits ++ InValueStr
      end,
      % Get the Decimal Point inputs
      case input_utils:get_boolean_array(Inputs, dec_pnts) of
        {ok, DecPnts} ->
          case lists:member(error, DecPnts) of
            false -> % no decimal point inputs are in error
              % Convert the digits decimal points to 7 segment representations
              DigitsAndDecPnts = lists:zip(Digits, DecPnts),
              Digits7Seg = lists:map(fun({Digit, DecPnt}) -> 
                      block_utils:char_to_segments(Digit, DecPnt) end, DigitsAndDecPnts),
              
              % Set the main output value to the formated input value
              % Including the decimal points in Output Value
              Value = lists:foldr(fun({Digit, DecPnt}, AccIn) ->
                      case DecPnt of
                        true -> [Digit | [$. | AccIn]];
                           _ -> [Digit | AccIn]
                      end
                    end, [], DigitsAndDecPnts),
              Status = normal;

            true -> % One or more of the decimal point inputs is in error
              {Value, Status} = input_utils:log_error(Config, dec_pnts, input_err),
              Digits7Seg = lists:duplicate(NumOfDigits, null)                
          end;
        {error, Reason} ->
          {Value, Status} = input_utils:log_error(Config, dec_pnts, Reason),
          Digits7Seg = lists:duplicate(NumOfDigits, null)                
      end;
    {error, Reason} ->
      {Value, Status} = input_utils:log_error(Config, input, Reason),
      Digits7Seg = lists:duplicate(NumOfDigits, null)                
  end,
  
  Outputs1 = output_utils:set_array_values(Outputs, digits, Digits7Seg),
  Outputs2 = output_utils:set_value_status(Outputs1, Value, Status),

  % Return updated block state
  {Config, Inputs, Outputs2, Private}.


%% 
%%  Delete the block
%%
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  {Config, Inputs, Outputs}.


%% ====================================================================
%% Internal functions
%% ====================================================================


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("block_io_test_gen.hrl").

test_sets() ->
  [
    % Test bad config values
    {[{num_of_digits, bad}], [], [{value, null}, {status, config_err}, {{digits, 1}, null}]},
    {[{number_base, 88}, {num_of_digits, 1}], [], [{value, null}, {status, config_err}, {{digits, 1}, null}]},
    {[{leading_zeros, bad}, {number_base, 10}], [], [{value, null}, {status, config_err}, {{digits, 1}, null}]},
   
    % Test bad input values
    {[{leading_zeros, false}], [{input, bad}], [{value, null}, {status, input_err}, {{digits, 1}, null}]},
    {[{input, 10}, {{dec_pnts, 1}, bad}], [{value, null}, {status, input_err}, {{digits, 1}, null}]},
    
    % Test good input and config values
    {[{input, 123}, {{dec_pnts, 1}, false}], [{value, "-"}, {status, normal}, {{digits, 1}, 16#40}]},
    {[{num_of_digits, 4}], [{input, -123}], [{value, "-123"}, {status, normal}, {{digits, 1}, 16#40}, {{digits, 2}, 16#06}, {{digits, 3}, 16#5B}, {{digits, 4}, 16#4F}]},
    {[{{dec_pnts, 1}, true}], [{value, "-.123"}, {status, normal}, {{digits, 1}, 16#C0}, {{digits, 2}, 16#06}, {{digits, 3}, 16#5B}, {{digits, 4}, 16#4F}]},
    {[{{dec_pnts, 1}, true}], [{value, "-.123"}, {status, normal}, {{digits, 1}, 16#C0}, {{digits, 2}, 16#06}, {{digits, 3}, 16#5B}, {{digits, 4}, 16#4F}]},
    {[{{dec_pnts, 3}, true}], [{value, "-.12.3"}, {status, normal}, {{digits, 1}, 16#C0}, {{digits, 2}, 16#06}, {{digits, 3}, 16#DB}, {{digits, 4}, 16#4F}]},
    {[{number_base, 16}], [{input, 255}, {{dec_pnts, 1}, false}, {{dec_pnts, 3}, false}], [{value, "  FF"}, {status, normal}, {{digits, 1}, 16#00}, {{digits, 2}, 16#00}, {{digits, 3}, 16#71}, {{digits, 4}, 16#71}]},
    {[{leading_zeros, true}], [{input, 255}, {{dec_pnts, 1}, false}], [{value, "00FF"}, {status, normal}, {{digits, 1}, 16#3F}, {{digits, 2}, 16#3F}, {{digits, 3}, 16#71}, {{digits, 4}, 16#71}]}
  ].

-endif.


% character to 7 segments byte mapping
%     [{$0,16#3F}, {$1,16#06}, {$2,16#5B}, {$3,16#4F}, {$4,16#66}, {$5,16#6D}, 
%     {$6,16#7D}, {$7,16#07}, {$8,16#7F}, {$9,16#6F}, 
%     {$A,16#77}, {$b,16#7C}, {$C,16#39}, {$d,16#5E}, {$E,16#79}, {$F,16#71},
%     {32,16#00}, {$-,16#40}],