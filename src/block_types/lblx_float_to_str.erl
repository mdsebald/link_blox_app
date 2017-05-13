%%% @doc 
%%% Block Type:  Float to String
%%% Description: Convert floating point input value to a string
%%%               
%%% @end 

-module(lblx_float_to_str).

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [string, conversion].

description() -> "Convert floating point value to a string".

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {left_justify, {false}},
      {field_width, {0}},
      {precision, {0}}
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {input, {empty, ?EMPTY_LINK}}
    ]). 


-spec default_outputs() -> list(output_attr()).
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [

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
             InitOutputs :: list(output_attr())) -> block_defn().

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

  case config_utils:get_boolean(Config, left_justify) of
    {ok, LeftJustify} ->

      case config_utils:get_integer_range(Config, field_width, 0, 120) of
        {ok, FieldWidth} ->

          case config_utils:get_integer_range(Config, precision, 0, 120) of
            {ok, Precision} ->
              FormatStr = build_format_str(LeftJustify, FieldWidth, Precision),
              Private1 = attrib_utils:add_attribute(Private, {format_str, {FormatStr}}),
              Value = null,
              Status = initialed;

            {error, Reason} ->
              Private1 = Private,
              {Value, Status} = config_utils:log_error(Config, precision, Reason)          
          end;
        
        {error, Reason} ->
          Private1 = Private,
          {Value, Status} = config_utils:log_error(Config, field_width, Reason)          
      end;

    {error, Reason} ->
      Private1 = Private,
      {Value, Status} = config_utils:log_error(Config, left_justify, Reason)          
  end,

  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  % This is the block state
  {Config, Inputs, Outputs1, Private1}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  
  case input_utils:get_float(Inputs, input) of
    {ok, null} ->
      Value = null,
      Status = normal;

    {ok, InputValue} ->
      {ok, FormatStr} = attrib_utils:get_value(Private, format_str),
      Value = lists:flatten(io_lib:format(FormatStr, [InputValue])),
      Status = normal;

    {error, Reason} ->
      {Value, Status} = input_utils:log_error(Config, input, Reason)
  end,

  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),

  % Return updated block state
  {Config, Inputs, Outputs1, Private}.


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
% Format floating point input value
%
-spec build_format_str(LeftJustify :: boolean(),
                       FieldWidth :: integer(),
                       Precision :: integer()) -> string().

build_format_str(LeftJustify, FieldWidth, Precision) ->
  add_precision(Precision, 
    add_field_width(FieldWidth, 
      add_left_justified(LeftJustify, "\~"))) ++ "f".

      
add_left_justified(LeftJustify, FormatStr) ->
  case LeftJustify of
    true -> FormatStr ++ "-";
       _ -> FormatStr
  end.

add_field_width(FieldWidth, FormatStr) ->
  case FieldWidth of
    0 -> FormatStr ++ ".";
    _ -> lists:flatten(io_lib:format("~s~b.", [FormatStr, FieldWidth]))
  end.

add_precision(Precision, FormatStr) ->
  case Precision of
    0 -> FormatStr;
    _ -> lists:flatten(io_lib:format("~s~b", [FormatStr, Precision]))
  end.



%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% Perform minimum block unit test

block_test() ->
  unit_test_utils:min_block_test(?MODULE).


-endif.