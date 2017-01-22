%%% @doc 
%%% Get and Validate Block Config values   
%%%               
%%% @end 

-module(config_utils).

-author("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
          name/1, 
          module/1, 
          name_module/1, 
          name_module_version/1,
          get_any_type/2, 
          get_integer/2, 
          get_integer_range/4, 
          get_float/2, 
          get_boolean/2,
          get_string/2,
          get_value/3, 
          resize_attribute_array_value/4,
          log_error/3
]).

%%
%%  Get block name from Config attribute values
%%
-spec name(Config :: list(config_attr()) |
           block_defn() | 
           block_state()) -> block_name().

name({Config, _Inputs, _Outputs, _Private}) ->
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  BlockName;
  
name({Config, _Inputs, _Outputs}) ->  
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  BlockName;
  
name(Config) ->
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  BlockName.

  
%%
%% Get block module from Config attribute values 
%%
-spec module(Config :: list(config_attr()) | 
             block_defn() | 
             block_state()) -> module().

module({Config, _Inputs, _Outputs, _Private}) ->
  {ok, BlockModule} = attrib_utils:get_value(Config, block_module),
  BlockModule;
  
module({Config, _Inputs, _Outputs}) ->
  {ok, BlockModule} = attrib_utils:get_value(Config, block_module),
  BlockModule;

module(Config) ->
  {ok, BlockModule} = attrib_utils:get_value(Config, block_module),
  BlockModule.


%%
%%  Get block name and module from Config attribute values
%%
-spec name_module(Config :: list(config_attr()) |
                  block_defn() | 
                  block_state()) -> {block_name(), module()}.

name_module({Config, _Inputs, _Outputs, _Private}) ->
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  {ok, BlockModule} = attrib_utils:get_value(Config, block_module),
  {BlockName, BlockModule};
  
name_module({Config, _Inputs, _Outputs}) ->  
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  {ok, BlockModule} =  attrib_utils:get_value(Config, block_module),
  {BlockName, BlockModule};
                       
name_module(Config) ->
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  {ok, BlockModule} =  attrib_utils:get_value(Config, block_module),
  {BlockName, BlockModule}.
  
  
%%
%%  Get block name, module, and version from Config attribute values
%%
-spec name_module_version(Config :: list(config_attr()) |
                          block_defn() | 
                          block_state()) -> {block_name(), module(), string()}.

name_module_version({Config, _Inputs, _Outputs, _Private}) ->
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  {ok, BlockModule} = attrib_utils:get_value(Config, block_module),
  {ok, Version} = attrib_utils:get_value(Config, version),
  {BlockName, BlockModule, Version};
  
name_module_version({Config, _Inputs, _Outputs}) ->  
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  {ok, BlockModule} =  attrib_utils:get_value(Config, block_module),
  {ok, Version} = attrib_utils:get_value(Config, version),
  {BlockName, BlockModule, Version};
                       
name_module_version(Config) ->
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  {ok, BlockModule} =  attrib_utils:get_value(Config, block_module),
  {ok, Version} = attrib_utils:get_value(Config, version),
  {BlockName, BlockModule, Version}.

%%
%% Get configuration value of any type and check for errors.
%%
-spec get_any_type(Config :: list(config_attr()),
                   ValueId :: value_id()) -> generic_config_value().

get_any_type(Config, ValueId) ->
  % Return true for every value
  CheckType = fun(_Value) -> true end,
  get_value(Config, ValueId, CheckType).

%%
%% Get an integer configuration value and check for errors.
%%
-spec get_integer_range(Config :: list(config_attr()), 
                        ValueId :: value_id(),
                        Min :: integer(),
                        Max :: integer()) -> integer_config_value().

get_integer_range(Config, ValueId, Min, Max) ->
  CheckType = fun is_integer/1,
  case get_value(Config, ValueId, CheckType) of
    {error, Reason} -> 
      {error, Reason};
    {ok, Value} ->
      if (Value < Min ) orelse (Max < Value) ->
        {error, range};
      true -> 
        {ok, Value}
      end
  end.
       

-spec get_integer(Config :: list(config_attr()), 
                  ValueId :: value_id()) -> integer_config_value().

get_integer(Config, ValueId) ->
  CheckType = fun is_integer/1,
  get_value(Config, ValueId, CheckType).


%%
%% Get a floating point configuration value and check for errors.
%%
-spec get_float(Config :: list(config_attr()), 
                ValueId :: value_id()) -> float_config_value().

get_float(Config, ValueId) ->
  CheckType = fun is_float/1,
  get_value(Config, ValueId, CheckType).
  
  
%%
%% Get a boolean configuration value and check for errors
%%
-spec get_boolean(Config :: list(config_attr()), 
                  ValueId :: value_id()) -> boolean_config_value().

get_boolean(Config, ValueId) ->
  CheckType = fun is_boolean/1,
  get_value(Config, ValueId, CheckType).


%%
%% Get a string configuration value and check for errors
%%
-spec get_string(Config :: list(config_attr()), 
                ValueId :: value_id()) -> string_config_value().

get_string(Config, ValueId) ->
  CheckType = fun block_utils:is_string/1,  
  get_value(Config, ValueId, CheckType).


%%
%% Generic get configuration value, check for errors.
%%
-spec get_value(Config :: list(config_attr()),
                ValueId :: value_id(),
                CheckType :: fun()) -> term().
                
get_value(Config, ValueId, CheckType) ->
   % get the whole attribute, not just the value
  case attrib_utils:get_attribute(Config, ValueId) of
    {error, not_found}  -> {error, not_found};
    
    % If this is a non-array value, check it
    {ok, {_ValueName, {Value}}} ->
      case CheckType(Value) of
        true  -> {ok, Value};
        false -> {error, bad_type}   
      end;

    % if this is an array value, check the index, and get the nth element
    {ok, {ValueName, ArrayValue}} ->
      % if this is an array value, the ValueName from get_attribute()
      % will match ValueName in the ValueId tuple
      case ValueId of
        {ValueName, ArrayIndex} ->
          if (0 < ArrayIndex) andalso (ArrayIndex =< length(ArrayValue)) ->
            {Value} = lists:nth(ArrayIndex, ArrayValue),
            case CheckType(Value) of
              true  -> {ok, Value};
              false -> {error, bad_type}   
            end;
          true->  % array index is out of bounds
            {error, invalid_index}
          end;
        _ -> {error, invalid_value}
      end
  end.
 

%%
%% Resize an array value in the Config attribute list
%% to match the target quantity
%% Returns updated Config attribute list
%%
-spec resize_attribute_array_value(Config :: list(config_attr()),
                                   ArrayValueName :: value_name(),
                                   TargQuant :: pos_integer(),
                                   DefaultValue :: config_value()) -> list(config_attr()).
                             
resize_attribute_array_value(Config, ArrayValueName, TargQuant, DefaultValue)->
  attrib_utils:resize_attribute_array_value(Config, ArrayValueName, TargQuant, DefaultValue).


%%
%% Log config value error
%%
-spec log_error(Config :: list(),
                ValueName :: atom(),
                Reason :: atom()) -> {not_active, config_err}.
                  
log_error(Config, ValueName, Reason) ->
  BlockName = name(Config),
  error_logger:error_msg("~p Invalid '~p' config value: ~p~n", 
                            [BlockName, ValueName, Reason]),
  {not_active, config_err}.
  
  
%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% ====================================================================
% Test name()
% 
name_config_test()->
  Config = test_data:config_utils_config_attribs1(),
  
  ExpectedResult = test_config_utils,
  
  Result = name(Config),
  ?assertEqual(ExpectedResult, Result).
  
name_block_defn_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  BlockDefn = {Config, Inputs, Outputs},
  
  ExpectedResult = test_config_utils,
  
  Result = name(BlockDefn),
  ?assertEqual(ExpectedResult, Result).
  
name_block_state_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  Private = [],
  BlockState = {Config, Inputs, Outputs, Private},
  
  ExpectedResult = test_config_utils,
  
  Result = name(BlockState),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================

% ====================================================================
% Test name_module()
% 
name_module_config_test()->
  Config = test_data:config_utils_config_attribs1(),
  
  ExpectedResult = {test_config_utils, type_test},
  
  Result = name_module(Config),
  ?assertEqual(ExpectedResult, Result).
  
name_module_block_defn_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  BlockDefn = {Config, Inputs, Outputs},
  
  ExpectedResult = {test_config_utils, type_test},
  
  Result = name_module(BlockDefn),
  ?assertEqual(ExpectedResult, Result).
  
name_module_block_state_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  Private = [],
  BlockState = {Config, Inputs, Outputs, Private},
  
  ExpectedResult = {test_config_utils, type_test},
  
  Result = name_module(BlockState),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================

% ====================================================================
% Test name_module_version()
% 
name_module_version_config_test()->
  Config = test_data:config_utils_config_attribs1(),
  
  ExpectedResult = {test_config_utils, type_test, "0.0.0"},
  
  Result = name_module_version(Config),
  ?assertEqual(ExpectedResult, Result).
  
name_module_version_block_defn_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  BlockDefn = {Config, Inputs, Outputs},
  
  ExpectedResult = {test_config_utils, type_test, "0.0.0"},
  
  Result = name_module_version(BlockDefn),
  ?assertEqual(ExpectedResult, Result).
  
name_module_version_block_state_test()->
  Config = test_data:config_utils_config_attribs1(),
  Inputs = [],
  Outputs = [],
  Private = [],
  BlockState = {Config, Inputs, Outputs, Private},
  
  ExpectedResult = {test_config_utils, type_test, "0.0.0"},
  
  Result = name_module_version(BlockState),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================

% ====================================================================
% Test get_integer_range()
% 
get_integer_range_test()->
  Config = test_data:config_utils_config_attribs1(),
  ValueName = integer_good,
  
  ExpectedResult = {error, range},
  
  Result = get_integer_range(Config, ValueName, 1, 100),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================
  


get_value_test() ->
  _Config = test_data:config_utils_config_attribs1().


% ====================================================================
% Test log_error()
%     
log_error_test() ->
  Config = test_data:config_utils_config_attribs1(),
  
  ExpectedResult =  {not_active, config_err},
  
  Result = log_error(Config, value_name, bad_value),
  ?assertEqual(ExpectedResult, Result) .
% ====================================================================

-endif.