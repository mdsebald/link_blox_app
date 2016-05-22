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
-export([name/1, module/1, name_module/1, name_module_version/1]).
-export([get_any_type/2, get_integer/2, get_integer_range/4]). 
-export([get_float/2, get_boolean/2]).
-export([get_value/3, resize_attribute_array_value/4]).
-export([log_error/3]).

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
%%  Get block name and module from Config attribute values
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
                   ValueName :: value_name()) -> generic_config_value().

get_any_type(Config, ValueName) ->
  % Return true for every value
  CheckType = fun(_Value) -> true end,
  get_value(Config, ValueName, CheckType).

%%
%% Get an integer configuration value and check for errors.
%%
-spec get_integer_range(Config :: list(config_attr()), 
                        ValueName :: value_name(),
                        Min :: integer(),
                        Max :: integer()) -> integer_config_value().

get_integer_range(Config, ValueName, Min, Max) ->
  CheckType = fun is_integer/1,
  case get_value(Config, ValueName, CheckType) of
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
                  ValueName :: value_name()) -> integer_config_value().

get_integer(Config, ValueName) ->
  CheckType = fun is_integer/1,
  get_value(Config, ValueName, CheckType).


%%
%% Get a floating point configuration value and check for errors.
%%
-spec get_float(Config :: list(config_attr()), 
                ValueName :: value_name()) -> float_config_value().

get_float(Config, ValueName) ->
  CheckType = fun is_float/1,
  get_value(Config, ValueName, CheckType).
  
  
%%
%% Get a boolean configuration value and check for errors
%%
-spec get_boolean(Config :: list(config_attr()), 
                  ValueName :: value_name()) -> boolean_config_value().

get_boolean(Config, ValueName) ->
  CheckType = fun is_boolean/1,
  get_value(Config, ValueName, CheckType).


%%
%% Generic get configuration value, check for errors.
%%
-spec get_value(Config :: list(config_attr()),
                ValueName :: value_name(),
                CheckType :: fun()) -> term().
                
get_value(Config, ValueName, CheckType) ->
  case attrib_utils:get_attribute(Config, ValueName) of
    {error, not_found}  -> {error, not_found};
    
    {ok, {ValueName, {Value}}} ->
      case CheckType(Value) of
        true  -> {ok, Value};
        false -> {error, bad_type}   
      end;
    % Attribute value was not a config value  
    _ -> {error, not_config}
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
% Test data
%
test_config_attribs1() ->
  [ {block_name, {test_config_utils}},
    {block_module, {type_test}},
    {version, {"0.0.0"}},
    {description, {"Unit Testing Data"}},
    {number1, {123.45}},
    {string1, {"Testing"}},
    {bool_array, [{true}, {false}]},
    {integer1, {123}},
    {integer_array, [{234}, {456}, {-123}]},
    {float_good, {123.45}},
    {float_bad, {xyz}},
    {integer_good, {12345}},
    {integer_bad, {"bad"}},
    {boolean_good, {true}},
    {boolean_bad, {0.0}},
    {not_active_good, {not_active}},
    {empty_good, {empty}},
    {empty_bad, {empty, {knot, empty, link}}},
    {not_config, {123, [test1,test2]}}
  ].

% ====================================================================

% ====================================================================
% Test name()
% 
name_config_test()->
  Config = test_config_attribs1(),
  
  ExpectedResult = test_config_utils,
  
  Result = name(Config),
  ?assertEqual(ExpectedResult, Result).
  
name_block_defn_test()->
  Config = test_config_attribs1(),
  Inputs = [],
  Outputs = [],
  BlockDefn = {Config, Inputs, Outputs},
  
  ExpectedResult = test_config_utils,
  
  Result = name(BlockDefn),
  ?assertEqual(ExpectedResult, Result).
  
name_block_state_test()->
  Config = test_config_attribs1(),
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
  Config = test_config_attribs1(),
  
  ExpectedResult = {test_config_utils, type_test},
  
  Result = name_module(Config),
  ?assertEqual(ExpectedResult, Result).
  
name_module_block_defn_test()->
  Config = test_config_attribs1(),
  Inputs = [],
  Outputs = [],
  BlockDefn = {Config, Inputs, Outputs},
  
  ExpectedResult = {test_config_utils, type_test},
  
  Result = name_module(BlockDefn),
  ?assertEqual(ExpectedResult, Result).
  
name_module_block_state_test()->
  Config = test_config_attribs1(),
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
  Config = test_config_attribs1(),
  
  ExpectedResult = {test_config_utils, type_test, "0.0.0"},
  
  Result = name_module_version(Config),
  ?assertEqual(ExpectedResult, Result).
  
name_module_version_block_defn_test()->
  Config = test_config_attribs1(),
  Inputs = [],
  Outputs = [],
  BlockDefn = {Config, Inputs, Outputs},
  
  ExpectedResult = {test_config_utils, type_test, "0.0.0"},
  
  Result = name_module_version(BlockDefn),
  ?assertEqual(ExpectedResult, Result).
  
name_module_version_block_state_test()->
  Config = test_config_attribs1(),
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
  Config = test_config_attribs1(),
  ValueName = integer_good,
  
  ExpectedResult = {error, range},
  
  Result = get_integer_range(Config, ValueName, 1, 100),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================
  


get_value_test() ->
  _Config = test_config_attribs1().


% ====================================================================
% Test log_error()
%     
log_error_test() ->
  Config = test_config_attribs1(),
  
  ExpectedResult =  {not_active, config_err},
  
  Result = log_error(Config, value_name, bad_value),
  ?assertEqual(ExpectedResult, Result) .
% ====================================================================

-endif.