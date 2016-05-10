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
           block_state()) -> atom().

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
                  block_state()) -> {atom(), module()}.

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
                          block_state()) -> {atom(), module(), string()}.

name_module_version({Config, _Inputs, _Outputs, _Private}) ->
  {ok, BlockName} = attrib_utils:get_value(Config, block_name),
  {ok, BlockModule} = attrib_utils:get_value(Config, block_module),
  {ok, Version} = attrib_utils:get_value(Config, version),
  {BlockName, BlockModule, Version}.

%%
%% Get configuration value of any type and check for errors.
%%
-spec get_any_type(Config :: list(config_attr()),
                   ValueName :: atom()) -> generic_config_value().

get_any_type(Config, ValueName) ->
  % Return true for every value
  CheckType = fun(_Value) -> true end,
  get_value(Config, ValueName, CheckType).

%%
%% Get an integer configuration value and check for errors.
%%
-spec get_integer_range(Config :: list(config_attr()), 
                        ValueName :: atom(),
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
                  ValueName :: atom()) -> integer_config_value().

get_integer(Config, ValueName) ->
  CheckType = fun is_integer/1,
  get_value(Config, ValueName, CheckType).


%%
%% Get a floating point configuration value and check for errors.
%%
-spec get_float(Config :: list(config_attr()), 
                ValueName :: atom()) -> float_config_value().

get_float(Config, ValueName) ->
  CheckType = fun is_float/1,
  get_value(Config, ValueName, CheckType).
  
  
%%
%% Get a boolean configuration value and check for errors
%%
-spec get_boolean(Config :: list(config_attr()), 
                  ValueName :: atom()) -> boolean_config_value().

get_boolean(Config, ValueName) ->
  CheckType = fun is_boolean/1,
  get_value(Config, ValueName, CheckType).


%%
%% Generic get configuration value, check for errors.
%%
-spec get_value(Config :: list(config_attr()),
                ValueName :: atom(),
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
                                   TargQuant :: integer(),
                                   DefaultValue :: config_value()) -> list(config_attr()).
                             
resize_attribute_array_value(Config, ArrayValueName, TargQuant, DefaultValue)->
  case attrib_utils:get_attribute(Config, ArrayValueName) of
    % If attribute not found, just return Config attribute list unchanged
    {error, not_found} -> Config;
      
    {ok, {ArrayValueName, ArrayValues}} ->
      NewValuesArray = 
          resize_array_value(ArrayValues, TargQuant, DefaultValue),
      attrib_utils:replace_attribute(Config, ArrayValueName, 
                        {ArrayValueName, NewValuesArray})
  end.


%%
%% Resize the array of values to match the target quantity.
%% Always add to or delete from the end of the array.
%% There will always be at least one value in the array. 
%%  
-spec resize_array_value(ValuesArray :: list(config_value()),
                         TargQuant :: integer(),
                         DefaultValue :: config_value()) -> list(config_value()).
                                     
resize_array_value(ValuesArray, TargQuant, DefaultValue)->
  ValuesQuant = length(ValuesArray),
  if ValuesQuant == TargQuant ->
    % quantity of Values in array matches target, nothing to do 
    ValuesArray;
  true ->
    if ValuesQuant < TargQuant ->
      % not enough values, add the required number of default values
      AddedValues = lists:duplicate((TargQuant-ValuesQuant), DefaultValue),
      ValuesArray ++ AddedValues;
    true ->
      %  too many values, split the list and ignore the deleted ones on the end
      {KeepValues, _DeleteValues} = lists:split(TargQuant, ValuesArray),
      KeepValues
    end
  end.


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

% Test configuration value list
test_configs() ->
  [ {float_good, 123.45},
    {float_bad, xyz}
    {integer_good, 12345},
    {integer_bad, "bad"},
    {boolean_good, true},
    {boolean_bad, 0.0},
    {not_active_good, not_active},
    {empty_good, empty},
    {empty_bad, empty, {knot, empty, link}},
    {not_config, 123, [test1,test2]}
  ].
  
  
get_value_test() ->
  TestInputs = test_inputs().
    


-endif.