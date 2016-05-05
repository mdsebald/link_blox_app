%%% @doc 
%%% Get and Validate Block Input values   
%%%               
%%% @end 

-module(input_utils).

-author("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_any_type/2, get_integer/2, get_float/2, get_boolean/2]).
-export([get_value/3, resize_attribute_array_value/4]).
-export([log_error/3]).


%%
%% Get input value of any type and check for errors.
%%
-spec get_any_type(Inputs :: list(),
                   ValueName :: atom()) -> generic_input_value().

get_any_type(Inputs, ValueName) ->
  % Return true for every value
  CheckType = fun(_Value) -> true end,
  get_value(Inputs, ValueName, CheckType).

%%
%% Get an integer input value and check for errors.
%%
-spec get_integer(Inputs :: list(), 
                  ValueName :: atom()) -> integer_input_value().

get_integer(Inputs, ValueName) ->
  CheckType = fun is_integer/1,
  get_value(Inputs, ValueName, CheckType).


%%
%% Get a floating point input value and check for errors.
%%
-spec get_float(Inputs :: list(), 
                ValueName :: atom()) -> float_input_value().

get_float(Inputs, ValueName) ->
  CheckType = fun is_float/1,
  get_value(Inputs, ValueName, CheckType).
  
  
%%
%% Get a boolean input value and check for errors
%%
-spec get_boolean(Inputs :: list(), 
                  ValueName :: atom()) -> boolean_input_value().

get_boolean(Inputs, ValueName) ->
  CheckType = fun is_boolean/1,
  get_value(Inputs, ValueName, CheckType).


%%
%% Generic get input value, check for errors.
%%
-spec get_value(Inputs :: list(),
                ValueName :: atom(),
                CheckType :: fun()) -> term().
                
get_value(Inputs, ValueName, CheckType) ->
  case block_utils:get_attribute(Inputs, ValueName) of
    {error, not_found}  -> {error, not_found};
    
    {ok, {ValueName, {Value, Link}}} ->
      case Value of
        % not_active is a valid value
        not_active -> {ok, not_active};
        
        empty ->   
          case Link of
            % if the input value is empty and the link is empty
            % treat this like a not_active value
            ?EMPTY_LINK -> {ok, not_active};
            % input is linked to another block but value is empty,
            % this is an error
            _ -> {error, bad_link}
          end;
        
        Value ->
          case CheckType(Value) of
            true  -> {ok, Value};
            false -> {error, bad_type}
          end   
      end;
    % Attribute value was not an input value  
    _ -> {error, not_input}    
  end.
  

%%
%% Resize an array value in the Inputs attribute list
%% If there are fewer values in the array than the target quantity,
%% add values to the array using the DefaultValue
%% If there are more values in the array than the target Quantity,
%% and delete the excess values
%% Returns updated Inputs attribute list
%%
-spec resize_attribute_array_value(BlockName :: atom(),
                                   Inputs :: list(),
                                   TargQuant :: integer(),
                                   ArrayValueName :: atom(),
                                   DefaultValue :: term()) -> list().
                             
resize_attribute_array_value(BlockName, Configs, TargQuant, ArrayValueName, DefaultValue)->
  case block_utils:get_attribute(Configs, ArrayValueName) of
    % If attribute not found, just return Configs attribute list unchanged
    {error, not_found} -> Configs;
      
    {ok, {ArrayValueName, ArrayValues}} ->
      NewValuesArray = 
          resize_array_value(BlockName, ArrayValueName, ArrayValues, 
                             TargQuant, DefaultValue),
      block_utils:replace_attribute(Configs, ArrayValueName, 
                        {ArrayValueName, NewValuesArray})
  end.


%%
%% Resize the array of values to match the target quantity.
%% Always add to or delete from the end of the array.
%% There will always be at least one value in the array. 
%%  
-spec resize_array_value(BlockName :: atom(),
                         ArrayValueName :: atom(), 
                         ValuesArray :: list(),
                         TargQuant :: integer(),
                         DefaultValue :: term()) -> list().
                                     
resize_array_value(BlockName, ArrayValueName, ValuesArray, TargQuant, DefaultValue)->
  ValuesQuant = length(ValuesArray),
  if ValuesQuant =:= TargQuant ->
    % quantity of Values in array matches target, nothing to do 
    ValuesArray;
  true ->
    if ValuesQuant < TargQuant ->
      % not enough values, add the required number of default values
      AddedValues = lists:duplicate((TargQuant-ValuesQuant), DefaultValue),
      ValuesArray ++ AddedValues;
    true ->
      %  too many values, split the list and ignore the deleted ones on the end
      {KeepValues, DeleteValues} = lists:split(TargQuant, ValuesArray),
      fun(DeleteInput) -> link_utils:unlink(BlockName, {DeleteInput})
      KeepValues
    end
  end.

  
%%
%% Log input value error
%%
-spec log_error(Config :: list(),
                ValueName :: atom(),
                Reason :: atom()) -> {not_active, input_err}.
                  
log_error(Config, ValueName, Reason) ->
  BlockName = config_utils:name(Config),
  error_logger:error_msg("~p Invalid '~p' input value: ~p~n", 
                            [BlockName, ValueName, Reason]),
  {not_active, input_err}.
  
  
%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% Test input value list
test_inputs() ->
  [ {float_good, 123.45, {null, block1, value}},
    {float_bad, xyz, ?EMPTY_LINK}
    {integer_good, 12345, {null, block2, value}},
    {integer_bad, "bad", ?EMPTY_LINK},
    {boolean_good, true, ?EMPTY_LINK},
    {boolean_bad, 0.0, ?EMPTY_LINK},
    {not_active_good, not_active, ?EMPTY_LINK},
    {empty_good, empty, ?EMPTY_LINK},
    {empty_bad, empty, {knot, empty, link}},
    {not_input, 123, [test1,test2]}
  ].
  
  
get_value_test() ->
  TestInputs = test_inputs().
    


-endif.