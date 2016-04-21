%%% @doc 
%%% Get and Validate Block Config values   
%%%               
%%% @end 

-module(lblx_configs).

-author("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_any_type/2, get_integer/2, get_float/2, get_boolean/2]).
-export([get_value/3]).
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
    not_found  -> {error, not_found};
    
    {ValueName, Value, Link} ->
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
%% Log input value error
%%
-spec log_error(Config :: list(),
                ValueName :: atom(),
                Reason :: atom()) -> ok.
                  
log_error(Config, ValueName, Reason) ->
  BlockName = block_utils:name(Config),
  error_logger:error_msg("~p Invalid '~p' input value: ~p~n", 
                            [BlockName, ValueName, Reason]),
  ok.
  
  
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