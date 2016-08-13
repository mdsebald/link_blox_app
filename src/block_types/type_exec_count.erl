%%% @doc 
%%% Block Type: Execution Counter
%%% Description: Increment/decrement count value output every time block is executed.
%%5              On initialize or Reset input is true, set output value to initial count value
%%%              On block execution, count up/down to final value
%%%              If Rollover config parameter is true, on next execution,
%%%              set Carry output value to true, and reset output value to initial count 
%%%              Carry output value is false for every other case  
%%%              
%%% @end 

-module(type_exec_count).

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([description/0, version/0]). 
-export([create/2, create/4, create/5, initialize/1, execute/1, delete/1]).


description() -> "Incr/Decr counter output value on block execution".

version() -> "0.1.0".   


%% Merge the block type specific, Config, Input, Output, and Private attributes
%% with the common Config, Input, Output, and Private attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {rollover, {true}} 
    ]).
                            
                             
-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {reset, {false, ?EMPTY_LINK}},
      {initial_value, {0, ?EMPTY_LINK}},
      {final_value, {9, ?EMPTY_LINK}}
    ]). 

-spec default_outputs() -> list(output_attr()).
                           
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {carry, {not_active, []}}
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

create(BlockName, Description, InitConfig, InitInputs, InitOutputs)->

  %% Update Default Config, Input, Output, and Private attribute values 
  %% with the initial values passed into this function.
  %%
  %% If any of the intial attributes do not already exist in the 
  %% default attribute lists, merge_attribute_lists() will create them.
  %% (This is useful for block types where the number of attributes is not fixed)
    
  Config = attrib_utils:merge_attribute_lists(default_configs(BlockName, Description), InitConfig),
  Inputs = attrib_utils:merge_attribute_lists(default_inputs(), InitInputs), 
  Outputs = attrib_utils:merge_attribute_lists(default_outputs(), InitOutputs),

  % This is the block definition, 
  {Config, Inputs, Outputs}.

%%
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  {ok, InitialValue} = attrib_utils:get_value(Inputs, initial_value),
    
  % If the Initial input value is a fixed value integer, 
  % We can imediately set the initial block output value, 
  % Otherwise we need to wait for the initial input value to get set, via block execution
  if is_integer(InitialValue) ->
    Outputs1 = output_utils:set_value_status(Outputs, InitialValue, initialed);
  true ->    
     Outputs1 = output_utils:set_value_status(Outputs, not_active, initialed)
  end,
    
  {Config, Inputs, Outputs1, Private}.


%%
%%  On each block execution increment/decrement count output
%%
-spec execute(block_state()) -> block_state().

execute({Config, Inputs, Outputs, Private}) ->

  case input_utils:get_boolean(Inputs, reset) of
    {ok, Reset} ->
      
      case  input_utils:get_integer(Inputs, initial_value) of
        {ok, InitialValue} ->
        
          case input_utils:get_integer(Inputs, final_value) of
            {ok, FinalValue} ->
               
              case config_utils:get_boolean(Config, rollover) of
                {ok, Rollover} ->
                  % Initial and Final values must be integers, can't be empty or not_active
                  if (not is_integer(InitialValue)) orelse (not is_integer(FinalValue)) ->
                    Value = not_active, Status = no_input, Carry = not_active;
                  true ->
                    % Input and Config values are good
                    {ok, CurrentValue} = attrib_utils:get_value(Outputs, value),
                    % if Current output value has not been set 
                    % to a normal integer value yet,set it to the initial value, 
                    % because we have good initial and final input values at this point    
                    if not is_integer(CurrentValue) ->
                      Value = InitialValue, Status = normal, Carry = false;
                    true ->
                      if is_boolean(Reset) andalso Reset ->  
                        % Reset input is true, set output value to initial value,
                        Value = InitialValue, Status = normal, Carry = false;
                      true -> % Reset input is false or missing, 
                              % increment/decrement counter value output                
                        if (CurrentValue == FinalValue) ->
                          if is_boolean(Rollover) andalso Rollover -> 
                            Value = InitialValue, Status = normal, Carry = true;

                          true -> % Reset input is off and rollover config is off
                                  % Hold count value output at final value
                            Value = CurrentValue, Status = normal, Carry = false 
                          end;

                        true -> % Count has not reached final value, 
                          % Determine if count should be incremented or decremented
                          if (InitialValue < FinalValue) -> % Count up
                            Value = CurrentValue + 1, Status = normal, Carry = false;
                          true ->
                            if (FinalValue < InitialValue) -> % Count down
                              Value = CurrentValue - 1, Status = normal, Carry = false;
                            true -> % Inital value and final value are equal
                              % Hold count at current value
                              Value = CurrentValue, Status = normal, Carry = false
                            end
                          end
                        end
                      end
                    end    
                  end; 
                    
                {error, Reason} ->
                  {Value, Status} = config_utils:log_error(Config, rollover, Reason),
                  Carry = not_active
              end;
            {error, Reason} ->
              {Value, Status} = input_utils:log_error(Config, final_value, Reason),
              Carry = not_active
          end;
        {error, Reason} -> 
          {Value, Status} = input_utils:log_error(Config, initial_value, Reason),
          Carry = not_active
      end;
    {error, Reason} ->
      {Value, Status} = input_utils:log_error(Config, reset, Reason),
      Carry = not_active
  end,
  
  % Update outputs        
  {ok, Outputs1} = attrib_utils:set_values(Outputs,
                  [{value, Value}, {status, Status}, {carry, Carry}]),
    
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockValues :: block_state()) -> block_state().

delete({Config, Inputs, Outputs, _Private}) -> 
  {Config, Inputs, Outputs}.


%% ====================================================================
%% Internal functions
%% ====================================================================
