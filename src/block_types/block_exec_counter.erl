%%% @doc 
%%% Block Type: Execution Counter
%%% Description: Increment/decrement count value output every time block is executed.
%%5              On Reset input or startup, set output value to initial count value
%%%              On block execution, count up/down to final value
%%%              If Rollover config parameter is true, on next execution,
%%%              set Carry output value to true, and reset output value to initial count   
%%%              
%%% @end 

-module(block_exec_counter).

-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/1, create/3, create/5, initialize/1, execute/1, delete/1]).


type_name()-> exec_counter.  

version() -> "0.1.0".   

%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: atom()) -> block_state().

create(BlockName) -> create(BlockName, [], [], [], []).
   
create(BlockName, InitConfig, InitInputs) -> create(BlockName, InitConfig, InitInputs, [],[]).

create(BlockName, InitConfig, InitInputs, InitOutputs, InitPrivate)->

    error_logger:info_msg("Creating: ~p Type: ~p Version: ~s~n", [BlockName, type_name(), version()]),
    
    %% Update Default Config, Input, Output, and Private attribute values 
    %% with the initial values passed into this function.
    %%
    %% If any of the intial attributes do not already exist in the 
    %% default attribute lists, merge_attribute_lists() will create them.
    %% (This is useful for block types where the number of attributes is not fixed)
    
    Config = block_utils:merge_attribute_lists(default_configs(BlockName), InitConfig),
    Inputs = block_utils:merge_attribute_lists(default_inputs(), InitInputs), 
    Outputs = block_utils:merge_attribute_lists(default_outputs(), InitOutputs),
    Private = block_utils:merge_attribute_lists(default_private(), InitPrivate),

    % This is the block state, 
	{BlockName, ?MODULE, Config, Inputs, Outputs, Private}.

%%
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->
    	
    InitialValue = block_utils:get_value(Inputs, initial_value),
    
    % If the Initial input value is a fixed value integer, 
    % We can imediately set the initial block output value, 
    % Otherwise we need to wait for the initial input value to get set, via block execution
    if is_integer(InitialValue) ->
        NewOutputs = block_utils:set_value_status(Outputs, InitialValue, initialized);
    true ->    
       NewOutputs = block_utils:set_value_status(Outputs, not_active, initialized)
    end,
    
    % Perform initial block execution
	{BlockName, BlockModule, Config, Inputs, NewOutputs, Private}.


%%
%%  On each block execution increment/decrement count output
%%
-spec execute(block_state()) -> block_state().

execute({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

    Reset = block_utils:get_boolean(Inputs, reset),
    InitialValue = block_utils:get_integer(Inputs, initial_value),
    FinalValue = block_utils:get_integer(Inputs, final_value),
    Rollover = block_utils:get_boolean(Config, rollover),
    
    % Check for errors on input/config values
    if (Reset == error) orelse (InitialValue == error) orelse 
       (FinalValue == error) orelse (Rollover == error) ->
            Value = not_active, Status = input_error, Carry = not_active; 
        
    true -> % input values are normal, continue with block execution
    
        % Initial and Final values must be integers, can't be empty or not_active
        if (not is_integer(InitialValue)) orelse (not is_integer(FinalValue)) ->
            Value = not_active, Status = input_empty, Carry = not_active; 
 
        true -> 
            CurrentValue = block_utils:get_value(Outputs, value),
            % if Current output value has not been set to a normal integer value yet
            % set it to the initial value, because we have good initial and final input values at this point    
            if not is_integer(CurrentValue) ->
                Value = InitialValue, Status = normal, Carry = false;
                 
            true ->
                if is_boolean(Reset) andalso Reset ->  
                    % Reset input is true, set output value to initial value,
                    Value = InitialValue, Status = normal, Carry = false;
            
                true -> % Reset input is false or missing, increment/decrement counter value output                
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
        end
    end,
    
    % Update outputs        
    NewOutputs = block_utils:set_values(Outputs,  [{value, Value}, {status, Status}, {carry, Carry}]),
    
    {BlockName, BlockModule, Config, Inputs, NewOutputs,Private}.


%% 
%%  Delete the block
%%	
-spec delete(block_state()) -> block_state().

delete({BlockName, BlockModule, Config, Inputs, Outputs, Private}) -> 
    % Perform any block type specific delete functionality here
    {BlockName, BlockModule, Config, Inputs, Outputs, Private}.


%% ====================================================================
%% Internal functions
%% ====================================================================

default_configs(BlockName) -> 
    block_utils:merge_attribute_lists(block_common:configs(BlockName, type_name(), version()), 
                            [
                                {rollover, true} 
                            ]). 
 
default_inputs() -> 
     block_utils:merge_attribute_lists(block_common:inputs(),
                            [
                               {reset, false, ?EMPTY_LINK},
                               {initial_value, 0, ?EMPTY_LINK},
                               {final_value, 9, ?EMPTY_LINK}
                            ]). 
                            
default_outputs() -> 
        block_utils:merge_attribute_lists(block_common:outputs(),
                            [
                              {carry, not_active, []}
                            ]). 
                            
default_private() -> 
        block_utils:merge_attribute_lists(block_common:private(),
                            [
                            ]).
