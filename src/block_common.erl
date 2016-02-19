%%
%% @author Mark
%% @doc Block functions common to all block types 
%% 


-module(block_common).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/3, configs/3, inputs/0, outputs/0, internals/0, initialize/3, execute/5, delete/2]).


%% 
%% Common block Create function
%% Return initial set of block values, containing 
%% block values that are common for all block types
%%

create(Name, Type, Version) ->
    
    % Common Configs
    BlockName = {block_name, Name},
	BlockType = {block_type, Type},
    
    log_state("Creating", [BlockName, BlockType]),
    
	BlockVersion = {version, Version},
    
    Executors = {executors, []},  % List of other blocks that are allowed execute this block.
                                    % If this list contains one or more block names, 
                                    % then execute only on receiving an execute command message
                                    % i.e. Implement Control Flow 

    Timeout = {timeout, 0},  % If > 0, execute block every 'Timeout' milliseconds.
                               % This is used to execute a block at fixed intervals
                               % instead of being executed by other blocks.
                               % or executed on change of input value
    
    % Comnon Inputs
	Enable = {enable, true, {fixed, null, null}},

    % Common Outputs
	Value = {value, not_active, []},
	Status = {status, created, []},
    
    % Common Internals
	ExecCount = {exec_count, 0},
	LastExec = {last_exec, not_active},
    TimerRef = {timer_ref, empty},
	
	% Return a set of initial common block values consisting of:
    % Configs, Inputs, Outputs, and Internal values
    
    { 
      [BlockName, BlockType, BlockVersion, Executors, Timeout],  % Configs
	  [Enable], % Inputs
	  [Value, Status], % Outputs
      [ExecCount, LastExec, TimerRef] % Internals
    }.


%%
%% Create list of common block config attributes
%%
configs(Name, Type, Version) ->
    Configs = 
    [ 
      {block_name, Name},
	  {block_type, Type},
	  {version, Version},
    
      {executors, []},  % List of other blocks that are allowed execute this block.
                          % If this list contains one or more block names, 
                          % then execute only on receiving an execute command message
                          % i.e. Implement Control Flow 

      {timeout, 0}  % If > 0, execute block every 'Timeout' milliseconds.
    ],                % This is used to execute a block at fixed intervals
                      % instead of being executed by other blocks.
                      % or executed on change of input value
    
    log_state("Creating", Configs),
    Configs.


%%
%% Common Inputs
%%
inputs() ->
    [
      {enable, true, {fixed, null, null}} % Block will execute as long as enable input is true
    ].
    
%%
%% Common Outputs
%%
outputs() ->
    [ 
      {value, not_active, []},
	  {status, created, []}
    ].
    
%%
%% Common Internals
%%
internals() ->
    [ 
      {exec_count, 0},
	  {last_exec, not_active},
      {timer_ref, empty}
    ].
       



%%
%% Common block initialization funcion
%%

initialize(Configs, Outputs, Internals) ->

    log_state("Initializing", Configs),
    NewInternals = setup_execute_timer(Configs, Internals), 
    NewOutputs = block_utils:set_output_value(Outputs, status, initialized),
    
    {NewOutputs, NewInternals}.


%%
%% Common block execute function
%% Update the Output values common to all blocks, Value, Status, Execute Count, Last Executed Time
%%
execute(Configs, Outputs, Internals, Value, Status) ->

	case block_utils:get_output_value(Outputs, status) of
        normal ->
	       NewInternals1 = update_exec_count(Internals),
	       NewInternals2 = block_utils:set_internal_value(NewInternals1, last_exec, calendar:now_to_local_time(erlang:timestamp())),
           NewInternals3 = setup_execute_timer(Configs, NewInternals2);
        _ -> % block is not executing freeze the exec_count and last_exec time
           NewInternals3 = Internals 
    end,
    
    % Now update the value and status outputs
    NewOutputs1 = block_utils:set_output_value(Outputs, value, Value),
	NewOutputs2 = block_utils:set_output_value(NewOutputs1, status, Status),

    {NewOutputs2, NewInternals3}.


%%
%%  Common block delete function
%%
delete(Configs, Internals) ->
    log_state("Deleting", Configs),    
    % Cancel execution timer if it exists
    TimerReferenceValue = block_utils:get_internal_value(Internals, timer_ref),
    case TimerReferenceValue of
        empty -> empty;
        
        _ ->  timer:cancel(TimerReferenceValue)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

setup_execute_timer(Configs, Internals) ->
    case block_utils:get_config_value(Configs, executors) of
        [] ->  % The list of executors is empty, check if this block should be executed via timer
            ExecuteTimerValue = block_utils:get_config_value(Configs, timeout),
            if (ExecuteTimerValue > 0 ) ->
                BlockName = block_utils:get_config_value(Configs, block_name),
                % Setup timer to execute block after timer expires
                case timer:apply_after(ExecuteTimerValue, 'block_server', execute, [BlockName]) of
                    {ok, TimerReferenceValue} -> 
                        NewInternals = block_utils:set_internal_value(Internals, timer_ref, TimerReferenceValue);
         
                    {error, Reason} -> 
                        NewInternals = block_utils:set_internal_value(Internals, timer_ref, empty),
                        io:format("Error: ~p Setting execution timer ~n", [Reason])
                end;
            true ->  % Timeout value is less than or equal to zero, don't set up execution via timer
                NewInternals = Internals
            end;
            
        _ ->  % there is something in the executors list, don't setup execution via timer  
            NewInternals = Internals
    end,
    NewInternals.


update_exec_count(Internals) ->
	% Arbitrarily roll over Execution Counter at 1,000,000,000
	case block_utils:get_internal_value(Internals, exec_count) + 1 of
		1000000000   -> block_utils:set_internal_value(Internals, exec_count, 0);
		NewExecCount -> block_utils:set_internal_value(Internals, exec_count, NewExecCount)
	end.

%% print the indicated state to the shell
log_state (State, Configs) ->
    BlockName = block_utils:get_config_value(Configs, block_name),
    BlockType = block_utils:get_config_value(Configs, block_type),
    
    io:format("~s: ~p Type: ~p~n", [State, BlockName, BlockType]).
    