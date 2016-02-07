%%
%% @author Mark
%% @doc Block functions common to all block types 
%% 


-module(block_common).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/3, initialize/3, execute/5, delete/2]).


%% 
%% Common block Create function
%% Return initial set of block values, containing 
%% block values that are common for all block types
%%

create(Name, Type, Version) ->
    
    % Common Configs
    BlockName = {'BlockName', Name},
	BlockType = {'BlockType', Type},
    
    log_state("Creating", [BlockName, BlockType]),
    
	BlockVersion = {'Version', Version},
    
    Executors = {'Executors', []},  % List of other blocks that are allowed execute this block.
                                    % If this list contains one or more block names, 
                                    % then execute only on receiving an execute command message
                                    % i.e. Implement Control Flow 

    Timeout = {'Timeout', 0},  % If > 0, execute block every 'Timeout' milliseconds.
                               % This is used to execute a block at fixed intervals
                               % instead of being executed by other blocks.
                               % or executed on change of input value
    
    % Comnon Inputs
	Enable = {'Enable', true, {fixed, null, null}},

    % Common Outputs
	Value = {'Value', not_active, []},
	Status = {'Status', created, []},
    
    % Common Internals
	ExecCount = {'ExecCount', 0},
	LastExec = {'LastExec', not_active},
    TimerRef = {'TimerRef', empty},
	
	% Return a set of initial common block values consisting of:
    % Configs, Inputs, Outputs, and Internal values
    
    { 
      [BlockName, BlockType, BlockVersion, Executors, Timeout],  % Configs
	  [Enable], % Inputs
	  [Value, Status], % Outputs
      [ExecCount, LastExec, TimerRef] % Internals
    }.


%%
%% Common block initialization funcion
%%

initialize(Configs, Outputs, Internals) ->

    log_state("Initializing", Configs),
    NewInternals = setup_execute_timer(Configs, Internals), 
    NewOutputs = block_utils:set_output_value(Outputs, 'Status', initialized),
    
    {NewOutputs, NewInternals}.


%%
%% Common block execute function
%% Update the Output values common to all blocks, Value, Status, Execute Count, Last Executed Time
%%
execute(Configs, Outputs, Internals, Value, Status) ->

	NewOutputs1 = block_utils:set_output_value(Outputs, 'Value', Value),
	NewOutputs2 = block_utils:set_output_value(NewOutputs1, 'Status', Status),
    
	NewInternals1 = update_exec_count(Internals),
	NewInternals2 = block_utils:set_internal_value(NewInternals1, 'LastExec', calendar:now_to_local_time(erlang:timestamp())),
    NewInternals3 = setup_execute_timer(Configs, NewInternals2),
    {NewOutputs2, NewInternals3}.


%%
%%  Common block delete function
%%
delete(Configs, Internals) ->
    log_state("Deleting", Configs),    
    % Cancel execution timer if it exists
    TimerReferenceValue = block_utils:get_internal_value(Internals, 'TimerRef' ),
    case TimerReferenceValue of
        empty -> empty;
        
        _ ->  timer:cancel(TimerReferenceValue)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

setup_execute_timer(Configs, Internals) ->
    case block_utils:get_config_value(Configs, 'Executors') of
        [] ->  % The list of executors is empty, check if this block should be executed via timer
            ExecuteTimerValue = block_utils:get_config_value(Configs, 'Timeout'),
            if (ExecuteTimerValue > 0 ) ->
                BlockName = block_utils:get_config_value(Configs, 'BlockName'),
                % Setup timer to execute block after timer expires
                case timer:apply_after(ExecuteTimerValue, 'block_server', execute, [BlockName]) of
                    {ok, TimerReferenceValue} -> 
                        NewInternals = block_utils:set_internal_value(Internals, 'TimerRef', TimerReferenceValue);
         
                    {error, Reason} -> 
                        NewInternals = block_utils:set_internal_value(Internals, 'TimerRef', empty),
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
	case block_utils:get_internal_value(Internals, 'ExecCount') + 1 of
		1000000000   -> block_utils:set_internal_value(Internals, 'ExecCount', 0);
		NewExecCount -> block_utils:set_internal_value(Internals, 'ExecCount', NewExecCount)
	end.

%% print the indicated state to the shell
log_state (State, Configs) ->
    BlockName = block_utils:get_config_value(Configs, 'BlockName'),
    BlockType = block_utils:get_config_value(Configs, 'BlockType'),
    
    io:format("~s: ~p Type: ~p~n", [State, BlockName, BlockType]).
    