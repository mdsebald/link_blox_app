%%
%% @author Mark
%% @doc Block functions common to all block types 
%% 


-module(block_common).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/3, configs/3, inputs/0, outputs/0, private/0, initialize/3, execute/5, delete/2]).


%% 
%% Common block Create function
%% Return initial set of block values, containing 
%% block values that are common for all block types
%%

create(Name, Type, Version) ->
    
    % Common Config
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
    
    % Common Private
	ExecCount = {exec_count, 0},
	LastExec = {last_exec, not_active},
    TimerRef = {timer_ref, empty},
	
	% Return a set of initial common block values consisting of:
    % Config, Inputs, Outputs, and Private values
    
    { 
      [BlockName, BlockType, BlockVersion, Executors, Timeout],  % Config
	  [Enable], % Inputs
	  [Value, Status], % Outputs
      [ExecCount, LastExec, TimerRef] % Private
    }.


%%
%% Common Config Attributes
%%
configs(Name, Type, Version) ->
    Config = 
    [ 
      {block_name, Name},
	  {block_type, Type},
	  {version, Version},
    ],
    
    log_state("Creating", Config),
    Config.


%%
%% Common Input Attributes
%%
inputs() ->
    [
      {enable, true, {fixed, null, null}},      % Block will execute as long as enable input is true
      
      {execute_in, empty, {fixed, null, null}},     % Link to block allowed execute this block.
                                                % If this list contains one or more block names, 
                                                % then execute only on receiving an execute command message
                                                % i.e. Implement Control Flow 
      
      {execute_interval, 0, {fixed, null, null}}   % If > 0, execute block every 'execute_interval' milliseconds.
                                                % This is used to execute a block at fixed intervals
                                                % instead of being executed by other blocks.
                                                % or executed on change of input value
      
    ].
    
%%
%% Common Output Attributes
%%
outputs() ->
    [ 
      {execute_out, false, []},
      {status, created, []},
      {value, not_active, []}
    ].
    
    
%%
%% Common Private Attributes
%%
private() ->
    [ 
      {exec_count, 0},
	  {last_exec, not_active},
      {timer_ref, empty}
    ].
       



%%
%% Common block initialization funcion
%%

initialize(Config, Outputs, Private) ->

    log_state("Initializing", Config),
    NewPrivate = setup_execute_timer(Config, Private), 
    NewOutputs = block_utils:set_output_value(Outputs, status, initialized),
    
    {NewOutputs, NewPrivate}.


%%
%% Common block execute function
%% Update the Output values common to all blocks, Value, Status, Execute Count, Last Executed Time
%%
execute(Config, Outputs, Private, Value, Status) ->

	case block_utils:get_output_value(Outputs, status) of
        initialized ->
           NewPrivate2 = block_utils:set_private_value(Private, last_exec, calendar:now_to_local_time(erlang:timestamp())),
           NewPrivate3 = setup_execute_timer(Config, NewPrivate2);

        normal ->
	       NewPrivate1 = update_exec_count(Private),
	       NewPrivate2 = block_utils:set_private_value(NewPrivate1, last_exec, calendar:now_to_local_time(erlang:timestamp())),
           NewPrivate3 = setup_execute_timer(Config, NewPrivate2);
        _ -> % block is not executing freeze the exec_count and last_exec time
           NewPrivate3 = Private 
    end,
    
    % Now update the value and status outputs
    NewOutputs1 = block_utils:set_output_value(Outputs, value, Value),
	NewOutputs2 = block_utils:set_output_value(NewOutputs1, status, Status),

    {NewOutputs2, NewPrivate3}.


%%
%%  Common block delete function
%%
delete(Config, Private) ->
    log_state("Deleting", Config),    
    % Cancel execution timer if it exists
    TimerReferenceValue = block_utils:get_private_value(Private, timer_ref),
    case TimerReferenceValue of
        empty -> empty;
        
        _ ->  timer:cancel(TimerReferenceValue)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

setup_execute_timer(Config, Private) ->
    case block_utils:get_config_value(Config, executors) of
        [] ->  % The list of executors is empty, check if this block should be executed via timer
            ExecuteTimerValue = block_utils:get_config_value(Config, timeout),
            if (ExecuteTimerValue > 0 ) ->
                BlockName = block_utils:get_config_value(Config, block_name),
                % Setup timer to execute block after timer expires
                case timer:apply_after(ExecuteTimerValue, block_server, execute, [BlockName]) of
                    {ok, TimerReferenceValue} -> 
                        NewPrivate = block_utils:set_private_value(Private, timer_ref, TimerReferenceValue);
         
                    {error, Reason} -> 
                        NewPrivate = block_utils:set_private_value(Private, timer_ref, empty),
                        io:format("Error: ~p Setting execution timer ~n", [Reason])
                end;
            true ->  % Timeout value is less than or equal to zero, don't set up execution via timer
                NewPrivate = Private
            end;
            
        _ ->  % there is something in the executors list, don't setup execution via timer  
            NewPrivate = Private
    end,
    NewPrivate.


update_exec_count(Private) ->
	% Arbitrarily roll over Execution Counter at 999,999,999
	case block_utils:get_private_value(Private, exec_count) + 1 of
		1000000000   -> block_utils:set_private_value(Private, exec_count, 0);
		NewExecCount -> block_utils:set_private_value(Private, exec_count, NewExecCount)
	end.

%% print the indicated state to the shell
log_state (State, Config) ->
    BlockName = block_utils:get_config_value(Config, block_name),
    BlockType = block_utils:get_config_value(Config, block_type),
    
    io:format("~s: ~p Type: ~p~n", [State, BlockName, BlockType]).
    