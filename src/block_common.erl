%%%
%%% @doc 
%%% Block functions common to all block types 
%%% 
%%% @end
%%%

-module(block_common).

-author ("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([configs/3, inputs/0, outputs/0, private/0]).
-export([execute/2, initialize/1, delete/1]).


%%
%% Common Config Attributes
%%
configs(Name, Type, Version) ->
    [ 
      {block_name, Name},
	  {block_type, Type},
	  {version, Version}
     ].


%%
%% Common Input Attributes
%%
inputs() ->
    [
      {enable, true, ?EMPTY_LINK},       % Block will execute as long as enable input is true
      
      {exec_in, empty, ?EMPTY_LINK},     % Link to block that will execute this block.
                                         % May only be linked to the 'exec_out' block output value
                                         % i.e. implement Control Flow
                                            
      {exec_interval, 0, ?EMPTY_LINK}    % If > 0, execute block every 'exec_interval' milliseconds.
                                         % Used to execute a block at fixed intervals
                                         % instead of being executed via exec_out/exec_in link
                                         % or executed on change of input value  
    ].
    
    
%%
%% Common Output Attributes
%%
outputs() ->
    [ 
      {exec_out, false, []},    % Blocks with the 'exec_in' input linked to this output 
                                % will be executed by this block, each time this block is executed
                                % This output may only be linked to exec_in inputs
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
      {timer_ref, empty},
      {exec_method, empty}
    ].
       


%%
%% Common block execute function
%% TODO: Handle initial execution (if needed)
%%
-spec execute(BlockValues :: block_state(), ExecMethod :: exec_method()) -> block_state().

execute(BlockValues, ExecMethod) ->

    {BlockName, BlockModule, Config, Inputs, Outputs, Private} = BlockValues,
    
    EnableInput = block_utils:get_value(Inputs, enable),
    
    if is_boolean(EnableInput) ->
        if EnableInput -> % Block is enabled
            {_, _, _, _, NewOutputsX, NewPrivateX} = BlockModule:execute(BlockValues),
            NewStatus = block_utils:get_value(NewOutputsX, status),
            if  NewStatus == normal ->
                NewPrivateY = update_execute_track(NewPrivateX, ExecMethod);
            true ->  % Block Status is not normal
                % Assume custom block code has taken care of updating output value(s) appropriately
                % Don't update execution tracking
                NewPrivateY = NewPrivateX
            end;
        true  ->   % Block is disabled
            NewOutputsX = update_all_outputs(Outputs, not_active, disabled),
            % Don't udpate execution tracking
            NewPrivateY = Private
        end;
    
    true -> % Invalid Enable input type or value
        error_logger:error_msg("~p Invalid enable Input value: ~p ~n", [BlockName, EnableInput]),
        NewOutputsX = update_all_outputs(Outputs, not_active, input_err),
        % Don't udpate execution tracking
        NewPrivateY = Private
    end,
    
    {Status, NewPrivate} = update_execution_timer(BlockName, Inputs, NewPrivateY), 
    
    if (Status /= normal) ->  % Some kind error setting execution timer
        NewOutputs = update_all_outputs(NewOutputsX, not_active, Status);
    true -> % Execution timer status is normal
        NewOutputs = NewOutputsX
    end,
 
    % Update the block inputs linked to the block outputs that have just been updated (Data Flow)
	update_blocks(BlockName, Outputs, NewOutputs),
    
    % Execute the blocks connected to the exec_out output value (Control Flow)
    update_execute(NewOutputs),
    
    % Return the new updated block state
    {BlockName, BlockModule, Config, Inputs, NewOutputs, NewPrivate}.


%%
%% Update the block execution timer 
%% Return status and updated Timer Reference 
%%
update_execution_timer(BlockName, Inputs, Private) ->

    ExecuteInterval = block_utils:get_value(Inputs, exec_interval),
    TimerRef = block_utils:get_value(Private, timer_ref),
    
    % Cancel block execution timer, if it is set   
    cancel_timer(BlockName, TimerRef), 
    
    % Check validity of ExecuteInterval input value
    % TODO: Check validity of config values once on startup
    if is_integer(ExecuteInterval) ->
     
        if (ExecuteInterval == 0) ->
            Status = normal, 
            NewTimerRef = empty;
        true ->
            if (ExecuteInterval > 0) ->
                {Status, NewTimerRef} = set_timer(BlockName, ExecuteInterval);
            true -> % Execute Interval input value is negative
                Status = input_err, 
                NewTimerRef = empty,
                error_logger:error_msg("~p Negative exec_interval value: ~p ~n", [BlockName, ExecuteInterval])
            end
        end;
    true ->  % Execute Interval input value is not an integer
        Status = input_err, 
        NewTimerRef = empty,
        error_logger:error_msg("~p Invalid exec_interval value: ~p ~n", [BlockName, ExecuteInterval])
    end,
    NewPrivate = block_utils:set_value(Private, timer_ref, NewTimerRef),
    {Status, NewPrivate}.
    
% Cancel block execution timer, if the timer is set   
cancel_timer(BlockName, TimerRef) ->
    if (TimerRef /= empty) ->
        case timer:cancel(TimerRef) of 
            {ok, cancel} -> 
                ok;
            
            {error, Reason} ->
                error_logger:error_msg("~p Error: ~p Canceling execution timer ~p ~n", [BlockName, Reason, TimerRef]),
                error
        end;
    true -> ok
    end.

% Setup timer to execute block after timer expires 
set_timer(BlockName, ExecuteInterval) ->
    case timer:apply_after(ExecuteInterval, block_server, timer_execute, [BlockName]) of
        {ok, TimerRef} -> 
            {normal, TimerRef};
         
        {error, Reason} -> 
           error_logger:error_msg("~p Error: ~p Setting execution timer ~n", [BlockName, Reason]),
            {error_proc, empty}
    end.   	

%%
%% Track execute method, time and count
%%
-spec update_execute_track(Private :: list(), ExecMethod :: atom()) -> list().

update_execute_track(Private, ExecMethod) ->
   
    % Record method of execution
    PrivateX = block_utils:set_value(Private, exec_method, ExecMethod),
    
    % Record last executed timestamp
    TS = {_, _, Micro} = os:timestamp(),
    {{_Year, _Month, _Day},{Hour, Minute, Second}} = calendar:now_to_local_time(TS),
    
	PrivateY = block_utils:set_value(PrivateX, last_exec, {Hour, Minute, Second, Micro}),

	% Arbitrarily roll over Execution Counter at 999,999,999
	case block_utils:get_value(PrivateY, exec_count) + 1 of
		1000000000   -> block_utils:set_value(PrivateY, exec_count, 0);
		NewExecCount -> block_utils:set_value(PrivateY, exec_count, NewExecCount)
	end.
    
%% 
%% Update all outputs to the New value,
%% except update status output to the New Staus value
%% Used to mass update block outputs in disabled or error conditions
%% 
-spec update_all_outputs(Outputs :: list(), NewValue :: term(), NewStatus :: atom()) -> list().

update_all_outputs(Outputs, NewValue, NewStatus) ->
    lists:map(
        fun(Output) ->
            {ValueName, _Value, BlockNames} = Output,
            case ValueName of
                status -> {ValueName, NewStatus, BlockNames};
                _      -> {ValueName, NewValue,  BlockNames}
            end
         end,
         Outputs).


%%
%% Send an update message to each block linked to any output value that has changed
%% This assumes CurrentOutputs and NewOutputs, have the same ValueNames and order for all outputs
%%
-spec update_blocks(FromBlockName :: atom(), 
                    CurrentOutputs :: list(), NewOutputs :: list()) -> ok.

update_blocks(_FromBlockName, [], [])-> ok;

update_blocks(FromBlockName, CurrentOutputs, NewOutputs)->
	
	[CurrentOutput | RemainingCurrentOutputs] = CurrentOutputs,
	[NewOutput | RemainingNewOutputs] = NewOutputs,
	
	{ValueName, CurrentValue, Links} = CurrentOutput,
	{ValueName, NewValue, Links} = NewOutput,

    % For each output value that changed, call update() to send 
    % a new value message to each linked block.
    % Don't check the 'exec_out' output, that is for control flow execution
	if (ValueName /= exec_out) andalso (CurrentValue /= NewValue) -> 
        update_linked_inputs(Links, FromBlockName, ValueName, NewValue);
        true -> ok % else do nothing
    end,
    
    update_blocks(FromBlockName, RemainingCurrentOutputs, RemainingNewOutputs).

%% update each block input value in the list of block names, 
%% linked to this block's output value        
update_linked_inputs([], _FromBlockName, _ValueName, _NewValue) -> 
    ok;
update_linked_inputs([BlockName | RemainingLinks], FromBlockName, ValueName, NewValue) ->
    block_server:update(BlockName, FromBlockName, ValueName, NewValue),
    update_linked_inputs(RemainingLinks, FromBlockName, ValueName, NewValue).

         
%%    
%% Send an exec_out_execute message to each block connected to the 'exec_out' output of this block
%% This will implement control flow execution, versus data flow done in the update_blocks function. 
%%
-spec update_execute(list()) -> ok.

update_execute(Outputs) ->	
    {exec_out,  _Value, BlockNames} = block_utils:get_attribute(Outputs, exec_out),
    execute_out(BlockNames).

%% Execute each block in the list of block names, assigned to this block's 'exec_out' output   
execute_out([]) ->
    ok;
execute_out([BlockName | RemainingBlockNames]) ->
    block_server:exec_out_execute(BlockName),
    execute_out(RemainingBlockNames).
    



%%
%% Common block initialization function
%%
-spec initialize(BlockValues :: block_state()) -> block_state().

initialize({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->
    
    % In case this block is set to execute via timer, initialize the timer
    {_Status, NewPrivate} = update_execution_timer(BlockName, Inputs, Private), 
    
    % Perform block type specific initialization 
    BlockModule:initialize({BlockName, BlockModule, Config, Inputs, Outputs, NewPrivate}).
    

%%
%%  Common block delete function
%%
-spec delete(BlockValues :: block_state()) -> ok.

delete(BlockValues) ->
    {BlockName, BlockModule, _Config, Inputs, _Outputs, Private} = BlockValues,

    % Cancel execution timer if it exists
    case block_utils:get_value(Private, timer_ref) of
        empty -> empty;
        TimerRef ->  timer:cancel(TimerRef)
    end,
    
    % Scan this blocks inputs, and unlink from other block outputs
    block_links:unlink_blocks(BlockName, Inputs),
    
    % Scan all blocks and delete any references to this block
    block_links:delete_references(BlockName),
    
    % Perform block type specific delete actions
    BlockModule:delete(BlockValues).

    
%% ====================================================================
%% Internal functions
%% ====================================================================

