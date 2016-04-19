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
-export([configs/6, inputs/0, outputs/0]).
-export([execute/2, initialize/1, delete/1]).


%%
%% Common Config Attributes
%% Config values are set once on block creation and never modified
%%
-spec configs(Name :: atom(),
              Module :: module(),
              Comment :: string(),
              Type :: string(),
              Version :: string(),
              Description :: string()) -> list().

configs(Name, Module, Comment, Type, Version, Description) ->
  [
    % Block Name, must be unique for all blocks on this node
    {block_name, Name},

    % Block module, the block code 
    {block_module, Module},

    % User defined description of this block
    {comment, Comment},

    % Block type string 
	  {block_type, Type},

    % Block Version string
	  {version, Version},

    % Description of this block type
    {description, Description}
  ].


%%
%% Common Input Attributes, 
%% Inputs may be set to a fixed value or linked to a block output value
%%
inputs() ->
  [
    % Block will execute as long as disable input is false/not_active
    % When disable input is true, all block outputs set to not_active,
    % and block status is set to disabled.  Block will not execute, 
    % Default block to disabled, on create. 
    % Set disable input to false in create function if you want block 
    % to begin executing on create.
    {disable, true, ?EMPTY_LINK},
 
   
    % Block will execute as long as freeze input is false/not_active
    % When freeze input is true, all block outputs remain at last value
    % and block status is set to frozen.  Block will not be executed.
    {freeze, false, ?EMPTY_LINK},

    % Disable true/on value takes precedent over Freeze true/on value.

    % Link exec_in to block that will execute this block.
    % May only be linked to the 'exec_out' block output value
    % i.e. implement Control Flow
    {exec_in, empty, ?EMPTY_LINK},  

    % If > 0, execute block every 'exec_interval' milliseconds.
    % Used to execute a block at fixed intervals
    % instead of being executed via exec_out/exec_in link
    % or executed on change of input values
    {exec_interval, 0, ?EMPTY_LINK}

    % exec_in and exec_interval may both be used to execute the block.
    % They are not mutually exclusive.
    % If exec_in is linked to another block or exec_interval > 0,
    % the block will no longer execute on change of input state
  ].


%%
%% Common Output Attributes
%% Block output values are modified upon block execution
%%
outputs() ->
  [
    % Blocks with the 'exec_in' input linked to this output
    % will be executed by this block, each time this block is executed
    % This output may only be linked to exec_in inputs
    {exec_out, false, []},

    % Current block status                            
    {status, created, []},        

    % Reason block was executed
    {exec_method, empty, []},

    % Time stamp of last time block was executed
    % Hours, Minutes, Seconds, Microseconds
    {last_exec, empty, []},

    % Main block output value
    {value, not_active, []}
  ].


%%
%% Common block initialization function
%%
-spec initialize(block_defn()) -> block_state().

initialize({Config, Inputs, Outputs}) ->

  {BlockName, BlockModule} = block_utils:name_module(Config),
 
  % Initialize the private attributes values list here.
  % Timer reference attribute is common to all block types
  Private = [ {timer_ref, empty} ],

  % In case this block is set to execute via timer, initialize the timer
  {_Status, Private1} = update_execution_timer(BlockName, Inputs, Private), 

  % Perform block type specific initialization 
  BlockModule:initialize({Config, Inputs, Outputs, Private1}).


%%
%% Common block execute function
%%
-spec execute(BlockValues :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute(BlockValues, ExecMethod) ->

  {Config, Inputs, Outputs, Private} = BlockValues,
  
  {BlockName, BlockModule} = block_utils:name_module(Config),
    
  Disable = block_utils:get_value(Inputs, disable),
  case check_boolean_input(Disable) of
    not_active ->
      Freeze = block_utils:get_value(Inputs, freeze),
      case check_boolean_input(Freeze) of
        not_active -> % block is not disabled or frozen, execute it
          {Config, Inputs, OutputsX, PrivateX} = BlockModule:execute(BlockValues),
          OutputsY = update_execute_track(OutputsX, ExecMethod);
                    
        active -> % Block is frozen
          % Just update the status output, all other outputs are frozen
          OutputsY = block_utils:set_status(Outputs, frozen),
          % Nothing to update in private values
          PrivateX = Private;
                    
        error -> % Freeze input value error
          error_logger:error_msg("~p Invalid freeze input value: ~p ~n", [BlockName, Freeze]),
          OutputsY = update_all_outputs(Outputs, not_active, input_err),
          % Nothing to update in private values
          PrivateX = Private
      end;
    active -> % Block is disabled
      OutputsY = update_all_outputs(Outputs, not_active, disabled),
      % Nothing to update in private values
      PrivateX = Private;
        
    error -> % Disable input value error 
      error_logger:error_msg("~p Invalid disable input value: ~p ~n", [BlockName, Disable]),
      OutputsY = update_all_outputs(Outputs, not_active, input_err),
      % Nothing to update in private values
      PrivateX = Private
  end,
    
  {Status, PrivateY} = update_execution_timer(BlockName, Inputs, PrivateX), 
    
  if (Status /= normal) ->  % Some kind error setting execution timer
    OutputsZ = update_all_outputs(OutputsY, not_active, Status);
  true -> % Execution timer status is normal
    OutputsZ = OutputsY
  end,

  % Update the block inputs linked to the block outputs that have just been updated (Data Flow)
  update_blocks(BlockName, Outputs, OutputsZ),
    
  % Execute the blocks connected to the exec_out output value (Control Flow)
  update_execute(OutputsZ),
    
  % Return the updated block state
  {Config, Inputs, OutputsZ, PrivateY}.


%
% Check the value of the disable or freeze control value input
%
check_boolean_input(Value) ->
  case Value of
    true       -> active;
    false      -> not_active; 
    not_active -> not_active;
    empty      -> not_active;
    _Error     -> error
  end.
 

%%
%% Update the block execution timer 
%%
-spec update_execution_timer(BlockName :: atom(),
                             Inputs :: list(),
                             Private :: list()) -> {atom(), list()}.
                             
update_execution_timer(BlockName, Inputs, Private) ->

  ExecuteInterval = block_utils:get_value(Inputs, exec_interval),
  TimerRef = block_utils:get_value(Private, timer_ref),

  % Cancel block execution timer, if it is set   
  cancel_timer(BlockName, TimerRef), 
    
  % Check validity of ExecuteInterval input value
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
    error_logger:error_msg("~p Invalid exec_interval value: ~p ~n",
                           [BlockName, ExecuteInterval])
  end,
  NewPrivate = block_utils:set_value(Private, timer_ref, NewTimerRef),
  {Status, NewPrivate}.


%%
%% Cancel block execution timer, if the timer is set
%%
-spec cancel_timer(BlockName :: atom(),
                   TimerRev :: term()) -> ok | {error, term()}.

cancel_timer(BlockName, TimerRef) ->
  if (TimerRef /= empty) ->
    case erlang:cancel_timer(TimerRef) of 
      {ok, cancel} -> 
        ok;

      {error, Reason} ->
        error_logger:error_msg("~p Error: ~p Canceling execution timer ~p ~n", 
                                [BlockName, Reason, TimerRef]),
        error
    end;
  true -> ok
  end.

%%
%% Set timer to execute the block on expiration
%%
-spec set_timer(BlockName :: atom(),
                ExecuteInterval :: integer()) -> 
                {normal, term()} | {error_proc, empty}.

set_timer(BlockName, ExecuteInterval) ->
  case erlang:send_after(ExecuteInterval, BlockName, timer_execute) of
    {ok, TimerRef} -> 
      {normal, TimerRef};
         
    {error, Reason} -> 
      error_logger:error_msg("~p Error: ~p Setting execution timer ~n",
                             [BlockName, Reason]),
      {error_proc, empty}
  end.


%%
%% Track execute method, time and count
%%
-spec update_execute_track(Outputs :: list(), 
                           ExecMethod :: atom()) -> list().

update_execute_track(Outputs, ExecMethod) ->

  % Record method of execution
  OutputsX = block_utils:set_value(Outputs, exec_method, ExecMethod),

  % Record last executed timestamp
  TS = {_, _, Micro} = os:timestamp(),
  {{_Year, _Month, _Day},{Hour, Minute, Second}} = calendar:now_to_local_time(TS),

	block_utils:set_value(OutputsX, last_exec, {Hour, Minute, Second, Micro}).


%% 
%% Update all outputs to the New value,
%% except update status output to the New Staus value
%% Used to mass update block outputs in disabled or error conditions
%% 
-spec update_all_outputs(Outputs :: list(), 
                         NewValue :: term(), 
                         NewStatus :: atom()) -> list().

update_all_outputs(Outputs, NewValue, NewStatus) ->
  lists:map(
    fun(Output) ->
      {ValueName, _Value, BlockNames} = Output,
      case ValueName of
        status -> {ValueName, NewStatus, BlockNames};
             _ -> {ValueName, NewValue,  BlockNames}
      end
    end,
    Outputs).


%%
%% Send an update message to each block linked to any output value that has changed
%% This assumes CurrentOutputs and NewOutputs, have the same ValueNames and order for all outputs
%%
-spec update_blocks(FromBlockName :: atom(), 
                    CurrentOutputs :: list(), 
                    NewOutputs :: list()) -> ok.

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
  true -> 
    ok % else do nothing
  end,

  update_blocks(FromBlockName, RemainingCurrentOutputs, RemainingNewOutputs).

%%
%% update each block input value in the list of block names, 
%% linked to this block's output value
%%
-spec update_linked_inputs(list(), 
                           FromBlockName :: atom(),
                           ValueName :: atom(),
                           NewValue :: term()) -> ok.
                                   
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

%%
%% Execute each block in the list of block names, assigned to this block's 'exec_out' output
%%
-spec execute_out(list()) -> ok.
   
execute_out([]) ->
  ok;
execute_out([BlockName | RemainingBlockNames]) ->
  block_server:exec_out_execute(BlockName),
  execute_out(RemainingBlockNames).


%%
%%  Common block delete function
%%
-spec delete(BlockValues :: block_state()) -> ok.

delete(BlockValues) ->
  {Config, Inputs, Outputs, Private} = BlockValues,
  
  {BlockName, BlockModule} = block_utils:name_module(Config),

  % Cancel execution timer if it exists
  case block_utils:get_value(Private, timer_ref) of
    empty    -> empty;
    TimerRef ->  erlang:cancel_timer(TimerRef)
  end,
    
  % Scan this block's inputs, and unlink from other block outputs
  block_links:unlink_blocks(BlockName, Inputs),
  
  % Set all output values of this block, including status, to 'empty'
  EmptyOutputs = update_all_outputs(Outputs, empty, empty),
    
  % Update the block inputs linked to this block's outputs 
  % This will set the input value of any block 
  % linked to this deleted block, to 'empty'.
  update_blocks(BlockName, Outputs, EmptyOutputs),
    
  % Execute the blocks connected to the exec_out output value
  % of this block, one last time.
  update_execute(EmptyOutputs),
    
  % Perform block type specific delete actions
  BlockModule:delete(BlockValues).


%% ====================================================================
%% Internal functions
%% ====================================================================
