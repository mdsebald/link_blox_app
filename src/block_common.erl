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
-export([
          configs/4, 
          inputs/0, 
          outputs/0,
          execute/2,
          initialize/1,
          delete/1,
          update_linked_inputs/4
]).

%%
%% Common Config Attributes
%% Config values are set once on block creation and never modified
%%
-spec configs(Name :: atom(),
              Module :: module(),
              Version :: string(),
              Description :: string()) -> list(config_attr()).

configs(Name, Module, Version, Description) ->
  [
    % Block Name, must be unique for all blocks on this node
    {block_name, {Name}},

    % Block module, the block code 
    {block_module, {Module}},
    
     % Block Version string
	  {version, {Version}},

    % User defined description of this block
    {description, {Description}}    
  ].


%%
%% Common Input Attributes, 
%% Inputs may be set to a fixed value or linked to a block output value
%%
-spec inputs() -> list(input_attr()).

inputs() ->
  [
    % Block will execute as long as disable input is false/not_active
    % When disable input is true, all block outputs set to not_active,
    % and block status is set to disabled.  Block will not execute, 
    % Default block to disabled, on create. 
    % Set disable input to false in create function if you want block 
    % to begin executing on create.
    {disable, {true, ?EMPTY_LINK}},
 
   
    % Block will execute as long as freeze input is false/not_active
    % When freeze input is true, all block outputs remain at last value
    % and block status is set to frozen.  Block will not be executed.
    {freeze, {false, ?EMPTY_LINK}},

    % Disable true/on value takes precedent over Freeze true/on value.

    % Link exec_in to block that will execute this block.
    % May only be linked to the 'exec_out' block output value
    % i.e. implement Control Flow
    {exec_in, {empty, ?EMPTY_LINK}},  

    % If > 0, execute block every 'exec_interval' milliseconds.
    % Used to execute a block at fixed intervals
    % instead of being executed via exec_out/exec_in link
    % or executed on change of input values
    {exec_interval, {0, ?EMPTY_LINK}}

    % exec_in and exec_interval may both be used to execute the block.
    % They are not mutually exclusive.
    % If exec_in is linked to another block or exec_interval > 0,
    % the block will no longer execute on change of input state
  ].


%%
%% Common Output Attributes
%% Block output values are modified upon block execution
%%
-spec outputs() -> list(output_attr).
outputs() ->
  [
    % Blocks with the 'exec_in' input linked to this output
    % will be executed by this block, each time this block is executed
    % This output may only be linked to exec_in inputs
    {exec_out, {false, []}},

    % Current block status                            
    {status, {created, []}},     

    % Reason block was executed
    {exec_method, {empty, []}},

    % Time stamp of last time block was executed
    % Hours, Minutes, Seconds, Microseconds
    {last_exec, {empty, []}},

    % Main block output value
    {value, {not_active, []}}
  ].


%%
%% Common block initialization function
%%
-spec initialize(block_defn()) -> block_state().

initialize({Config, Inputs, Outputs}) ->

  {BlockName, BlockModule} = config_utils:name_module(Config),
 
  % Initialize the private attributes values list here.
  % Timer reference attribute is common to all block types
  Private = [ {timer_ref, {empty}} ],

  % In case this block is set to execute via timer, initialize the timer
  {_Status, Private1} = update_execution_timer(BlockName, Inputs, Private), 

  % Perform block type specific initialization 
  BlockModule:initialize({Config, Inputs, Outputs, Private1}).
  

%%
%% Common block execute function
%% TODO: Don't execute block when status is in error,
%%
-spec execute(BlockValues :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute(BlockValues, ExecMethod) ->

  {Config, Inputs, Outputs, Private} = BlockValues,
  BlockStatus = output_utils:get_status(Outputs),

  % Check block status before executing
  case ok_to_execute(BlockStatus, ExecMethod) of
    true ->
      {BlockName, BlockModule} = config_utils:name_module(Config),
    
      {ok, Disable} = attrib_utils:get_value(Inputs, disable),
      case input_utils:check_boolean_input(Disable) of
        not_active ->
          {ok, Freeze} = attrib_utils:get_value(Inputs, freeze),
          case input_utils:check_boolean_input(Freeze) of
            not_active -> % block is not disabled or frozen, execute it
              {Config, Inputs, Outputs1, Private1} = BlockModule:execute(BlockValues),
              Outputs2 = update_execute_track(Outputs1, ExecMethod);
                    
            active -> % Block is frozen
              % Just update the status output, all other outputs are frozen
              Outputs2 = output_utils:set_status(Outputs, frozen),
              % Nothing to update in private values
              Private1 = Private;
                    
            error -> % Freeze input value error
              error_logger:error_msg("~p Invalid freeze input value: ~p ~n", [BlockName, Freeze]),
              Outputs2 = output_utils:update_all_outputs(Outputs, not_active, input_err),
              % Nothing to update in private values
              Private1 = Private
          end;
        active -> % Block is disabled
          Outputs2 = output_utils:update_all_outputs(Outputs, not_active, disabled),
          % Nothing to update in private values
          Private1 = Private;
        
        error -> % Disable input value error 
          error_logger:error_msg("~p Invalid disable input value: ~p ~n", [BlockName, Disable]),
          Outputs2 = output_utils:update_all_outputs(Outputs, not_active, input_err),
          % Nothing to update in private values
          Private1 = Private
      end,
      
      {Status, Private2} = update_execution_timer(BlockName, Inputs, Private1), 
    
      if (Status /= normal) ->  % Some kind error setting execution timer
        Outputs3 = output_utils:update_all_outputs(Outputs2, not_active, Status);
      true -> % Execution timer status is normal
        Outputs3 = Outputs2
      end,

      % Update the block inputs linked to the block outputs 
      % that have just been updated (Data Flow)
      update_blocks(BlockName, Outputs, Outputs3),
    
      % Execute the blocks connected to the exec_out output value (Control Flow)
      update_execute(Outputs3),
    
      % Return the updated block state
      {Config, Inputs, Outputs3, Private2};

    false -> % Block is in error state
      % Just return the current block state, unchanged
      {Config, Inputs, Outputs, Private}
  end.


%%
%% Don't execute block if the block is configured incorrectly,
%% if there is a problem accessing the hardware,
%% or if an input is missing or in error
%%
-spec ok_to_execute(BlockStatus :: block_status(),
                    ExecMethod :: exec_method()) -> boolean().

ok_to_execute(BlockStatus, ExecMethod) ->
  case ExecMethod of
    % always execute the block on a manual command
    manual -> true;

    _ ->
      case lists:member(BlockStatus, [input_err, config_err, proc_err, no_input]) of
        true -> false;
        false -> true
      end
  end.


%%
%% Update the block execution timer 
%%
-spec update_execution_timer(BlockName :: block_name(),
                             Inputs :: list(input_attr()),
                             Private :: list(private_attr())) -> {atom(), list(private_attr())}.
                             
update_execution_timer(BlockName, Inputs, Private) ->

  {ok, ExecuteInterval} = attrib_utils:get_value(Inputs, exec_interval),
  {ok, TimerRef} = attrib_utils:get_value(Private, timer_ref),

  % Cancel block execution timer, if it is set   
  cancel_timer(TimerRef), 
    
  % Check validity of ExecuteInterval input value
  if is_integer(ExecuteInterval) ->
     
    if (ExecuteInterval == 0) ->
      Status = normal, 
      NewTimerRef = empty;
    true ->
      if (ExecuteInterval > 0) ->
        NewTimerRef = set_timer(BlockName, ExecuteInterval),
        Status = normal; 
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
  {ok, Private1} = attrib_utils:set_value(Private, timer_ref, NewTimerRef),
  {Status, Private1}.


%%
%% Cancel block execution timer, if the timer is set
%%
-spec cancel_timer(TimerRef :: reference()) -> integer() | false.

cancel_timer(TimerRef) ->
  if (TimerRef /= empty) -> erlang:cancel_timer(TimerRef);
  true -> false
  end.

%%
%% Set timer to execute the block on expiration  
%% 
-spec set_timer(BlockName :: atom(),
                ExecuteInterval :: integer()) -> 
                reference().

set_timer(BlockName, ExecuteInterval) ->
  erlang:send_after(ExecuteInterval, BlockName, timer_execute).


%%
%% Track execute method, time and count
%%
-spec update_execute_track(Outputs :: list(attribute()), 
                           ExecMethod :: atom()) -> list(attribute()).

update_execute_track(Outputs, ExecMethod) ->

  % Record method of execution
  {ok, Outputs1} = attrib_utils:set_value(Outputs, exec_method, ExecMethod),

  % Record last executed timestamp
  TS = {_, _, Micro} = os:timestamp(),
  {{_Year, _Month, _Day},{Hour, Minute, Second}} = calendar:now_to_local_time(TS),

	{ok, Outputs2} = attrib_utils:set_value(Outputs1, last_exec, 
                               {Hour, Minute, Second, Micro}),
  Outputs2.


%%
%% Send an update message to each block linked to any output value that has changed
%% This assumes CurrentOutputs and NewOutputs, have the same ValueNames and order for all outputs
%%
-spec update_blocks(FromBlockName :: block_name(), 
                    CurrentOutputs :: list(output_attr()), 
                    NewOutputs :: list(output_attr())) -> ok.

update_blocks(_FromBlockName, [], [])-> ok;

update_blocks(FromBlockName, 
              [CurrentOutput | CurrentOutputs], 
              [NewOutput | NewOutputs])->

  case CurrentOutput of
    % Non-array value output
    {ValueName, {CurrentValue, Refs}} ->
      {ValueName, {NewValue, Refs}} = NewOutput,
      
      % For each output value that changed, call update() to send 
      % a new value message to each linked block.
      % Don't check the 'exec_out' output, that is for control flow execution
      if (ValueName /= exec_out) andalso (CurrentValue /= NewValue) -> 
        update_linked_inputs(FromBlockName, ValueName, NewValue, Refs);
      true -> 
        ok % else do nothing
      end,
      update_blocks(FromBlockName, CurrentOutputs, NewOutputs);
      
    % Array value output
    {ValueName, CurrentArrayValues} ->
      {ValueName, NewArrayValues} = NewOutput,
        check_array_values(FromBlockName, ValueName, 1,
                           CurrentArrayValues, NewArrayValues) 
  end,

  update_blocks(FromBlockName, CurrentOutputs, NewOutputs).


%%
%% Check the values in the output array value
%% 
-spec check_array_values(FromBlockName :: block_name(),
                         ValueName :: value_name(),
                         ArrayIndex :: pos_integer(),
                         CurrentArrayValues :: list(), 
                         NewArrayValues :: list()) -> ok.

check_array_values(_FromBlockName, _ValueName, _ArrayIndex, [], []) ->
  ok;
  
check_array_values(FromBlockName, ValueName, ArrayIndex, 
                   [{CurrentValue, Refs} | CurrentArrayValues], 
                   [{NewValue, Refs} | NewArrayValues]) ->
                   
  if  CurrentValue /= NewValue -> 
    update_linked_inputs(FromBlockName, {ValueName, ArrayIndex}, NewValue, Refs);
  true -> 
    ok
  end,
                   
  check_array_values(FromBlockName, ValueName, ArrayIndex+1, 
                       CurrentArrayValues, NewArrayValues).


%%
%% update each block input value in the list of block names, 
%% linked to this block's output value
%%
-spec update_linked_inputs(FromBlockName :: block_name(),
                           ValueId :: value_id(),
                           NewValue :: value(),
                           Refs :: link_refs()) -> ok.
                           
update_linked_inputs(FromBlockName, ValueId, NewValue, Refs) ->
  % Eliminate duplicate references, 
  % One update message to the same block, 
  % will update all inputs linked to this output
  DedupedRefs = sets:to_list(sets:from_list(Refs)),
  lists:map(fun(Ref) -> 
            block_server:update(Ref, FromBlockName, ValueId, NewValue) 
            end, 
            DedupedRefs),
  ok.


%%    
%% Send an exec_out_execute message to each block connected to the 'exec_out' output of this block
%% This will implement control flow execution, versus data flow done in the update_blocks function. 
%%
-spec update_execute(Outputs :: list(output_attr())) -> ok.

update_execute(Outputs) ->	
  {ok, {exec_out,  {_Value, Refs}}} = attrib_utils:get_attribute(Outputs, exec_out),
  
  lists:map(fun(Ref) -> block_server:exec_out_execute(Ref) end, Refs),
  ok.


%%
%% Common block delete function, 
%% Return the updated block state, in case calling function wants to reuse it 
%%
-spec delete(BlockValues :: block_state()) -> block_state().

delete({Config, Inputs, Outputs, Private}) ->
  
  {BlockName, BlockModule} = config_utils:name_module(Config),

  % Cancel execution timer if it exists
  case attrib_utils:get_value(Private, timer_ref) of
    {ok, empty}      -> ok;
    {ok, TimerRef}   -> erlang:cancel_timer(TimerRef);
    {error, _Reason} -> ok  % Don't care if timer_ref doesn't exist
  end,
    
  % Scan this block's inputs, and unlink from other block outputs
  link_utils:unlink_inputs(BlockName, Inputs),

  % Set all linked input values to empty, 
  EmptyInputs = link_utils:empty_linked_inputs(Inputs),
  
  % Set all output values of this block, including status, to 'empty'
  EmptyOutputs = output_utils:update_all_outputs(Outputs, empty, empty),
    
  % Update the block inputs linked to this block's outputs 
  % This will set the input value of any block 
  % linked to this deleted block, to 'empty'.
  update_blocks(BlockName, Outputs, EmptyOutputs),
    
  % Execute the blocks connected to the exec_out output value
  % of this block, one last time.
  update_execute(EmptyOutputs),
    
  % Perform block type specific delete actions
  BlockModule:delete({Config, EmptyInputs, EmptyOutputs, Private}).


%% ====================================================================
%% Internal functions
%% ====================================================================
