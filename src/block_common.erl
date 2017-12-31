%%%
%%% @doc 
%%% Block functionality common to all block types 
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
          temp_delete/1,
          update_execution_timer/3,
          update_linked_inputs/2
]).

%%
%% Common Config Attributes
%% Config values are set once on block creation and never modified
%%
-spec configs(Name :: atom(),
              Module :: module(),
              Version :: string(),
              Description :: string()) -> config_attribs().

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
-spec inputs() -> input_attribs().

inputs() ->
  [
    % Block will execute as long as disable input is false/null
    % When disable input is true, all block outputs set to null,
    % and block status is set to disabled.  Block will not execute, 
    % Default block to disabled, on create. 
    % Set disable input to false in create function if you want block 
    % to begin executing on create.
    {disable, {true, {true}}},
 
   
    % Block will execute as long as freeze input is false/null
    % When freeze input is true, all block outputs remain at last value
    % and block status is set to frozen.  Block will not be executed.
    {freeze, {false, {false}}},

    % Disable true/on value takes precedent over Freeze true/on value.

    % Link exec_in to block that will execute this block.
    % May only be linked to the 'exec_out' block output value
    % i.e. implement Control Flow
    {exec_in, {[], {[]}}},  

    % If > 0, execute block every 'exec_interval' milliseconds.
    % Used to execute a block at fixed intervals
    % instead of being executed via exec_out/exec_in link
    % or executed on change of input values
    {exec_interval, {0, {0}}}

    % exec_in and exec_interval may both be used to execute the block.
    % They are not mutually exclusive.
    % If exec_in is linked to another block or exec_interval > 0,
    % the block will no longer execute on change of input state
  ].


%%
%% Common Output Attributes
%% Block output values are modified upon block execution
%%
-spec outputs() -> output_attribs().

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
    {value, {null, []}}
  ].


%%
%% Common block initialization function
%%
-spec initialize(BlockDefn :: block_defn()) -> block_state().

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
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute(BlockState, ExecMethod) ->

  {Config, Inputs, Outputs, Private} = BlockState,
  BlockStatus = output_utils:get_status(Outputs),

  % Check block status before executing
  case ok_to_execute(BlockStatus, ExecMethod) of
    true ->
      {BlockName, BlockModule} = config_utils:name_module(Config),
    
      case input_utils:get_boolean(Inputs, disable) of
        {ok, true} -> % Block is disabled
          Outputs2 = output_utils:update_all_outputs(Outputs, null, disabled),
          % Nothing to update in private values
          Private1 = Private;
     
        {ok, _NotDisabled} -> % Disable input is false or null
          case input_utils:get_boolean(Inputs, freeze) of
            {ok, true} -> % Block is frozen
              % Just update the status output, all other outputs are frozen
              Outputs2 = output_utils:set_status(Outputs, frozen),
              % Nothing to update in private values
              Private1 = Private;

            {ok, _NotFrozen} -> % block is not disabled or frozen, execute it
              {Config, Inputs, Outputs1, Private1} = BlockModule:execute(BlockState, ExecMethod),
              Outputs2 = update_execute_track(Outputs1, ExecMethod);
                    
            {error, Reason} -> % Freeze input value error
              input_utils:log_error(Config, freeze, Reason),
              Outputs2 = output_utils:update_all_outputs(Outputs, null, input_err),
              % Nothing to update in private values
              Private1 = Private
          end;
        
        {error, Reason} -> % Disable input value error 
          input_utils:log_error(Config, disable, Reason),
          Outputs2 = output_utils:update_all_outputs(Outputs, null, input_err),
          % Nothing to update in private values
          Private1 = Private
      end,
      
      {Status, Private2} = update_execution_timer(BlockName, Inputs, Private1), 
    
      if (Status /= normal) ->  % Some kind error setting execution timer
        Outputs3 = output_utils:update_all_outputs(Outputs2, null, Status);
      true -> % Execution timer status is normal
        Outputs3 = Outputs2
      end,

      % Update the block inputs linked to the block outputs 
      % that have just been updated (Data Flow)
      update_blocks(Outputs, Outputs3),
    
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

    % if input value changed, allow block to execute if normal, disabled, frozen, or some kind of input error
    % The changed input value could have enabled, thawed, or otherwise fixed the input value error
    % i.e. execute if BlockStatus a member of this list
    input_cos ->
      lists:member(BlockStatus, [input_err, no_input, initialed, normal, disabled, frozen]);
    
    % If block received a message from a subsystem, execute the block if in one of these states:
    message ->
      lists:member(BlockStatus, [input_err, no_input, initialed, normal, disabled, frozen]);

    % If block execution timer expired, execute the block if in one of these states:
    timer ->
      lists:member(BlockStatus, [input_err, no_input, initialed, normal]);

    % If block executed via exec_out, execute the block if in one of these states:
    exec_out ->
      lists:member(BlockStatus, [input_err, no_input, initialed, normal]);

    _ ->
      % For any other execution method, don't execute if BlockStatus is a member of this list
      not lists:member(BlockStatus, [input_err, config_err, proc_err, no_input])
  end.


%%
%% Update the block execution timer 
%%
-spec update_execution_timer(BlockName :: block_name(),
                             Inputs :: input_attribs(),
                             Private :: private_attribs()) -> {atom(), private_attribs()}.
                             
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
        log_server:error(negative_exec_interval_value, [BlockName, ExecuteInterval])
      end
    end;
  true ->  % Execute Interval input value is not an integer
    Status = input_err, 
    NewTimerRef = empty,
    log_server:error(invalid_exec_interval_value, [BlockName, ExecuteInterval])
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
-spec set_timer(BlockName :: block_name(),
                ExecuteInterval :: pos_integer()) -> 
                reference().

set_timer(BlockName, ExecuteInterval) ->
  erlang:send_after(ExecuteInterval, BlockName, timer_execute).


%%
%% Track execute method and timestamp
%%
-spec update_execute_track(Outputs :: output_attribs(), 
                           ExecMethod :: exec_method()) -> output_attribs().

update_execute_track(Outputs, ExecMethod) ->

  % Record method of execution
  {ok, Outputs1} = attrib_utils:set_values(Outputs, [{exec_method, ExecMethod}, {exec_out, ExecMethod}]),

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
-spec update_blocks(CurrentOutputs :: output_attribs(), 
                    NewOutputs :: output_attribs()) -> ok.

update_blocks(CurrentOutputs, NewOutputs) ->
  update_blocks(CurrentOutputs, NewOutputs, []).

% No update messages to send
update_blocks([], [], []) -> 
  ok;

% After building the messages, send an update message to each linked block
update_blocks([], [], BlockUpdateMsgs) -> 
  lists:foreach(fun({ToBlockName, NewInputValues}) ->
                    block_server:update(ToBlockName, NewInputValues)
                end, 
                BlockUpdateMsgs),
  ok;

%
% Build update messages for each block in the links for the output values
% that have been modified
% BlockUpdateMsgs = [{BlockName1, [{ValueId11, NewValue11}, {ValueId12, NewValue12}, ...]}, 
%                    {BlockName2, [{ValueId21, NewValue21}, {ValueId22, NewValue22}, ...]}, 
%                      ...]
%
update_blocks([CurrentOutput | CurrentOutputs], 
              [NewOutput | NewOutputs], BlockUpdateMsgs)->

  case CurrentOutput of
    % Non-array value output
    {ValueName, {CurrentValue, Links}} ->
      {ValueName, {NewValue, Links}} = NewOutput,
      
      % For each output value that changed, add the updated value to
      % The messages that will be sent to the blocks linked to this output
      % Don't check the 'exec_out' output, that is for control flow execution
      if (CurrentValue /= NewValue) andalso (ValueName /= exec_out) -> 
        
        NewBlockUpdateMsgs = 
          lists:foldl(fun(Link, AccUpdateMsgs) -> 
                        add_update_msg(AccUpdateMsgs, Link, NewValue) 
                      end,
                      BlockUpdateMsgs,
                      Links);
      true -> 
        % else do nothing
        NewBlockUpdateMsgs = BlockUpdateMsgs
      end;
      
    % Array value output
    {ValueName, CurrentArrayValues} ->
      {ValueName, NewArrayValues} = NewOutput,
        NewBlockUpdateMsgs = check_array_values(BlockUpdateMsgs, ValueName,
                                                CurrentArrayValues, NewArrayValues) 
  end,

  update_blocks(CurrentOutputs, NewOutputs, NewBlockUpdateMsgs).


%%
%% Check the values in an array of output values
%% Loop through each value in the array. If value has changed,
%% Examine each Link, and add to the current BlockUpdateMsgs
%% 
-spec check_array_values(BlockUpdateMsgs :: list(),
                         ValueName :: value_name(),
                         CurrentArrayValues :: list(), 
                         NewArrayValues :: list()) -> list().

check_array_values(BlockUpdateMsgs, _ValueName, [], []) ->
  BlockUpdateMsgs;
  
check_array_values(BlockUpdateMsgs, ValueName,
                   [{CurrentValue, Links} | CurrentArrayValues], 
                   [{NewValue, Links} | NewArrayValues]) ->
                   
  if  CurrentValue /= NewValue -> 
    NewBlockUpdateMsgs = 
      lists:foldl(fun(Link, AccUpdateMsgs) -> 
                    add_update_msg(AccUpdateMsgs, Link, NewValue) 
                  end,
                  BlockUpdateMsgs,
                  Links);
  true -> 
    % else do nothing
    NewBlockUpdateMsgs = BlockUpdateMsgs
  end,
                   
  check_array_values(NewBlockUpdateMsgs, ValueName,
                     CurrentArrayValues, NewArrayValues).


%%
%% Add the new ValueId / Value to the block update message for the destination block
%% Add a new messsage to the list of messages,
%% if there is not a message already created for the destination block
%% 
-spec add_update_msg(BlockUpdateMsgs :: list(),
                     Link :: link_def(),
                     NewValue :: value()) -> list().

add_update_msg(BlockUpdateMsgs, {DestBlock, ValueId}, NewValue) ->
  case lists:keyfind(DestBlock, 1, BlockUpdateMsgs) of
    % List already contains a message for this destination block,
    % Add the ValueId/Value tuple to this block's list of values to update
    % If not already in list of ValueId/Value tuples
    {DestBlock, ExistingValues} -> 
      case lists:keyfind(ValueId, 1, ExistingValues) of
        false -> 
          NewUpdateMsg = {DestBlock, [{ValueId, NewValue} | ExistingValues]},
          lists:keyreplace(DestBlock, 1, BlockUpdateMsgs, NewUpdateMsg);

        _AleadyContains -> % List of ValueId/Value tuples already contains this ValueId
                           % Return the list of messages unchanged.
          BlockUpdateMsgs
      end;

    false -> % No message created for this destination block yet.
             % Just add destination block and list of one ValueId/Value tuple.
      [{DestBlock, [{ValueId, NewValue}]} | BlockUpdateMsgs]
  end.


%%
%% update each block input value, in the list of links
%%
-spec update_linked_inputs(NewValue :: value(),
                           Links :: link_defs()) -> ok.
                           
update_linked_inputs(NewValue, Links) ->

  lists:foreach(fun(Link) ->
                  case Link of
                    {ToBlockName, ToValueId} ->
                      block_server:update(ToBlockName, [{ToValueId, NewValue}]);

                    InvalidLink ->
                      log_server:error(err_unrecognized_link, [InvalidLink])
                  end
                end, 
                Links).


%%    
%% Send an exec_out_execute message to each block connected to the 'exec_out' output of this block
%% This will implement control flow execution, versus data flow done in the update_blocks function. 
%%
-spec update_execute(Outputs :: output_attribs()) -> ok.

update_execute(Outputs) ->

  % exec_out attribute is never an array, 
  % Don't need to worry about arrays of values or indexes here
  {ok, {exec_out,  {_Value, Links}}} = attrib_utils:get_attribute(Outputs, exec_out),

  lists:foreach(fun(Link) ->
                  case Link of
                    {ToBlockName, _ToValueId} ->
                      block_server:execute(ToBlockName, exec_out);

                    InvalidLink ->
                      log_server:error(err_unrecognized_link, [InvalidLink])
                  end
                end, 
                Links).


%%
%% Common block delete function, 
%% Return the updated block state, in case calling function wants to reuse it 
%%
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, Private}) ->
  
  {BlockName, BlockModule} = config_utils:name_module(Config),

  % Cancel execution timer if it exists
  case attrib_utils:get_value(Private, timer_ref) of
    {ok, empty}      -> ok;
    {ok, TimerRef}   -> erlang:cancel_timer(TimerRef);
    {error, _Reason} -> ok  % Don't care if timer_ref doesn't exist
  end,
    
  % Tell all other blocks on this node to remove any links to this block
  link_utils:unlink_block(BlockName),

  % Set all input values to default value
  DefaultInputs = input_utils:default_inputs(Inputs),
  
  % Set all output values of this block, including status, to 'empty'
  EmptyOutputs = output_utils:update_all_outputs(Outputs, empty, empty),
    
  % Update the block inputs linked to this block's outputs 
  % This will set the input value of any block 
  % linked to this deleted block, to 'empty'.
  update_blocks(Outputs, EmptyOutputs),
    
  % Execute the blocks connected to the exec_out output value
  % of this block, one last time.
  update_execute(EmptyOutputs),

  % Perform block type specific delete actions
  BlockModule:delete({Config, DefaultInputs, EmptyOutputs, Private}).


%%
%% Temporary delete function, 
%% Go through the motions of deleting a block without unlinking from other blocks
%% Used when a block needs to be reinitialized
%%
-spec temp_delete(BlockState :: block_state()) -> block_defn().

temp_delete({Config, Inputs, Outputs, Private}) ->
  
  % Cancel execution timer if it exists
  case attrib_utils:get_value(Private, timer_ref) of
    {ok, empty}      -> ok;
    {ok, TimerRef}   -> erlang:cancel_timer(TimerRef);
    {error, _Reason} -> ok  % Don't care if timer_ref doesn't exist
  end,

  {_BlockName, BlockModule} = config_utils:name_module(Config),

  % Perform block type specific delete actions
  BlockModule:delete({Config, Inputs, Outputs, Private}).
  

%% ====================================================================
%% Internal functions
%% ====================================================================
