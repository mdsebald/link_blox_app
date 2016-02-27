%%% @doc 
%%% Block server.  gen_server behavior to execute custom block functionality
%%%
%%% @end

-module(block_server).

-author("Mark Sebald").

-include("block_state.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/1, delete/1, connect/3, disconnect/2, get_value/2, set_value/3, override/2, get_values/1]).
-export([execute/1, timer_execute/1, execute_out_execute/1, update/4, configure/1, reconfigure/2]).

%% Create a function block with the given Name, Functionality, and Values
create(BlockValues)->
	{BlockName, _BlockModule, _Config, _Inputs, _Outputs, _Private} = BlockValues,
	gen_server:start_link({local, BlockName}, ?MODULE, BlockValues, []).


%% Delete the named block
delete(BlockName)->
	gen_server:stop(BlockName).


%% Get the value of 'ValueName' from this block
get_value(BlockName, ValueName)->
	gen_server:call(BlockName, {get_value, ValueName}).


%% Set the ValueName = Value in this block
set_value(BlockName, ValueName, Value)->
	gen_server:call(BlockName, {set_value, ValueName, Value}).


%% Override the Value of this block with the given Value
override(BlockName, Value)->
	gen_server:call(BlockName, {override, Value}).

%% Get the current set of values for this block
get_values(BlockName) ->
	gen_server:call(BlockName, get_values).

%% Execute the block 
execute(BlockName) ->
    gen_server:cast(BlockName, execute).
    
%% Execute the block on timer timeout
timer_execute(BlockName) ->
    gen_server:cast(BlockName, timer_execute).
    
%% Execute blocks connected to the 'execute_out' output 
%% i.e Control Flow
execute_out_execute([]) ->
    ok;
execute_out_execute(BlockNames) ->
    [BlockName | RemainingBlockNames] = BlockNames,
    gen_server:cast(BlockName, execute_out_execute),
    execute_out_execute(RemainingBlockNames).

%% Send the given Value i.e {BlockName, ValueName, Value} to each block the list of BlockNames
%% Push the value from the output of one block to the inputs of the connected blocks
%% i.e. Data Flow
%% TODO: Convert to a common Target Link: {NodeName, BlockName, ValueName} plus Value
update([], _FromBlockName, _ValueName, _Value) ->
	ok;
update(BlockNames, FromBlockName, ValueName, Value) ->
	[BlockName | RemainingBlockNames] = BlockNames,
	%io:format("~p sending update message to: ~p~n", [FromBlockName, BlockName]),
	gen_server:cast(BlockName, {update, FromBlockName, ValueName, Value}),
	update(RemainingBlockNames, FromBlockName, ValueName, Value).


%% Perform initial configuration of the block.  Block values are already in the state variable
configure(BlockName)->
	%io:format("Send configure cast message to: ~p~n", [BlockName]),
	gen_server:cast(BlockName, configure).


%% Reconfigure the block with the given set of block values
reconfigure(BlockName, BlockValues)->
	gen_server:cast(BlockName, {reconfigure, BlockValues}).


%% Connect the value 'ValueName' of this block to 'ToBlockName' 
connect(BlockName, ValueName, ToBlockName) ->
	gen_server:cast(BlockName, {connect, ValueName, ToBlockName}).


%% Disconnect the value 'ValueName' of this block from the calling block 
disconnect(BlockName, ValueName) ->
	gen_server:cast(BlockName, {disconnect, ValueName}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================



	

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init(BlockValues) ->
	{BlockName, BlockModule, _Config, _Inputs, _Outputs, _Private} = BlockValues,
	
	%TODO: Need to perform a sanity check here, make sure BlockModule type and version, matches BlockValues type and version
   	
	% Perform custom block initialization if needed
	NewBlockValues = BlockModule:initialize(BlockValues),
		
	% Call configure, to send a configure cast message to self
	% Already have initial block values in State variable, 
	configure(BlockName),

    {ok, NewBlockValues}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================

handle_call(get_values, _From, BlockValues) ->
	{reply, BlockValues, BlockValues};
    
handle_call({get_value, ValueName}, _From, BlockValues) ->
    Value = block_utils:get_value(BlockValues, ValueName),
    {reply, Value, BlockValues};

handle_call({set_value, ValueName, Value}, _From, BlockValues) ->
    NewBlockValues = block_utils:set_value(BlockValues, ValueName, Value),
    {reply, {ValueName, Value}, NewBlockValues};
	
handle_call(Request, From, BlockValues) ->
	io:format("Unknown call message: ~p From: ~p~n", [Request, From]),
    {reply, ok, BlockValues}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================

%% ====================================================================
%% Execute the block using the current set of Block values,
%% This message is used to directly execute the block evaluate function, 
%% ====================================================================
handle_cast(execute, CurrentBlockValues) ->
	% Execute the block code
	NewBlockValues = execute_block(CurrentBlockValues),
	{noreply, NewBlockValues};
    
%% ====================================================================
%% Execute the block using the current set of Block values,
%% This message is used to execute the block on a timer timeout, 
%% ====================================================================
handle_cast(timer_execute, CurrentBlockValues) ->
	% Execute the block code
	NewBlockValues = execute_block(CurrentBlockValues),
	{noreply, NewBlockValues};


%% ====================================================================
%% Execute the block using the current set of Block values,
%% This message is used to execute the block on command from another block,  
%% ====================================================================
handle_cast(execute_out_execute, CurrentBlockValues) ->
	% Execute the block code
	NewBlockValues = execute_block(CurrentBlockValues),
	{noreply, NewBlockValues};

%% ====================================================================
%% Update this block's input value(s) with the block value received in this message
%% ====================================================================
handle_cast({update, FromBlockName, ValueName, Value}, CurrentBlockValues) ->
	
	{BlockName, BlockModule, Config, Inputs, CurrentOutputs, Private} = CurrentBlockValues,
	
	% Update the block input(s), that are linked this value, with the new Value
	NewInputs = update_linked_input_values(Inputs, ValueName, FromBlockName, null, Value),
	
    % Don't execute block if block is executed via timer or executor execution
    % Just update the input values and leave it at that
    TimerRef = block_utils:get_private_value(Private, timer_ref),
    {execute_in, _Value, ExecuteLink} = block_utils:get_attribute_value(NewInputs, execute_in),
    
    % TODO: Make {fixed, null, null} a constant null_link
    if (TimerRef == empty) andalso (ExecuteLink == {fixed, null, null}) ->
        % Block is executed via change of input value, Data Flow
	    NewBlockValues = execute_block({BlockName, BlockModule, Config, NewInputs, CurrentOutputs, Private});
	true -> % Block will be executed via timer timeout or linked block execution, just return
        NewBlockValues = {BlockName, BlockModule, Config, NewInputs, CurrentOutputs, Private}
    end,

	{noreply, NewBlockValues};

%% =====================================================================
%% Configure the block, with the block values passed in State variable
%% =====================================================================
handle_cast(configure, BlockValues) ->
	% Perform configuration in a message handling routine because
	% it may take several config passes as we wait for other blocks to start up
	
	
	%io:format("~p cast configure message~n", [BlockName]),
	
	%io:format("~p BlockValues: ~p~n", [BlockName, BlockValues]),
	
	% If any of this block's inputs are pointing at another block that is not running / registered yet
	% Set the block status output and last error output, delay, and call configuration again.
	{BlockName, _BlockModule, _Config, Inputs, _Outputs, _Private} = BlockValues,
	
	case unregistered_blocks(Inputs) of
		ok ->
			io:format("~p all linked blocks running~n", [BlockName]),
			% All blocks connected to this block's inputs are running/registered.
			% Connect this block's inputs to other block's outputs as specified by the input linkss
			connect_blocks(BlockName, Inputs);
				
		{error, MissingBlockName} ->
			io:format("~p waiting for ~p to start~n", [BlockName, MissingBlockName]),
			% Not all blocks connected to this block or runnning/registerd yet.
			% Delay and try configuring again
			block_utils:sleep(100),  % TODO: Is this the correct delay?, could it be shorter? 
			configure(BlockName)
	end,
	
	{noreply, BlockValues};


%% =====================================================================
%% Reconfigure the block, with the new set of block values, update State
%% =====================================================================
handle_cast({reconfigure, NewBlockValues}, BlockValues) ->
	% TODO: Sanity check make sure new block name, type and version match old block name, type and version/(same major rev)
	{BlockName, _BlockModule, _Config, _Inputs, _Outputs, _Private} = BlockValues, 
	io:format("~p: Reconfiguring block~n", [BlockName]),

	% Replace current state Block values with new values and configure block again
	% Check that new block values match the current block type and block name that is running
	% Disconnect existing block links first
	configure(BlockName),
	{noreply, NewBlockValues};

%% =====================================================================
%% Connect the value 'ValueName' of this block to the block 'ToBlockName'
%% =====================================================================
handle_cast({connect, ValueName, ToBlockName}, BlockValues) ->
	
	{BlockName, _BlockModule, _Config, _Inputs, _Outputs, _Private} = BlockValues,
		
	%% Add the connection to 'ToBlockName' to this output 'ValueName's list of connections
	NewBlockValues = block_utils:add_connection(BlockValues, ValueName, ToBlockName),

	%io:format("~p connecting NewBlockValues: ~p~n", [BlockName, NewBlockValues]),
	
	% Send the current value of this output to the block 'ToBlockName'
	Value = block_utils:get_value(BlockValues, ValueName),
	
	% Make ToBlockName into a list of 1 connection
	update([ToBlockName], BlockName, ValueName, Value),
			
	{noreply, NewBlockValues};
	
handle_cast(Msg, State) ->
	io:format("Unknown cast message: ~p~n", [Msg]),
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({gpio_interrupt, _Pin, _Condition}, CurrentBlockValues) ->

    % GPIO Interupt message from Erlang ALE library, 
    % Execute the block connected to this interrupt
    % io:format("Got ~p interrupt from pin# ~p ~n", [Condition, Pin]),
    NewBlockValues = execute_block(CurrentBlockValues),

    {noreply, NewBlockValues};

handle_info(Info, State) ->
    io:format("Unknown info message: ~p~n", [Info]),
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, BlockValues) ->
	{_BlockName, BlockModule, _Config, _Inputs, _Outputs, _Private} = BlockValues,
    % TODO: Need to unlink this block from the supervisor, or it will just get restarted
    % TODO: delete the links between this block and other blocks
	BlockModule:delete(BlockValues),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


%% Return the name of the first block connected to this block, that is not registered, 
%% Return 'ok' if all bocks connected to this block are registered / running
unregistered_blocks([])->
	ok;
unregistered_blocks(BlockInputs)->
	
	[Input | RemainingInputs] = BlockInputs,

	{_ValueName, _Value, Link} = Input,
	% get the value name, block name, and node name components of this input value link
	
	{LinkValueName, LinkBlockName, _LinkNodeName} = Link,   %TODO: Handle getting values from other nodes
	
	% if this input is not a fixed value
	if LinkValueName /= fixed ->
		% if the block name of this link is not null
		if LinkBlockName /= null ->
			case whereis(LinkBlockName) of
				undefined  -> {error, LinkBlockName};
		        	 _Pid  -> unregistered_blocks(RemainingInputs)
			end;
		true ->
			unregistered_blocks(RemainingInputs)
		end;
	true -> 
		unregistered_blocks(RemainingInputs)
	end.


%% Send a connect message to each block linked to the input of this block
connect_blocks(BlockName, BlockInputs)->
	connect_blocks(BlockName, BlockInputs, 0).

connect_blocks(_BlockName, [], ConnectionsRequested)->
	{ok, ConnectionsRequested};

connect_blocks(BlockName, BlockInputs, ConnectionsRequested)->
	
	[Input | RemainingBlockInputs] = BlockInputs,
	
	{ValueName, Value, Link} = Input,
	{LinkValueName, LinkBlockName, _LinkNodeName} = Link, %TODO: Handle getting values from other nodes
	
	% if this input is not a fixed value
	if LinkValueName /= fixed ->
		   
		% if the block name of this link is not null
		if LinkBlockName /= null ->
			   
			%if the block input value is still empty, send a connect message to the block linked to this input
			if Value == empty ->
				io:format("Connecting Output <~p:~p> To Input <~p:~p>~n", [LinkBlockName, LinkValueName, BlockName, ValueName]),
				connect(LinkBlockName, LinkValueName, BlockName),
				connect_blocks(BlockName, RemainingBlockInputs, ConnectionsRequested + 1);
			true ->
				connect_blocks(BlockName, RemainingBlockInputs, ConnectionsRequested)
			end;
		true ->
			connect_blocks(BlockName, RemainingBlockInputs, ConnectionsRequested)
		end;
	true -> 
		connect_blocks(BlockName, RemainingBlockInputs, ConnectionsRequested)
	end.


%% Send an update message to each block connected to any output value that has changed
%% This assumes CurrentOutputs and NewOutputs, have the same ValueNames and order for all outputs
update_blocks(_FromBlockName, [], [])-> 
	%io:format("~p compared all outputs~n", [FromBlockName]),
	ok;

update_blocks(FromBlockName, CurrentOutputs, NewOutputs)->
	
	[CurrentOutput | RemainingCurrentOutputs] = CurrentOutputs,
	[NewOutput | RemainingNewOutputs] = NewOutputs,
	
	{ValueName, CurrentValue, Connections} = CurrentOutput,
	{ValueName, NewValue, Connections} = NewOutput,
	
	%io:format("~p update_blocks, ValueName: ~p, comparing CurrentValue: ~p and NewValue: ~p~n", [FromBlockName, ValueName, CurrentValue, NewValue]),

    % For each output value that changed, call update() to send a the new value message to each connected block.
    % don't check the 'execute_out' output, that is for control flow execution
	if (ValueName /= execute_out) andalso (CurrentValue /= NewValue) -> 
        update(Connections, FromBlockName, ValueName, NewValue);
    true -> ok % else do nothing
    end,
    
    update_blocks(FromBlockName, RemainingCurrentOutputs, RemainingNewOutputs).
  
%%    
%% Send an execute message to each block connected to the 'execute_out' output of this block
%% This will implement control flow execution, versus data flow done in the update_blocks function. 
%%
update_execute(Outputs)->
	
    {execute_out,  _Value, BlockNames} = block_utils:get_attribute_value(Outputs, execute_out),
    execute_out_execute(BlockNames).


%%
%% Execute the block code
%%
execute_block(BlockValues) ->

    {BlockName, BlockModule, Config, Inputs, Outputs, Private} = BlockValues,
    
    EnableInput = block_utils:get_input_value(Inputs, enable),
    
    if is_boolean(EnableInput) ->
        if EnableInput -> % Block is enabled
            {_, _, _, _, NewOutputsX, NewPrivateX} = BlockModule:execute(BlockValues),
            NewStatus = block_utils:get_output_value(NewOutputsX, status),
            if  NewStatus == normal ->
                NewPrivateY = update_execute_track(NewPrivateX);
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
        io:format("~p Error: Invalid enable Input value: ~p ~n", [BlockName, EnableInput]),
        NewOutputsX = update_all_outputs(Outputs, not_active, input_error),
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
    
    % Execute the blocks connected to the execute_out output value (Control Flow)
    update_execute(NewOutputs),
    
    % Return the new updated block state
    {BlockName, BlockModule, Config, Inputs, NewOutputs, NewPrivate}.



%%
%% Update the block execution timer 
%% Return status and updated Timer Reference 
%%
update_execution_timer(BlockName, Inputs, Private) ->

    ExecuteInterval = block_utils:get_input_value(Inputs, execute_interval),
    TimerRef = block_utils:get_private_value(Private, timer_ref),
    
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
                Status = input_error, 
                NewTimerRef = empty,
                io:format("~p Error: Negative execute_interval Input value: ~p ~n", [BlockName, ExecuteInterval])
            end
        end;
    true ->  % Execute Interval input value is not an integer
        Status = input_error, 
        NewTimerRef = empty,
        io:format("~p Error: Invalid execute_interval Input value: ~p ~n", [BlockName, ExecuteInterval])
    end,
    NewPrivate = block_utils:set_private_value(Private, timer_ref, NewTimerRef),
    {Status, NewPrivate}.
    
% Cancel block execution timer, if the timer is set   
cancel_timer(BlockName, TimerRef) ->
    if (TimerRef /= empty) ->
        case timer:cancel(TimerRef) of 
            {ok, cancel} -> 
                ok;
            
            {error, Reason} ->
                io:format("~p Error: ~p Canceling execution timer ~p ~n", [BlockName, Reason, TimerRef]),
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
            io:format("~p Error: ~p Setting execution timer ~n", [BlockName, Reason]),
            {process_error, empty}
    end.   	

% Track execute time and count
update_execute_track(Private) ->
    % Record last executed timestamp
	NewPrivate = block_utils:set_private_value(Private, last_exec, erlang:monotonic_time(micro_seconds)),

	% Arbitrarily roll over Execution Counter at 999,999,999
	case block_utils:get_private_value(NewPrivate, exec_count) + 1 of
		1000000000   -> block_utils:set_private_value(NewPrivate, exec_count, 0);
		NewExecCount -> block_utils:set_private_value(NewPrivate, exec_count, NewExecCount)
	end.
    
    
%%    
%% Update the value of every input in this block linked to the 
%% 'ValueName:FromBlockName:NodeName' output value
%% Return the updated list of Input values
%% TODO: Switch to passing around a target link {NodeName, BlockName, ValueName}
%%
update_linked_input_values(Inputs, NewValueName, FromBlockName, NodeName, NewValue) ->

	TargetLink = {NewValueName, FromBlockName, NodeName},

	% Update the value of each input record pointing at the given value 
	lists:map(
		fun(Input) -> 
			{ValueName, _Value, Link} = Input,
			case Link =:= TargetLink of
				true  -> {ValueName, NewValue, Link};
				false -> Input	% This block input is not linked to the target block output, don't change the input value 
			end
		end, 
		Inputs).
 
 
%% 
%% Update all outputs to the New value,
%% except update status output to the New Staus value
%% Used to mass update block outputs in disabled or error conditions
%% 
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
