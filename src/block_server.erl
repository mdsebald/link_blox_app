%% @author Mark Sebald
%% @doc Plock server.  gen_server behavior to execute custom block functionality


-module(block_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/1, delete/1, connect/3, disconnect/2, get/2, set/2, override/2, get_values/1]).
-export([execute/1, update/4, configure/1, reconfigure/2]).

%% Create a function block with the given Name, Functionality, and Values
create(BlockValues)->
	{BlockName, _BlockModule, _Conifgs, _Inputs, _Outputs, _Internals} = BlockValues,
	gen_server:start_link({local, BlockName}, ?MODULE, BlockValues, []).


%% Delete the named block
delete(BlockName)->
	gen_server:stop(BlockName).


%% Get the value of 'ValueName' from this block
get(BlockName, ValueName)->
	gen_server:call(BlockName, {get, ValueName}).


%% Set the Value = {ValueName, ValueType, Value...} in this block
set(BlockName, Value)->
	gen_server:call(BlockName, {set, Value}).


%% Override the Value of this block with the given Value = {ValueName, ValueType, Value}
override(BlockName, Value)->
	gen_server:call(BlockName, {override, Value}).

%% Get the current set of values for this block
get_values(BlockName) ->
	gen_server:call(BlockName, get_values).

%% Execute the block with the current set of values
execute(BlockName) ->
    gen_server:cast(BlockName, execute).

%% Send the given Value i.e {BlockName, ValueName, Value} to each block the list of BlockNames
%% i.e. Push the value from the output of one block to the inputs of the connected blocks
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
	{BlockName, BlockModule, _Conifgs, _Inputs, _Outputs, _Internals} = BlockValues,
	
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
%% This could come from another block, or the block may be operating on a timer 
%% ====================================================================
handle_cast(execute, CurrentBlockValues) ->
	
   {BlockName, BlockModule, _Conifgs, _Inputs, CurrentOutputs, _Internals} = CurrentBlockValues,
    
	% call custom evaluate() function
    % i.e. read inputs and calculate output values
	NewBlockValues = BlockModule:execute(CurrentBlockValues),
    
	%io:format("~p cast execute message NewBlockValues: ~p~n", [BlockName, NewBlockValues]),
	
	% Update each block connected to any of the outputs that changed when the block inputs were evaluated,  
	{BlockName, BlockModule, _NewConifgs, _NewInputs, NewOutputs, _NewInternals} = NewBlockValues,

	update_blocks(BlockName, CurrentOutputs, NewOutputs),

	{noreply, NewBlockValues};

%% ====================================================================
%% Update this block's input value(s) with the block value received in this message
%% ====================================================================
handle_cast({update, FromBlockName, ValueName, Value}, CurrentBlockValues) ->
	
	{BlockName, BlockModule, Conifgs, _Inputs, CurrentOutputs, _Internals} = CurrentBlockValues,
	
	% Update the block input(s), that are linked this value, with the new Value
	UpdatedInputBlockValues = block_utils:set_input_link_value(CurrentBlockValues, ValueName, FromBlockName, null, Value),
	
	% call custom evaluate() function
    % i.e. read inputs and calculate output values
	NewBlockValues = BlockModule:execute(UpdatedInputBlockValues),
	
	%io:format("~p cast update message NewBlockValues: ~p~n", [BlockName, NewBlockValues]),
	
	% Update each block connected to any of the outputs that changed when the block inputs were evaluated,  
	{BlockName, BlockModule, Conifgs, _NewInputs, NewOutputs, _NewInternals} = NewBlockValues,

	update_blocks(BlockName, CurrentOutputs, NewOutputs),

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
	{BlockName, _BlockModule, _Conifgs, Inputs, _Outputs, _Internals} = BlockValues,
	
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
	{BlockName, _BlockModule, _Conifgs, _Inputs, _Outputs, _Internals} = BlockValues, 
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
	
	{BlockName, _BlockModule, _Conifgs, _Inputs, _Outputs, _Internals} = BlockValues,
		
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
    
    {BlockName, BlockModule, _Conifgs, _Inputs, CurrentOutputs, _Internals} = CurrentBlockValues,
    % GPIO Interupt message from Erlang ALE library, 
    % Execute the block connected to this interrupt
    % io:format("Got ~p interrupt from pin# ~p ~n", [Condition, Pin]),
    NewBlockValues = BlockModule:execute(CurrentBlockValues),
    
    % Update each block connected to any of the outputs that changed when the block inputs were evaluated,  
	{BlockName, BlockModule, _Conifgs, _NewInputs, NewOutputs, _NewInternals} = NewBlockValues,

	update_blocks(BlockName, CurrentOutputs, NewOutputs),

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
	{_BlockName, BlockModule, _Conifgs, _Inputs, _Outputs, _Internals} = BlockValues,
	BlockModule:terminate(BlockValues),
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
	case CurrentValue /= NewValue of
		true ->
			update(Connections, FromBlockName, ValueName, NewValue),
			update_blocks(FromBlockName, RemainingCurrentOutputs, RemainingNewOutputs);

		false ->  
			% output values are same, just continue
	   		update_blocks(FromBlockName, RemainingCurrentOutputs, RemainingNewOutputs)
	end.
	
