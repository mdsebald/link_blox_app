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
-export([create/1, delete/1]). 
-export([get_value/2, set_value/3, override/2, get_values/1]).
-export([execute/1, exec_out_execute/1]).
-export([update/4, init_configure/1, configure/1, reconfigure/2]).
-export([link/3, unlink/3]).

%% Create a function block with the given Name, Functionality, and Values
create(BlockValues)->
  BlockName = config_utils:name(BlockValues),
  gen_server:start_link({local, BlockName}, ?MODULE, BlockValues, []).


%% Delete the named block
delete(BlockName)->
  gen_server:call(BlockName, stop).


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


%% Link the value 'ValueName' of this block to 'ToBlockName' 
link(BlockName, ValueName, ToBlockName) ->
  gen_server:call(BlockName, {link, ValueName, ToBlockName}).
  
%% Execute the block 
execute(BlockName) ->
  gen_server:cast(BlockName, execute).


%% This block's 'exec_in' is linked to an 'exec_out' output 
%% i.e Implement Control Flow
exec_out_execute(BlockName) ->
  gen_server:cast(BlockName, exec_out_execute).


%% Update this block's input value that is linked
%% to the given output value i.e {BlockName, ValueName, Value} 
%% In other words, push the value from the output of one block 
%% to the input(s) of the linked block
%% i.e. Implement Data Flow
%% TODO: Convert to a common Target Link: {NodeName, BlockName, ValueName} plus Value
update(BlockName, FromBlockName, ValueName, Value) ->
  gen_server:cast(BlockName, {update, FromBlockName, ValueName, Value}).


%% Perform initial configuration of the block.  
%% Block values are already in the state variable
init_configure(BlockName)->
  gen_server:cast(BlockName, init_configure).
  
  
%% Perform configuration of the block.
%% Review block inputs and configure links as necessary 
configure(BlockName)->
	gen_server:cast(BlockName, configure).


%% Reconfigure the block with the given set of block values
reconfigure(BlockName, BlockValues)->
  gen_server:cast(BlockName, {reconfigure, BlockValues}).


%% Unlink the value 'ValueName' of this block from the calling block 
unlink(BlockName, ValueName, ToBlockName) ->
  gen_server:cast(BlockName, {unlink, ValueName, ToBlockName}).


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
  BlockName = config_utils:name(BlockValues),

  error_logger:info_msg("Initializing: ~p~n", [BlockName]),

  %TODO: Need to perform a sanity check here, 
  % make sure BlockModule type and version, matches BlockValues type and version
 	
  % Perform block initialization
  NewBlockValues = block_common:initialize(BlockValues),
	
  % Perform intial configuration
  % Basically, tell all blocks to check their input links
  % and link to this block if necessary
  block_server:init_configure(BlockName),
  
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

%% =====================================================================
%% Get all block values
%% =====================================================================    
handle_call(get_values, _From, BlockValues) ->
  {reply, BlockValues, BlockValues};

  
%% =====================================================================
%% Get a block value
%% =====================================================================    
handle_call({get_value, ValueName}, _From, BlockValues) ->
  {ok, Value} = attrib_utils:get_value_any(BlockValues, ValueName),
  {reply, Value, BlockValues};


%% =====================================================================
%% Set a block value
%% ===================================================================== 
handle_call({set_value, ValueName, Value}, _From, BlockValues) ->
  NewBlockValues = attrib_utils:set_value_any(BlockValues, ValueName, Value),
  {reply, {ValueName, Value}, NewBlockValues};
  
  
%% =====================================================================
%% Link the value 'ValueName' of this block to the block 'ToBlockName'
%% =====================================================================
handle_call({link, ValueName, ToBlockName}, _From, BlockValues) ->

  {Config, Inputs, Outputs, Private} = BlockValues,
	
  %% Add the block 'ToBlockName' to the output 'ValueName's list of block references
  NewOutputs = link_utils:add_ref(Outputs, ValueName, ToBlockName),

  % Send the current value of this output to the block 'ToBlockName'
  {ok, Value} = attrib_utils:get_value(NewOutputs, ValueName),
 
  {reply, Value, {Config, Inputs, NewOutputs, Private}};
  
  
%% =====================================================================
%% Delete block
%% =====================================================================    
handle_call(stop, _From, BlockValues) ->

  BlockName = config_utils:name(BlockValues),    
  error_logger:info_msg("Deleting: ~p~n", [BlockName]),
    
  % Perform common block delete actions
  block_common:delete(BlockValues),
  
  {stop, normal, ok, BlockValues};
    
  
%% =====================================================================
%% Unknown Call message
%% =====================================================================      
handle_call(Request, From, BlockValues) ->
  error_logger:warning_msg("Unknown call message: ~p From: ~p~n", 
                            [Request, From]),
  {reply, ok, BlockValues}.


%%
%% handle_cast/2
%%
-spec handle_cast(Request :: term(), 
                  BlockValues :: block_state()) -> {noreply, block_state()}.


%% ====================================================================
%% Execute the block using the current set of Block values,
%% This message is used to directly execute the block evaluate function, 
%% ====================================================================
handle_cast(execute, BlockValues) ->

  % Execute the block
  NewBlockValues = block_common:execute(BlockValues, manual),
  {noreply, NewBlockValues};


%% ====================================================================
%% Execute the block using the current set of Block values,
%% This message is used to execute the block on command from another block,  
%% ====================================================================
handle_cast(exec_out_execute, BlockValues) ->
  % Execute the block code
  NewBlockValues = block_common:execute(BlockValues, exec_out),
  {noreply, NewBlockValues};


%% ====================================================================
%% Update this block's input value(s) with the block value received in this message
%% ====================================================================
handle_cast({update, FromBlockName, ValueName, Value}, BlockValues) ->
	
  {Config, Inputs, Outputs, Private} = BlockValues,
	
	% Update the block input(s), that are linked this value, with the new Value
	NewInputs = link_utils:update_linked_input_values(Inputs, {FromBlockName, ValueName}, Value),
	
  % Execute the block because input values have changed
  NewBlockValues = update_block({Config, NewInputs, Outputs, Private}),
 
	{noreply, NewBlockValues};

%% =====================================================================
%% Initial block configuration
%% =====================================================================
handle_cast(init_configure, BlockValues) ->
  % Send a configure message to each running block
  % This will force all blocks to check their input attributes
  % that have links, and link to this block if needed
   
  % Must  separately from the init() function, 
  % because you can't call block_supervisor:block_names()
  % (i.e.supervisor:which_child()) from the init() function 
  % of a child process. 
  BlockNames = block_supervisor:block_names(),
  Configure = fun(EachBlock) ->
                block_server:configure(EachBlock)
              end,
  lists:foreach(Configure, BlockNames),
  {noreply, BlockValues};


%% =====================================================================
%% Configure the block, with the block values passed in State variable
%% =====================================================================
handle_cast(configure, BlockValues) ->

  % Link inputs with links to output values of other blocks
  {Config, Inputs, Outputs, Private} = BlockValues,
  BlockName = config_utils:name(Config),
  NewInputs = link_utils:link_blocks(BlockName, Inputs),
  
  % Execute the block because input value(s) may have changed
  NewBlockValues = update_block({Config, NewInputs, Outputs, Private}),
 
  {noreply, NewBlockValues};


%% =====================================================================
%% Reconfigure the block, with the new set of block values, update State
%% =====================================================================
handle_cast({reconfigure, NewBlockValues}, BlockValues) ->
  % TODO: Sanity check make sure new block name, type and version 
  % match old block name, type and version/(same major rev)
  {Config, _Inputs, _Outputs, _Private} = BlockValues,
  BlockName = config_utils:name(Config), 
  error_logger:info_msg("~p: Reconfiguring block~n", [BlockName]),

  % Replace current state Block values with new values and configure block again
  % Check that new block values match the current block type and block name that is running
  %  TODO: Disconnect existing block links first
  configure(BlockName),
  {noreply, NewBlockValues};


%% =====================================================================
%% Unlink the value 'ValueName' of this block to the block 'ToBlockName'
%% =====================================================================
handle_cast({unlink, ValueName, ToBlockName}, BlockValues) ->

  {Config, Inputs, Outputs, Private} = BlockValues,
	
  %% remove the block 'ToBlockName' from the output 'ValueName's list of linked blocks
  NewOutputs = link_utils:delete_ref(Outputs, ValueName, ToBlockName),

  {noreply, {Config, Inputs, NewOutputs, Private}};


%% =====================================================================
%% Unknown Cast message
%% =====================================================================      
handle_cast(Msg, BlockValues) ->
  error_logger:warning_msg("Unknown cast message: ~p~n", [Msg]),
  {noreply, BlockValues}.


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


%% ====================================================================
%% GPIO Interupt message from Erlang ALE library, 
%% Execute the block connected to this interrupt
%% =====================================================================

handle_info({gpio_interrupt, _Pin, _Condition}, CurrentBlockValues) ->
  % io:format("Got ~p interrupt from pin# ~p ~n", [Condition, Pin]),
  NewBlockValues = block_common:execute(CurrentBlockValues, hardware),
  {noreply, NewBlockValues};


%% =====================================================================
%% Timer timeout from erlang:send_after() function
%% Execute block with timer as the reason
%% =====================================================================  
handle_info(timer_execute, CurrentBlockValues) ->
  NewBlockValues = block_common:execute(CurrentBlockValues, timer),
  {noreply, NewBlockValues};
  

%% =====================================================================
%% Unknown Info message
%% =====================================================================
handle_info(Info, State) ->
  error_logger:warning_msg("Unknown info message: ~p~n", [Info]),
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
terminate(normal, _BlockValues) ->
  ok;
    
terminate(Reason, BlockValues) ->
  BlockName = config_utils:name(BlockValues),

  error_logger:error_msg("Abnormal Termination: ~p  Reason: ~p~n", [BlockName, Reason]),
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

%%
%% Execute block on changed input values, 
%% This could happen on block configure after inputs are initially linked
%% or from an update message from a linked block that has executed 
%%
-spec update_block(block_state()) -> block_state().

update_block({Config, NewInputs, Outputs, Private}) ->
  
  % Don't execute block if block is executed via timer or executor execution
  % Just update the input values and leave it at that
  {ok, TimerRef} = attrib_utils:get_value(Private, timer_ref),
  {ok, {exec_in, {_Value, ExecuteLink}}} = attrib_utils:get_attribute(NewInputs, exec_in),
    
  if (TimerRef == empty) andalso (ExecuteLink == ?EMPTY_LINK) ->
    NewBlockValues = block_common:execute({Config, NewInputs, Outputs, Private}, 
                                                input_cos);

  true -> % Block will be executed via timer timeout or linked block execution, just return
    NewBlockValues = {Config, NewInputs, Outputs, Private}
  end,
  
  NewBlockValues.
  