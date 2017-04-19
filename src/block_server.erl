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
-export([
          start/1, 
          delete/1, 
          get_value/2, 
          set_value/3,
          set_link/3,
          override/2, 
          get_block/1,
          execute/2,
          execute/3,
          update/3,
          update/4, 
          init_configure/1, 
          configure/1, 
          reconfigure/2,
          link/3,
          link/4,
          unlink/3,
          unlink/4
]).



%% Start up a block with the given Name, Functionality, and Values
start(BlockValues)->
  BlockName = config_utils:name(BlockValues),
  gen_server:start_link({local, BlockName}, ?MODULE, BlockValues, []).


%% Delete the named block
delete(BlockName)->
  gen_server:call(BlockName, stop).


%% Get the value of 'Value ID' from this block
get_value(BlockName, ValueId)->
  gen_server:call(BlockName, {get_value, ValueId}).


%% Set the ValueId= Value in this block
-spec set_value(BlockName :: block_name(),
                ValueId :: value_id(),
                Value :: value()) -> ok | {error, atom()}.

set_value(BlockName, ValueId, Value)->
  gen_server:call(BlockName, {set_value, ValueId, Value}).


%% Link the Input value of this block, to the given Node/Block/Value (i.e. Link) 
-spec set_link(BlockName :: block_name(),
               InputValueId :: value_id(),
               Link :: input_link()) -> term().

set_link(BlockName, InputValueId, Link) ->
  gen_server:call(BlockName, {set_link, InputValueId, Link}).


%% Override the Value of this block with the given Value
override(BlockName, Value)->
  gen_server:call(BlockName, {override, Value}).


%% Get the current set of values for this block
get_block(BlockName) ->
  gen_server:call(BlockName, get_block).


%% Execute the block 
-spec execute(BlockName :: block_name(),
              Reason :: exec_method()) -> term().

execute(BlockName, Reason) ->
  gen_server:cast(BlockName, {execute, Reason}).

%% Execute block on another node
-spec execute(Node :: node(),
              BlockName :: block_name(),
              Reason :: exec_method()) -> term().

execute(Node, BlockName, Reason) ->
  gen_server:call({BlockName, Node}, {execute, Reason}).


%% Update block input value 
%% i.e. Implement Data Flow
-spec update(BlockName :: block_name(),
             Link :: input_link(),
             Value :: attr_value()) -> term().
            
update(BlockName, Link, Value) ->
  gen_server:cast(BlockName, {update, Link, Value}).

%% Update block input value on another node
-spec update(Node :: node(),
             BlockName :: block_name(),
             Link :: input_link(),
             Value :: attr_value()) -> term().

update(Node, BlockName, Link, Value) ->
  gen_server:cast({BlockName, Node}, {update, Link, Value}).


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


%% Link the output value: 'ValueId' of this block 
%% to to the <Node>:BlockName:InputValueId reference
-spec link(BlockName :: block_name(),
           ValueId :: value_id(),
           Reference :: block_name() | {node(), block_name()}) -> term().

link(BlockName, ValueId, Reference) ->
  gen_server:call(BlockName, {link, ValueId, Reference}).

%% Link block on another node
-spec link(Node :: node(),
           BlockName :: block_name(),
           ValueId :: value_id(),
           Reference :: block_name() | {node(), block_name()}) -> term().

link(Node, BlockName, ValueId, Reference) ->
  gen_server:call({BlockName, Node}, {link, ValueId, Reference}).


%% Unlink the output value: 'ValueId' of this block from BlockName:InputValueId reference
-spec unlink(BlockName :: block_name(),
             ValueId :: value_id(),
             Reference :: link_ref()) -> term().

unlink(BlockName, ValueId, Reference) ->
  gen_server:cast(BlockName, {unlink, ValueId, Reference}).

%% Unlink the block on another node
-spec unlink(Node :: node(),
             BlockName :: block_name(),
             ValueId :: value_id(),
             Reference :: link_ref()) -> term().

unlink(Node, BlockName, ValueId, Reference) ->
  gen_server:cast({BlockName, Node}, {unlink, ValueId, Reference}).


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

  log_server:info(initializing_block, [BlockName]),

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
handle_call(get_block, _From, BlockValues) ->
  {reply, BlockValues, BlockValues};

  
%% =====================================================================
%% Get a block value
%% =====================================================================    
handle_call({get_value, ValueId}, _From, BlockValues) ->
  Result = attrib_utils:get_value_any(BlockValues, ValueId),
  {reply, Result, BlockValues};


%% =====================================================================
%% Set a block value
%% ===================================================================== 
handle_call({set_value, ValueId, Value}, _From, BlockValues) ->
  {Config, Inputs, Outputs, Private} = BlockValues,

  case attrib_utils:is_attribute_type(Config, ValueId) of
    true ->
      % Modifying a config value, need to delete and re-initialize the block
      {Config1, Inputs1, Outputs1} = 
                             block_common:delete(BlockValues),

      case attrib_utils:set_value(Config1, ValueId, Value) of
        {ok, Config2} ->
          {Config3, Inputs2, Outputs2, Private1} = 
                      block_common:initialize({Config2, Inputs1, Outputs1}),

          BlockName = config_utils:name(Config3),
          % re-establish the links to other blocks 
          block_server:init_configure(BlockName),
          NewBlockValues = {Config3, Inputs2, Outputs2, Private1},
          Result = ok;

        {error, Reason} ->
          NewBlockValues = {Config1, Inputs1, Outputs1, Private},
          Result = {error, Reason}
      end;

    _ -> % Not a Config attribute, try the Inputs
      case attrib_utils:is_attribute_type(Inputs, ValueId) of
        true ->
          case attrib_utils:set_value(Inputs, ValueId, Value) of
            {ok, NewInputs} ->
              % Block input value changed, execute block with reason 'input_cos'
              NewBlockValues = 
               block_common:execute({Config, NewInputs, Outputs, Private}, input_cos),
               Result = ok;
            {error, Reason} ->
              NewBlockValues = BlockValues,
              Result = {error, Reason}
          end;

        _ ->  % Not an Input attribute, try the Outputs
          case attrib_utils:is_attribute_type(Outputs, ValueId) of
            true ->
              case attrib_utils:set_value(Outputs, ValueId, Value) of
                {ok, NewOutputs} ->
                  NewBlockValues = {Config, Inputs, NewOutputs, Private},
                  Result = ok;
                {error, Reason} ->
                  NewBlockValues = BlockValues,
                  Result = {error, Reason}
              end;

            _ -> % Not an Output either, do nothing
              NewBlockValues = BlockValues,
              Result = {error, not_found}
          end
      end
  end,
  {reply, Result, NewBlockValues};


%% =====================================================================
%% Set the link of the given input value to the given link value
%% ===================================================================== 
handle_call({set_link, InputValueId, Link}, _From, BlockValues) ->
  {Config, Inputs, Outputs, Private} = BlockValues,
 
  BlockName = config_utils:name(Config),
  case link_utils:set_link(BlockName, Inputs, InputValueId, Link) of
    {ok, Inputs1} ->

      % Attempt to get the current value from the link
      % set_link() always defaults the input value to 'empty'
      % so don't need to call get_value() to get the current input value
      Inputs2 = link_utils:evaluate_link(BlockName, InputValueId, empty,
                                         Link, Inputs1),

      % Execute the block because input value may have changed
      NewBlockValues = update_block({Config, Inputs2, Outputs, Private}),
      Result = ok;

    {error, Reason} ->
      Result = {error, Reason},
      NewBlockValues = {Config, Inputs, Outputs, Private}
  end,
  
  {reply, Result, NewBlockValues};


%% =====================================================================
%% Link the output value 'ValueId' of this block to the block Reference
%% =====================================================================
handle_call({link, ValueId, Reference}, _From, BlockValues) ->

  {Config, Inputs, Outputs, Private} = BlockValues,

  %% Add the block Reference to the output 'ValueId's list of block references
  NewOutputs = link_utils:add_ref(Outputs, ValueId, Reference),

  % Send the current value of this output to the Reference
  case attrib_utils:get_value(NewOutputs, ValueId) of 
    {ok, Value} -> ok;
 
    {error, Reason} ->
      log_server:error(err_fetching_value, [Reason, ValueId]),
      Value = not_active
  end,

  {reply, Value, {Config, Inputs, NewOutputs, Private}};
  
  
%% =====================================================================
%% Delete block
%% =====================================================================    
handle_call(stop, _From, BlockValues) ->

  BlockName = config_utils:name(BlockValues),    
  log_server:info(deleting_block, [BlockName]),
    
  % Perform common block delete actions
  block_common:delete(BlockValues),
  
  {stop, normal, ok, BlockValues};
    
  
%% =====================================================================
%% Unknown Call message
%% =====================================================================      
handle_call(Request, From, BlockValues) ->
  BlockName = config_utils:name(BlockValues),
  log_server:warning(block_server_unknown_call_msg_from, [BlockName, Request, From]),
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
handle_cast({execute, Reason}, BlockValues) ->

  % Execute the block
  NewBlockValues = block_common:execute(BlockValues, Reason),
  {noreply, NewBlockValues};


%% ====================================================================
%% Update this block's input value(s) with the block value received in this message
%% ====================================================================
handle_cast({update, Link, Value}, BlockValues) ->

  {Config, Inputs, Outputs, Private} = BlockValues,

  % Update the block input(s), that are linked this value, with the new Value
  NewInputs = link_utils:update_linked_input_values(Inputs, Link, Value),

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
   
  % Must be called separately from the init() function, 
  % because you can't call block_supervisor:block_names()
  % (i.e.supervisor:which_child()) from the init() function 
  % of a child process. 
  block_utils:configure_all_blocks(),
  
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
  BlockName = config_utils:name(BlockValues), 
  log_server:info(reconfiguring_block, [BlockName]),

  % Replace current state Block values with new values and configure block again
  % Check that new block values match the current block type and block name that is running
  %  TODO: Disconnect existing block links first
  configure(BlockName),
  {noreply, NewBlockValues};


%% =====================================================================
%% Unlink the output value 'ValueId' of this block from 'BlockName:InputValueId' Reference
%% =====================================================================
handle_cast({unlink, ValueId, Reference}, BlockValues) ->

  {Config, Inputs, Outputs, Private} = BlockValues,

  %% Remove Reference from the output ValueId's list of linked blocks References
  NewOutputs = link_utils:delete_ref(Outputs, ValueId, Reference),

  {noreply, {Config, Inputs, NewOutputs, Private}};


%% =====================================================================
%% Unknown Cast message
%% =====================================================================      
handle_cast(Msg, BlockValues) ->
  BlockName = config_utils:name(BlockValues),
  log_server:warning(block_server_unknown_cast_msg, [BlockName,Msg]),
  {noreply, BlockValues}.


%% ====================================================================
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

handle_info({gpio_interrupt, Pin, Condition}, BlockValues) ->
  log_server:debug("Rx interrupt: ~p from GPIO pin: ~p ~n", [Condition, Pin]),
  NewBlockValues = block_common:execute(BlockValues, hardware),
  {noreply, NewBlockValues};


%% =====================================================================
%% Timer timeout from erlang:send_after() function
%% Execute block with timer as the reason
%% =====================================================================

handle_info(timer_execute, BlockValues) ->
  NewBlockValues = block_common:execute(BlockValues, timer),
  {noreply, NewBlockValues};


%%======================================================================
%% Process messages from emqttc (MQTT Related functionality)
%%======================================================================

%% =====================================================================
%% MQTT Publish message from a subscribed to Topic.
%% =====================================================================
handle_info({publish, Topic, Payload}, BlockValues) ->
  {Config, Inputs, Outputs, Private} = BlockValues,
  BlockName = config_utils:name(Config),
  log_server:debug("~p Rx MQTT pub msg Topic: ~p Payload: ~p", 
                      [BlockName, Topic, Payload]),

  % Add the message topic and payload to the front of the list of pub messages,
  % and execute the block
  case attrib_utils:get_value(Private, pub_msgs) of
    {ok, PubMsgs} ->
      {ok, Private1} = attrib_utils:set_value(Private, pub_msgs, [{Topic, Payload} | PubMsgs]),
      NewBlockValues = block_common:execute({Config, Inputs, Outputs, Private1}, message);

    {error, Reason} ->
      log_server:error(err_updating_is_this_an_mqtt_pub_sub_block, [Reason, BlockName]),
      NewBlockValues = BlockValues
  end,
  {noreply, NewBlockValues};

%% =====================================================================
%% MQTT Client connected message
%% =====================================================================
handle_info({mqttc, _Client, connected}, BlockValues) ->
  {Config, Inputs, Outputs, Private} = BlockValues,
  BlockName = config_utils:name(Config),
  log_server:info(connected_to_MQTT_broker, [BlockName]),

  % Update the connection state in the block values and execute the block
  case attrib_utils:set_value(Private, conn_state, true) of
    {ok, Private1} ->
      NewBlockValues = block_common:execute({Config, Inputs, Outputs, Private1}, message);

    {error, Reason} ->
      log_server:error(err_updating_is_this_an_mqtt_pub_sub_block, [Reason, BlockName]),
      NewBlockValues = BlockValues
  end,
  {noreply, NewBlockValues};

%% =====================================================================
%% MQTT Client disconnected message
%% =====================================================================
handle_info({mqttc, _Client,  disconnected}, BlockValues) ->
    {Config, Inputs, Outputs, Private} = BlockValues,
    BlockName = config_utils:name(Config),
    log_server:info(disconnected_from_MQTT_broker, [BlockName]),
    
    % Update the connection state in the block values, and execute the block
    case attrib_utils:set_value(Private, conn_state, false) of
      {ok, Private1} ->
        NewBlockValues = block_common:execute({Config, Inputs, Outputs, Private1}, message);

      {error, Reason} ->
        log_server:error(err_updating_is_this_an_mqtt_pub_sub_block, [Reason, BlockName]),
        NewBlockValues = BlockValues
    end,
    {noreply, NewBlockValues};
  
%% =====================================================================
%% MQTT Client shutdown message
%% =====================================================================
handle_info({'EXIT', _Client, {shutdown, ShutdownReason}}, BlockValues) ->
    {Config, Inputs, Outputs, Private} = BlockValues,
    BlockName = config_utils:name(Config),
    log_server:info(mqtt_client_shutdown, [BlockName, ShutdownReason]),
    
    % Reset the client reference, to indicate MQTT client needs to be restarted
    case attrib_utils:set_value(Private, client, not_active) of
      {ok, Private1} ->
        NewBlockValues = block_common:execute({Config, Inputs, Outputs, Private1}, message);

      {error, Reason} ->
        log_server:error(err_updating_is_this_an_mqtt_pub_sub_block, [Reason, BlockName]),
        NewBlockValues = BlockValues
    end,
    {noreply, NewBlockValues};


%% =====================================================================
%% Unknown Info message
%% =====================================================================
handle_info(Info, State) ->
  log_server:warning(block_server_unknown_info_msg, [Info]),
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

  log_server:error(block_server_abnormal_termination, [BlockName, Reason]),
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
    block_common:execute({Config, NewInputs, Outputs, Private}, input_cos);

  true -> % Block will be executed via timer timeout or linked block execution, just return
    {Config, NewInputs, Outputs, Private}
  end.

  