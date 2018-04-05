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
          get_input_value/2,
          set_value/3,
          add_link/3,
          del_link/3,
          add_exec_in/2,
          del_exec_in/2,
          override/2, 
          get_block/1,
          execute/2,
          update/2,
          init_configure/1, 
          configure/1, 
          reconfigure/2,
          unlink_input/2,
          unlink_block/2,
          publish_values/3
]).



%% Start up a block with the given Name, Functionality, and Values
start(BlockState)->
  BlockName = config_utils:name(BlockState),
  gen_server:start_link({local, BlockName}, ?MODULE, BlockState, []).


%% Delete the named block
delete(BlockName)->
  gen_server:call(BlockName, stop).


%% Get the value of 'Value ID' from this block
-spec get_value(BlockName :: block_name(),
                ValueId :: value_id()) -> attrib_result_value().

get_value(BlockName, ValueId)->
  gen_server:call(BlockName, {get_value, ValueId}).


%% Get the value of 'Value ID' from this block, must be an input attribute
-spec get_input_value(BlockName :: block_name(),
                      ValueId :: value_id()) -> generic_input_value().

get_input_value(BlockName, ValueId)->
    gen_server:call(BlockName, {get_input_value, ValueId}).
 
  
%% Set the ValueId= Value in this block
-spec set_value(BlockName :: block_name(),
                ValueId :: value_id(),
                Value :: value()) -> ok | {error, atom()}.

set_value(BlockName, ValueId, Value)->
  gen_server:call(BlockName, {set_value, ValueId, Value}).


%% Link the Output value of this block, to the given input Block/Value (i.e. Link) 
-spec add_link(BlockName :: block_name(),
               ValueId :: value_id(),
               Link :: link_def()) -> term().

add_link(BlockName, ValueId, Link) ->
  gen_server:call(BlockName, {add_link, ValueId, Link}).

%% Unlink the Output value of this block, from the given input Block/Value (i.e. Link) 
-spec del_link(BlockName :: block_name(),
               ValueId :: value_id(),
               Link :: link_def()) -> term().

del_link(BlockName, ValueId, Link) ->
  gen_server:call(BlockName, {del_link, ValueId, Link}).


%% Indicate to this block, that it will be executed via control flow
%%  by adding the executor block name to its exec_in Value. 
-spec add_exec_in(BlockName :: block_name(),
                  ExecutorBlockName :: block_name()) -> ok | {error, atom()}.

add_exec_in(BlockName, ExecutorBlockName) ->
  gen_server:call(BlockName, {add_exec_in, ExecutorBlockName}).


%% Remove the executor block name from this block's exec_in Value. 
-spec del_exec_in(BlockName :: block_name(),
                  ExecutorBlockName :: block_name()) -> ok | {error, atom()}.

del_exec_in(BlockName, ExecutorBlockName) ->
  gen_server:call(BlockName, {del_exec_in, ExecutorBlockName}).


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


%% Update block input value 
%% i.e. Implement Data Flow
-spec update(BlockName :: block_name(),
             InputValues :: input_attribs()) -> term().
            
update(BlockName, InputValues) ->
  gen_server:cast(BlockName, {update, InputValues}).


%% Perform initial configuration of the block.  
%% Block values are already in the state variable
init_configure(BlockName)->
  gen_server:cast(BlockName, init_configure).
  
  
%% Perform configuration of the block.
%% Review block inputs and configure links as necessary 
configure(BlockName)->
  gen_server:cast(BlockName, configure).


%% Reconfigure the block with the given set of block values
reconfigure(BlockName, BlockState)->
  gen_server:cast(BlockName, {reconfigure, BlockState}).


%% Remove this Link from any of the block output values
-spec unlink_input(BlockName :: block_name(),
                   Link :: link_def()) -> term().

unlink_input(BlockName, Link) ->
  gen_server:cast(BlockName, {unlink_input, Link}).


%% Remove any Links in output values using the LinkBlockName
-spec unlink_block(BlockName :: block_name(),
                   LinkBlockName :: block_name()) -> term().

unlink_block(BlockName, LinkBlockName) ->
  gen_server:cast(BlockName, {unlink_block, LinkBlockName}).


%% Publish block values to a node and block.
%% block_server does not handle this message.
%% The message will be directed to the receive_values block module to be processed
-spec publish_values(Node :: node(),
                     BlockName :: block_name(),
                     Values :: block_values()) -> term().

publish_values(Node, BlockName, Values) ->
    gen_server:cast({BlockName, Node}, {publish_values, Values}).


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
init(BlockState) ->
  BlockName = config_utils:name(BlockState),

  logger:info(initializing_block, [BlockName]),

  % Perform block initialization
  NewBlockState1 = block_common:initialize(BlockState),

  % Perform intial configuration
  % Basically, tell all blocks to check their input links
  % and link to this block if necessary
  % block_server:init_configure(BlockName),
  % Should not need to reconfigure all blocks, just update (execute, if necessary) block

  % Update the block
  NewBlockState2 = update_block(NewBlockState1),
  
  {ok, NewBlockState2}.


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
handle_call(get_block, _From, BlockState) ->
  {reply, BlockState, BlockState};

  
%% =====================================================================
%% Get a block value
%% =====================================================================    
handle_call({get_value, ValueId}, _From, BlockState) ->
  Result = attrib_utils:get_value_any(BlockState, ValueId),
  {reply, Result, BlockState};


%% =====================================================================
%% Get a block input value
%% =====================================================================    
handle_call({get_input_value, ValueId}, _From, BlockState) ->
    {_Config, Inputs, _Outputs, _Private} = BlockState,
    Result = input_utils:get_any_type(Inputs, ValueId),
    {reply, Result, BlockState};


%% =====================================================================
%% Set a block value
%% ===================================================================== 
handle_call({set_value, ValueId, Value}, _From, BlockState) ->
  {Config, Inputs, Outputs, Private} = BlockState,

  case attrib_utils:is_attribute_type(Config, ValueId) of
    true ->
      % Modifying a config value, need to temporarily delete and re-initialize the block
      {Config1, Inputs1, Outputs1} = 
                             block_common:temp_delete(BlockState),

      case attrib_utils:set_value(Config1, ValueId, Value) of
        {ok, Config2} ->
          {Config3, Inputs2, Outputs2, Private1} = 
                      block_common:initialize({Config2, Inputs1, Outputs1}),

          % Update block after initialize
          NewBlockState = update_block({Config3, Inputs2, Outputs2, Private1}),
          Result = ok;

        {error, Reason} ->
          NewBlockState = {Config1, Inputs1, Outputs1, Private},
          Result = {error, Reason}
      end;

    _ -> % Not a Config attribute, try the Inputs
      case attrib_utils:is_attribute_type(Inputs, ValueId) of
        true ->
          % Setting input value from API, make it a fixed value so it is saved to the config file
          case input_utils:set_fixed_value(Inputs, ValueId, Value) of
            {ok, NewInputs} ->
              % Block input value changed, execute block with reason 'input_cos'
              NewBlockState = 
               block_common:execute({Config, NewInputs, Outputs, Private}, input_cos),
               Result = ok;
            {error, Reason} ->
              NewBlockState = BlockState,
              Result = {error, Reason}
          end;

        _ ->  % Not an Input attribute, try the Outputs
          case attrib_utils:is_attribute_type(Outputs, ValueId) of
            true ->
              case attrib_utils:set_value(Outputs, ValueId, Value) of
                {ok, NewOutputs} ->
                  NewBlockState = {Config, Inputs, NewOutputs, Private},
                  % Update inputs linked to the output value that changed
                  case output_utils:get_links(NewOutputs, ValueId) of
                    {ok, Links} ->
                      block_common:update_linked_inputs(Value, Links),
                      Result = ok;
                    {error, Reason} ->
                      Result = {error, Reason}
                  end;
                {error, Reason} ->
                  NewBlockState = BlockState,
                  Result = {error, Reason}
              end;

            _ -> % Not an Output either, do nothing
              NewBlockState = BlockState,
              Result = {error, not_found}
          end
      end
  end,
  {reply, Result, NewBlockState};


%% =====================================================================
%% Link the given output value to the given input link value
%% ===================================================================== 
handle_call({add_link, ValueId, Link}, _From, BlockState) ->
  {Config, Inputs, Outputs, Private} = BlockState,
  
  BlockName = config_utils:name(Config),
  case link_utils:add_link(BlockName, Outputs, ValueId, Link) of
    {ok, Outputs1} ->
      NewBlockState = {Config, Inputs, Outputs1, Private},

      % Set the added linked input value to current output value
      case attrib_utils:get_value(Outputs1, ValueId) of 
        {ok, Value} ->
          {ToBlockName, ToValueId} = Link,
          update(ToBlockName, [{ToValueId, Value}]),
          Result = ok;

        {error, Reason} ->
          Result = {error, Reason}
      end;
    {error, Reason} ->
      Result = {error, Reason},
      NewBlockState = {Config, Inputs, Outputs, Private}
  end,
  
  {reply, Result, NewBlockState};


%% =====================================================================
%% Unlink the given output value from the given input link value
%% ===================================================================== 
handle_call({del_link, ValueId, Link}, _From, BlockState) ->
    {Config, Inputs, Outputs, Private} = BlockState,

    BlockName = config_utils:name(Config),
    case link_utils:del_link(BlockName, Outputs, ValueId, Link) of
      {ok, Outputs1} ->
        % TODO: Set unlinked input value to default input value?
        Result = ok,
        NewBlockState = {Config, Inputs, Outputs1, Private};
  
      {error, Reason} ->
        Result = {error, Reason},
        NewBlockState = {Config, Inputs, Outputs, Private}
    end,
    
    {reply, Result, NewBlockState};


%% =====================================================================
%% Add the ExecutorBlockName to the list of block names on the exec_in input
%% to implement control flow
%% ===================================================================== 
handle_call({add_exec_in, ExecutorBlockName}, _From, BlockState) ->
    {Config, Inputs, Outputs, Private} = BlockState,
   
    case input_utils:add_exec_in(Inputs, ExecutorBlockName) of
      {ok, Inputs1} ->
        Result = ok,
        NewBlockState = {Config, Inputs1, Outputs, Private};
  
      {error, Reason} ->
        Result = {error, Reason},
        NewBlockState = {Config, Inputs, Outputs, Private}
    end,
    
    {reply, Result, NewBlockState};
  
%% =====================================================================
%% Delete the ExecutorBlockName from the list of block names on the exec_in input
%% to remove a control flow
%% ===================================================================== 
handle_call({del_exec_in, ExecutorBlockName}, _From, BlockState) ->
    {Config, Inputs, Outputs, Private} = BlockState,
   
    case input_utils:del_exec_in(Inputs, ExecutorBlockName) of
      {ok, Inputs1} ->
        Result = ok,
        NewBlockState = {Config, Inputs1, Outputs, Private};
  
      {error, Reason} ->
        Result = {error, Reason},
        NewBlockState = {Config, Inputs, Outputs, Private}
    end,
    
    {reply, Result, NewBlockState};
  
  
%% =====================================================================
%% Delete block
%% =====================================================================    
handle_call(stop, _From, BlockState) ->

  BlockName = config_utils:name(BlockState),    
  logger:info(deleting_block, [BlockName]),
    
  % Perform common block delete actions
  block_common:delete(BlockState),
  
  {stop, normal, ok, BlockState};
    
  
%% =====================================================================
%% For all other Call messages:
%%  Invoke the block type specific handle_call() function, if it exists
%% =====================================================================      
handle_call(Request, From, BlockState) ->
  Module = config_utils:module(BlockState),
  case erlang:function_exported(Module, handle_call, 3) of
    true ->
      Module:handle_call(Request, From, BlockState);
    false ->
      BlockName = config_utils:name(BlockState),
      logger:warning(block_server_unknown_call_msg_from, [BlockName, Request, From]),
      {reply, ok, BlockState}
  end.


%%
%% handle_cast/2
%%
-spec handle_cast(Request :: term(), 
                  BlockState :: block_state()) -> {noreply, block_state()}.


%% ====================================================================
%% Execute the block using the current set of Block values,
%% This message is used to directly execute the block execute function, 
%% ====================================================================
handle_cast({execute, Reason}, BlockState) ->

  % Execute the block
  NewBlockState = block_common:execute(BlockState, Reason),
  {noreply, NewBlockState};


%% ====================================================================
%% Update this block's input value(s) with the block value received in this message
%% ====================================================================
handle_cast({update, NewInputValues}, BlockState) ->

  {Config, Inputs, Outputs, Private} = BlockState,

  % TODO: Create an input_utils:set_link_values() function, just update the current value, not default value, current value not saved to config file
  %       attrib_utils:set_value(s)() will update the default value of the input too, and that value saved to config file
  % Update the block input(s), that are linked this value, with the new Value
  {ok, NewInputs} = attrib_utils:set_values(Inputs, NewInputValues),

  % Execute the block because input values have changed
  NewBlockState = update_block({Config, NewInputs, Outputs, Private}),
 
  {noreply, NewBlockState};


%% =====================================================================
%% Initial block configuration
%% =====================================================================
handle_cast(init_configure, BlockState) ->
  % Send a configure message to each running block
  % This will force all blocks to check their input attributes
  % that have links, and link to this block if needed
   
  % Must be called separately from the init() function, 
  % because you can't call block_supervisor:block_names()
  % (i.e.supervisor:which_child()) from the init() function 
  % of a child process. 
  block_utils:configure_all_blocks(),
  
  {noreply, BlockState};


%% =====================================================================
%% Configure the block, with the block values passed in State variable
%% =====================================================================
handle_cast(configure, BlockState) ->

  % Execute the block because input value(s) may have changed
  NewBlockState = update_block(BlockState),
 
  {noreply, NewBlockState};


%% =====================================================================
%% Reconfigure the block, with the new set of block values, update State
%% =====================================================================
handle_cast({reconfigure, NewBlockState}, BlockState) ->
  % TODO: Sanity check make sure new block name, type and version 
  % match old block name, type and version/(same major rev)
  BlockName = config_utils:name(BlockState), 
  logger:info(reconfiguring_block, [BlockName]),

  % Replace current state Block values with new values and configure block again
  % Check that new block values match the current block type and block name that is running
  %  TODO: Disconnect existing block links first
  configure(BlockName),
  {noreply, NewBlockState};


%% =====================================================================
%% Revmove the given link if it is in the list of Links of any output value
%% =====================================================================
handle_cast({unlink_input, Link}, BlockState) ->

  {Config, Inputs, Outputs, Private} = BlockState,
  BlockName = config_utils:name(Config),
  NewOutputs = link_utils:unlink_outputs(BlockName, Outputs, Link),

  {noreply, {Config, Inputs, NewOutputs, Private}};


%% =====================================================================
%% Revmove any link from the output values' list of Links,
%%   if the link connects to the LinkBlockName block.
%% =====================================================================
handle_cast({unlink_block, LinkBlockName}, BlockState) ->
    
  {Config, Inputs, Outputs, Private} = BlockState,
  BlockName = config_utils:name(Config),
  NewOutputs = link_utils:unlink_outputs_block(BlockName, Outputs, LinkBlockName),

  {noreply, {Config, Inputs, NewOutputs, Private}};


%% =====================================================================
%% For all other Cast messages:
%%  Invoke the block type specific handle_cast() function, if it exists
%% =====================================================================      
handle_cast(Msg, BlockState) ->
  Module = config_utils:module(BlockState),
  case erlang:function_exported(Module, handle_cast, 2) of
    true ->
      Module:handle_cast(Msg, BlockState);
    false ->
      BlockName = config_utils:name(BlockState),
      logger:warning(block_server_unknown_cast_msg, [BlockName, Msg]),
      {noreply, BlockState}
  end.


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


%% =====================================================================
%% Timer timeout from erlang:send_after() function
%% Execute block with timer as the reason
%% =====================================================================

handle_info(timer_execute, BlockState) ->
  NewBlockState = block_common:execute(BlockState, timer),
  {noreply, NewBlockState};


%% ======================================================================
%% For all other Info messages:
%%  Invoke the block type specific handle_info() function, if it exists
%% ======================================================================
handle_info(Info, BlockState) ->
  Module = config_utils:module(BlockState),
  case erlang:function_exported(Module, handle_info, 2) of
    true ->
      Module:handle_info(Info, BlockState);
    false ->
      BlockName = config_utils:name(BlockState),
      logger:warning(block_server_unknown_info_msg, [BlockName, Info]),
      {noreply, BlockState}
  end.


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
    
terminate(Reason, BlockState) ->
  BlockName = config_utils:name(BlockState),

  logger:error(block_server_abnormal_termination, [BlockName, Reason]),
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
  
  % Don't execute block if block is executed via timer or control flow
  %   i.e. exec_in list value, contains one or more block names
  % Just update the input values and leave it at that
  {ok, TimerRef} = attrib_utils:get_value(Private, timer_ref),
  {ok, ExecInValue} = attrib_utils:get_value(NewInputs, exec_in),
    
  if (TimerRef == empty) andalso (ExecInValue == []) ->
    block_common:execute({Config, NewInputs, Outputs, Private}, input_cos);

  true -> % Block will be executed via timer timeout or linked block execution, just return
    {Config, NewInputs, Outputs, Private}
  end.

  