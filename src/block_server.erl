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
          set_def_value/2,
          add_link/3,
          del_link/3,
          add_exec_link/2,
          del_exec_link/2,
          override/2, 
          get_block/1,
          execute/2,
          execute/3,
          update/2,
          init_configure/1, 
          update/1, 
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
 
  
%% Set the ValueId = Value in this block
-spec set_value(BlockName :: block_name(),
                ValueId :: value_id(),
                Value :: value()) -> ok | {error, atom()}.

set_value(BlockName, ValueId, Value)->
  gen_server:call(BlockName, {set_value, ValueId, Value}).


%% Set the input ValueId to the default value in this block
-spec set_def_value(BlockName :: block_name(),
                ValueId :: value_id()) -> ok | {error, atom()}.

set_def_value(BlockName, ValueId)->
  gen_server:call(BlockName, {set_def_value, ValueId}).


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


%% Link the exec_out output of this block to the exec_in input of the given block
-spec add_exec_link(BlockName :: block_name(),
                    ExecuteeBlockName :: block_name()) -> term().

add_exec_link(BlockName, ExecuteeBlockName) ->
  gen_server:call(BlockName, {add_exec_link, ExecuteeBlockName}).


%% Unlink the exec_out output of this block from the exec_in input of the given block
-spec del_exec_link(BlockName :: block_name(),
                    ExecuteeBlockName :: block_name()) -> term().

del_exec_link(BlockName, ExecuteeBlockName) ->
  gen_server:call(BlockName, {del_exec_link, ExecuteeBlockName}).


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


%% Execute the block, indicate the executor block name
-spec execute(BlockName :: block_name(),
              ExecutorBlockName :: block_name(),
              exec_out) -> term().

execute(BlockName, ExecutorBlockName, exec_out) ->
  gen_server:cast(BlockName, {execute, ExecutorBlockName, exec_out}).


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
  
  
%% Perform an update of the block
%% i.e. Attempt to execute the block
update(BlockName)->
  gen_server:cast(BlockName, update).


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

  m_logger:debug(starting_server_for_block, [BlockName]),

  % Initialize and execute the new block
  % Do this via message outside the init() function.
  % Forces the block type code to wait for init() to complete 
  % before sending or receiving messages.
  % init_configure(BlockName),

  {ok, BlockState}.


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
%% Set a block input to its default value
%% ===================================================================== 
handle_call({set_def_value, ValueId}, _From, BlockState) ->
  {Config, Inputs, Outputs, Private} = BlockState,
  case input_utils:set_to_default(Inputs, ValueId) of
    {ok, NewInputs} ->
      Result = ok,
      NewBlockState = {Config, NewInputs, Outputs, Private};

    {error, Reason} ->
      Result = {error, Reason},
      NewBlockState = BlockState
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
        % Set unlinked input value to default value
        {ToBlockName, ToValueId} = Link,
        case block_server:set_def_value(ToBlockName, ToValueId) of
          ok -> ok;

          {error, Reason} ->
            m_logger:error(err_setting_default_value, [Reason, ToBlockName, ValueId])
        end,

        Result = ok,
        NewBlockState = {Config, Inputs, Outputs1, Private};
  
      {error, Reason} ->
        Result = {error, Reason},
        NewBlockState = {Config, Inputs, Outputs, Private}
    end,
    
    {reply, Result, NewBlockState};


%% =====================================================================
%% Link the exec_out output value to the given block exec_in input value
%% ===================================================================== 
handle_call({add_exec_link, ExecuteeBlockName}, _From, BlockState) ->
  {Config, Inputs, Outputs, Private} = BlockState,
  
  BlockName = config_utils:name(Config),
  case link_utils:add_link(BlockName, Outputs, exec_out, {ExecuteeBlockName, exec_in}) of
    {ok, Outputs1} ->
      block_supervisor:add_exec_link(ExecuteeBlockName, BlockName),
      Result = ok,
      NewBlockState = {Config, Inputs, Outputs1, Private};

    {error, Reason} ->
      Result = {error, Reason},
      NewBlockState = {Config, Inputs, Outputs, Private}
  end,
  
  {reply, Result, NewBlockState};


%% =====================================================================
%% Unlink the exec_out output value from the given exec_in input link value
%% ===================================================================== 
handle_call({del_exec_link, ExecuteeBlockName}, _From, BlockState) ->
    {Config, Inputs, Outputs, Private} = BlockState,

    BlockName = config_utils:name(Config),
    case link_utils:del_link(BlockName, Outputs, exec_out, {ExecuteeBlockName, exec_in}) of
      {ok, Outputs1} ->
        % Set exec_in input value to default value
        case block_server:set_def_value(ExecuteeBlockName, exec_in) of
          ok -> ok;

          {error, Reason} ->
            m_logger:error(err_setting_default_value, [Reason, ExecuteeBlockName, exec_in])
        end,
        block_supervisor:del_exec_link(ExecuteeBlockName, BlockName),
        Result = ok,
        NewBlockState = {Config, Inputs, Outputs1, Private};
  
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
  m_logger:info(deleting_block, [BlockName]),
    
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
      m_logger:warning(block_server_unknown_call_msg_from, [BlockName, Request, From]),
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
%% Execute the block using the current set of Block values,
%% This message is used to execute the block via the exec_out to exec_in link (Control Flow).  
%% As such, indicate which block is executing this block by setting the 
%% exec_in input value to the name of the executor block.
%% ====================================================================
handle_cast({execute, ExecutorBlockName, exec_out}, BlockState) ->

  {Config, Inputs, Outputs, Private} = BlockState,
  {ok, NewInputs} = attrib_utils:set_value(Inputs, exec_in, ExecutorBlockName),
  % Execute the block
  NewBlockState = block_common:execute({Config, NewInputs, Outputs, Private}, exec_out),
  {noreply, NewBlockState};


%% ====================================================================
%% Update this block's input value(s) with the block value received in this message
%% ====================================================================
handle_cast({update, NewInputValues}, BlockState) ->

  {Config, Inputs, Outputs, Private} = BlockState,

   % Update the block input(s), that are linked this value, with the new Value
  {ok, NewInputs} = attrib_utils:set_values(Inputs, NewInputValues),

  % Execute the block because input values have changed
  NewBlockState = update_block({Config, NewInputs, Outputs, Private}),
 
  {noreply, NewBlockState};


%% =====================================================================
%% Initial block configuration
%% =====================================================================
handle_cast(init_configure, BlockState) ->
  % Initialize the just created block and immediately execute it.
   
  % Must be called separately from the init() function, 
  % because you can't call block_supervisor:block_names()
  % (i.e.supervisor:which_child()) from the init() function 
  % of a child process. 

  % Perform block initialization
  NewBlockState1 = block_common:initialize(BlockState),

  % Update the block
  NewBlockState2 = update_block(NewBlockState1),
  
  {noreply, NewBlockState2};


%% =====================================================================
%% Update (attempt to execute) the block,
%% =====================================================================
handle_cast(update, BlockState) ->

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
  m_logger:info(reconfiguring_block, [BlockName]),

  % Replace current state Block values with new values and configure block again
  % TODO: Check that new block values match the current block type and block name that is running
  init_configure(BlockName),
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
      m_logger:warning(block_server_unknown_cast_msg, [BlockName, Msg]),
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
      m_logger:warning(block_server_unknown_info_msg, [BlockName, Info]),
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

  m_logger:error(block_server_abnormal_termination, [BlockName, Reason]),
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
  BlockName = config_utils:name(Config),
  % Don't execute block if block is executed via timer or exec_in (i.e Control Flow)
  % Just update the input values and leave it at that
  {ok, TimerRef} = attrib_utils:get_value(Private, timer_ref),
    
  case (TimerRef /= empty) orelse (block_supervisor:is_exec_linked(BlockName)) of
    true -> % Block will be executed via timer timeout or linked block execution, just return
      {Config, NewInputs, Outputs, Private};
      
    false -> % OK to execute block
        block_common:execute({Config, NewInputs, Outputs, Private}, input_cos)
  end.

  