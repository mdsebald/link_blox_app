%%% @doc 
%%% UI Server.  gen_server behavior to handle requests from UI clients
%%%
%%% @end

-module(ui_server).

-author("Mark Sebald").

-include("../block_state.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/0, command/2]). 


%% Create a function block with the given Name, Functionality, and Values
create()->
  gen_server:start_link({local, 'LinkBlox_UI'}, ?MODULE, null, []).


%% Perform UI command
command(Command, Args)->
  gen_server:call( Command, Args).




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

