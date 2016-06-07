%%% @doc 
%%% API Server.  gen_server behavior to handle requests from UI clients
%%%
%%% @end

-module(linkblox_api).

-author("Mark Sebald").

-include("block_state.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, is_block_name/1, is_block_type/1]). 


%% Startup the UI Server
start()->
  gen_server:start_link({local, linkblox_api}, ?MODULE, null, []).

%%
%% Process API calls
%%

%% Is BlockName a valid block name?
is_block_name(BlockName) ->
	gen_server:call(linkblox_api, {is_block_name, BlockName}).

%% Is BlockTypeStr a valid block type name?
is_block_type(BlockTypeStr) ->
	gen_server:call(linkblox_api, {is_block_type, BlockTypeStr}).

%command(Command, Args)->
 % gen_server:call(linkblox_api, {comand, Command, Args}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
	

%% ==================================================================== 
%% init/1
%% ====================================================================
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.

init(null) ->
  error_logger:info_msg("Initializing UI Server~n"),
  {ok, []}.

%% ====================================================================
%% handle_call/3
%% ====================================================================
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

%% =====================================================================
%% Is BlockName a valid block?
%% =====================================================================    
handle_call({is_block_name, BlockName}, _From, State) ->
  Result = lists:member(BlockName, block_supervisor:block_names()),
  {reply, Result, State};

%% =====================================================================
%% Is BlockTypeName a valid block type?
%% =====================================================================    
handle_call({is_block_type, BlockTypeStr}, _From, State) ->
  Result = lists:member(BlockTypeStr, block_types:block_type_names()),
  {reply, Result, State};
  
%% =====================================================================
%% Unknown Call message
%% =====================================================================      
handle_call(Request, From, State) ->
  error_logger:warning_msg("Unknown call message: ~p From: ~p~n", 
                            [Request, From]),
  {reply, ok, State}.


%% ====================================================================
%% handle_cast/2
%% ====================================================================
-spec handle_cast(Request :: term(), State :: term()) -> Result when
  Result :: {noreply, NewState}
     | {noreply, NewState, Timeout}
     | {noreply, NewState, hibernate}
     | {stop, Reason, NewState},
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity,
  Reason :: term().

%% =====================================================================
%% Unknown Cast message
%% =====================================================================      
handle_cast(Msg, State) ->
  error_logger:warning_msg("Unknown cast message: ~p~n", [Msg]),
  {noreply, State}.


%% ====================================================================
%% handle_info/2
%% ====================================================================
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.


%% =====================================================================
%% Unknown Info message
%% =====================================================================
handle_info(Info, State) ->
  error_logger:warning_msg("Unknown info message: ~p~n", [Info]),
  {noreply, State}.


%% ====================================================================
%% terminate/2
%% ====================================================================
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
      
terminate(normal, _State) ->
  ok;
    
terminate(Reason, _State) ->
  error_logger:error_msg("UI Server, Abnormal Termination: ~p~n", [Reason]),
  ok.


%% ====================================================================
%% code_change/3
%% ====================================================================
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
  
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

