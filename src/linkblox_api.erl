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
-export([
					start/0,
					stop/1,
					create_block/4,
					delete_block/2,
					get_block/2,
					get_value/3,
					get_block_names/1,
					execute_block/2,
         	is_block_name/2, 
				 	is_block_type/2
]). 


%% Start the LinkBlox API Server
start()->
  gen_server:start_link({local, linkblox_api}, ?MODULE, null, []).


%% Stop the LinkBlox API server
stop(Node) ->
	gen_server:call({linkblox_api, Node}, stop).


%%
%% Process API calls
%%

%% Create a block
-spec create_block(Node :: node(),
									 BlockType :: type_name(),
									 BlockName :: block_name(),
									 InitAttribs :: list()) -> term().

create_block(Node, BlockType, BlockName, InitAttribs) ->
	gen_server:call({linkblox_api, Node}, 
	                {create_block, BlockType, BlockName, InitAttribs}).


%% delete a block
-spec delete_block(Node :: node(),
									 BlockName :: block_name()) -> term().

delete_block(Node, BlockName) ->
	gen_server:call({linkblox_api, Node}, 
	                {delete_block, BlockName}).


%% Get block values
-spec get_block(Node :: node(),
								BlockName :: block_name()) -> term().

get_block(Node, BlockName) ->
	gen_server:call({linkblox_api, Node}, {get_block, BlockName}).


%% Get a block value
-spec get_value(Node :: node(),
								BlockName :: block_name(),
								ValueId :: value_id()) -> term().

get_value(Node, BlockName, ValueId) ->
	gen_server:call({linkblox_api, Node}, {get_value, BlockName, ValueId}).


%% Get list of block names
-spec get_block_names(Node :: node()) -> term().

get_block_names(Node) ->
	gen_server:call({linkblox_api, Node}, get_block_names).


%% Execute the block
-spec execute_block(Node :: node(),
                    BlockName :: block_name()) -> term().

execute_block(Node, BlockName) ->
	gen_server:call({linkblox_api, Node}, {execute_block, BlockName}).


%% Is BlockName a valid block name?
-spec is_block_name(Node :: node(),
										BlockName :: block_name()) -> term().

is_block_name(Node, BlockName) ->
	gen_server:call({linkblox_api,Node}, {is_block_name, BlockName}).


%% Is BlockTypeStr a valid block type name?
-spec is_block_type(Node :: node(),
										BlockType :: type_name()) -> term().

is_block_type(Node, BlockType) ->
	gen_server:call({linkblox_api, Node}, {is_block_type, BlockType}).

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
  error_logger:info_msg("Starting LinkBlox API server~n"),
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
%% Stop the API server
%% =====================================================================
handle_call(stop, _From, State) ->  
  error_logger:info_msg("Stopping LinkBlox API server~n"),

  {stop, normal, ok, State};

%% =====================================================================
%% Create a block
%% =====================================================================    
% TODO: Set initial attribute values
handle_call({create_block, BlockType, BlockName, _InitAttribs}, _From, State) ->
	case lists:member(BlockType, block_types:block_type_names()) of
		true ->
			BlockModule = block_types:block_type_to_module(BlockType),
  		case valid_block_name(BlockName) of
				false ->
					BlockValues = BlockModule:create(BlockName, "Default Comment"),
					case block_supervisor:create_block(BlockValues) of
            {ok, _Pid} -> 
          		Result = ok;
            {error, Reason} -> 
              Result = {error, Reason}
					end;
				_ ->
					Result = {error, block_exists}
			end;
		_ ->
			Result = {error, invalid_block_type}
	end,
  {reply, Result, State};

%% =====================================================================
%% Delete a block
%% =====================================================================    
handle_call({delete_block, BlockName}, _From, State) ->
	case valid_block_name(BlockName) of
		true ->
			case block_supervisor:delete_block(BlockName) of
        ok -> 
					Result = ok;
				{error, Reason} -> 
					Result = {error, Reason}
			end;
		_ ->
			Result = {error, block_not_found}
	end,
  {reply, Result, State};


%% =====================================================================
%% Get all block values
%% =====================================================================    
handle_call({get_block, BlockName}, _From, State) ->
	case valid_block_name(BlockName) of
		true ->
			{Config, Inputs, Outputs, _Private} = block_server:get_block(BlockName),
			% Strip private values,
			Result = {ok, {Config, Inputs, Outputs}};
		_ ->
			Result = {error, block_not_found}
	end,
  {reply, Result, State};


%% =====================================================================
%% Get a block value
%% =====================================================================    
handle_call({get_value, BlockName, ValueId}, _From, State) ->
	case valid_block_name(BlockName) of
		true ->
      case block_server:get_value(BlockName, ValueId) of
      	{error, not_found} ->
					Result = {error, value_not_found};
				{ok, CurrentValue} ->
					Result = {ok, CurrentValue}
			end;
		_ ->
			Result = {error, block_not_found}
	end,
  {reply, Result, State};


%% =====================================================================
%% Get a list of all block names
%% =====================================================================    
handle_call(get_block_names, _From, State) ->
	BlockNames = block_supervisor:block_names(),
  % Convert block name atoms to strings
	Result = lists:map(fun(BlockName) -> atom_to_list(BlockName) end, BlockNames),
  {reply, Result, State};


%% =====================================================================
%% Execute block
%% =====================================================================    
handle_call({execute_block, BlockName}, _From, State) ->
	case valid_block_name(BlockName) of
		true ->
			block_server:execute(BlockName),
			Result = ok;
		_ ->
			Result = {error, block_not_found}
	end,
  {reply, Result, State};


%% =====================================================================
%% Is BlockNameStr a valid block name?
%% =====================================================================    
handle_call({is_block_name, BlockName}, _From, State) ->
  case valid_block_name(BlockName) of
		true-> 
			Result = true;
		_ ->
			Result = false
	end,
  {reply, Result, State};


%% =====================================================================
%% Is BlockTypeStr a valid block type?
%% =====================================================================    
handle_call({is_block_type, BlockType}, _From, State) ->
  Result = lists:member(BlockType, block_types:block_type_names()),
  {reply, Result, State};


%% =====================================================================
%% Unknown Call message
%% =====================================================================      
handle_call(Request, From, State) ->
  error_logger:warning_msg("linkblox_api: Unknown call message: ~p From: ~p~n", 
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
  error_logger:warning_msg("linkblox_api: Unknown cast message: ~p~n", [Msg]),
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
  error_logger:error_msg("LinkBlox API server, Abnormal Termination: ~p~n", [Reason]),
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

%%
%% Is the BlockName a valid block name?
%%
-spec valid_block_name(BlockName :: block_name())-> boolean().

valid_block_name(BlockName)->
  lists:member(BlockName, block_supervisor:block_names()).


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-ifdef(UNDER_DEVELOPMENT).

linkblox_api_test_() ->
	{
		setup,
		fun start_linkblox_api/0,
		fun stop_linkblox_api/1,
		fun() ->
			{
				inorder,
			 	[
					create_block_test(),
					delete_block_test()
				]
			}
		end
	}.

start_linkblox_api() ->
	% Need to start LinkBlox Supervisor
	% That will start the API Server, (This module) and the block supervisor.
	start().
 
stop_linkblox_api(_) ->
	stop(node()).

create_block_test() ->
	create_block(node(), "template", "test_block", []).

delete_block_test() ->
	delete_block(node(), "test_block"). 

-endif.
-endif.