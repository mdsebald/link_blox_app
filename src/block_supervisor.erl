%%%
%%% @doc 
%%% Supervisor for LinkBlox app.
%%% @end
%%%

-module(block_supervisor).

-author("Mark Sebald").

-behaviour(supervisor).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, create_block/1, delete_block/1]).
-export([block_names/0, block_processes/0]).

start_link(BlockValuesFile) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, BlockValuesFile).
    
%%
%%  Create a block
%%    
create_block(BlockValues) ->
   [BlockSpec] = create_block_specs([BlockValues]),                    
   supervisor:start_child(?MODULE, BlockSpec).
   
%%
%% Delete a block
%%
delete_block(BlockName) ->
  % Send a delete message to block an wait for response
  block_server:delete(BlockName),
    
  % Delete the block's child spec, 
  % so a block using the same name may be created later
  supervisor:delete_child(?MODULE, BlockName).
    
%% 
%% Get the block names of currently running processes
%%
block_names() -> 
  block_names(block_processes(), []).    
    
block_names([], BlockNames) -> 
  BlockNames;
    
block_names([BlockProcess | RemainingProcesses], BlockNames) ->
  % Only return block names of processes that are running
  case element(2, BlockProcess) of
    restarting -> NewBlockNames = BlockNames;
    undefined  -> NewBlockNames = BlockNames;
    _Pid       ->
      case element(1, BlockProcess) of
        linkblox_api ->  
          % TODO: start the linkblox_api with another supervisor
          % In the meantime, just ignore it, it is not a block,  
          NewBlockNames = BlockNames;

        BlockName ->
          NewBlockNames = [BlockName | BlockNames]
      end
  end,
  block_names(RemainingProcesses, NewBlockNames).
  
  
%% 
%% get the current list of block processes
%%
block_processes() -> 
  supervisor:which_children(?MODULE).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% ====================================================================
%% init/1
%% ====================================================================
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.

init(BlockValuesFile) ->

  % Start the UI loop, 
  spawn(ui_main, ui_init, []),
  
  % Spec for API Server
  ApiServerSpec = #{id => linkblox_api, restart => transient,
             start => {linkblox_api, start, []}},

	case block_config:read_config(BlockValuesFile) of
		{ok, BlockValuesList} ->
      error_logger:info_msg("Loading block Values config file: ~p~n", [BlockValuesFile]),

			% TODO: Check for good, "ok" return value
			BlockSpecs = create_block_specs(BlockValuesList),
      
			SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
			{ok, {SupFlags, [ApiServerSpec | BlockSpecs]}};
      
		{error, Reason} ->
			error_logger:error_msg("~p error, reading Block Values config file: ~p~n", [Reason, BlockValuesFile]),
            error_logger:error_msg("Loading Demo config... ~n"),
      
      BlockSpecs = create_block_specs(block_config:create_demo_config()),
                   
			SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
            
			{ok, {SupFlags, [ApiServerSpec | BlockSpecs]}}
		end.
		

%% ====================================================================
%% Internal functions
%% ====================================================================
create_block_specs(BlockValuesList) ->
	create_block_specs(BlockValuesList, []).
	
create_block_specs([], BlockSpecs) -> BlockSpecs;

create_block_specs(BlockValuesList, BlockSpecs) ->
	[BlockValues | RemainingBlockValuesList] = BlockValuesList,	
	% TODO: Check for expected term match, before creating child spec 
	
  {BlockName, BlockModule, Version} = config_utils:name_module_version(BlockValues),
  
  error_logger:info_msg("Creating: ~p Type: ~p Version: ~s~n", 
                        [BlockName, BlockModule, Version]),

	BlockSpec = #{id => BlockName, restart => transient,
                   start => {block_server, create, [BlockValues]}},
	NewBlockSpecs = [BlockSpec | BlockSpecs],
	
	create_block_specs(RemainingBlockValuesList, NewBlockSpecs).