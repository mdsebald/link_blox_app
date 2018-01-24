%%%
%%% @doc 
%%% Block Supervisor
%%% @end
%%%

-module(block_supervisor).

-author("Mark Sebald").

-include("block_state.hrl").

-behaviour(supervisor).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
          start_link/1, 
          start_block/1, 
          delete_block/1,
          is_block/1,
          block_names/0, 
          block_processes/0
]).

start_link(BlockValuesFile) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, BlockValuesFile).
    
%%
%%  Start a block
%%    
start_block(BlockState) ->
  [BlockSpec] = create_block_specs([BlockState]),                    
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
%% Determine if BlockName is a valid block name
%%
-spec is_block(BlockName :: block_name()) -> boolean().

is_block(BlockName) ->
  lists:member(BlockName, block_names()).


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
  logger:info(starting_linkblox_block_supervisor),

  case block_utils:get_blocks_from_file(BlockValuesFile) of
    {ok, BlockValuesList} ->
      % TODO: Check for good, "ok" return value
      BlockSpecs = create_block_specs(BlockValuesList),
      
      SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
      {ok, {SupFlags, BlockSpecs}};
      
    {error, _Reason} ->
      logger:info(loading_demo_config),

      BlockSpecs = create_block_specs(demo_config:create_demo_config()),
             
      SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
      {ok, {SupFlags, BlockSpecs}}
  end.


%% ====================================================================
%% Internal functions
%% ====================================================================
create_block_specs(BlockValuesList) ->
  create_block_specs(BlockValuesList, []).

create_block_specs([], BlockSpecs) -> BlockSpecs;

create_block_specs(BlockValuesList, BlockSpecs) ->
  [BlockState | RemainingBlockValuesList] = BlockValuesList,
  % TODO: Check for expected term match, before creating child spec 

  {BlockName, BlockModule, Version} = config_utils:name_module_version(BlockState),
  BlockTypeStr = type_utils:type_name(BlockModule),
  logger:info(creating_type_version, [BlockName, BlockTypeStr, Version]),

  BlockSpec = #{id => BlockName, restart => transient,
              start => {block_server, start, [BlockState]}},
  NewBlockSpecs = [BlockSpec | BlockSpecs],

  create_block_specs(RemainingBlockValuesList, NewBlockSpecs).
