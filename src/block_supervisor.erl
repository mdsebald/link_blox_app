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
          block_processes/0,
          is_exec_linked/1,
          add_exec_link/2,
          del_exec_link/2, 
          del_block_exec_links/1,
          get_exec_links/0
]).

start_link(BlockValuesFile) ->
  Return = supervisor:start_link({local, ?MODULE}, ?MODULE, BlockValuesFile),
  % Perform the initial configuration of all blocks just created
  lists:foreach(fun(BlockName) -> 
            block_server:init_configure(BlockName) end, 
            block_names()),
  Return.


%%
%%  Start a block
%%    
start_block(BlockState) ->
  [BlockSpec] = create_block_specs([BlockState]),                    
  case supervisor:start_child(?MODULE, BlockSpec) of
    {ok, Child} ->
      BlockName = config_utils:name(BlockState),
      block_server:init_configure(BlockName),
      {ok, Child};
    {error, Error} -> 
      {error, Error}
  end.
   
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


%%
%% Determine if the named block is linked to the exec_out of another block
%%
-spec is_exec_linked(BlockName :: block_name()) -> boolean().

is_exec_linked(BlockName) ->
  ets:member(exec_links, BlockName).

%%
%% Add reference this block is linked to the ExecutorBlockName exec_out output
%%
-spec add_exec_link(BlockName :: block_name(),
                    ExecutorBlockName :: block_name()) -> true.

add_exec_link(BlockName, ExecutorBlockName) ->
  m_logger:debug("Adding exec link:  ~p => ~p", [ExecutorBlockName, BlockName]),
  ets:insert(exec_links, {BlockName, ExecutorBlockName}).


%%
%% Delete reference this block is linked to the ExecutorBlockName exec_out output
%%
-spec del_exec_link(BlockName :: block_name(),
                    ExecutorBlockName :: block_name()) -> true.

del_exec_link(BlockName, ExecutorBlockName) ->
  m_logger:debug("Deleting exec link:  ~p => ~p", [ExecutorBlockName, BlockName]),
  ets:delete_object(exec_links, {BlockName, ExecutorBlockName}).


%%
%% Delete this block from the list of exec links
%% Call when a block is deleted
%%
-spec del_block_exec_links(BlockName :: block_name()) -> ok.

del_block_exec_links(BlockName) ->
  m_logger:debug("Deleting all exec links to  ~p", [BlockName]),
  ets:take(exec_links, BlockName),
  ok.

%%
%% Get the contents of exec links table
%%
-spec get_exec_links() -> [{block_name(), block_name()}].

get_exec_links() ->
    ets:foldl(fun(ExecLink, Accum) -> [ExecLink | Accum] end, [], exec_links).


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
  m_logger:info(starting_linkblox_block_supervisor),

  % Create a table to indicate which blocks are exec_linked
  m_logger:debug("Create table of exec links"),
  ets:new(exec_links, [named_table, public, bag]),

  case block_utils:get_blocks_from_file(BlockValuesFile) of
    {ok, BlockValuesList} ->
      % TODO: Check for good, "ok" return value
      BlockSpecs = create_block_specs(BlockValuesList),
      
      SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
      {ok, {SupFlags, BlockSpecs}};
      
    {error, _Reason} ->
      m_logger:info(loading_demo_config),

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


  {Config, _Inputs, Outputs} = BlockState,
  {BlockName, BlockModule, Version} = config_utils:name_module_version(Config),

  % Initialize the exec_links table
  {ok, {exec_out, {_Value, Links}}} = attrib_utils:get_attribute(Outputs, exec_out),

  lists:foreach(fun(Link) ->
                  case Link of
                    {ToBlockName, _ToValueId} ->
                      add_exec_link(ToBlockName, BlockName);

                    InvalidLink ->
                      m_logger:error(err_unrecognized_link, [InvalidLink])
                  end
                end, 
                Links),

  BlockTypeStr = type_utils:type_name(BlockModule),
  m_logger:info(creating_type_version, [BlockName, BlockTypeStr, Version]),

  BlockSpec = #{id => BlockName, restart => transient,
              start => {block_server, start, [BlockState]}},
  NewBlockSpecs = [BlockSpec | BlockSpecs],

  create_block_specs(RemainingBlockValuesList, NewBlockSpecs).
