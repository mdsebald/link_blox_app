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
-export([start_link/1, create_block/1, delete_block/1, block_processes/0]).

start_link(BlockValuesFile) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, BlockValuesFile).
    
%%
%%  Create a block
%%    
create_block(BlockValues) ->
   [BlockSpec] = create_block_specs([BlockValues]),                    
   supervisor:start_child(?MODULE, BlockSpec).
   
%%
%% Delete the block child specification
%% Call this after calling the block_server to delete the block
%% This will remove the child spec, so another block with the same name may be created
%% TODO: Combine with block_server:delete().  
%%       Make block_server:delete a call message instead of cast, 
%%       so you can check the response before deleting the child spec here
%%
delete_block(BlockName) ->
    supervisor:delete_child(?MODULE, BlockName).
    

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
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
%% ====================================================================
init(BlockValuesFile) ->

    % Start up the timer server, for blocks executed on a timer
	timer:start(),
    
    % Start the UI loop
    spawn(lblx_ui_main, ui_loop, []),
    
	case block_config:read_config(BlockValuesFile) of
		{ok, BlockValuesList} ->
			% TODO: Check for good, "ok" return value
			BlockSpecs = create_block_specs(BlockValuesList),
			SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
			{ok, {SupFlags, BlockSpecs} };
		{error, Reason} ->
			error_logger:error_msg("~p error, reading Block Values config file: ~p~n", [Reason, BlockValuesFile]),
            error_logger:error_msg("Loading Demo config... ~n"),
            
            BlockSpecs = create_block_specs(block_config:create_demo_config()),
            
            % UiSpec = #{id => lblx_ui_main, restart => transient,
            %       start => {lblx_ui_main, ui_loop, []}},
                   
			SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
            
			{ok, {SupFlags, BlockSpecs} }
		end.
		
%% 
%% get the current list of block processes
%%
block_processes() -> supervisor:which_children(?MODULE).

%% ====================================================================
%% Internal functions
%% ====================================================================
create_block_specs(BlockValuesList) ->
	create_block_specs(BlockValuesList, []).
	
create_block_specs([], BlockSpecs) -> BlockSpecs;

create_block_specs(BlockValuesList, BlockSpecs) ->
	[BlockValues | RemainingBlockValuesList] = BlockValuesList,	
	% TODO: Check for expected term match, before creating child spec 
	{Config, _Inputs, _Outputs} = BlockValues,
  BlockName = block_utils:name(Config),
  error_logger:info_msg("Creating: ~p Type: ~p Version: ~s~n", 
                        [BlockName, 
                         block_utils:get_value(Config, block_type), 
                         block_utils:get_value(Config, version)]),

	BlockSpec = #{id => BlockName, restart => transient,
                   start => {block_server, create, [BlockValues]}},
	NewBlockSpecs = [BlockSpec | BlockSpecs],
	
	create_block_specs(RemainingBlockValuesList, NewBlockSpecs).
	
