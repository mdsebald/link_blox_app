%% @author Mark Sebald
%% @doc Supervisor for LinkBlox app.


-module(block_supervisor).

-behaviour(supervisor).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, block_names/0, block_status/0]).

start_link(BlockValuesFile) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, BlockValuesFile).

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
    
	case block_config:read_config(BlockValuesFile) of
		{ok, BlockValuesList} ->
			% TODO: Check for good, "ok" return value
			ChildSpecs = create_child_specs(BlockValuesList),
			SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
			{ok, {SupFlags, ChildSpecs} };
		{error, Reason} ->
			error_logger:error_msg("~p error, reading Block Values config file: ~p~n", [Reason, BlockValuesFile]),
            error_logger:error_msg("Loading Demo config... ~n"),
            ChildSpecs = create_child_specs(block_config:create_demo_config()),
			SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
			{ok, {SupFlags, ChildSpecs} }
		end.
		
%	ChildSpecs = [
%					#{id => BlockName, start => {'BlockPoint_srv', create, [BlockValues]}},
%
%				 ],


%% 
%% get the current list of block processes
%%
block_processes() -> supervisor:which_children(?MODULE).

%% 
%% get the  block names of currently running processes
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
            BlockName = element(1, BlockProcess),
            NewBlockNames = [BlockName | BlockNames]
    end,
    block_names(RemainingProcesses, NewBlockNames).

%%
%% Print out status off each running block
%%
block_status() ->
    io:fwrite("~n~-16s ~-16s ~-12s ~-12s ~-12s ~-10s~n", 
                  ["Block Type", "Block Name", "Output Val", "Status", "Exec Method", "Last Exec"]), 
    block_status(block_names()).
    
block_status([]) -> ok;

block_status([BlockName | RemainingBlockNames]) ->
    BlockType = block_server:get_value(BlockName, block_type),
    Value = block_server:get_value(BlockName, value),
    Status = block_server:get_value(BlockName, status),
    ExecMethod = block_server:get_value(BlockName, exec_method),
    LastExecuted = block_server:get_value(BlockName, last_exec),
    
    io:fwrite("~-16s ~-16s ~-12w ~-12w ~-12w ~-10w~n", 
              [io_lib:write(BlockType), io_lib:write(BlockName), 
               Value, Status, ExecMethod, LastExecuted]),
    block_status(RemainingBlockNames).


%% ====================================================================
%% Internal functions
%% ====================================================================
create_child_specs(BlockValuesList) ->
	create_child_specs(BlockValuesList, []).
	
create_child_specs([], ChildSpecs) -> ChildSpecs;

create_child_specs(BlockValuesList, ChildSpecs) ->
	[BlockValues | RemainingBlockValuesList] = BlockValuesList,	
	% TODO: Check for expected term match, before creating child spec 
	{BlockName, _BlockModule, Config, _Inputs, _Outputs, _Private} = BlockValues,
    error_logger:info_msg("Creating: ~p Type: ~p Version: ~s~n", 
                          [BlockName, 
                           block_utils:get_value(Config, block_type), 
                           block_utils:get_value(Config, version)]),

	ChildSpec = #{id => BlockName, restart => transient,
                   start => {block_server, create, [BlockValues]}},
	NewChildSpecs = [ChildSpec | ChildSpecs],
	
	create_child_specs(RemainingBlockValuesList, NewChildSpecs).
	
