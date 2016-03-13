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
-export([start_link/1, block_processes/0]).

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
    
    % Start the UI loop
    spawn('LinkBlox_ui', ui_loop, []),
    
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
		
%% 
%% get the current list of block processes
%%
block_processes() -> supervisor:which_children(?MODULE).

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
	
