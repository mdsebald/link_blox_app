%% @author Mark Sebald
%% @doc Supervisor for LinkBlox app.


-module(block_supervisor).

-behaviour(supervisor).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

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

%% ====================================================================
%% Internal functions
%% ====================================================================
create_child_specs(BlockValuesList) ->
	create_child_specs(BlockValuesList, []).
	
create_child_specs([], ChildSpecs) -> ChildSpecs;

create_child_specs(BlockValuesList, ChildSpecs) ->
	[BlockValues | RemainingBlockValuesList] = BlockValuesList,	
	% TODO: Check for expected term match, before creating child spec 
	{BlockName, _BlockModule, _Params, _Inputs, _Outputs, _Interms} = BlockValues,

	ChildSpec = #{id => BlockName, start => {block_server, create, [BlockValues]}},
	NewChildSpecs = [ChildSpec | ChildSpecs],
	
	create_child_specs(RemainingBlockValuesList, NewChildSpecs).
	
