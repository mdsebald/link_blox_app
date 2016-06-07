%%% @doc
%%% LinkBlox application
%%%
%%% @end


-module('LinkBlox_app').

-author("Mark Sebald").

-behaviour(application).

-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================


%% ====================================================================
%% Behavioural functions
%% ====================================================================


%% ====================================================================
%% start/2
%% ====================================================================
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.

start(normal, BlockValuesFile) ->
    case block_supervisor:start_link(BlockValuesFile) of
			{ok, Pid} ->
				error_logger:info_msg("block_supervisor started~n"),
				{ok, Pid};
				
		Error ->
			error_logger:error_msg("Error: ~p starting block_supervisor.~n", [Error]),
			Error
    end.


%% ====================================================================
%% stop/1
%% ====================================================================
-spec stop(State :: term()) ->  Any :: term().

stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
