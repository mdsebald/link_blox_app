%% @author Mark Sebald
%% @doc startup the block engine app


-module(demo).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

start() ->
	'BlockPoint_app':start(normal, "/vagrant/BlockPoint/TestConfig.bpt").


%% ====================================================================
%% Internal functions
%% ====================================================================


