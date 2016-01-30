%% @author Mark
%% @doc Setup blocks prior to execution,  Do not call these functions on running blocks 


-module(blkpnt_setup).

%% ====================================================================
%% API functions
%% ====================================================================
-export([common_values/2]).


%% Return Values list containing block values that are common for all block types
common_values(BlockName, BlockModule) ->
	
	BlockTypeParam = {'BlockType', BlockModule:type_name()},
	VersionParam = {'Version', BlockModule:version()},
    TimeoutParam = {'Timeout', 0},  % If > 0, execute block every 'Timeout' milliseconds
	
	EnableInput = {'Enable', true, {fixed, null, null}},

	ValueOutput = {'Value', not_active, []},
	StatusOutput = {'Status', created, []},
	ExecCountOutput = {'ExecCount', 0, []},
	LastExecOutput = {'LastExec', not_active, []},
	
	   {BlockName, BlockModule,
		[BlockTypeParam, VersionParam],
		[EnableInput],
		[ValueOutput, StatusOutput, ExecCountOutput, LastExecOutput],
		[]}.



%% ====================================================================
%% Internal functions
%% ====================================================================


