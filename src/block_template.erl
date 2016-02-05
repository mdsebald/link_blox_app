%%
%% @author Mark Sebald
%% @doc Block Template module. 
%% Copy this module to begin coding a new block type
%% 

-module(block_template).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/2, initialize/1, execute/1, delete/1]).


%% Create a set of block values for this block type.  
%% Any Config, Input, Output, or Internal parameters 
%% not already defined in the set of common block values, 
%% will be created here and intialized to their default values.  
%% Initial Config and Input values are set here.
   
create(BlockName, {InitConfigs, InitInputs})->

    % Create an initial set of common block values
	{CommConfigs, CommInputs, CommOutputs, CommInternals} = 
                             block_common:create(BlockName, type_name(), version()),
	
    % Create any Config, Input, Output, and/or Internal parameters
    % specific for this block type and intialize them to their default values
    
    %% Update Config and Input parameter values with the
    %% Initial Config and Input values passed into this function
    %% If any Inital Config or Input parameter does not exist yet, 
    %% create it here 
    %%(e.g. The number of inputs for certain block types 
    %% will not be known until the block is created.)

    % This is the block state, 
	{BlockName, ?MODULE, Configs, Inputs, Outputs, Internals}.

 
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution

initialize({BlockName, BlockModule, Configs, Inputs, Outputs, Internals}) ->
    block_common:initialize(Configs, Internals)
	


	{BlockName, BlockModule, Params, Inputs, Outputs, NewInterms}.



execute({BlockName, BlockModule, Params, Inputs, Outputs, Interms}) ->

	case blkpnt_utils:get_input_value(Inputs, 'Enable') of
		true ->
		    case blkpnt_utils:get_output_value(Outputs, 'Value') of
                true -> 	
                    NewOutputs = blkpnt_utils:update_common_outputs(Outputs, false, normal);
                false ->
                	NewOutputs = blkpnt_utils:update_common_outputs(Outputs, true, normal);
                not_active ->
                    NewOutputs = blkpnt_utils:update_common_outputs(Outputs, true, normal)
            end;               

		false ->	% Block is Disabled
			NewOutputs = blkpnt_utils:update_common_outputs(Outputs, not_active, disabled)
	end,
    
    block_common:execute(),

    {BlockName, BlockModule, Params, Inputs, NewOutputs, NewInterms}.

	
    
delete({BlockName, _BlockModule, _Params, _Inputs, _Outputs, _IntTerms}) ->
	block_common:delete(Configs, Internals).
    % Per



%% ====================================================================
%% Internal functions
%% ====================================================================

type_name()-> 'Template'.

version() -> "0.1.0".