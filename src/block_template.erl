%%
%% @author Mark Sebald
%% @doc Block Type:  
%% Description: 
%% 

-module(block_template).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/3, initialize/1, execute/1, delete/1]).


%% Create a set of block values for this block type.  
%% Any Config, Input, Output, or Internal attributes 
%% not already defined in the set of common block values, 
%% will be created here and intialized to their default values.  
%% Initial Config and Input values are set here.
   
create(BlockName, InitConfigs, InitInputs)->

    % Create an initial set of common block values
	{CommonConfigs, CommonInputs, CommonOutputs, CommonInternals} = 
                             block_common:create(BlockName, type_name(), version()),
	
    % Create any Config, Input, Output, and/or Internal attributes
    % specific for this block type and intialize them to their default values
    
    %% Update Config and Input attribute values with the
    %% Initial Config and Input values passed into this function
    %% If any block type specific Config, Input, Output 
    %% attribute does not exist yet, create it here 
    %% (e.g. The number of inputs for certain block types 
    %%  will not be known until the block is created.)
    
    Configs = block_utils:merge_attribute_lists(CommonConfigs, InitConfigs),
    Inputs = block_utils:merge_attribute_lists(CommonInputs, InitInputs), 
    Outputs = CommonOutputs,
    Internals = CommonInternals,

    % This is the block state, 
	{BlockName, ?MODULE, Configs, Inputs, Outputs, Internals}.

 
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution

initialize({BlockName, BlockModule, Configs, Inputs, Outputs, Internals}) ->

    % Perform common block initializations
    {InitOutputs, InitInternals} = block_common:initialize(Configs, Outputs, Internals),
	
    % Perform block type specific initializations here, and update the state variables
    NewOutputs = InitOutputs,
    NewInternals = InitInternals,

	{BlockName, BlockModule, Configs, Inputs, NewOutputs, NewInternals}.

%%
%%  Execute the block specific functionality
%%

execute({BlockName, BlockModule, Configs, Inputs, Outputs, Internals}) ->

    % Always check if block is enabled first
	case block_utils:get_input_value(Inputs, 'Enable') of
		true ->
		    % Perform block type specific actions here, calculate new outut value(s)
            Value = true,
            % Perform common execute function for normally executing block
            {NewOutputs, NewInternals} = block_common:execute(Configs, Outputs, Internals, Value, normal); 

		false ->	% Block is Disabled, perform common execute function for a disabled block 
			{NewOutputs, NewInternals} = block_common:execute(Configs, Outputs, Internals, not_active, disabled) 
	end,
    
    {BlockName, BlockModule, Configs, Inputs, NewOutputs, NewInternals}.


%% 
%%  Delete the block
%%	
    
delete({_BlockName, _BlockModule, Configs, _Inputs, _Outputs, Internals}) ->
	block_common:delete(Configs, Internals).
    % Perform any other block type specific delete functionality here



%% ====================================================================
%% Internal functions
%% ====================================================================

type_name()-> 'Template'.

version() -> "0.1.0".