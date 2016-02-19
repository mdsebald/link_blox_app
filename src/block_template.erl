%%
%% @author Mark Sebald
%% @doc Block Type:  
%% Description: 
%% 

-module(block_template).

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/1, create/3, create/5, initialize/1, execute/1, delete/1]).


type_name()-> template.  % atom, specifying the block type, usually the module name minus "block_"

version() -> "0.1.0".   % Major.Minor.Patch, Major version change is a breaking change

  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
-spec create(BlockName :: atom()) -> block_state().

create(BlockName) -> create(BlockName, [], [], [], []).
   
create(BlockName, InitConfigs, InitInputs) -> create(BlockName, InitConfigs, InitInputs, [],[]).

create(BlockName, InitConfigs, InitInputs, InitOutputs, InitInternals)->
     
    %% Update Default Config, Input, Output, and Internal attribute values 
    %% with the initial values passed into this function.
    %%
    %% If any of the intial attributes do not already exist in the 
    %% default attribute lists, merge_attribute_lists() will create them.
    %% (This is useful for block types where the number of attributes is not fixed)
    
    Configs = block_utils:merge_attribute_lists(default_configs(BlockName), InitConfigs),
    Inputs = block_utils:merge_attribute_lists(default_inputs(), InitInputs), 
    Outputs = block_utils:merge_attribute_lists(default_outputs(), InitOutputs),
    Internals = block_utils:merge_attribute_lists(default_internals(), InitInternals),

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

default_configs(BlockName) -> 
    block_utils:merge_attribute_lists(block_common:configs(BlockName, type_name(), version()), 
                            []).  % Insert block type specific config attributes here
 
default_inputs() -> 
     block_utils:merge_attribute_lists(block_common:inputs(),
                            []). % Insert block type specific input attributes here
                            
default_outputs() -> 
        block_utils:merge_attribute_lists(block_common:outputs(),
                            []). % Insert block type specific output attributes here
                            
default_internals() -> 
        block_utils:merge_attribute_lists(block_common:internals(),
                            []). % Insert block type specific internal attributes here
