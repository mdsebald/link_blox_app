%%% @doc 
%%% Block Type: Toggle 
%%% Description: Toggle the binary Value output each time the block is executed
%%%               
%%% @end 

-module(block_toggle).

-author("Mark Sebald").

-include("../block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/3, initialize/1, execute/1, delete/1]).


%% Create a set of block values for this block type.  
%% Any Config, Input, Output, or Private attributes 
%% not already defined in the set of common block values, 
%% will be created here and intialized to their default values.  
%% Initial Config and Input values are set here.
   
create(BlockName, InitConfig, InitInputs)->

    % Create an initial set of common block values
	{CommonConfig, CommonInputs, CommonOutputs, CommonPrivate} = 
                             block_common:create(BlockName, type_name(), version()),
	
    % Create any Config, Input, Output, and/or Private attributes
    % specific for this block type and intialize them to their default values
    
    %% Update Config and Input attribute values with the
    %% Initial Config and Input values passed into this function
    %% Any block type sp Config or Input attribute does not exist yet, 
    %% create it here 
    %%(e.g. The number of inputs for certain block types 
    %% will not be known until the block is created.)
    
    Config = block_utils:merge_attribute_lists(CommonConfig, InitConfig),
    Inputs = block_utils:merge_attribute_lists(CommonInputs, InitInputs), 
    Outputs = CommonOutputs,
    Private = CommonPrivate,

    % This is the block state, 
	{BlockName, ?MODULE, Config, Inputs, Outputs, Private}.

 
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
-spec initialize(block_state()) -> block_state().

initialize({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

    % Perform common block initializations
    {NewOutputs, NewPrivate} = block_common:initialize(Config, Outputs, Private),
	
    % Perform block type specific initializations here, and update the state variables

	{BlockName, BlockModule, Config, Inputs, NewOutputs, NewPrivate}.


%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

    % Always check if block is enabled first
	case block_utils:get_input_value(Inputs, enable) of
		true ->
            case block_utils:get_output_value(Outputs, value) of
                true ->  Value = false;
                false -> Value = true;
                not_active -> Value = true
            end,
            % Perform common execute function for normally executing block
            {NewOutputs, NewPrivate} = block_common:execute(Config, Outputs, Private, Value, normal); 

		false ->	% Block is Disabled, perform common execute function for a disabled block 
			{NewOutputs, NewPrivate} = block_common:execute(Config, Outputs, Private, not_active, disabled) 
	end,
    
    {BlockName, BlockModule, Config, Inputs, NewOutputs, NewPrivate}.


%% 
%%  Delete the block
%%	
    
delete({_BlockName, _BlockModule, Config, _Inputs, _Outputs, Private}) ->
	block_common:delete(Config, Private).
    % Perform any other block type specific delete functionality here



%% ====================================================================
%% Internal functions
%% ====================================================================

type_name()-> toggle.

version() -> "0.1.0".