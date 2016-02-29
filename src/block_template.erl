% INSTRUCTIONS: Copy this module and modify as appropriate 
%               for the function this block will perform.
%               Comments marked "INSTRUCTIONS:" may be deleted 

%%% @doc 
%%% Block Type:  
%%% Description:   
%%%               
%%% @end 

-module(block_template).  % INSTRUCTIONS: Modify to match new module name

-author("Your Name").

-include("block_state.hrl").  % INSTRUCTIONS: Adjust path to hrl file as needed

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/1, create/3, create/5, initialize/1, execute/1, delete/1]).


type_name()-> template.  % INSTRUCTIONS: Atom, specifying the block type, usually the module name minus "block_"

version() -> "0.1.0".   % INSTRUCTIONS: Major.Minor.Patch, Major version change implies a breaking change

%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: atom()) -> block_state().

create(BlockName) -> create(BlockName, [], [], [], []).
   
create(BlockName, InitConfig, InitInputs) -> create(BlockName, InitConfig, InitInputs, [],[]).

create(BlockName, InitConfig, InitInputs, InitOutputs, InitPrivate)->

    block_common:log_state("Creating", BlockName, type_name()),

    %% Update Default Config, Input, Output, and Private attribute values 
    %% with the initial values passed into this function.
    %%
    %% If any of the intial attributes do not already exist in the 
    %% default attribute lists, merge_attribute_lists() will create them.
    %% (This is useful for block types where the number of attributes is not fixed)
    
    Config = block_utils:merge_attribute_lists(default_configs(BlockName), InitConfig),
    Inputs = block_utils:merge_attribute_lists(default_inputs(), InitInputs), 
    Outputs = block_utils:merge_attribute_lists(default_outputs(), InitOutputs),
    Private = block_utils:merge_attribute_lists(default_private(), InitPrivate),

    % This is the block state, 
	{BlockName, ?MODULE, Config, Inputs, Outputs, Private}.

%%
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->
    
    % Perform block type specific initializations here
    NewOutputs = Outputs,
    NewPrivate = Private,

    % Perform initial block execution
    block_common:execute({BlockName, BlockModule, Config, Inputs, NewOutputs, NewPrivate}, initial).

%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

    % INSTRUCTIONS: Perform block type specific actions here, 
    % read input value(s) calculate new outut value(s)
    % set block output status value
    NewOutputs = Outputs,
    NewPrivate = Private,

    {BlockName, BlockModule, Config, Inputs, NewOutputs, NewPrivate}.


%% 
%%  Delete the block
%%	
-spec delete(block_state()) -> block_state().

delete({BlockName, BlockModule, Config, Inputs, Outputs, Private}) -> 
    % INSTRUCTIONS: Perform any block type specific delete functionality here
    {BlockName, BlockModule, Config, Inputs, Outputs, Private}.



%% ====================================================================
%% Internal functions
%% ====================================================================

-spec default_configs(BlockName :: atom()) -> list().

default_configs(BlockName) -> 
    block_utils:merge_attribute_lists(block_common:configs(BlockName, type_name(), version()), 
                            []).  % INTRUCTIONS: Insert block type specific config attributes here


 -spec default_inputs() -> list().

default_inputs() -> 
     block_utils:merge_attribute_lists(block_common:inputs(),
                            []). % INTRUCTIONS: Insert block type specific input attributes here


-spec default_outputs() -> list().
                            
default_outputs() -> 
        block_utils:merge_attribute_lists(block_common:outputs(),
                            []). % INTRUCTIONS: Insert block type specific output attributes here


 -spec default_private() -> list().
                           
default_private() -> 
        block_utils:merge_attribute_lists(block_common:private(),
                            []). % INTRUCTIONS: Insert block type specific private attributes here
