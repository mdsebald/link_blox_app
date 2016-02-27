%%% @doc 
%%% Block Type: Toggle
%%% Description: Toggle binary output valie    
%%%               
%%% @end 

-module(block_toggle).

-author("Mark Sebald").

-include("../block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/1, create/3, create/5, initialize/1, execute/1, delete/1]).


type_name()-> toggle.  % atom, specifying the block type, usually the module name minus "block_"

version() -> "0.1.0".   % Major.Minor.Patch, Major version change is a breaking change

  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
-spec create(BlockName :: atom()) -> block_state().

create(BlockName) -> create(BlockName, [], [], [], []).
   
create(BlockName, InitConfig, InitInputs) -> create(BlockName, InitConfig, InitInputs, [],[]).

create(BlockName, InitConfig, InitInputs, InitOutputs, InitPrivate)->
     
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

    % Perform common block initializations
    InitPrivate = block_common:initialize(Config, Inputs, Private),
	
    % Perform block type specific initializations here, and update the state variables
    NewOutputsX = block_utils:set_output_value(Outputs, value, not_active),
    NewOutputs = block_utils:set_output_value(NewOutputsX, status, initialized),
    NewPrivate = InitPrivate,

	{BlockName, BlockModule, Config, Inputs, NewOutputs, NewPrivate}.

%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->
    % Toggle output everytime block is executed
    case block_utils:get_output_value(Outputs, value) of
        true       -> Value = false,      Status = normal;
        false      -> Value = true,       Status = normal;
        not_active -> Value = true,       Status = normal;
        _          -> Value = not_active, Status = error
    end,
	
    NewOutputs1 = block_utils:set_output_value(Outputs, value, Value),
    NewOutputs2 = block_utils:set_output_value(NewOutputs1, status, Status),
    
    {BlockName, BlockModule, Config, Inputs, NewOutputs2, Private}.


%% 
%%  Delete the block
%%	
    
delete({_BlockName, _BlockModule, Config, _Inputs, _Outputs, Private}) ->
	block_common:delete(Config, Private).
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
                            
default_private() -> 
        block_utils:merge_attribute_lists(block_common:private(),
                            []). % Insert block type specific private attributes here
