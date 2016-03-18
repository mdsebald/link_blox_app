%%% @doc 
%%% Block Type: Toggle Output
%%% Description: Toggle binary output value each time block is executed  
%%%               
%%% @end 

-module(lblx_toggle).

-author("Mark Sebald").

-include("../block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([type_name/0, description/0, version/0]). 
-export([create/1, create/3, create/5, initialize/1, execute/1, delete/1]).


type_name() -> "toggle".

description() -> "Toggle binary output value on block execution".

version() -> "0.1.0". 


%% Merge the block type specific, Config, Input, Output, and Private attributes
%% with the common Config, Input, Output, and Private attributes, that all block types have

-spec default_configs(BlockName :: atom()) -> list().

default_configs(BlockName) -> 
    block_utils:merge_attribute_lists(block_common:configs(BlockName, type_name(), version()), 
                            [
                                
                            ]).  


-spec default_inputs() -> list().
 
default_inputs() -> 
     block_utils:merge_attribute_lists(block_common:inputs(),
                            [
                                
                            ]).


-spec default_outputs() -> list().
                            
default_outputs() -> 
        block_utils:merge_attribute_lists(block_common:outputs(),
                            [
                                
                            ]).
                            
                            
-spec default_private() -> list().
                            
default_private() -> 
        block_utils:merge_attribute_lists(block_common:private(),
                            [
                                
                            ]).

  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
-spec create(BlockName :: atom()) -> block_state().

create(BlockName) -> create(BlockName, [], [], [], []).
   
-spec create(BlockName :: atom(), list(), list()) -> block_state().
   
create(BlockName, InitConfig, InitInputs) -> create(BlockName, InitConfig, InitInputs, [],[]).

-spec create(BlockName :: atom(), list(), list(), list(), list()) -> block_state().

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
	
    NewOutputs = block_utils:set_value_status(Outputs, not_active, initialed),
     
    {BlockName, BlockModule, Config, Inputs, NewOutputs, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

    % Toggle output everytime block is executed
    case block_utils:get_value(Outputs, value) of
        true       -> Value = false,      Status = normal;
        false      -> Value = true,       Status = normal;
        not_active -> Value = true,       Status = normal;
        _          -> Value = not_active, Status = error
    end,
	
    NewOutputs = block_utils:set_value_status(Outputs, Value, Status),
     
    {BlockName, BlockModule, Config, Inputs, NewOutputs, Private}.


%% 
%%  Delete the block
%%	
-spec delete(block_state()) -> ok.

delete({_BlockName, _BlockModule, _Config, _Inputs, _Outputs, _Private}) ->
    ok.



%% ====================================================================
%% Internal functions
%% ====================================================================

