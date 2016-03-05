%%% @doc 
%%% Block Type:  7 Segment Decoder / Driver
%%% Description: Decode an input integer input value to activate
%%%              the segements of a seven segment display  
%%%               
%%% @end 

-module(block_7_segment).  % INSTRUCTIONS: Modify to match new module name

-author("Mark Sebald").

-include("../block_state.hrl").  % INSTRUCTIONS: Adjust path to hrl file as needed

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/1, create/3, create/5, initialize/1, execute/1, delete/1]).


type_name()-> '7_segment'. 

version() -> "0.1.0".  

%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: atom()) -> block_state().

create(BlockName) -> create(BlockName, [], [], [], []).
   
create(BlockName, InitConfig, InitInputs) -> create(BlockName, InitConfig, InitInputs, [],[]).

create(BlockName, InitConfig, InitInputs, InitOutputs, InitPrivate)->

    io:format("Creating: ~p Type: ~p~n", [BlockName, type_name()]),

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
    {BlockName, BlockModule, Config, Inputs, NewOutputs, NewPrivate}.

%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

    % Decode input integer value into 7 segment outputs 
    case block_utils:get_value(Inputs, input) of
        0 -> NewOutputs = block_utils:set_values(Outputs, [
            {seg_a, true}, {seg_b, true}, {seg_c, true}, {seg_d, true}, 
            {seg_e, true}, {seg_f, true}, {seg_g, false}
            ]);
        1 -> NewOutputs = block_utils:set_values(Outputs, [
            {seg_a, false}, {seg_b, true}, {seg_c, true}, {seg_d, false}, 
            {seg_e, false}, {seg_f, false}, {seg_g, false}
            ]);
        2 -> NewOutputs = block_utils:set_values(Outputs, [
            {seg_a, true}, {seg_b, true}, {seg_c, false}, {seg_d, true}, 
            {seg_e, true}, {seg_f, false}, {seg_g, true}
            ]);
        3 -> NewOutputs = block_utils:set_values(Outputs, [
            {seg_a, true}, {seg_b, true}, {seg_c, true}, {seg_d, true}, 
            {seg_e, false}, {seg_f, false}, {seg_g, true}
            ]);
        4 -> NewOutputs = block_utils:set_values(Outputs, [
            {seg_a, false}, {seg_b, true}, {seg_c, true}, {seg_d, false}, 
            {seg_e, false}, {seg_f, true}, {seg_g, true}
            ]);
        5 -> NewOutputs = block_utils:set_values(Outputs, [
            {seg_a, true}, {seg_b, false}, {seg_c, true}, {seg_d, true}, 
            {seg_e, false}, {seg_f, true}, {seg_g, true}
            ]);
        6 -> NewOutputs = block_utils:set_values(Outputs, [
            {seg_a, true}, {seg_b, false}, {seg_c, true}, {seg_d, true}, 
            {seg_e, true}, {seg_f, true}, {seg_g, true}
            ]);
        7 -> NewOutputs = block_utils:set_values(Outputs, [
            {seg_a, true}, {seg_b, true}, {seg_c, true}, {seg_d, false}, 
            {seg_e, false}, {seg_f, false}, {seg_g, false}
            ]);
        8 -> NewOutputs = block_utils:set_values(Outputs, [
            {seg_a, true}, {seg_b, true}, {seg_c, true}, {seg_d, true}, 
            {seg_e, true}, {seg_f, true}, {seg_g, true}
            ]);
        9 -> NewOutputs = block_utils:set_values(Outputs, [
            {seg_a, true}, {seg_b, true}, {seg_c, true}, {seg_d, false}, 
            {seg_e, false}, {seg_f, true}, {seg_g, true}
            ]);
         Invalid ->
            io:format("Error: Invalid Decimal 7 Segment Value: ~p~n", [Invalid]), 
            NewOutputs = block_utils:set_values(Outputs, [
            {seg_a, false}, {seg_b, false}, {seg_c, false}, {seg_d, false}, 
            {seg_e, false}, {seg_f, false}, {seg_g, false}
            ])
     end,  
 
    {BlockName, BlockModule, Config, Inputs, NewOutputs, Private}.


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
                            []).  


 -spec default_inputs() -> list().

default_inputs() -> 
     block_utils:merge_attribute_lists(block_common:inputs(),
                            [
                                {input, empty, ?EMPTY_LINK}
                            ]). 


-spec default_outputs() -> list().
                            
default_outputs() -> 
        block_utils:merge_attribute_lists(block_common:outputs(),
                            [
                                {seg_a, not_active, []},
                                {seg_b, not_active, []},
                                {seg_c, not_active, []},
                                {seg_d, not_active, []},
                                {seg_e, not_active, []},
                                {seg_f, not_active, []},
                                {seg_g, not_active, []}
                            ]). 


 -spec default_private() -> list().
                           
default_private() -> 
        block_utils:merge_attribute_lists(block_common:private(),
                            []).
             
 
        
