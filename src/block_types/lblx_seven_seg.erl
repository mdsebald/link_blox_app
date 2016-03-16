%%% @doc 
%%% Block Type:  Seven Segment Decoder
%%% Description: Decode integer input value to activate
%%%              the segements of a seven segment display  
%%%               
%%% @end 

-module(lblx_seven_seg). 

-author("Mark Sebald").

-include("../block_state.hrl").  

%% ====================================================================
%% API functions
%% ====================================================================
-export([type_name/0, version/0]). 
-export([create/1, create/3, create/5, initialize/1, execute/1, delete/1]).


type_name() -> seven_seg. 

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
                            [
                                
                            ]).

%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
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
    
    % Perform block type specific initializations here
    NewOutputs = block_utils:set_value(Outputs, status, initialed),

    {BlockName, BlockModule, Config, Inputs, NewOutputs, Private}.

%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({BlockName, BlockModule, Config, Inputs, Outputs, Private}) ->

    Input = block_utils:get_value(Inputs, input),
    
    % Decode input integer value into 7 segment outputs 
    % If the input value is not an integer from 0-9, just set output error
    case Input of
        0 -> Value = Input, Status = normal,
             SegA = true,  SegB = true,  SegC = true, 
             SegD = true,  SegE = true,  SegF = true,  SegG = false;
            
        1 -> Value = Input, Status = normal,
             SegA = false, SegB = true,  SegC = true, 
             SegD = false, SegE = false, SegF = false, SegG = false;
            
        2 -> Value = Input, Status = normal,
             SegA = true,  SegB = true,  SegC = false, 
             SegD = true,  SegE = true,  SegF = false, SegG = true;
            
        3 -> Value = Input, Status = normal,
             SegA = true, SegB = true, SegC = true, 
             SegD = true, SegE = false, SegF = false, SegG = true;
            
        4 -> Value = Input, Status = normal,
             SegA = false, SegB = true,  SegC = true, 
             SegD = false, SegE = false, SegF = true,  SegG = true;
             
        5 -> Value = Input, Status = normal,
             SegA = true,  SegB = false, SegC = true, 
             SegD = true,  SegE = false, SegF = true,  SegG = true;
            
        6 -> Value = Input, Status = normal,
             SegA = true,  SegB = false, SegC = true, 
             SegD = true,  SegE = true,  SegF = true,  SegG = true;
            
        7 -> Value = Input, Status = normal,
             SegA = true,  SegB = true,  SegC = true, 
             SegD = false, SegE = false, SegF = false, SegG = false;
             
        8 -> Value = Input, Status = normal,
             SegA = true, SegB = true, SegC = true, 
             SegD = true, SegE = true, SegF = true, SegG = true;
            
        9 -> Value = Input, Status = normal,
             SegA = true, SegB = true, SegC = true, 
             SegD = false, SegE = false, SegF = true, SegG = true;
            
         Invalid ->
            error_logger:error_msg("~p Invalid Value: ~p~n", [BlockName, Invalid]), 
            Value = not_active, Status = error_in,
            SegA = not_active, SegB = not_active, SegC = not_active, 
            SegD = not_active, SegE = not_active, SegF = not_active, SegG = not_active 
      end,  
 
    % update the outputs
    NewOutputs = block_utils:set_values(Outputs, 
           [
               {value, Value}, {status, Status},
               {seg_a, SegA}, {seg_b, SegB}, {seg_c, SegC}, 
               {seg_d, SegD}, {seg_e, SegE}, {seg_f, SegF}, {seg_g, SegG}
           ]),
 
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

             
 
        
