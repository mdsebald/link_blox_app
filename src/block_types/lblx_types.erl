%%% @doc 
%%% Get a list of block types and current version   
%%%               
%%% @end 

-module(lblx_types). 

-author("Mark Sebald").

-include("../block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([block_type_modules/0, block_types_info/0]).


%%
%%  Add each block type module name to this functions
%%  Allows UI to retrieve block type name strings, version, and description
%%  For consistency add new block types in alphabetical order 
%%  by type name string.
%%  Do not add the "template" block type
%%

block_type_modules() ->
   [
        lblx_exec_count,
        lblx_pi_gpio_di,
        lblx_pi_gpio_do,
        lblx_seven_seg,
        lblx_toggle
    ].  
    

block_types_info() ->
    lists:map(fun(ModuleName) -> block_type_info(ModuleName) end,
               block_type_modules()).
     
    
block_type_info(ModuleName) ->
    {ModuleName:type_name(), ModuleName:version(), ModuleName:description()}.
    