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
-export([block_type_modules/0, block_type_to_module/1]).
-export([block_types_info/0, block_type_info/1]).
-export([block_type_names/0, block_type_name/1]).


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
    
 block_type_to_module(BlockTypeStr) ->
    Modules = lists:filter( fun(Module)-> Module:type_name() == BlockTypeStr end, 
        block_type_modules()),
    case length(Modules) of
        0 -> not_found;
        1 -> lists:nth(1, Modules);
        _ -> error  % More than one block module has the same type name
                    % If this happens, there is an error in the source code
    end.
    

block_types_info() ->
    lists:map(fun(Module) -> block_type_info(Module) end,
               block_type_modules()).
     
    
block_type_info(Module) ->
    {Module:type_name(), Module:version(), Module:description()}.
    

block_type_names() ->
    lists:map(fun(Module) -> block_type_name(Module) end,
               block_type_modules()).
     
    
block_type_name(Module) ->
    Module:type_name().
    
