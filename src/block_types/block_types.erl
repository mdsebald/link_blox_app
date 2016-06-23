%%% @doc 
%%% Get a list of block types and current version   
%%%               
%%% @end 

-module(block_types). 

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
-spec block_type_modules() -> list(module()).

block_type_modules() ->
  [
    type_exec_count,
    type_gpio_di,
    type_gpio_do,
    type_ht16k33_4digit_led,
    type_int_to_7seg,
    type_mcp9808_temp,
    type_one_digit_7seg,
    type_rotary_encoder,
    type_seven_seg_decoder,
    type_toggle
  ].  
    
    
%%
%%  Get the block module for the given block type string
%% 
-spec block_type_to_module(BlockType :: type_name()) -> module() | not_found | error.

block_type_to_module(BlockType) ->
  Modules = lists:filter(fun(Module)-> block_type_name(Module) == BlockType end, 
                 block_type_modules()),
  case length(Modules) of
    0 -> not_found;
    1 -> lists:nth(1, Modules);
    _ -> error  % More than one block module has the same type name
                % If this happens, there is an error in the source code
  end.
  
  
%%
%% Get a list of block types and associated info 
%% for all of the block types in this application
%%
-spec block_types_info() -> list({string(), string(), string()}).

block_types_info() ->
  lists:map(fun(Module) -> block_type_info(Module) end,
               block_type_modules()).

%%
%% Get block module type name, version, and description
%%     
-spec block_type_info(module()) -> {string(), string(), string()}.
    
block_type_info(Module) ->
  {atom_to_list(block_type_name(Module)), Module:version(), Module:description()}.

%%
%% Get a list of block type names for all of the block modules in this app
%%
-spec block_type_names() -> list(atom()).

block_type_names() ->
  lists:map(fun(Module) -> block_type_name(Module) end,
               block_type_modules()).
 
%%
%% Get the block type name for this block module
%%
-spec block_type_name(module()) -> atom().
    
block_type_name(Module) ->
  ModuleStr = atom_to_list(Module),
  % Module always starts with "type_"  
  % Removing that leaves the block type name 
  list_to_atom(string:substr(ModuleStr, 6)).  


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% ====================================================================
% Test block_type_name()
% 
block_type_name_test() ->
  Module = type_test_module,
  ExpectedResult = test_module,

  Result = block_type_name(Module),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================

% ====================================================================
% Test block_type_names()
%  
block_type_names_test() ->

ExpectedResult = 
  [
    exec_count,
    gpio_di,
    gpio_do,
    ht16k33_4digit_led,
    int_to_7seg,
    mcp9808_temp,
    one_digit_7seg,
    rotary_encoder,
    seven_seg_decoder,
    toggle
  ],  

  Result = block_type_names(),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================

% ====================================================================
% Test block_type_to_module()
% 
block_type_to_module_test() ->
  BlockType = gpio_do,
  ExpectedResult = type_gpio_do,

  Result = block_type_to_module(BlockType),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================

-endif.
