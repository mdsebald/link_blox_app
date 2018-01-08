%%% @doc 
%%% Common block type utilty functions   
%%%               
%%% @end 

-module(type_utils). 

-author("Mark Sebald").

-include("../block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
          modules/0, 
          type_to_module/1,
          types_info/0, 
          type_info/1,
          type_names/0, 
          type_name/1,
          module_exists/1
]).


%%
%%  Create a list of all possible block types
%%  by searching for all file names with 
%%  the pattern "lblx_*.beam in the code path".
%%  Thanks to:  alind.io
%%
-spec modules() -> list(module()).
    
modules() ->
  [list_to_atom(filename:basename(F, ".beam")) || P <- code:get_path(), 
    F <- filelib:wildcard("lblx_*.beam", P)].


%%
%%  Get the block module for the given block type string
%% 
-spec type_to_module(BlockType :: type_name()) -> module() | not_found | error.

type_to_module(BlockType) ->
  Modules = lists:filter(fun(Module)-> type_name(Module) == BlockType end, modules()),
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
-spec types_info() -> list({string(), string(), string()}).

types_info() ->
  lists:map(fun(Module) -> type_info(Module) end, modules()).

%%
%% Get block module type name, version, and description
%%     
-spec type_info(module()) -> {string(), string(), string()}.
    
type_info(Module) ->
  {atom_to_list(type_name(Module)), Module:version(), ui_utils:get_block_type_descr(Module)}.


%%
%% Get a list of block type names for all of the block modules in this app
%%
-spec type_names() -> list(atom()).

type_names() ->
  lists:map(fun(Module) -> type_name(Module) end, modules()).


%%
%% Get the block type name for this block module
%%
-spec type_name(module()) -> atom().
    
type_name(Module) ->
  ModuleStr = atom_to_list(Module),
  % Module always starts with "type_"  
  % Removing that leaves the block type name 
  list_to_atom(string:substr(ModuleStr, 6)).


%%
%% Check if given block module, exists on this node
%%
-spec module_exists(BlockModule :: module()) -> true | false.

module_exists(BlockModule) ->
  lists:member(BlockModule, modules()).


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% ====================================================================
% Test block_type_name()
% 
type_name_test() ->
  Module = type_test_module,
  ExpectedResult = test_module,

  Result = type_name(Module),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================

% ====================================================================
% Test block_type_to_module()
% 
type_to_module_test() ->
  BlockType = gpio_do,
  ExpectedResult = error,

  % This scans subfolders also so it comes up with double set of block types.
  % Just assume this will error for now.
  Result = type_to_module(BlockType),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================

-endif.
