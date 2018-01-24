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
          types_info/0, 
          type_info/1,
          type_name/1,
          type_module/1,
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
%% Get a list of block types and associated info 
%% for all of the block types in this application
%%
-spec types_info() -> list({string(), string(), string(), string()}).

types_info() ->
  lists:map(fun(Module) -> type_info(Module) end, modules()).


%%
%% Get block module type name, version, and description
%%     
-spec type_info(module()) -> {string(), string(), string(), string()}.
    
type_info(Module) ->
  case ui_utils:get_block_type_strings(Module) of
    {Module, TypeNameStr, Description} ->
      {atom_to_list(Module), Module:version(), TypeNameStr, Description};

    false ->
      % block type strings in language module, 
      % does not have an entry for this Module name
      % Make sure it is really a block type module
      case lists:member({version, 0}, Module:module_info(exports)) of
        true ->
          {atom_to_list(Module), Module:version(), "", ""};
        false ->
          logger:error(err_invalid_block_type_module, [Module]),
          {atom_to_list(Module), ui_utils:get_ui_string(err_invalid_block_type_module), "", ""}
      end
  end.


%%
%% Get the block type name string corresponding to this block module
%%
-spec type_name(Module :: module()) -> string().
    
type_name(Module) ->
  case ui_utils:get_block_type_strings(Module) of
    {Module, BlockTypeStr, _Description} -> BlockTypeStr;

    false -> 
      % Block module not found in list of block types for this node
      % Just use the Module as the block type string
      atom_to_list(Module)
  end.


%%
%% Get the block type module corresponding to the block type string
%%
-spec type_module(BlockTypeStr :: string()) -> module().
    
type_module(BlockTypeStr) ->
  case ui_utils:get_block_type_strings(BlockTypeStr) of
    {Module, BlockTypeStr, _Description} -> Module;

    false -> 
      % Block type string, not found in list of block types for this node
      % Just change the block type name string directly into a Module
      list_to_atom(BlockTypeStr)
  end.


%%
%% Check if given block module, exists on this node
%%
-spec module_exists(Module :: module()) -> true | false.

module_exists(Module) ->
  lists:member(Module, modules()).


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% ====================================================================
% Test modules()
% 
modules_test() ->
  Result = modules(),
  ?assert(is_list(Result)),
  ?assert(length(Result) > 40),
  [First | _Rest] = Result,
  FirstStr = atom_to_list(First),
  ?assert(lists:sublist(FirstStr, 5) == "lblx_").
% ====================================================================

% ====================================================================
% Test type_info()
% 
type_info_good_test() ->
  ui_utils:set_lang_mod(lang_en_us),
  Result = type_info(lblx_logic_or),
  ?assert(Result == {"lblx_logic_or", "0.1.0", "logic_or", "OR of All of the Binary Inputs"}).

type_info_no_entry_in_lang_mod_test() ->
  ui_utils:set_lang_mod(lang_en_us),
  Result = type_info(lblx_test),
  % ?debugFmt("Result: ~p", [Result]),
  ?assert(Result == {"lblx_test", "0.1.0", "", ""}).
    
type_info_not_block_type_module_test() ->
  ui_utils:set_lang_mod(lang_en_us),
  Result = type_info(logger),  % Just pick a valid module that is not a block type
  ?assert(Result == {"logger", "Invalid block type module", "", ""}).

% ====================================================================

% ====================================================================
% Test types_info()
% 
types_info_test() ->
  ui_utils:set_lang_mod(lang_en_us),
  Result = types_info(),
  ?assert(is_list(Result)),
  ?assert(length(Result) > 40),
  [First | _Rest] = Result,
  ?assert(is_tuple(First)).
% ====================================================================

% ====================================================================
% Test type_name()
% 
type_name_good_test() ->
  ui_utils:set_lang_mod(lang_en_us),
  Result = type_name(lblx_logic_or),
  ?assert(Result == "logic_or").

type_name_no_entry_in_lang_mod_test() ->
  ui_utils:set_lang_mod(lang_en_us),
  Result = type_name(lblx_test),
  ?assert(Result == "lblx_test").

type_name_bad_test() ->
  ui_utils:set_lang_mod(lang_en_us),
  Result = type_name(lblx_unknown),
  ?assert(Result == "lblx_unknown").
% ====================================================================

% ====================================================================
% Test type_module()
% 
type_module_good_test() ->
  ui_utils:set_lang_mod(lang_en_us),
  Result = type_module("logic_or"),
  ?assert(Result == lblx_logic_or).

type_module_no_entry_in_lang_mod_test() ->
  ui_utils:set_lang_mod(lang_en_us),
  Result = type_module("lblx_test"),
  ?assert(Result == lblx_test).

type_module_bad_test() ->
  ui_utils:set_lang_mod(lang_en_us),
  Result = type_module("lblx_unknown"),
  ?assert(Result == lblx_unknown).
% ====================================================================


% ====================================================================
% Test module_exists()
% 
module_exists_true_test() ->
  ?assert(module_exists(lblx_logic_or)).

module_exists_false_test() ->
  ?assertNot(module_exists(lblx_invalid)).

% ====================================================================

-endif.
