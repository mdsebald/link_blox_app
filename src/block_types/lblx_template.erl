% INSTRUCTIONS: Copy this module and modify as appropriate 
%               for the function this block will perform.
%               Comments marked "INSTRUCTIONS:" may be deleted 

%%% @doc 
%%% Block Type:  
%%% Description:   
%%%               
%%% @end 

% INSTRUCTIONS: Modify to match new block type module name
% All block type module names must begin with "lblx_" 
% to uniquely identify them as LinkBlox block type modules.
-module(lblx_template).  
  
-author("Your Name").

 % INSTRUCTIONS: Adjust path to hrl file as needed
-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/1, delete/1]).


% INSTRUCTIONS: Classify block type, by assigning it to one or more groups
-spec groups() -> list(type_group()).
groups() -> [none].


% INSTRUCTIONS: String describing block function
-spec description() -> string().
description() -> "Short description of block function".


% INSTRUCTIONS: Set block type version number.
% Use pattern: Major.Minor.Patch
% When a block is created, the Config version attribute value 
% is set to this version.
% When a block is loaded from a config file, the version attribute value
% is compared to this.  
% If the versions are different, the upgrade() function is called.
-spec version() -> string().
version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      % INTRUCTIONS: Insert block type specific config attribute tuples here
      % Config attribute tuples consist of a value name and a value
      % Example: {gpio_pin, {0}}
      % Array Example: {start_rows, [{1}, {2}]}
      % The block is (re) initialized, when any config value is modified.                   
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      % INTRUCTIONS: Insert block type specific input attribute tuples here
      % Input attribute tuples consist of a value name, a value, and a link
      % Example: {hi_limit, {100, ?EMPTY_LINK}}
      % Array Example: {inputs, [{empty, ?EMPTY_LINK}]}
      % Inputs may be fixed values, or linked to a block output value 
    ]). 


-spec default_outputs() -> list(output_attr()).
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      % INTRUCTIONS: Insert block type specific output attribute tuples here
      % Output attribute tuples consist of a value name, a calculated value, 
      % and a list of blocks that reference (have links to) this output value
      % Output values are always set to 'not_actve' and empty reference list on creation
      % Example: {dwell, {not_active, []}}
      % Array Example: {digit, [{not_active, []}]}  
    ]). 


%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: block_name(),
             Description :: string()) -> block_defn().

create(BlockName, Description) -> 
  create(BlockName, Description, [], [], []).

-spec create(BlockName :: block_name(),
             Description :: string(),  
             InitConfig :: list(config_attr()), 
             InitInputs :: list(input_attr())) -> block_defn().
   
create(BlockName, Description, InitConfig, InitInputs) -> 
  create(BlockName, Description, InitConfig, InitInputs, []).

-spec create(BlockName :: block_name(),
             Description :: string(), 
             InitConfig :: list(config_attr()), 
             InitInputs :: list(input_attr()), 
             InitOutputs :: list(output_attr())) -> block_defn().

create(BlockName, Description, InitConfig, InitInputs, InitOutputs) ->

  % Update Default Config, Input, Output, and Private attribute values 
  % with the initial values passed into this function.
  %
  % If any of the intial attributes do not already exist in the 
  % default attribute lists, merge_attribute_lists() will create them.
    
  Config = attrib_utils:merge_attribute_lists(default_configs(BlockName, Description), InitConfig),
  Inputs = attrib_utils:merge_attribute_lists(default_inputs(), InitInputs), 
  Outputs = attrib_utils:merge_attribute_lists(default_outputs(), InitOutputs),

  % This is the block definition, 
  {Config, Inputs, Outputs}.


%%
%% Upgrade block attribute values, when block code and block data versions are different
%% 
-spec upgrade(BlockDefn :: block_defn()) -> {ok, block_defn()} | {error, atom()}.

upgrade({Config, Inputs, Outputs}) ->

  % INSTRUCTIONS:  This function is called, on block creation, when the
  % module's version does not match the version in the block's config data.
  % Depending on the version(s) perform any necessary adjustments to the 
  % block's attributes, to make it compatible with the current block type's code.
  % If upgrading the attributes is not possible, return an error and reason.

  ModuleVer = version(),
  {BlockName, BlockModule, ConfigVer} = config_utils:name_module_version(Config),
  BlockType = type_utils:type_name(BlockModule),

  case attrib_utils:set_value(Config, version, version()) of
    {ok, UpdConfig} ->
      log_server:info(block_type_upgraded_from_ver_to, 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      log_server:error(err_upgrading_block_type_from_ver_to, 
                            [Reason, BlockName, BlockType, ConfigVer, ModuleVer]),
      {error, Reason}
  end.


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
    
  % INSTRUCTIONS: Perform block type specific initializations here
  % Add and intialize private attributes here
  Outputs1 = Outputs,
  Private1 = Private,

  % This is the block state
  {Config, Inputs, Outputs1, Private1}.


%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({Config, Inputs, Outputs, Private}) ->

  % INSTRUCTIONS: Perform block type specific actions here, 
  % read input value(s) calculate new output value(s)
  % set block output status value
  Outputs1 = Outputs,
  Private1 = Private,

  % Return updated block state
  {Config, Inputs, Outputs1, Private1}.


%% 
%%  Delete the block
%%	
-spec delete(BlockValues :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  % INSTRUCTIONS: Perform any block type specific delete functionality here
  % Return block definition, (Block state - Private values)
  % in case calling function wants to reuse them.
  %
  % Private values are created in the block initialization routine
  % So they should be deleted here
  
  {Config, Inputs, Outputs}.



%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Tests
%% ====================================================================

% INSTRUCTIONS:  Create unit tests here.  

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% At a minimum, call the block type's create(), upgrade(), initialize(), execute(), and delete() functions.

block_test() ->
  log_server:start(lang_en_us),
  BlockDefn = create(create_test, "Unit Testing Block"),
  {ok, BlockDefn} = upgrade(BlockDefn),
  BlockState = block_common:initialize(BlockDefn),
  execute(BlockState),
  _BlockDefnFinal = delete(BlockState),
  ?assert(true).

-endif.
