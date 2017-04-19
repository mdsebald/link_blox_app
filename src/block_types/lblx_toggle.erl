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
-export([groups/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [none].

description() -> "Toggle binary output value on block execution".

version() -> "0.1.0". 


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [

    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [

    ]). 


-spec default_outputs() -> list(output_attr()).
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [

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
             InitOutputs :: list()) -> block_defn().

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
%% Initialize block values before starting execution
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  Outputs1 = output_utils:set_value_status(Outputs, null, initialed),
 
  {Config, Inputs, Outputs1, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockValues :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  % Toggle output everytime block is executed
  case attrib_utils:get_value(Outputs, value) of
    {ok, true}       -> Value = false,      Status = normal;
    {ok, false}      -> Value = true,       Status = normal;
    {ok, null} -> Value = true,       Status = normal;
    _                -> Value = null, Status = error
  end,
	
  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),
     
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockValues :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  {Config, Inputs, Outputs}.


%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% At a minimum, call the block type's create(), upgrade(), initialize(), execute(), and delete() functions.

block_test() ->
  log_server:start(lang_en_us),
  BlockDefn = create(create_test, "Unit Testing Block"),
  {ok, BlockDefn} = upgrade(BlockDefn),
  BlockState = block_common:initialize(BlockDefn),
  execute(BlockState, input_cos),
  _BlockDefnFinal = delete(BlockState),
  ?assert(true).

-endif.