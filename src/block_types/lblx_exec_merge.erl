%%% @doc 
%%% Block Type:  Merge Exec Out signals
%%% Description:   Merge multiple blocks exec_out signals into a single exec_out signal.
%%%                
%%%               
%%% @end 

-module(lblx_exec_merge).  
  
-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1, handle_info/2]).


% INSTRUCTIONS: Classify block type, by assigning it to one or more groups
groups() -> [exec].

% INSTRUCTIONS: String describing block function
description() -> "Merge exec_out control flow signals".

% Use pattern: Major.Minor.Patch
% When a block is created, the Config version attribute value 
% is set to this version.
% When a block is loaded from a config file, the version attribute value
% is compared to this.  
% If the versions are different, the upgrade() function is called.
version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {num_of_inputs, {2}}               
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
       {exec_inputs, [{empty, ?EMPTY_LINK}, {empty, ?EMPTY_LINK}]}
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
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
    
  % Check the config values
  case config_utils:get_integer_range(Config, num_of_inputs, 2, 99) of
      {ok, NumOfInputs} ->              
        % Create N inputs
        BlockName = config_utils:name(Config),
        Inputs1 = input_utils:resize_attribute_array_value(BlockName, Inputs, 
                              exec_inputs, NumOfInputs, {empty, ?EMPTY_LINK}),

        % All config values are OK      
        % Initialize output value
        Value = null,
        Status = initialed;

      {error, Reason} ->
        Inputs1 = Inputs,
        {Value, Status} = config_utils:log_error(Config, num_of_inputs, Reason)
  end,

  Outputs1 = output_utils:set_value_status(Outputs, Value, Status),  

  % This is the block state
  {Config, Inputs1, Outputs1, Private}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

 
  % All inputs are exec_input type, which will only be connected 
  % There is no value to read, validate, or calculate output value
  Outputs1 = output_utils:set_value_status(Outputs, null, normal),
  
  % Return updated block state
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  
  {Config, Inputs, Outputs}.


%% 
%% Unknown Info message, just log a warning
%% 
-spec handle_info(Info :: term(), 
                  BlockState :: block_state()) -> {noreply, block_state()}.

handle_info(Info, BlockState) ->

  log_server:warning(block_server_unknown_info_msg, [Info]),
  {noreply, BlockState}.


%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%  Invoke a minimal block test, 
%  Call the block's create(), upgrade(), initialize(), execute(), and delete() functions

block_test() ->
  unit_test_utils:min_block_test(?MODULE).

-endif.
