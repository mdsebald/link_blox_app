%%% @doc 
%%% Block Type: mqtt_publish 
%%% Description: Publish value(s) to MQTT (Message Queue Telemetry Transport) Broker (Server)
%%%               
%%% @end 

-module(lblx_mqtt_publish). 
  
-author("Mark Sebald").

 % INSTRUCTIONS: Adjust path to hrl file as needed
-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/1, delete/1]).

groups() -> [web].

description() -> "Publish Values to MQTT Broker".

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {host, {"localhost"}},
      {port, {1883}},
      {client_id, {"LinkBloxClientId"}},
      {clean_session, {true}},
      {keep_alive, {60}},
      {protocol_version, {4}},
      {username, {""}},
      {password, {""}},
      {topic, {""}}
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {input, {"", ?EMPTY_LINK}}
    ]). 


-spec default_outputs() -> list(output_attr()).
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {topic_pub, {not_active, []}},
      {connected, {not_active, []}}
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
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  {ok, Host} = attrib_utils:get_value(Config, host),
  {ok, Port} = attrib_utils:get_value(Config, port),
  {ok, ClientId} = attrib_utils:get_value(Config, client_id),
  {ok, KeepAlive} = attrib_utils:get_value(Config, keep_alive),
  
  io:format("Host: ~p Port: ~p ClientId ~p KeepAlive ~p~n", [Host, Port, ClientId, KeepAlive]),

  % Use OTP standard error_logger, rest of LinkBlox uses this too
  case emqttc:start_link([{host, Host},
                          {port, Port},
                          {client_id, <<ClientId>>},
                          {keepalive, KeepAlive},
                          {logger, {error_logger, all}}]) of
    {ok, Client} ->
      log_server:info(started_MQTT_client),
      {ok, Private1} = attrib_utils:set_value(Private, client, Client);

    {error, Reason} ->
      log_server:error(err_starting_MQTT_client, [Reason]),
      Private1 = Private
  end,

  Outputs1 = output_utils:set_value_status(Outputs, not_active, initialed),

  % This is the block state
  {Config, Inputs, Outputs1, Private1}.


%%
%%  Execute the block specific functionality
%%
-spec execute(block_state()) -> block_state().

execute({Config, Inputs, Outputs, Private}) ->

  % Output updated in block server module?
  Outputs1 = Outputs,
  Private1 = Private,

  % Return updated block state
  {Config, Inputs, Outputs1, Private1}.


%% 
%%  Delete the block
%%	
-spec delete(BlockValues :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, Private}) ->
  Client = attrib_utils:get_value(Private, client),
  emqttc:disconnect(Client),
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
   execute(BlockState),
  _BlockDefnFinal = delete(BlockState),
  ?assert(true).

-endif.