%%% @doc 
%%% Block Type: mqtt_pub_sub
%%% Description: Publish and subscribe to values 
%%% via an MQTT (Message Queue Telemetry Transport) Broker (Server)
%%%               
%%% @end 

-module(lblx_mqtt_pub_sub). 
  
-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, description/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/1, delete/1]).

groups() -> [web].

description() -> "Publish and Subscribe values to and from an MQTT Broker".

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> list(config_attr()).

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {host, {"broker.hivemq.com"}},  % test MQTT broker, should already be running
      {port, {1883}},
      {client_id, {""}},  % Default client ID is the block name
      {clean_sess, {true}},
      {keepalive, {60}},
      {proto_ver, {4}},
      {username, {""}},
      {password, {""}},
      {logger, {all}}, % logging level, for debug purposes 
                         % valid values: all, debug, info, warning, error, critical, none
      {num_of_pubs, {1}},
      {pub_topics, [{""}]},
      {num_of_subs, {1}},
      {sub_topics, [{""}]}
    ]). 


-spec default_inputs() -> list(input_attr()).

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {pub_inputs, [{"", ?EMPTY_LINK}]}
    ]). 


-spec default_outputs() -> list(output_attr()).
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {connected, {not_active, []}},
      {sub_values, [{not_active, []}]}
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
  
  Private1 = attrib_utils:add_attribute(Private, {client, {empty}}),
  
  case config_pubs(Config, Inputs) of
    {ok, Config1, Inputs1} ->

      case config_subs(Config1, Outputs) of
        {ok, Config2, Outputs1} ->

          Options = get_options(Config2),

          case emqttc:start_link(Options) of
            {ok, Client} ->
              log_server:info(started_MQTT_client),
              Value = not_active, Status = initialed,
              {ok, Private2} = attrib_utils:set_value(Private1, client, Client);

            {error, Reason} ->
              log_server:error(err_starting_MQTT_client, [Reason]),
              Status = proc_err,
              Value = not_active,
              Private2 = Private1
          end;

        {error, Reason} ->
          log_server:error(err_configuring_sub_outputs, [Reason]),
          Status = config_err,
          Value = not_active, 
          Config2 = Config1,
          Outputs1 = Outputs,
          Private2 = Private1
      end;

    {error, Reason} ->
      log_server:error(err_configuring_pub_inputs, [Reason]),
      Status = config_err,
      Value = not_active, 
      Config2 = Config,
      Inputs1 = Inputs,
      Outputs1 = Outputs,
      Private2 = Private1
  end,

  Outputs2 = output_utils:set_value_status(Outputs1, Value, Status),

  % This is the block state
  {Config2, Inputs1, Outputs2, Private2}.


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
  % Unsubscribe from each subscription first
  {ok, Client} = attrib_utils:get_value(Private, client),
  emqttc:disconnect(Client),
  {Config, Inputs, Outputs}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%
% Get the list of MQTT client config options
% The option will not be added to the options list
% if the coresponding config value is not set or invalid.
%
% NOTE: Client ID will always be set to the block name
% if the client_id config value is not set or invalid.
%
% The MQTT client will use its default values for missing options.
%
% All get_xxx() functions should take the list of Config attributes and 
% current options list in, and return the Config attributes and 
% updated options list, to facilitate chaining the functions
%

-spec get_options(Config :: list(config_attr())) -> list(tuple()).

get_options(Config) ->
  GetFuns = [
    fun get_logger/1,
    fun get_password/1,
    fun get_username/1, 
    fun get_proto_ver/1,
    fun get_keepalive/1,
    fun get_clean_sess/1,
    fun get_client_id/1,
    fun get_port/1,
    fun get_host/1],

  build_opts(GetFuns, {Config, []}).

%
% Use the Chain pattern to build list of options for MQTT client
%
build_opts([], {_Config, Options}) ->
  log_server:debug("MQTT client options: ~p", [Options]),
  Options;

build_opts([Fun | Funs], {Config, Options}) ->
  build_opts(Funs, Fun({Config, Options})).


%
% Get host name config
% 
-spec get_host({Config :: list(config_attr()),
                Options :: list(tuple())}) -> {list(config_attr()), list(tuple())}.

get_host({Config, Options}) -> 
  case get_string_config(Config, host) of
    {ok, Host} -> {Config, [{host, Host} | Options]};
             _ -> {Config, Options}
  end.

%
% Get port number config
%
-spec get_port({Config :: list(config_attr()),
                Options :: list(tuple())}) -> {list(config_attr()), list(tuple())}.

get_port({Config, Options}) -> 
  case get_pos_int_config(Config, port) of
    {ok, Port} -> {Config, [{port, Port} | Options]};
             _ -> {Config, Options}
  end.

%
% Get Client ID config.  Default Client ID to block name
%
-spec get_client_id({Config :: list(config_attr()),
                     Options :: list(tuple())}) -> {list(config_attr()), list(tuple())}.

get_client_id({Config, Options}) -> 
  case get_string_config(Config, client_id) of
    {ok, ClientId} -> 
      ClientIdBin = list_to_binary(ClientId);
    _ ->
      % Default MQTT client name to this block's name
      BlockName = config_utils:name(Config),
      ClientIdBin = list_to_binary(atom_to_list(BlockName))
  end,
  {Config, [{client_id, ClientIdBin} | Options]}.

%
% Get clean session config.  
%
-spec get_clean_sess({Config :: list(config_attr()),
                      Options :: list(tuple())}) -> {list(config_attr()), list(tuple())}.

get_clean_sess({Config, Options}) -> 
  case config_utils:get_boolean(Config, clean_sess) of
    {ok, CleanSess} -> 
      {Config, [{clean_sess, CleanSess} | Options]};
    {error, Reason} ->
      config_utils:log_error(Config, clean_sess, Reason),
      {Config, Options}
  end.

%
% Get keep alive time config
%
-spec get_keepalive({Config :: list(config_attr()),
                     Options :: list(tuple())}) -> {list(config_attr()), list(tuple())}.

get_keepalive({Config, Options}) -> 
  case get_pos_int_config(Config, keepalive) of
    {ok, KeepAlive} -> {Config, [{keepalive, KeepAlive} | Options]};
                  _ -> {Config, Options}
  end.

%
% Get protocol version config
%
-spec get_proto_ver({Config :: list(config_attr()),
                     Options :: list(tuple())}) -> {list(config_attr()), list(tuple())}.

get_proto_ver({Config, Options}) -> 
  case get_pos_int_config(Config, proto_ver) of
    {ok, ProtoVer} -> {Config, [{proto_ver, ProtoVer} | Options]};
                 _ -> {Config, Options}
  end.

%
% Get username config
%
-spec get_username({Config :: list(config_attr()),
                    Options :: list(tuple())}) -> {list(config_attr()), list(tuple())}.

get_username({Config, Options}) -> 
  case get_string_config(Config, username) of
    {ok, Username} -> 
      UsernameBin = list_to_binary(Username),
      {Config, [{username, UsernameBin} | Options]};
    _ -> 
      {Config, Options}
  end.

%
% Get password config
%
-spec get_password({Config :: list(config_attr()),
                    Options :: list(tuple())}) -> {list(config_attr()), list(tuple())}.

get_password({Config, Options}) ->
 case get_string_config(Config, password) of
    {ok, Password} -> 
      PasswordBin = list_to_binary(Password),
      {Config, [{password, PasswordBin} | Options]};
    _ -> 
      {Config, Options}
  end.

%
% Get logging level config
% Always user OTP standard 'error_logger' library, 
% because the rest of the LinkBlox app uses it too.
% Default logging level to none. 
%
-spec get_logger({Config :: list(config_attr()),
                  Options :: list(tuple())}) -> {list(config_attr()), list(tuple())}.

get_logger({Config, Options}) ->
 case config_utils:get_atom(Config, logger) of
    {ok, Logger} -> {Config, [{logger, {error_logger, Logger}} | Options]};
               _ -> {Config, [{logger, {error_logger, none}} | Options]}
  end.

%
% Get a string config value
%
-spec get_string_config(Config :: list(config_attr()),
                        ValueName :: value_name()) -> {list(config_attr()), list(tuple())}.

get_string_config(Config, ValueName) ->
  case config_utils:get_string(Config, ValueName) of
    {ok, Value} ->
      case string:len(Value) > 0 of
        true ->  {ok, Value};
        false -> {error, empty} % Call it an error so we don't try an set an empty option value
      end;

    {error, Reason} ->
      config_utils:log_error(Config, ValueName, Reason),
      {error, Reason}
  end.

%
% Get a positive (1...) integer config value
%
-spec get_pos_int_config(Config :: list(config_attr()),
                        ValueName :: value_name()) -> {list(config_attr()), list(tuple())}.

get_pos_int_config(Config, ValueName) ->
  case config_utils:get_pos_integer(Config, ValueName) of
    {ok, Value} -> {ok, Value};

    {error, Reason} ->
      config_utils:log_error(Config, ValueName, Reason),
      {error, Reason}
  end.


%
% Configure quantity of publish topics and inputs
%
-spec config_pubs(Config :: list(config_attr()),
                  Inputs :: list(input_attr())) -> {ok, list(config_attr()), list(input_attr())} | {error, atom()}.

config_pubs(Config, Inputs) ->
  case config_utils:get_integer_range(Config, num_of_pubs, 1, 99) of
    {ok, NumOfPubs} ->      
      BlockName = config_utils:name(Config),

      % Create publish topic config values, 
      % one topic string for each published input value 
      Config1 = config_utils:resize_attribute_array_value(Config, 
                                      pub_topics, NumOfPubs, {"PubTopic"}),
      % Create publish inputs
      Inputs1 = input_utils:resize_attribute_array_value(BlockName, Inputs, 
                                  pub_inputs, NumOfPubs, {empty, ?EMPTY_LINK}),
      % Return updated Config and Inputs attributes
      {ok, Config1, Inputs1};

    {error, Reason} ->
      config_utils:log_error(Config, num_of_pubs, Reason),
      {error, Reason}
  end.

%
% Configure quantity of subscription topics and outputs
%
-spec config_subs(Config :: list(config_attr()),
                  Outputs :: list(output_attr())) -> {ok, list(config_attr()), list(output_attr())} | {error, atom()}.

config_subs(Config, Outputs) ->
  case config_utils:get_integer_range(Config, num_of_subs, 1, 99) of
    {ok, NumOfSubs} ->      
      BlockName = config_utils:name(Config),
      % Create subscribe topic config values, 
      % one topic string for each subscribed output value 
      Config1 = config_utils:resize_attribute_array_value(Config, 
                                      sub_topics, NumOfSubs, {"SubTopic"}),
      % Create subscribe outputs
      Outputs1 = output_utils:resize_attribute_array_value(BlockName, Outputs, 
                                  sub_outputs, NumOfSubs, {not_active, []}),
      % Return updated Config and Outputs attributes
      {ok, Config1, Outputs1};
    
    {error, Reason} ->
      config_utils:log_error(Config, num_of_subs, Reason),
      {error, Reason}
  end.


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% At a minimum, call the block type's create(), upgrade(), initialize(), execute(), and delete() functions.

block_test() ->
  log_server:start(lang_en_us),
  BlockDefn = create(mqtt_pub_sub_test, "Unit Test Block"),
  {ok, BlockDefn} = upgrade(BlockDefn),
  BlockState = block_common:initialize(BlockDefn),
  execute(BlockState),
  delete(BlockState),
  ?assert(true).

-endif.