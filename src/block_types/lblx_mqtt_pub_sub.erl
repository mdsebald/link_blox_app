%%% @doc 
%%% BLOCKTYPE
%%% 
%%% DESCRIPTION
%%% Publish and subscribe to values via
%%% an MQTT (Message Queue Telemetry Transport) Broker (Server)
%%% LINKS
%%% https://www.hivemq.com/blog/how-to-get-started-with-mqtt             
%%% @end 

-module(lblx_mqtt_pub_sub). 
  
-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([groups/0, version/0]).
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).
-export([handle_info/2]).

groups() -> [web].

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {broker, {"localhost"}},  %| string | "localhost" | N/A |
      {port, {1883}}, %| int | 1883 | 1024 to 49151? |
      {client_id, {""}},  %| string | "" | N/A |
      {clean_sess, {true}}, %| bool | true | true, false |
      {keepalive, {60}},%| int | 60 | 1..max int |
      {proto_ver, {4}}, %| int | 4 | 4 |
      {username, {""}}, %| string | "" | N/A |
      {password, {""}}, %| string | "" | N/A |
      {logger, {all}},  %| enum | all | all, debug, info, ... |
      {num_of_pubs, {1}}, %| int | 1 | 1..99 |
      {pub_topics, [{""}]}, %| string array | "" | N/A |
      {num_of_subs, {1}}, %| int | 1 | 1..99 |
      {sub_topics, [{""}]} %| string array | "" | N/A |
    ]). 


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {pub_inputs, [{"", {""}}]} %| string array | "" | N/A |
    ]). 


-spec default_outputs() -> output_attribs().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {sub_values, [{null, []}]} %| string array | null | N/A |
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
             InitConfig :: config_attribs(), 
             InitInputs :: input_attribs()) -> block_defn().
   
create(BlockName, Description, InitConfig, InitInputs) -> 
  create(BlockName, Description, InitConfig, InitInputs, []).

-spec create(BlockName :: block_name(),
             Description :: string(), 
             InitConfig :: config_attribs(), 
             InitInputs :: input_attribs(), 
             InitOutputs :: output_attribs()) -> block_defn().

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
      logger:info(block_type_upgraded_from_ver_to, 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      logger:error(err_upgrading_block_type_from_ver_to, 
                            [Reason, BlockName, BlockType, ConfigVer, ModuleVer]),
      {error, Reason}
  end.

% TODO: Add quality of service, and auto reconnect config values

%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
  
  % Create private attributes
  Private1 = attrib_utils:merge_attribute_lists(Private, 
                                                [{client, {null}}, 
                                                 {pub_msgs, {[]}}, 
                                                 {conn_state, {null}},
                                                 {subscribed, {false}}]),
  % If and when MQTT client is shutdown, 
  % don't don't let this block die too.
  process_flag(trap_exit, true),
  case config_pubs(Config, Inputs) of
    {ok, Config1, Inputs1} ->

      case config_subs(Config1, Outputs) of
        {ok, Config2, Outputs1} ->

          % if broker name config is invalid, or zero length, this is a config error
          case config_utils:get_string(Config2, broker) of
            {ok, Broker} ->
              case string:len(Broker) > 0 of
                true ->

                  case input_utils:get_boolean(Inputs1, disable) of
                    {ok, false} ->
                      % block is already enabled, start MQTT client
                      Options = get_options(Config2),
                      case emqttc:start_link(Options) of 
                        {ok, Client} ->
                          {ok, Private2} = attrib_utils:set_value(Private1, client, Client),
                          logger:info(started_MQTT_client),
                          Status = initialed,
                          Value = null;

                        {error, Reason} ->
                          logger:error(err_starting_MQTT_client, [Reason]),
                          Status = proc_err,
                          Value = null,
                          Private2 = Private1
                      end;

                    {ok, true} ->
                      % block is currently disabled, don't start MQTT client
                      Status = initialed,
                      Value = null,
                      Private2 = Private1;
                    
                    {error, Reason} ->
                      % Bad disable input value
                      Status = input_err,
                      Value = null,
                      Private2 = Private1,
                      input_utils:log_error(Config, disable, Reason)
                  end;

                false ->
                  % Broker config value is empty
                  {Value, Status} = config_utils:log_error(Config, broker, empty),
                  Private2 = Private1
              end;

            {error, Reason} ->
              % Error reading broker config value
              {Value, Status} = config_utils:log_error(Config, broker, Reason),
              Private2 = Private1
          end;

        {error, Reason} ->
          logger:error(err_configuring_sub_outputs, [Reason]),
          Status = config_err,
          Value = null, 
          Config2 = Config1,
          Outputs1 = Outputs,
          Private2 = Private1
      end;

    {error, Reason} ->
      logger:error(err_configuring_pub_inputs, [Reason]),
      Status = config_err,
      Value = null, 
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
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, ExecMethod) ->

  BlockName = config_utils:name(Config),
  logger:debug("Executing: ~p Exec Method: ~p", [BlockName, ExecMethod]),

  % Block is enabled at this point, 
  % Start MQTT client if not already started
  case attrib_utils:get_value(Private, client) of
    {ok, null} ->
      Options = get_options(Config),
      case emqttc:start_link(Options) of 
        {ok, Client} ->
          {ok, Private1} = attrib_utils:set_value(Private, client, Client),
          logger:info(started_MQTT_client),

          % Update the status and main output
          Outputs1 = output_utils:set_value_status(Outputs, null, normal),
          % Return updated block state
          {Config, Inputs, Outputs1, Private1};

        {error, Reason} ->
          logger:error(err_starting_MQTT_client, [Reason]),
          % Update the status and main output
          Outputs1 = output_utils:set_value_status(Outputs, null, proc_err),
          % Return updated block state
          {Config, Inputs, Outputs1, Private}
      end;

    {ok, Client} -> 
      % MQTT Client already started
      % Update connected status, 
      % conn_state is set via messages from MQTT client
      case attrib_utils:get_value(Private, conn_state) of
        {ok, true} ->
          % Subscibe to the sub_topics config values, if not done yet
          case attrib_utils:get_value(Private, subscribed) of
            {ok, false} ->
              % publish input values to pub_topics
              pub_topics(Config, Inputs, Client),
              % subscribe to sub_topics, so output values get updated
              sub_topics(Config, Client),

              {ok, Private1} = attrib_utils:set_value(Private, subscribed, true);
            {ok, true} ->
              % Already subscribed 
              Private1 = Private
          end,

          case ExecMethod of
            manual ->
              % Block manually executed, publish all input values
              pub_topics(Config, Inputs, Client),

              % Update the status and main output
              Outputs1 = output_utils:set_value_status(Outputs, true, normal),
              % Return updated block state
              {Config, Inputs, Outputs1, Private1};
            
            input_cos ->
              % An input value has changed
              % Read inputs, publish values to corresponding topics
              % TODO: Determine which input value(s) changed and only publish that one/those
              pub_topics(Config, Inputs, Client),
            
              % Update the status and main output
              Outputs1 = output_utils:set_value_status(Outputs, true, normal),
              % Return updated block state
              {Config, Inputs, Outputs1, Private1};

            message ->
              % Read received publish message(s) if any, and update subscribed output values
              {Outputs1, Private2} = process_pub_msgs(Config, Outputs, Private1),

              % Update the status and main output
              Outputs2 = output_utils:set_value_status(Outputs1, true, normal),
              % Return updated block state
              {Config, Inputs, Outputs2, Private2};
            
            ExecMethod ->
              logger:debug("Ignoring Exec Method: ~p", [ExecMethod]),
              % Update the status and main output
              Outputs1 = output_utils:set_value_status(Outputs, true, normal),
              % Return updated block state
              {Config, Inputs, Outputs1, Private1}
          end;
    
        _ConnState ->
          % ConnState, could be false or null, Don't care
          % Client Disconnected from MQTT broker. Don't bother subcribing or publishing
          % Reset the subscribed flag, to force resubscribe on next connect
          {ok, Private1} = attrib_utils:set_value(Private, subscribed, false),

          % If MQTT client is not running, clear the Client pid to set up to connect again
          case process_info(Client) of
            undefined ->
              {ok, Private2} = attrib_utils:set_value(Private1, client, null);
            
            _ProcInfo ->
              Private2 = Private1
          end,
          
          % Update the status and main output
          Outputs1 = output_utils:set_value_status(Outputs, false, normal),
          % Return updated block state
          {Config, Inputs, Outputs1, Private2}
      end
  end.


%% 
%%  Delete the block
%%	
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, Private}) ->
  % If MQTT client has been started
  % Unsubscribe from each subscription and then disconnect from MQTT client
  {ok, Client} = attrib_utils:get_value(Private, client),
  case is_pid(Client) of
    true ->
      unsub_topics(Config, Client),
      emqttc:disconnect(Client);
    false -> ok 
  end,
  {Config, Inputs, Outputs}.


%%======================================================================
%% Process messages from emqttc 
%%======================================================================

-spec handle_info(Info :: term(), 
                  BlockState :: block_state()) -> {noreply, block_state()}.

%% 
%% MQTT Publish message from a subscribed-to Topic.
%%
handle_info({publish, Topic, Payload}, BlockState) ->
  {Config, Inputs, Outputs, Private} = BlockState,
  BlockName = config_utils:name(Config),
  logger:debug("~p Rx MQTT pub msg Topic: ~p Payload: ~p", 
                      [BlockName, Topic, Payload]),

  % Add the message topic and payload to the front of the list of pub messages,
  % and execute the block
  case attrib_utils:get_value(Private, pub_msgs) of
    {ok, PubMsgs} ->
      {ok, Private1} = attrib_utils:set_value(Private, pub_msgs, [{Topic, Payload} | PubMsgs]),
      NewBlockState = block_common:execute({Config, Inputs, Outputs, Private1}, message);

    {error, Reason} ->
      logger:error(err_updating_is_this_an_mqtt_pub_sub_block, [Reason, BlockName]),
      NewBlockState = BlockState
  end,
  {noreply, NewBlockState};

%% 
%% MQTT Client connected message
%% 
handle_info({mqttc, _Client, connected}, BlockState) ->
  {Config, Inputs, Outputs, Private} = BlockState,
  BlockName = config_utils:name(Config),
  logger:info(connected_to_MQTT_broker, [BlockName]),

  % Update the connection state in the block values and execute the block
  case attrib_utils:set_value(Private, conn_state, true) of
    {ok, Private1} ->
      NewBlockState = block_common:execute({Config, Inputs, Outputs, Private1}, message);

    {error, Reason} ->
      logger:error(err_updating_is_this_an_mqtt_pub_sub_block, [Reason, BlockName]),
      NewBlockState = BlockState
  end,
  {noreply, NewBlockState};

%% 
%% MQTT Client disconnected message
%% 
handle_info({mqttc, _Client,  disconnected}, BlockState) ->
    {Config, Inputs, Outputs, Private} = BlockState,
    BlockName = config_utils:name(Config),
    logger:info(disconnected_from_MQTT_broker, [BlockName]),
    
    % Update the connection state in the block values, and execute the block
    case attrib_utils:set_value(Private, conn_state, false) of
      {ok, Private1} ->
        NewBlockState = block_common:execute({Config, Inputs, Outputs, Private1}, message);

      {error, Reason} ->
        logger:error(err_updating_is_this_an_mqtt_pub_sub_block, [Reason, BlockName]),
        NewBlockState = BlockState
    end,
    {noreply, NewBlockState};
  
%% 
%% MQTT Client shutdown message
%% 
handle_info({'EXIT', _Client, {shutdown, ShutdownReason}}, BlockState) ->
    {Config, Inputs, Outputs, Private} = BlockState,
    BlockName = config_utils:name(Config),
    logger:info(mqtt_client_shutdown, [BlockName, ShutdownReason]),
    
    % Reset the client reference, to indicate MQTT client needs to be restarted
    case attrib_utils:set_value(Private, client, null) of
      {ok, Private1} ->
        NewBlockState = block_common:execute({Config, Inputs, Outputs, Private1}, message);

      {error, Reason} ->
        logger:error(err_updating_is_this_an_mqtt_pub_sub_block, [Reason, BlockName]),
        NewBlockState = BlockState
    end,
    {noreply, NewBlockState};

%%
%% Unknown Info message, just log a warning
%% 
handle_info(Info, BlockState) ->
  {BlockName, BlockModule} = config_utils:name_module(BlockState),
  logger:warning(block_type_name_unknown_info_msg, [BlockModule, BlockName, Info]),
  {noreply, BlockState}.  



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

-spec get_options(Config :: config_attribs()) -> list(tuple()).

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
    fun get_broker/1],

  build_opts(GetFuns, {Config, []}).

%
% Use the Chain pattern to build list of options for MQTT client
%
build_opts([], {_Config, Options}) ->
  logger:debug("MQTT client options: ~p", [Options]),
  Options;

build_opts([Fun | Funs], {Config, Options}) ->
  build_opts(Funs, Fun({Config, Options})).


%
% Get broker name config
% 
-spec get_broker({Config :: config_attribs(),
                  Options :: list(tuple())}) -> {config_attribs(), list(tuple())}.

get_broker({Config, Options}) -> 
  case get_string_config(Config, broker) of
    {ok, Broker} -> {Config, [{host, Broker} | Options]};
               _ -> {Config, Options}
  end.

%
% Get port number config
%
-spec get_port({Config :: config_attribs(),
                Options :: list(tuple())}) -> {config_attribs(), list(tuple())}.

get_port({Config, Options}) -> 
  case get_pos_int_config(Config, port) of
    {ok, Port} -> {Config, [{port, Port} | Options]};
             _ -> {Config, Options}
  end.

%
% Get Client ID config.  Default Client ID to block name
%
-spec get_client_id({Config :: config_attribs(),
                     Options :: list(tuple())}) -> {config_attribs(), list(tuple())}.

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
-spec get_clean_sess({Config :: config_attribs(),
                      Options :: list(tuple())}) -> {config_attribs(), list(tuple())}.

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
-spec get_keepalive({Config :: config_attribs(),
                     Options :: list(tuple())}) -> {config_attribs(), list(tuple())}.

get_keepalive({Config, Options}) -> 
  case get_pos_int_config(Config, keepalive) of
    {ok, KeepAlive} -> {Config, [{keepalive, KeepAlive} | Options]};
                  _ -> {Config, Options}
  end.

%
% Get protocol version config
%
-spec get_proto_ver({Config :: config_attribs(),
                     Options :: list(tuple())}) -> {config_attribs(), list(tuple())}.

get_proto_ver({Config, Options}) -> 
  case get_pos_int_config(Config, proto_ver) of
    {ok, ProtoVer} -> {Config, [{proto_ver, ProtoVer} | Options]};
                 _ -> {Config, Options}
  end.

%
% Get username config
%
-spec get_username({Config :: config_attribs(),
                    Options :: list(tuple())}) -> {config_attribs(), list(tuple())}.

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
-spec get_password({Config :: config_attribs(),
                    Options :: list(tuple())}) -> {config_attribs(), list(tuple())}.

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
% Always use the lager logging library, 
% because the rest of the LinkBlox app uses it too.
% Default logging level to none. 
%
-spec get_logger({Config :: config_attribs(),
                  Options :: list(tuple())}) -> {config_attribs(), list(tuple())}.

get_logger({Config, Options}) ->
 case config_utils:get_atom(Config, logger) of
    {ok, Logger} -> {Config, [{logger, {lager, Logger}} | Options]};
               _ -> {Config, [{logger, {lager, none}} | Options]}
  end.

%
% Get a string config value
%
-spec get_string_config(Config :: config_attribs(),
                        ValueName :: value_name()) -> {ok, string()} | {error, atom()}.

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
-spec get_pos_int_config(Config :: config_attribs(),
                         ValueName :: value_name()) -> {ok, value()} | {error, atom()}.

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
-spec config_pubs(Config :: config_attribs(),
                  Inputs :: input_attribs()) -> {ok, config_attribs(), input_attribs()} | {error, atom()}.

config_pubs(Config, Inputs) ->
  case config_utils:get_integer_range(Config, num_of_pubs, 1, 99) of
    {ok, NumOfPubs} ->      
      BlockName = config_utils:name(Config),

      % Create publish topic config values, 
      % one topic string for each published input value 
      Config1 = config_utils:resize_attribute_array_value(Config, 
                                                 pub_topics, NumOfPubs, {""}),
      % Create publish inputs
      Inputs1 = input_utils:resize_attribute_array_value(BlockName, Inputs, 
                                                 pub_inputs, NumOfPubs, {empty, {empty}}),
      % Return updated Config and Inputs attributes
      {ok, Config1, Inputs1};

    {error, Reason} ->
      config_utils:log_error(Config, num_of_pubs, Reason),
      {error, Reason}
  end.

%
% Configure quantity of subscription topics and outputs
%
-spec config_subs(Config :: config_attribs(),
                  Outputs :: output_attribs()) -> {ok, config_attribs(), output_attribs()} | {error, atom()}.

config_subs(Config, Outputs) ->
  case config_utils:get_integer_range(Config, num_of_subs, 1, 99) of
    {ok, NumOfSubs} ->      
      % Create subscribe topic config values, 
      % one topic string for each subscribed output value 
      Config1 = config_utils:resize_attribute_array_value(Config, 
                                                  sub_topics, NumOfSubs, {""}),
      % Create subscribe values outputs
      Outputs1 = output_utils:resize_attribute_array_value(Outputs, 
                                                  sub_values, NumOfSubs, {null, []}),
      % Return updated Config and Outputs attributes
      {ok, Config1, Outputs1};
    
    {error, Reason} ->
      config_utils:log_error(Config, num_of_subs, Reason),
      {error, Reason}
  end.

%
% Publish pub_inputs values to the topics in the pub_topics config values
%
-spec pub_topics(Config :: config_attribs(), 
                 Inputs :: input_attribs(),
                 Client :: pid()) -> ok.

pub_topics(Config, Inputs, Client) ->
  PubTopicsValues = get_pub_topics_values(Config, Inputs),

  % For each pub_topic config and pub input value pair, 
  % send a publish message to the MQTT client
  % PubTopicsValues is of the form: [{<<"TopicA">>,<<"TopicAValue">>}, ...]

  lists:foreach(fun({PubTopicBin, PubValueBin}) -> 
                     logger:debug("MQTT Client: ~p publishing value: ~p  to topic: ~p", [Client, PubValueBin, PubTopicBin]),
                     emqttc:publish(Client, PubTopicBin, PubValueBin)
                     end, PubTopicsValues). 

%
% Get a list of Topics and Input Values to publish
%
-spec get_pub_topics_values(Config :: config_attribs(),
                            Inputs :: input_attribs()) -> list(tuple()).

get_pub_topics_values(Config, Inputs) ->
  {ok, NumOfPubs} = config_utils:get_pos_integer(Config, num_of_pubs),
  get_pub_topics_values(Config, Inputs, NumOfPubs, []).

get_pub_topics_values(_Config, _Inputs, 0, TopicsValues) ->
  TopicsValues;

get_pub_topics_values(Config, Inputs, Index, TopicsValues) ->
  case config_utils:get_string(Config, {pub_topics, Index}) of
    {ok, PubTopic} ->
      case string:len(PubTopic) > 0 of
        true ->
          case input_utils:get_string(Inputs, {pub_inputs, Index}) of
            {ok, PubValue} ->
              case (PubValue /= null) andalso (string:len(PubValue) > 0) of
                true ->
                  PubTopicBin = list_to_binary(PubTopic),
                  PubValueBin = list_to_binary(PubValue),
                  NewTopicsValues = [{PubTopicBin, PubValueBin} | TopicsValues];
                false ->
                  % pub input value is zero length string, or null
                  NewTopicsValues = TopicsValues
              end;
            {error, Reason} ->
              % bad pub input[x] value 
              input_utils:log_error(Config, {pub_inputs, Index}, Reason),
              NewTopicsValues = TopicsValues
          end;  
        false ->
          % config pub topic zero length string
          NewTopicsValues = TopicsValues
      end;
    {error, Reason} ->
      % bad config pub topic value
      config_utils:log_error(Config, {pub_topics, Index}, Reason),
      NewTopicsValues = TopicsValues
  end,
  get_pub_topics_values(Config, Inputs, Index-1, NewTopicsValues).

%
% Subscribe to the topics in the config values
%
-spec sub_topics(Config :: config_attribs(), 
                 Client :: pid()) -> ok.

sub_topics(Config, Client) ->
  % for each sub_topic value, send a subscribe message to MQTT client
  % Get the whole array of sub_topics config values at once
  % SubTopics is of the form: [{"Topic1"}, {"Topic2"}, ...]
  case attrib_utils:get_attribute(Config, sub_topics) of
    {ok, {sub_topics, SubTopics}} ->
      lists:foreach(fun({SubTopic}) -> 
                      case block_utils:is_string(SubTopic) andalso (string:len(SubTopic) > 0) of
                        true ->
                          SubTopicBin = list_to_binary(SubTopic),
                          logger:debug("MQTT Client: ~p subscribing to: ~p", [Client, SubTopicBin]),
                          emqttc:subscribe(Client, SubTopicBin);
                        false -> 
                          ok % nothing to subscribe to
                      end
                    end, SubTopics);
    ErrorValue ->
      attrib_utils:log_error(Config, sub_topics, invalid, ErrorValue)
  end,
  ok.

%
% Unsubscribe to the topics in the sub_topics config values 
%
-spec unsub_topics(Config :: config_attribs(), 
                   Client :: pid()) -> ok.

unsub_topics(Config, Client) ->
  % For each sub_topic value send an unsubscribe message to MQTT client
  % Get the whole array of sub_topics config values at once
  % SubTopics is of the form [{"Topic1"}, {"Topic2"}, ...]
  case attrib_utils:get_attribute(Config, sub_topics) of
    {ok, {sub_topics, SubTopics}} ->
      lists:foreach(fun({SubTopic}) -> 
                      case block_utils:is_string(SubTopic) andalso (string:len(SubTopic) > 0) of
                        true ->
                          SubTopicBin = list_to_binary(SubTopic),
                          logger:debug("MQTT Client: ~p unsubscribing from: ~p", [Client, SubTopicBin]),
                          emqttc:unsubscribe(Client, SubTopicBin);
                        false -> 
                          ok % nothing to unsubscribe from
                      end
                    end, SubTopics);
    ErrorValue ->
      attrib_utils:log_error(Config, sub_topics, invalid, ErrorValue)
  end,
  ok.

%
% Process received publish messages, and update coresponding sub_values outputs
%
-spec process_pub_msgs(Config :: config_attribs(),
                       Outputs :: output_attribs(),
                       Private :: private_attribs()) -> {output_attribs(), private_attribs()}.

process_pub_msgs(Config, Outputs, Private) ->

  case attrib_utils:get_value(Private, pub_msgs) of
    {ok, []} ->
      % No messages to process
      {Outputs, Private};
    
    {ok, PubMsgs} ->
      % In case there is more than one pub message with the same topic
      % Only process the latest one, which will be first in the list
      DedupedPubMsgs = lists:ukeysort(1, PubMsgs),
      Outputs1 = update_sub_values(Config, Outputs, DedupedPubMsgs),
      % Clear pub message list
      {ok, Private1} = attrib_utils:set_value(Private, pub_msgs, []),
      {Outputs1, Private1};

    {error, Reason} ->
      % error reading pub messages, log error and clear 
      attrib_utils:log_error(Config, pub_msgs, Reason),
      {ok, Private1} = attrib_utils:set_value(Private, pub_msgs, []),
      {Outputs, Private1}
  end.

%
% Read publish messages and update corresponding sub_values outputs
%
-spec update_sub_values(Config :: config_attribs(), 
                        Outputs :: output_attribs(), 
                        PubMsgs :: list(tuple())) -> output_attribs().
                      
% When done reading messages, just return updated Output values
update_sub_values(_Config, Outputs, []) ->
  Outputs;

update_sub_values(Config, InitOutputs, [{PubMsgTopic, PubMsgValue} | PubMsgs]) ->
  % get each sub_topic config value and try to match it with the PubMsgTopic.  
  % The sub_value output with corresponding index will get updated with the PubMsgValue
  % Get the whole array of sub_topics config values at once
  % SubTopics is of the form [{"Topic1"}, {"Topic2"}, ...]
  case attrib_utils:get_attribute(Config, sub_topics) of
    {ok, {sub_topics, SubTopics}} ->
      PubMsgTopicStr = binary_to_list(PubMsgTopic),
      PubMsgValueStr = binary_to_list(PubMsgValue),
      {FinOutputs, _Index} =
        lists:foldl(fun({SubTopic}, {NewOutputs, Index}) -> 
                      case block_utils:is_string(SubTopic) andalso (string:len(SubTopic) > 0) of
                        true ->
                          case SubTopic == PubMsgTopicStr of
                            true ->
                              DebugStr = lists:flatten(io_lib:format("Topic: ~s Set sub_values[~b] to ~p", 
                                                                       [PubMsgTopicStr, Index, PubMsgValueStr])),
                              logger:debug(DebugStr),
                              {ok, UpdOutputs} = attrib_utils:set_value(NewOutputs, {sub_values, Index}, PubMsgValueStr),
                              {UpdOutputs, Index + 1};
                            
                            false -> {NewOutputs, Index + 1}
                          end;
                        false -> {NewOutputs, Index + 1}
                      end
                    end, 
                    {InitOutputs, 1}, % Initial Accumulator
                    SubTopics);
    ErrorVal ->
      attrib_utils:log_error(Config, sub_topics, invalid, ErrorVal),
      FinOutputs = InitOutputs
  end,
  update_sub_values(Config, FinOutputs, PubMsgs).


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("block_io_test_gen.hrl").

test_sets() ->
  [
    {[{status, normal}]}
  ].

-endif.