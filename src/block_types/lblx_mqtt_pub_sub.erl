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

% TODO: Add quality of service, and auto reconnect config values

%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->
  
  % Create private attributes
  Private1 = attrib_utils:merge_attribute_lists(Private, 
                                                [{client, {not_active}}, 
                                                 {pub_msgs, {[]}}, 
                                                 {conn_state, {not_active}}]),
  case config_pubs(Config, Inputs) of
    {ok, Config1, Inputs1} ->

      case config_subs(Config1, Outputs) of
        {ok, Config2, Outputs1} ->

          % if host name is missing, this is a config error
          case config_utils:get_string(Config2, host) of
            {ok, Host} ->
              case string:len(Host) > 0 of
                true ->
                  Options = get_options(Config2),
                  case emqttc:start_link(Options) of 
                    {ok, Client} ->
                      {ok, Private2} = attrib_utils:set_value(Private1, client, Client),
                      log_server:info(started_MQTT_client),
                      Status = initialed,
                      Value = not_active,
                      % Subscibe to the sub_topics config values
                      sub_topics(Config, Client);

                    {error, Reason} ->
                      log_server:error(err_starting_MQTT_client, [Reason]),
                      Status = proc_err,
                      Value = not_active,
                      Private2 = Private1
                  end;

                false ->
                  % Host config value is empty
                  {Value, Status} = config_utils:log_error(Config, host, empty),
                  Config2 = Config1,
                  Outputs1 = Outputs,
                  Private2 = Private1
              end;

            {error, Reason} ->
              % Error reading host config value
              {Value, Status} = config_utils:log_error(Config, host, Reason),
              Config2 = Config1,
              Outputs1 = Outputs,
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

  {ok, Client} = attrib_utils:get_value(Private, client),

  % Update connected status
  case attrib_utils:get_value(Private, conn_state) of
    {ok, true} ->
      {ok, Outputs1} = attrib_utils:set_value(Outputs, connected, true),
      % Read inputs, publish values to corresponding topics
      pub_topics(Config, Inputs, Client);
    
    {ok, false} ->
      {ok, Outputs1} = attrib_utils:set_value(Outputs, connected, false);
      % Don't bother publishing input values, if disconnected.

    _ ->
      {ok, Outputs1} = attrib_utils:set_value(Outputs, connected, not_active)
  end,

  % Read received publish messages and update subscribed output values
  {Outputs2, Private1} = process_pub_msgs(Config, Outputs1, Private),
  
  % Return updated block state
  {Config, Inputs, Outputs2, Private1}.


%% 
%%  Delete the block
%%	
-spec delete(BlockValues :: block_state()) -> block_defn().

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
                                                 pub_topics, NumOfPubs, {""}),
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
                                                  sub_topics, NumOfSubs, {""}),
      % Create subscribe outputs
      Outputs1 = output_utils:resize_attribute_array_value(BlockName, Outputs, 
                                                  sub_outputs, NumOfSubs, {not_active, []}),
      % Return updated Config and Outputs attributes
      {ok, Config1, Outputs1};
    
    {error, Reason} ->
      config_utils:log_error(Config, num_of_subs, Reason),
      {error, Reason}
  end.

%
% Publish pub_inputs values to the topics in the pub_topics config values
%
-spec pub_topics(Config :: list(config_attr()), 
                 Inputs :: list(input_attr()),
                 Client :: pid()) -> ok.

pub_topics(Config, Inputs, Client) ->
  PubTopicsValues = get_pub_topics_values(Config, Inputs),

  % For each pub_topic config and pub input value pair, 
  % send a publish message to the MQTT client
  % PubTopicsValues is of the form: [{<<"TopicA">>,<<"TopicAValue">>}, ...]

  lists:foreach(fun({PubTopicBin, PubValueBin}) -> 
                     log_server:debug("MQTT Client: ~p publishing value: ~p  to topic: ~p", [Client, PubValueBin, PubTopicBin]),
                     emqttc:publish(Client, PubTopicBin, PubValueBin)
                     end, PubTopicsValues). 

%
% Get a list of Topics and Input Values to publish
%
-spec get_pub_topics_values(Config :: list(config_attr()),
                            Inputs :: list(input_attr())) -> list(tuple()).

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
              case (PubValue /= not_active) andalso (string:len(PubValue) > 0) of
                true ->
                  PubTopicBin = list_to_binary(PubTopic),
                  PubValueBin = list_to_binary(PubValue),
                  NewTopicsValues = [{PubTopicBin, PubValueBin} | TopicsValues];
                false ->
                  % pub input value is zero length string, or not_active
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
-spec sub_topics(Config :: list(config_attr()), 
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
                          log_server:debug("MQTT Client: ~p subscribing to: ~p", [Client, SubTopicBin]),
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
-spec unsub_topics(Config :: list(config_attr()), 
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
                          log_server:debug("MQTT Client: ~p unsubscribing from: ~p", [Client, SubTopicBin]),
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
-spec process_pub_msgs(Config :: list(config_attr()),
                       Outputs :: list(output_attr()),
                       Private :: list(private_attr())) -> {list(output_attr()), list(private_attr())}.

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
-spec update_sub_values(Config :: list(config_attr()), 
                        Outputs :: list(output_attr()), 
                        PubMsgs :: list(tuple())) -> list(output_attr()).
                      
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
      {FinOutputs, _Index} =
        lists:foldl(fun({SubTopic}, {NewOutputs, Index}) -> 
                      case block_utils:is_string(SubTopic) andalso (string:len(SubTopic) > 0) of
                        true ->
                          case SubTopic == PubMsgTopic of
                            true ->
                              DebugStr = lists:flatten(io_lib:format("Topic: ~s Set sub_values[~b] to ~p", 
                                                                       [PubMsgTopic, Index, PubMsgValue])),
                              log_server:debug(DebugStr),
                              {ok, UpdOutputs} = attrib_utils:set_value(NewOutputs, {sub_values, Index}, PubMsgValue),
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