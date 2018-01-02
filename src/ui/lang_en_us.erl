%%% @doc
%%% 
%%% English US Language UI Strings 
%%%
%%% @end

-module(lang_en_us).

-author("Mark Sebald").

-export([
          ui_cmds/0,
          ui_strings/0,
          log_strings/0
]).


%%
%% Map a string to ui command atom, parameter(s) string, and help text
%%  Template:  {"cmd",       cmd_atom,             cmd_atom_params,          cmd_atom_help},
%%
-spec ui_cmds() -> list({string(), atom(), atom(), atom()}).

ui_cmds() ->
  [
    {"create",    cmd_create_block,     cmd_create_block_params,  cmd_create_block_help},
    {"copy",      cmd_copy_block,       cmd_copy_block_params,    cmd_copy_block_help},
    {"rename",    cmd_rename_block,     cmd_rename_block_params,  cmd_rename_block_help},
    {"execute",   cmd_execute_block,    cmd_block_name_param,     cmd_execute_block_help},
    {"delete",    cmd_delete_block,     cmd_block_name_param,     cmd_delete_block_help},
    {"disable",   cmd_disable_block,    cmd_block_name_param,     cmd_disable_block_help},
    {"enable",    cmd_enable_block,     cmd_block_name_param,     cmd_enable_block_help},
    {"freeze",    cmd_freeze_block,     cmd_block_name_param,     cmd_freeze_block_help},
    {"thaw",      cmd_thaw_block,       cmd_block_name_param,     cmd_thaw_block_help},
    {"get",       cmd_get_values,       cmd_block_name_param,     cmd_get_values_help},
    {"set",       cmd_set_value,        cmd_set_value_params,     cmd_set_value_help},
    {"link",      cmd_link_blocks,      cmd_link_blocks_params,   cmd_link_blocks_help},
    {"unlink",    cmd_unlink_blocks,    cmd_link_blocks_params,   cmd_unlink_blocks_help},
    {"xlink",     cmd_xlink_blocks,     cmd_xlink_blocks_params,  cmd_xlink_blocks_help},
    {"xunlink",   cmd_xunlink_blocks,   cmd_xlink_blocks_params,  cmd_xunlink_blocks_help},
    {"status",    cmd_status,           cmd_blank_params,         cmd_status_help},
    {"valid",     cmd_valid_block_name, cmd_block_name_param,     cmd_valid_block_name_help},
    {"load",      cmd_load_blocks,      cmd_file_name_params,     cmd_load_blocks_help},
    {"save",      cmd_save_blocks,      cmd_file_name_params,     cmd_save_blocks_help},
    {"node",      cmd_node,             cmd_blank_params,         cmd_node_help},
    {"nodes",     cmd_nodes,            cmd_blank_params,         cmd_nodes_help},
    {"connect",   cmd_connect,          cmd_node_name_params,     cmd_connect_help},
    {"hosts",     cmd_hosts,            cmd_blank_params,         cmd_hosts_help},
    {"exit",      cmd_exit,             cmd_blank_params,         cmd_exit_help},
    {"help",      cmd_help,             cmd_blank_params,         cmd_help_help}
  ].


ui_strings() -> #{
  welcome_str => "~n   W E L C O M E  T O  L i n k B l o x !~n~n",
  enter_command_str => "Enter command\n",

  cmd_atom_params => "command parameter list",
  cmd_create_block_params => "block-type-name new-block-name <description-string>",
  cmd_copy_block_params => "source-block-name dest-block-name",
  cmd_rename_block_params => "current-block-name new-block-name",
  cmd_set_value_params => "block-name value-name value",
  cmd_link_blocks_params => "output-block-name output-value-name input-block-name input-value-name",
  cmd_xlink_blocks_params => "output-block-name input-block-name",
  cmd_block_name_param => "block-name",
  cmd_file_name_params => "file-name | blank",
  cmd_node_name_params => "node-name",
  cmd_blank_params => "",

  cmd_atom_help => "Command help text",
  cmd_create_block_help => "Create a new block",
  cmd_copy_block_help => "Create a copy of an existing block",
  cmd_rename_block_help => "Rename a block",
  cmd_execute_block_help => "Execute a block",
  cmd_delete_block_help => "Delete a block",
  cmd_disable_block_help => "Prevent a block from executing",
  cmd_enable_block_help => "Allow a block to execute",
  cmd_freeze_block_help => "Freeze block's outputs at current values",
  cmd_thaw_block_help => "Allow a block's output values to be updated",
  cmd_get_values_help => "Get the specified block value",
  cmd_set_value_help => "Set the specified block value",
  cmd_link_blocks_help => "Link a block output value to the input value of a block",
  cmd_unlink_blocks_help => "Unlink a block output value from the input value of a block",
  cmd_status_help => "Display status of all created blocks on this node",
  cmd_valid_block_name_help => "Is the entered string a valid block name",
  cmd_load_blocks_help => "Load block definitions from a file",
  cmd_save_blocks_help => "Save all block defintions to a file",
  cmd_node_help => "Display the self node of the user",
  cmd_nodes_help => "Display a list of all currently connected nodes",
  cmd_connect_help => "Connect to another node",
  cmd_hosts_help => "Display the contents of the hosts file",
  cmd_exit_help => "Exit the LinkBlox command line",
  cmd_help_help => "Display help screen",

  linkblox_help => "~n     LinkBlox Help~n~n",
  no_help_for => "No help for: ~s~n",
  enter_str => "Enter", 

  config_str => "Config Values:~n",
  inputs_str => "Input Values: (Default)~n",
  outputs_str => "Output Values: (Links)~n",
  block_value_set_to_str => "~s:~s Set to: ~s~n",
  block_exists => "Block: ~s exists~n",
  block_does_not_exist => "Block ~p does not exist~n",
  block_type_created => "Block ~s:~s Created~n",
  dest_block_created => "Dest Block ~s Created~n",
  block_deleted => "Block ~s Deleted~n",
  block_disabled => "Block ~s Disabled~n",
  block_enabled => "Block ~s Enabled~n",
  block_frozen => "Block ~s Frozen~n",
  block_thawed => "Block ~s Thawed~n",
  block_output_linked_to_block_input => "Block Output: ~s Linked to Block Input: ~s~n",
  block_output_unlinked_from_block_input => "Block Output: ~s Unlinked from Block Input: ~s~n",
  block_execution_linked_to_block => "Block: ~s execution Linked to Block: ~s~n",
  block_execution_unlinked_from_block => "Block: ~s execution Unlinked from Block: ~s~n",
  block_output_unlinked => "Block Output: ~s:~s Unlinked~n",
  block_config_file_loaded => "Block config file: ~s loaded~n",
  block_config_file_saved => "Block config file: ~s saved~n",
  enter_config_file_name => "Enter file name, or press <Enter> for default: 'LinkBloxConfig': ",
  config_file_overwrite_warning => "This will overwrite ~s if the file exists. OK to continue? (Y/N): ",
  node_prompt_str => "Node: ~p~n",
  nodes_prompt_str => "Nodes: ~p~n",
  enter_node_name => "Enter node-name or local~n",
  connecting_to_local_node => "Connecting to local node~n",
  connected_to_node => "Connected to node: ~p~n",
  unable_to_connect_to_node => "Unable to connect to node: ~p~n",
  err_invalid_block_type => "Error: Invalid block type: ~s~n",
  err_block_already_exists => "Error: Block ~s already exists~n",
  err_dest_block_already_exists => "Error: Dest Block ~s already exists~n",
  err_creating_block => "Error: ~p creating block ~s ~n",
  err_creating_block_type => "Error: ~p creating block ~s:~s ~n",
  err_deleting_block => "Error: ~p deleting block ~s~n",
  err_disabling_block => "Error: ~p disabling block ~s~n",
  err_enabling_block => "Error: ~p enabling block ~s~n",
  err_freezing_block => "Error: ~p freezing block ~s~n",
  err_thawing_block => "Error: ~p thawing block ~s~n",
  err_block_not_found => "Error: Block ~s not found~n",
  err_block_value_not_found => "Error: Attribute: ~s does not exist~n",
  err_setting_block_value => "Error: ~p Setting: ~s:~s to ~s~n",
  err_source_block_does_not_exist => "Error: Source Block ~s does not exist~n",
  err_invalid_value_id => "Error: ~s is not a value of block: ~s~n",
  err_retrieving_value => "Error: ~p retrieving value: ~s:~s~n",
  err_invalid_value_id_str => "Error: Invalid Value Id string: ~s~n",
  err_linking_output_to_input => "Error: ~p Linking Output: ~s:~s to Input: ~p~n",
  err_unlinking_output_from_input => "Error: ~p Unlinking Output: ~s:~s from Input: ~p~n",
  err_adding_execution_link_from_to => "Error: ~p Adding execution link from:~s to: ~s~n",
  err_deleting_execution_link_from_to => "Error: ~p Deleting execution link from:~s to: ~s~n",

  err_converting_to_link => "Error: ~p Converting ~p to a Link~n",
  err_converting_to_output_value_id => "Error: ~p Converting ~s to Output Value ID~n",
  err_too_many_params => "Error: Too many parameters~n",
  unk_cmd_str => "Unknown command string: ~p~n",
  unk_cmd_atom => "Unknown command atom: ~p~n",
  err_unk_result_from_linkblox_api_get_block => "Error: Unkown result from linkblox_api:get_block(): ~p~n",
  err_unk_result_from_linkblox_api_get_value => "Error: Unkown result from linkblox_api:get_value(): ~p~n",
  inv_block_values => "Invalid Block Values. Unable to display. ~n",
  err_loading_block_config_file => "Error: ~p loading block conifg file: ~s~n",
  err_saving_block_config_file => "Error: ~p saving block conifg file: ~s~n",
  err_parsing_cmd_line => "Error: Parsing command: ~s"
}.

log_strings() -> #{
  starting_SSH_CLI_user_interface_on_port_language_module => "Starting SSH CLI User Interface on port: ~p Language Module: ~p",

  linkblox_startup_complete => "LinkBlox startup complete",
  err_starting_linkblox => "Error: ~p starting LinkBlox",

  host_name => "Host name: ~p",

  starting_logger => "Starting logger, Language Module: ~p",
  unknown_logger_call_msg => "logger, Unknown call message: ~p",
  unknown_logger_cast_msg => "logger, Unknown cast message: ~p",
  unknown_logger_info_msg => "logger, Unknown info message: ~p",
  logger_abnormal_termination => "logger, Abnormal termination, reason: ~p",
  
  starting_node_watcher => "Starting node_watcher",
  node_has_connected => "Node ~p has connected",
  node_has_disconnected => "Node ~p has disconnected",
  node_watcher_received_unexpected_msg => "node_watcher, Received unexpected message: ~p",

  starting_system_server => "Starting system_server",
  unknown_system_server_call_msg => "system_server, Unknown call message: ~p",
  unknown_system_server_cast_msg => "system_server, Unknown cast message: ~p",
  unknown_system_server_info_msg => "system_server, Unknown info message: ~p",
  system_server_abnormal_termination => "system_server, Abnormal termination, reason: ~p",

  starting_linkblox_API_server => "Starting LinkBlox API server",
  stopping_linkblox_API_server => "Stopping LinkBlox API server",
  linkblox_API_server_abnormal_termination => "LinkBlox API server, Abnormal Termination: ~p",
  linkblox_api_unknown_call_msg_from => "linkblox_api, Unknown call message: ~p From: ~p",
  linkblox_api_unknown_cast_msg => "linkblox_api, Unknown cast message: ~p",
  linkblox_api_unknown_info_msg => "linkblox_api, Unknown info message: ~p",
  linkblox_api_received_update_for_unknown_block => "linkblox_api, Received update for unknown block: ~p",
  linkblox_api_received_unlink_for_unknown_block => "linkblox_api, Recieved unlink for unknown block: ~p",

  starting_linkblox_block_supervisor => "Starting LinkBlox Block supervisor",
  loading_demo_config => "Loading Demo config...",
  creating_type_version => "Creating: ~p Type: ~p Version: ~s",

  block_config_saved_to_file => "Block config saved to file: ~s",
  opening_block_values_config_file => "Opening block Values config file: ~p",
  err_no_directory_saving_block_config_file => "Error: ~p no directory, saving block config file: ~s",
  err_reading_block_config_file => "Error: ~p reading block config file: ~p",

  err_invalid_reason => "~p Invalid '~s' reason: ~p",
  err_invalid_reason_value => "~p Invalid '~s' reason: ~p value: ~p",
  err_invalid_config_value => "~p Invalid '~s' config value: ~p",
  err_invalid_input_value => "~p Invalid '~s' input value: ~p",
  err_invalid_output_value => "~p Invalid '~s' output value: ~p",

  negative_exec_interval_value => "~p Negative exec_interval value: ~p",
  invalid_exec_interval_value => "~p Invalid exec_interval value: ~p",

  err_unrecognized_link => "Error: unrecognized link: ~p",
  block_created => "Block ~p created~n",
  initializing_block => "Initializing: ~p",
  deleting_block => "Deleting block: ~p",
  reconfiguring_block => "Reconfiguring block: ~p",
  err_creating_block => "Error: ~p creating block ~s",

  add_link_err_doesnt_exist_for_this_block => "add_link() Error. ~p Doesn't exist for this block",
  add_link_err_invalid_array_index => "add_link() Error. Invalid array index ~p",
  del_link_err_doesnt_exist_for_this_block => "del_link() Error. ~p Doesn't exist for this block",
  del_link_err_invalid_array_index => "del_link() Error. Invalid array index ~p",
  linked_block_does_not_exist => "Linked Block: ~p Does not exist",

  block_server_unknown_call_msg_from => "block_server(~p) Unknown call message: ~p From: ~p",
  block_server_unknown_cast_msg => "block_server(~p) Unknown cast message: ~p",
  block_server_unknown_info_msg => "block_server(~p), Unknown info message: ~p",
  block_server_abnormal_termination => "block_server, Abnormal Termination: ~p  Reason: ~p",

  block_type_name_unknown_call_msg_from => "block(~p:~p) Unknown call message: ~p From: ~p",
  block_type_name_unknown_cast_msg => "block(~p:~p) Unknown cast message: ~p",
  block_type_name_unknown_info_msg => "block(~p:~p), Unknown info message: ~p",

  block_type_upgraded_from_ver_to => "Block: ~p type: ~p upgraded from ver: ~s to: ~s",
  err_upgrading_block_type_from_ver_to => "Error: ~p upgrading block: ~p type: ~p from ver: ~s to: ~s",

  err_initiating_GPIO_pin => "~p Error: ~p intitiating GPIO pin: ~p",
  err_initiating_I2C_address => "Error: ~p intitiating I2C address: ~p",

  err_resetting_device => "Error: ~p resettting device",
  err_converting_sensor_values => "Error: ~p converting sensor values",
  err_resetting_sensor => "Error: ~p resetting sensor",
  err_reading_sensor => "Error: ~p reading sensor",
  err_reading_sensor_calibration => "Error: ~p reading sensor calibration",
  err_configuring_sensor => "Error: ~p configuring sensor",
  err_reading_sensor_forced_mode => "Error: ~p reading sensor forced mode",
  err_setting_sensor_config_register => "Error: ~p setting sensor config register",
  err_setting_humidity_mode => "Error: ~p setting humidity mode",
  err_setting_temperature_pressure_or_read_mode => "Error: ~p setting temperature, pressure, or read modes",
  err_is_an_invalid_standby_time_value => "Error: ~p is an invalid standby time value",
  err_reading_standby_time_value => "Error: ~p reading standby time value",
  err_is_an_invalid_filter_coefficient_value => "Error: ~p is an invalid filter coefficient value",
  err_reading_filter_coefficient_value => "Error: ~p reading filter coefficient value",
  err_is_an_invalid_humidity_mode_value => "Error: ~p is an invalid humidity mode value",
  err_reading_humidity_mode_value => "Error: ~p reading humidity mode value",
  err_is_an_invalid_temperature_mode_value => "Error: ~p is an invalid temperature mode value",
  err_reading_temperature_mode_value => "Error: ~p reading temperature mode value",
  err_is_an_invalid_pressure_mode_value => "Error: ~p is an invalid pressure mode value",
  err_reading_pressure_mode_value => "Error: ~p reading pressure mode value",
  err_is_an_invalid_read_mode_value_sleep_normal_forced => "Error: ~p is an invalid read mode value (sleep, normal, forced)",
  err_reading_read_mode_value => "Error: ~p reading read mode value",
  err_waiting_for_sleep_mode => "Error: ~p waiting for sleep mode",
  err_setting_forced_read_mode => "Error: ~p setting forced read mode",

  err_initializing_LCD_driver_I2C_address => "Error: ~p intitializing LCD driver, I2C address: ~p",
  err_initializing_LED_driver_I2C_address => "Error: ~p intitializing LED driver, I2C address: ~p",
  err_reading_temperature_sensor => "Error: ~p reading temperature sensor",
  err_LED_file_does_not_exist => "Error: LED file: ~p does not exist",

  connected_to_MQTT_broker => "~p connected to MQTT broker",
  disconnected_from_MQTT_broker => "~p disconnected from MQTT broker",
  mqtt_client_shutdown => "~p MQTT client shutdown: ~p",
  started_MQTT_client => "Started MQTT client",
  err_starting_MQTT_client => "Error: ~p starting MQTT client",
  err_configuring_pub_inputs => "Error: ~p configuring pub inputs",
  err_configuring_sub_outputs => "Error: ~p configuring sub outputs",
  err_updating_is_this_an_mqtt_pub_sub_block => "Error: ~p updating ~p. Is this an mqtt_pub_sub block?"
}.
 