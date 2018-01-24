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
          log_strings/0,
          block_type_strings/0,
          attrib_strings/0
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


%%
%%  Map of user interface string ids to actual strings
%%
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
  cmd_link_blocks_help => "Link a block output value to a block input value (data flow)",
  cmd_unlink_blocks_help => "Unlink a block output value from a block input value",
  cmd_xlink_blocks_help => "Link a block execute out to block execute input (control flow)",
  cmd_xunlink_blocks_help => "Unlink a block execute out from block execute input",
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
  block_type_created => "Block: ~s Type: ~s Created~n",
  dest_block_created => "Dest Block ~s Created~n",
  block_deleted => "Block ~s Deleted~n",
  block_disabled => "Block ~s Disabled~n",
  block_enabled => "Block ~s Enabled~n",
  block_frozen => "Block ~s Frozen~n",
  block_thawed => "Block ~s Thawed~n",
  block_execution_linked_to_block => "Block: ~s execution Linked to Block: ~s~n",
  block_execution_unlinked_from_block => "Block: ~s execution Unlinked from Block: ~s~n",
  block_config_file_loaded => "Block config file: ~s loaded~n",
  block_config_file_saved => "Block config file: ~s saved~n",
  enter_config_file_name => "Enter file name, or press <Enter> for default: ~s: ",
  config_file_overwrite_warning => "This will overwrite ~s if the file exists. OK to continue? (Y/N): ",
  block_does_not_exist_warning => "Block: ~s does not exist. OK to continue? (Y/N): ",  
  node_prompt_str => "Node: ~p~n",
  nodes_prompt_str => "Nodes: ~p~n",
  enter_node_name => "Enter node-name or local~n",
  connecting_to_local_node => "Connecting to local node~n",
  connected_to_node => "Connected to node: ~p~n",
  unable_to_connect_to_node => "Unable to connect to node: ~p~n",
  err_invalid_block_type => "Error: Invalid block type: ~s~n",
  err_block_already_exists => "Error: Block ~s already exists~n",
  err_dest_block_already_exists => "Error: Dest Block ~s already exists~n",
  err_creating_block => "Error: ~p, Creating block ~s~n",
  err_creating_block_type => "Error: ~p, Creating block type: ~s, Name: ~s~n",
  err_deleting_block => "Error: ~p, Deleting block ~s~n",
  err_disabling_block => "Error: ~p, Disabling block ~s~n",
  err_enabling_block => "Error: ~p, Enabling block ~s~n",
  err_freezing_block => "Error: ~p, Freezing block ~s~n",
  err_thawing_block => "Error: ~p, Thawing block ~s~n",
  err_block_not_found => "Error: Block ~s not found~n",
  err_block_value_not_found => "Error: Attribute: ~s does not exist~n",
  err_setting_block_value => "Error: ~p Setting: ~s:~s to ~s~n",
  err_source_block_does_not_exist => "Error: Source Block ~s does not exist~n",
  err_invalid_value_id => "Error: ~s is not a value of block: ~s~n",
  err_retrieving_value => "Error: ~p, Retrieving value: ~s:~s~n",
  err_invalid_value_id_str => "Error: Invalid Value Id string: ~s~n",
  block_output_linked_to_block_input => "Block Output: ~s:~s Linked to Block Input: ~s:~s~n",
  block_output_unlinked_from_block_input => "Block Output: ~s:~s Unlinked from Block Input: ~s:~s~n",
  block_output_already_linked_to_block_input => "Block Output: ~s is already Linked to Block Input: ~s~n",
  block_output_is_not_linked_to_block_input => "Block Output: ~s is not Linked to Block Input: ~s~n",
  err_linking_output_to_input => "Error: ~p, Linking Output: ~s:~s to Input: ~s:~s~n",
  err_unlinking_output_from_input => "Error: ~p, Unlinking Output: ~s:~s from Input: ~s:~s~n",
  err_adding_execution_link_from_to => "Error: ~p, Adding execution link from: ~s to: ~s~n",
  err_deleting_execution_link_from_to => "Error: ~p, Deleting execution link from: ~s to: ~s~n",

  err_converting_to_output_value_id => "Error: ~p, Converting ~s to Output Value ID~n",
  err_too_many_params => "Error: Too many parameters~n",
  unk_cmd_str => "Unknown command string: ~p~n",
  unk_cmd_atom => "Unknown command atom: ~p~n",
  err_unk_result_from_linkblox_api_get_block => "Error: Unkown result from linkblox_api:get_block(): ~p~n",
  err_unk_result_from_linkblox_api_get_value => "Error: Unkown result from linkblox_api:get_value(): ~p~n",
  inv_block_values => "Invalid Block Values. Unable to display. ~n",
  err_loading_block_config_file => "Error: ~p, Loading block conifg file: ~s~n",
  err_saving_block_config_file => "Error: ~p, Saving block conifg file: ~s~n",
  err_parsing_cmd_line => "Error: Parsing command: ~s",

  err_invalid_block_type_module => "Invalid block type module"
}.


%%
%% Map of log string ids to actual strings
%%
log_strings() -> #{
  starting_linkblox_lang_mod => "Starting LinkBlox app, using Language Module: ~p",
  starting_SSH_CLI_user_interface_on_port => "Starting SSH CLI User Interface on port: ~p",

  linkblox_startup_complete => "LinkBlox startup complete",
  err_starting_linkblox => "Error: ~p starting LinkBlox",

  distributed_node_started => "Distributed node: ~p started",
  err_distributed_node_already_started => "Distributed node: ~p already started",
  err_starting_distributed_node => "Error: ~p, Starting distributed node: ~p",

  host_name => "Host name: ~s",

  starting_logger => "Starting logger",
  unknown_logger_call_msg => "logger, Unknown call message: ~p",
  unknown_logger_cast_msg => "logger, Unknown cast message: ~p",
  unknown_logger_info_msg => "logger, Unknown info message: ~p",
  logger_abnormal_termination => "logger, Abnormal termination, reason: ~p",
  
  starting_node_watcher => "Starting node_watcher",
  node_has_connected => "Node ~p has connected",
  node_has_disconnected => "Node ~p has disconnected",
  node_watcher_received_unexpected_msg => "node_watcher, Received unexpected message: ~p",

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
  creating_type_version => "Creating: ~p Type: ~s Version: ~s",

  block_config_saved_to_file => "Block config saved to file: ~s",
  block_values_file => "Default block values file: ~s",
  opening_block_values_config_file => "Opening block Values config file: ~p",
  err_no_directory_saving_block_config_file => "Error: ~p no directory, saving block config file: ~s",
  err_reading_block_config_file => "Error: ~p reading block config file: ~p",

  err_invalid_reason => "~p Invalid '~s' reason: ~p",
  err_invalid_reason_value => "~p Invalid '~s' reason: ~p value: ~p",
  err_invalid_config_value => "~p Invalid '~s' config value: ~p",
  err_invalid_input_value => "~p Invalid '~s' input value: ~p",
  err_invalid_output_value => "~p Invalid '~s' output value: ~p",
  err_invalid_block_type_module => "~p Invalid block type module",

  negative_exec_interval_value => "~p Negative exec_interval value: ~p",
  invalid_exec_interval_value => "~p Invalid exec_interval value: ~p",

  err_unrecognized_link => "Error: unrecognized link: ~p",
  block_created => "Block ~p created~n",
  initializing_block => "Initializing: ~p",
  deleting_block => "Deleting block: ~p",
  reconfiguring_block => "Reconfiguring block: ~p",
  err_creating_block => "Error: ~p creating block ~s",

  block_output_linked_to_block_input => "Block Output: ~s Linked to Block Input: ~s",
  block_output_unlinked_from_block_input => "Block Output: ~s Unlinked from Block Input: ~s",
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
  err_LED_file_does_not_exist => "LED file: ~p does not exist",

  connected_to_MQTT_broker => "~p connected to MQTT broker",
  disconnected_from_MQTT_broker => "~p disconnected from MQTT broker",
  mqtt_client_shutdown => "~p MQTT client shutdown: ~p",
  started_MQTT_client => "Started MQTT client",
  err_starting_MQTT_client => "Error: ~p starting MQTT client",
  err_configuring_pub_inputs => "Error: ~p configuring pub inputs",
  err_configuring_sub_outputs => "Error: ~p configuring sub outputs",
  err_updating_is_this_an_mqtt_pub_sub_block => "Error: ~p updating ~p. Is this an mqtt_pub_sub block?"
}.


%%
%% Map block type module name to block type name and description strings
%% Add an entry for each block type
%% Block type name strings must be unique and not contain any spaces
%% Block type name strings are used to look up the block type module names and visa-versa
%%
block_type_strings() -> 
[
  {lblx_exec_count,      "exec_count",      "Incr/Decr counter output value on block execution"},
  {lblx_float_to_7seg,   "float_to_7seg",   "Convert floating point value input to multiple 7 segment digits outputs"},
  {lblx_float_to_str,    "float_to_str",    "Convert floating point value to a string"},
  {lblx_gpio_di,         "gpio_di",         "GPIO digital input"},
  {lblx_gpio_do,         "gpio_do",         "GPIO digital output"},
  {lblx_i2c_bme280,      "i2c_bme280",      "Bosch temperature, pressure, and humidity sensor with I2C interface"},
  {lblx_i2c_hd44780,     "i2c_hd44780",     "4 Row X 20 Col LCD display with I2C interface"},
  {lblx_i2c_ht16k33,     "i2c_ht16k33",     "4 digit 7 segment LED display with I2C interface"},
  {lblx_i2c_mcp9808,     "i2c_mcp9808",     "Precision temp sensor with I2C interface"},
  {lblx_i2c_pca9685,     "i2c_pca9685",     "PCA9685 16 Channel 12 bit PWM Output with I2C interface"},
  {lblx_identity,        "identity",        "Output value is identical to input value"},
  {lblx_int_to_7seg,     "int_to_7seg",     "Convert integer input to multiple 7 segment digits outputs"},
  {lblx_logic_and,       "logic_and",       "AND of All of the Binary Inputs"},
  {lblx_logic_config1,   "logic_config1",   "1 Input Configurable Logic Gate"},
  {lblx_logic_config1n,  "logic_config1n",  "1 Input Configurable Logic Gate, Null Values Allowed"},
  {lblx_logic_config2,   "logic_config2",   "2 Input Configurable Logic Gate"},
  {lblx_logic_config2n,  "logic_config2n",  "2 Input Configurable Logic Gate, Null Values Allowed"},
  {lblx_logic_config3,   "logic_config3",   "3 Input Configurable Logic Gate"},
  {lblx_logic_config3n,  "logic_config3n",  "3 Input Configurable Logic Gate, Null Values Allowed"},
  {lblx_logic_config4,   "logic_config4",   "4 Input Configurable Logic Gate"},
  {lblx_logic_jk_ff,     "logic_jk_ff",     "JK Flip-Flop"},
  {lblx_logic_nand,      "logic_nand",      "NAND of All of the Binary Inputs"},
  {lblx_logic_nor,       "logic_nor",       "NOR of All of the Binary Inputs"},
  {lblx_logic_not,       "logic_not",       "Invert Binary Input"},
  {lblx_logic_or,        "logic_or",        "OR of All of the Binary Inputs"},
  {lblx_logic_toggle,    "logic_toggle",    "Toggle binary output value on block execution"},
  {lblx_logic_tristate,  "logic_tristate",  "Tri-State a Binary Input Value"},
  {lblx_logic_xnor,      "logic_xnor",      "XNOR of the Binary Inputs"},
  {lblx_logic_xor,       "logic_xor",       "XOR of the Binary Inputs"},
  {lblx_math_avg,        "math_avg",        "Average All Input Values"},
  {lblx_math_limit,      "math_limit",      "Limit Input Value"},
  {lblx_math_sum,        "math_sum",        "Sum All Input Values"},
  {lblx_mqtt_pub_sub,    "mqtt_pub_sub",    "Publish and Subscribe values to and from an MQTT Broker"},
  {lblx_one_digit_7seg,  "one_digit_7seg",  "Single digit 7 segment LED driver"},
  {lblx_receive_values,  "receive_values",  "Receive values from other nodes"},
  {lblx_rotary_encoder,  "rotary_encoder",  "Rotary encoder with optional switch"},
  {lblx_rpi_led,         "rpi_led",         "Control Raspi on-board LED"},
  {lblx_select_bin,      "select_bin",      "Binary Input Selects the True or False Input Value"},
  {lblx_select_n,        "select_n",        "Integer Input Selects 1 of N Inputs"},
  {lblx_select_pri,      "select_pri",      "Select Highest Priority Active Input Value"},
  {lblx_select_tri,      "select_tri",      "Trinary Select True, False, or Null Input Value"},
  {lblx_send_values,     "send_values",     "Send Values to other nodes"},
  {lblx_template,        "template",        "Short description of block function"},
  {lblx_timer_min_on,    "timer_min_on",    "Output remain on for minimum specified time"}
].


%%
%% Map of attribute string ids to actual strings
%% Order alphabetically by the attribute string id
%% Add a map entry for each unique attribute id
%% Attribute strings must be unique and not contain any spaces
%% The attribute strings are used to look up the attribute IDs and visa-versa
%% 
attrib_strings() ->
[
  {'0_out', "0_out"},  % Output value for false input
  {'1_out', "1_out"},  % Output value for true input
  {'X_out', "X_out"},  % Output value for null input

  {'0_0_out', "0_0_out"},  % Output value for input 2 = false & 1 = false
  {'0_1_out', "0_1_out"},  % Output value for input 2 = false & 1 = true
  {'0_X_out', "0_X_out"},  % Output value for input 2 = false & 1 = null
  {'1_0_out', "1_0_out"},  % Output value for input 2 = true & 1 = false
  {'1_1_out', "1_1_out"},  % Output value for input 2 = true & 1 = true
  {'1_X_out', "1_X_out"},  % Output value for input 2 = true & 1 = null
  {'X_0_out', "X_0_out"},  % Output value for input 2 = null & 1 = false
  {'X_1_out', "X_1_out"},  % Output value for input 2 = null & 1 = true
  {'X_X_out', "X_X_out"},  % Output value for input 2 = null & 1 = null

  {'0_0_0_out', "0_0_0_out"},  % Output value for input 3 = false & 2 = false & 1 = false
  {'0_0_1_out', "0_0_1_out"},  % Output value for input 3 = false & 2 = false & 1 = true
  {'0_0_X_out', "0_0_X_out"},  % Output value for input 3 = false & 2 = false & 1 = null
  {'0_1_0_out', "0_1_0_out"},  % Output value for input 3 = false & 2 = true & 1 = false
  {'0_1_1_out', "0_1_1_out"},  % Output value for input 3 = false & 2 = true & 1 = true
  {'0_1_X_out', "0_1_X_out"},  % Output value for input 3 = false & 2 = true & 1 = null
  {'0_X_0_out', "0_X_0_out"},  % Output value for input 3 = false & 2 = null & 1 = false
  {'0_X_1_out', "0_X_1_out"},  % Output value for input 3 = false & 2 = null & 1 = true
  {'0_X_X_out', "0_X_X_out"},  % Output value for input 3 = false & 2 = null & 1 = null

  {'1_0_0_out', "1_0_0_out"},  % Output value for input 3 = true & 2 = false & 1 = false
  {'1_0_1_out', "1_0_1_out"},  % Output value for input 3 = true & 2 = false & 1 = true
  {'1_0_X_out', "1_0_X_out"},  % Output value for input 3 = true & 2 = false & 1 = null
  {'1_1_0_out', "1_1_0_out"},  % Output value for input 3 = true & 2 = true & 1 = false
  {'1_1_1_out', "1_1_1_out"},  % Output value for input 3 = true & 2 = true & 1 = true
  {'1_1_X_out', "1_1_X_out"},  % Output value for input 3 = true & 2 = true & 1 = null
  {'1_X_0_out', "1_X_0_out"},  % Output value for input 3 = true & 2 = null & 1 = false
  {'1_X_1_out', "1_X_1_out"},  % Output value for input 3 = true & 2 = null & 1 = true
  {'1_X_X_out', "1_X_X_out"},  % Output value for input 3 = true & 2 = null & 1 = null

  {'X_0_0_out', "X_0_0_out"},  % Output value for input 3 = null & 2 = false & 1 = false
  {'X_0_1_out', "X_0_1_out"},  % Output value for input 3 = null & 2 = false & 1 = true
  {'X_0_X_out', "X_0_X_out"},  % Output value for input 3 = null & 2 = false & 1 = null
  {'X_1_0_out', "X_1_0_out"},  % Output value for input 3 = null & 2 = true & 1 = false
  {'X_1_1_out', "X_1_1_out"},  % Output value for input 3 = null & 2 = true & 1 = true
  {'X_1_X_out', "X_1_X_out"},  % Output value for input 3 = null & 2 = true & 1 = null
  {'X_X_0_out', "X_X_0_out"},  % Output value for input 3 = null & 2 = null & 1 = false
  {'X_X_1_out', "X_X_1_out"},  % Output value for input 3 = null & 2 = null & 1 = true
  {'X_X_X_out', "X_X_X_out"},  % Output value for input 3 = null & 2 = null & 1 = null

  {'0_0_0_0_out', "0_0_0_0_out"},  % Output value for input 4 = false & 3 = false & 2 = false & 1 = false
  {'0_0_0_1_out', "0_0_0_1_out"},  % Output value for input 4 = false & 3 = false & 2 = false & 1 = true
  {'0_0_1_0_out', "0_0_1_0_out"},  % Output value for input 4 = false & 3 = false & 2 = true & 1 = false
  {'0_0_1_1_out', "0_0_1_1_out"},  % Output value for input 4 = false & 3 = false & 2 = true & 1 = true
  {'0_1_0_0_out', "0_1_0_0_out"},  % Output value for input 4 = false & 3 = true & 2 = false & 1 = false
  {'0_1_0_1_out', "0_1_0_1_out"},  % Output value for input 4 = false & 3 = true & 2 = false & 1 = true
  {'0_1_1_0_out', "0_1_1_0_out"},  % Output value for input 4 = false & 3 = true & 2 = true & 1 = false
  {'0_1_1_1_out', "0_1_1_1_out"},  % Output value for input 4 = false & 3 = true & 2 = true & 1 = true
  {'1_0_0_0_out', "1_0_0_0_out"},  % Output value for input 4 = true & 3 = false & 2 = false & 1 = false
  {'1_0_0_1_out', "1_0_0_1_out"},  % Output value for input 4 = true & 3 = false & 2 = false & 1 = true
  {'1_0_1_0_out', "1_0_1_0_out"},  % Output value for input 4 = true & 3 = false & 2 = true & 1 = false
  {'1_0_1_1_out', "1_0_1_1_out"},  % Output value for input 4 = true & 3 = false & 2 = true & 1 = true
  {'1_1_0_0_out', "1_1_0_0_out"},  % Output value for input 4 = true & 3 = true & 2 = false & 1 = false
  {'1_1_0_1_out', "1_1_0_1_out"},  % Output value for input 4 = true & 3 = true & 2 = false & 1 = true
  {'1_1_1_0_out', "1_1_1_0_out"},  % Output value for input 4 = true & 3 = true & 2 = true & 1 = false
  {'1_1_1_1_out', "1_1_1_1_out"},   % Output value for input 4 = true & 3 = true & 2 = true & 1 = true

  {active_true, "active_true"}, 
  {active_false, "active_false"}, 
  {backlight, "backlight"}, 
  {blink_cursor, "blink_cursor"}, 
  {blink_rate, "blink_rate"}, 
  {block_name, "block_name"}, 
  {block_module, "block_module"}, 
  {brightness, "brightness"}, 
  {broker, "broker"},  % MQTT server URL
  {carry, "carry"}, 
  {clear, "clear"}, 
  {clean_sess, "clean_sess"}, 
  {client_id, "client_id"}, 
  {colon, "colon"}, 
  {cursor, "cursor"}, 
  {dec_pnt, "dec_pnt"},  % Decimal Point
  {default_value, "default_value"}, 
  {deg_f, "deg_f"},  % Degrees Fahrenheit
  {description, "description"}, 
  {digits, "digits"}, 
  {digit1, "digit1"}, 
  {digit2, "digit2"}, 
  {digit3, "digit3"}, 
  {digit4, "digit4"}, 
  {disable, "disable"}, 
  {display, "display"}, 
  {display_on, "display_on"}, 
  {exec_in, "exec_in"}, 
  {exec_interval, "exec_interval"}, 
  {exec_method, "exec_method"}, 
  {exec_out, "exec_out"}, 
  {false_input, "false_input"}, 
  {field_width, "field_width"}, 
  {field_widths, "field_widths"}, 
  {filter_coeff, "filter_coeff"}, 
  {final_value, "final_value"}, 
  {freeze, "freeze"}, 
  {gpio_pin, "gpio_pin"}, 
  {gpio_pin_phase_A, "gpio_pin_phase_A"}, 
  {gpio_pin_phase_B, "gpio_pin_phase_B"}, 
  {gpio_pin_switch, "gpio_pin_switch"}, 
  {high_limit, "high_limit"}, 
  {humid, "humid"},  % Humidity
  {humid_mode, "humid_mode"},  % Humidity Mode
  {humid_offset, "humid_offset"},  % Humidity Offset
  {i2c_addr, "i2c_addr"}, 
  {i2c_device, "i2c_device"}, 
  {ignore_nulls, "ignore_nulls"}, 
  {inch_merc, "inch_merc"},  % Inches of Mercury (barometric pressure)
  {initial_state, "initial_state"}, 
  {initial_value, "initial_value"}, 
  {input, "input"}, 
  {input1, "input1"}, 
  {input2, "input2"}, 
  {input3, "input3"}, 
  {input4, "input4"}, 
  {inputs, "inputs"}, 
  {input_a, "input_a"}, 
  {input_b, "input_b"}, 
  {input_j, "input_j"}, 
  {input_k, "input_k"}, 
  {invert_output, "invert_output"}, 
  {keepalive, "keepalive"}, 
  {last_exec, "last_exec"}, 
  {leading_zeros, "leading_zeros"}, 
  {led_id, "led_id"}, 
  {left_justify, "left_justify"}, 
  {logger, "logger"}, 
  {low_limit, "low_limit"}, 
  {min_on_time, "min_on_time"}, 
  {neg_overflow, "neg_overflow"}, 
  {neg_precision, "neg_precision"}, 
  {nodes, "nodes"}, 
  {nodes_state, "nodes_state"}, 
  {not_active_str, "not_active_str"}, 
  {null_input, "null_input"}, 
  {number_base, "number_base"}, 
  {num_of_digits, "num_of_digits"}, 
  {num_of_inputs, "num_of_inputs"}, 
  {num_of_nodes, "num_of_nodes"}, 
  {num_of_pubs, "num_of_pubs"},  % MQTT quantity of topics to publish
  {num_of_subs, "num_of_subs"},  % MQTT quantity of topics to subscribe to
  {num_of_values, "num_of_values"}, 
  {password, "password"}, 
  {phase_int_edge, "phase_int_edge"}, 
  {port, "port"},  % MQTT server port number
  {pos_overflow, "pos_overflow"}, 
  {pos_precision, "pos_precision"}, 
  {precision, "precision"}, 
  {press, "press"},  % Pressure
  {press_mode, "press_mode"},  % Pressure Mode
  {press_offset, "press_offset"},  % Pressure Offset
  {proto_ver, "proto_ver"},  % MQTT protocol version
  {pub_inputs, "pub_inputs"},  % MQTT values to publis
  {pub_topics, "pub_topics"},  % MQTT topics to publish
  {read_mode, "read_mode"}, 
  {receive_values, "receive_values"}, 
  {reset, "reset"}, 
  {rollover, "rollover"}, 
  {seg_a, "seg_a"}, 
  {seg_b, "seg_b"}, 
  {seg_c, "seg_c"}, 
  {seg_d, "seg_d"}, 
  {seg_e, "seg_e"}, 
  {seg_f, "seg_f"}, 
  {seg_g, "seg_g"}, 
  {seg_dp, "seg_dp"}, 
  {segments, "segments"}, 
  {select, "select"}, 
  {send_values, "send_values"}, 
  {standby_time, "standby_time"}, 
  {start_cols, "start_cols"}, 
  {start_rows, "start_rows"}, 
  {status, "status"}, 
  {sub_topics, "sub_topics"},  % MQTT topics to subscribe to
  {sub_values, "sub_values"},  % MQTT value of subscribed topics
  {switch, "switch"}, 
  {switch_int_edge, "switch_int_edge"}, 
  {temp, "temp"}, 
  {temp_mode, "temp_mode"},  % Temperature Mode
  {temp_offset, "temp_offset"},  % Temperature Offset
  {true_input, "true_input"}, 
  {username, "username"}, 
  {value, "value"}, 
  {version, "version"}
].


