%%% @doc
%%% 
%%% English US Language UI Strings 
%%%
%%% @end

-module(lang_en_us).

-author("Mark Sebald").

-export([
          cmds_map/0,
          strings_map/0
]).


%%
%% Map string to ui command atom
%%
-spec cmds_map() -> list({string(), atom(), string()}).

cmds_map() ->
  [
    {"create",    cmd_create_block,     "<block type name> <new block name>"},
    {"copy",      cmd_copy_block,       "<source block name> <dest block name>"},
    {"rename",    cmd_rename_block,     "<current block name> <new block name>"},
    {"execute",   cmd_execute_block,    "<block name>"},
    {"delete",    cmd_delete_block,     "<block name>"},
    {"disable",   cmd_disable_block,    "<block name>"},
    {"enable",    cmd_enable_block,     "<block name>"},
    {"freeze",    cmd_freeze_block,     "<block name>"},
    {"thaw",      cmd_thaw_block,       "<block name>"},
    {"get",       cmd_get_values,       "<block name>"},
    {"set",       cmd_set_value,        "<block name> <attribute name> <value>"},
    {"link",      cmd_link_blocks,      "<block name> <input name> <block name> <output name>"},
    {"unlink",    cmd_unlink_blocks,    "<block name> <input name>"},
    {"status",    cmd_status,           ""},
    {"valid",     cmd_valid_block_name, "<block name>"},
    {"load",      cmd_load_blocks,      "<file name> | blank"},
    {"save",      cmd_save_blocks,      "<file name> | blank"},
    {"node",      cmd_node,             ""},
    {"nodes",     cmd_nodes,            ""},
    {"connect",   cmd_connect,          "<node name>"},
    {"hosts",     cmd_hosts,            ""},
    {"exit",      cmd_exit,             ""},
    {"help",      cmd_help,             "display help screen"}
  ].


-spec strings_map() -> map().

strings_map() -> #{
  welcome_str => "~n   W E L C O M E  T O  L i n k B l o x !~n~n",
  enter_command_str => "Enter command\n",
  config_str => "Config:~n",
  inputs_str => "Inputs:~n",
  outputs_str => "Outputs:~n",
  self_link_str => "  Link: ~p",
  block_link_str => "  Link: ~p:~p",
  node_link_str => "  Link: ~p:~p:~p",
  reference_str => "  Refs: ~p",
  block_value_set_to_str => "~s:~s Set to: ~s~n",
  enter_block_name => "Enter block-name~n",
  block_exists => "Block: ~s exists~n",
  block_does_not_exist => "Block ~p does not exist~n",
  block_type_created => "Block ~s:~s Created~n",
  dest_block_created => "Dest Block ~s Created~n",
  block_deleted => "Block ~s Deleted~n",
  block_disabled => "Block ~s Disabled~n",
  block_enabled => "Block ~s Enabled~n",
  block_frozen => "Block ~s Frozen~n",
  block_thawed => "Block ~s Thawed~n",
  block_input_linked_to_block_output => "Block Input: ~s:~s Linked to Block Output: ~p~n",
  block_input_unlinked => "Block Input: ~s:~s Unlinked~n",
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
  err_source_block_does_not_exist => "Error: Source Block ~s does not exists~n",
  err_invalid_value_id => "Error: ~s is not a value of block: ~s~n",
  err_retrieving_value => "Error: ~p retrieving value: ~s:~s~n",
  err_invalid_value_id_str => "Error: Invalid Value Id string: ~s~n",
  err_linking_input_to_output => "Error: ~p Linking Input: ~s:~s to Output: ~p~n",
  err_unlinking_input => "Error: ~p Unlinking Input: ~s:~s~n",
  err_converting_to_link => "Error: ~p Converting ~p to a Link~n",
  err_converting_to_input_value_id => "Error: ~p Converting ~s to Input Value ID~n",
  err_too_many_params => "Error: Too many parameters~n",
  unk_cmd_str => "Unknown command string: ~p~n",
  unk_cmd_atom => "Unknown command atom: ~p~n",
  err_unk_result_from_linkblox_api_get_block => "Error: Unkown result from linkblox_api:get_block(): ~p~n",
  err_unk_result_from_linkblox_api_get_value => "Error: Unkown result from linkblox_api:get_value(): ~p~n",
  inv_block_values => "Invalid Block Values. Unable to display. ~n",
  err_loading_block_config_file => "Error: ~p loading block conifg file: ~s~n",
  err_saving_block_config_file => "Error: ~p saving block conifg file: ~s~n",
  err_parsing_cmd_line => "Error: Parsing command: ~s",

  err_invalid_freeze_input_value => "~p Invalid freeze input value: ~p ~n",

  starting_log_server => "Starting log_server, Language Module: ~p~n",
  err_unknown_log_server_call_msg => "log_server, Unknown call message: ~p~n",
  err_unknown_log_server_cast_msg => "log_server, Unknown cast message: ~p~n",
  err_unknown_log_server_info_msg => "log_server, Unknown info message: ~p~n",
  err_log_server_abnormal_termination => "log_server, Abnormal termination, reason: ~p~n",
  host_name => "Host name: ~p~n",
  linkblox_startup_complete => "LinkBlox startup complete~n",
  err_starting_linkblox => "Error: ~p starting LinkBlox~n"

}.
 