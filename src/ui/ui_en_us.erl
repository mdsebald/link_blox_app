%%% @doc
%%% 
%%% English US  
%%  UI Strings 
%%% Map input strings to UI commands
%%%
%%% @end

-module(ui_en_us).

-author("Mark Sebald").

-export([
          cmd_string_map/0
        ]).


%%
%% map string to ui command atom
%%
cmd_string_map() ->
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
    {"types",     cmd_block_types,      ""},
    {"valid",     cmd_valid_block_name, "<block name>"},
    {"load",      cmd_load_blocks,      "<file name> | blank"},
    {"save",      cmd_save_blocks,      "<file name> | blank"},
    {"node",      cmd_node,             ""},
    {"nodes",     cmd_nodes,            ""},
    {"connect",   cmd_connect,          "<node name>"},
    {"hosts",     cmd_hosts,            ""},
    {"help",      cmd_help,             "display help screen"}
  ].


  