%%% @doc
%%% 
%%% SSH Command Line Interface for LinkBlox app.
%%%
%%% @end

-module(ui_ssh_cli).

-author("Mark Sebald").

-include("../block_state.hrl"). 


%% ====================================================================
%% UI functions
%% ====================================================================
-export([start/2, start/3]).

% Functions must be exported in order to be invoked via Module:Function(Args)
-export([
          ui_create_block/1,    ui_create_block_help/0,
          ui_copy_block/1,      ui_copy_block_help/0,
          ui_rename_block/1,    ui_rename_block_help/0,
          ui_execute_block/1,   ui_execute_block_help/0,
          ui_delete_block/1,    ui_delete_block_help/0,
          ui_disable_block/1,   ui_disable_block_help/0,
          ui_enable_block/1,    ui_enable_block_help/0,
          ui_freeze_block/1,    ui_freeze_block_help/0,
          ui_thaw_block/1,      ui_thaw_block_help/0,
          ui_get_values/1,      ui_get_values_help/0,
          ui_set_value/1,       ui_set_value_help/0,
          ui_link_blocks/1,     ui_link_blocks_help/0,
          ui_unlink_blocks/1,   ui_unlink_blocks_help/0,
          ui_status/1,          ui_status_help/0,
          ui_valid_block_name/1, ui_valid_block_name_help/0,
          ui_load_blocks/1,     ui_load_blocks_help/0,
          ui_save_blocks/1,     ui_save_blocks_help/0,
          ui_node/1,            ui_node_help/0,
          ui_nodes/1,           ui_nodes_help/0,
          ui_connect/1,         ui_connect_help/0,
          ui_hosts/1,           ui_hosts_help/0,
          ui_exit/1,            ui_exit_help/0,
          ui_help/1,            ui_help_help/0
        ]).


%%
%% Start the SSH daemon
%%
start(Port, LangMod) ->
    start(Port, LangMod, []).

start(Port, LangMod, Options) ->
    log_server:info(starting_SSH_CLI_user_interface_on_port_language_module, [Port, LangMod]),
    crypto:start(),
    ssh:start(),
    ssh:daemon(any, Port, [{shell, fun(U, H) -> start_shell(U, H, LangMod) end} | Options]).

%% 
%% Start user input loop
%%
start_shell(User, Peer, LangMod) ->
  spawn(fun() ->
    cmds_map(LangMod),
    strings_map(LangMod),
	  io:setopts([{expand_fun, fun(Bef) -> expand(Bef) end}]),
    format_out(welcome_str),
		format_out(enter_command_str),
		put(user, User),
		put(peer_name, Peer),
    % default current node to local node
    curr_node(node()),
		shell_loop()
	end).


%% 
%% Get and Evaluate user input loop
%%
shell_loop() ->
  % use the current node for the UI prompt
  Prompt = atom_to_list(curr_node()) ++ "> ",
  % read user input
  Line = ui_utils:get_input(Prompt),

  Result = eval_input(Line),
  case Result of
	  done -> 
	    exit(normal);
	  _ -> 
      % Put and extra blank line after each command
      io:format("~n"),
	    shell_loop()
    end.


%% 
%% Evaluate user input
%%
eval_input(Line) ->
  case ui_utils:parse_cli_params(Line) of
    {ok, []} -> [];
    {ok, [Command | Params]} ->
	    case cmd_string_to_cmd_atom(Command) of
        unknown_cmd ->
          format_out(unk_cmd_str, [Command]);
        CmdAtom ->
          case cmd_atom_to_function(CmdAtom) of
            unknown_cmd ->
              format_out(unk_cmd_atom, [CmdAtom]);

            {Module, Function} ->
		          case catch Module:Function(Params) of
			          {'EXIT', Error} ->
			            {error, Error}; % wrong_number_of_arguments
			          Result ->
			            Result
		          end
          end
	    end;
    {error, _Params} ->
      format_out(err_parsing_cmd_line, [Line])
  end.


%% 
%% Translate a command string to an atom
%% This indirection allows command mapping using different languages,
%%
cmd_string_to_cmd_atom(Command) ->
  case lists:keysearch(Command, 1, cmds_map()) of
	  {value, {_, CmdAtom, _}} -> CmdAtom;
	  false -> unknown_cmd
  end.


%%
%% Translate command atom to command module and function
%%
cmd_atom_to_function(CmdAtom) ->
  case lists:keysearch(CmdAtom, 1, cmd_atom_map()) of
	  {value, {_, Module, Function, _HelpFunction}} -> {Module, Function};
	  false -> unknown_cmd
  end.

%%
%% Translate command atom to command module and help function
%%
cmd_atom_to_help_function(CmdAtom) ->
  case lists:keysearch(CmdAtom, 1, cmd_atom_map()) of
	  {value, {_, Module, _Function, HelpFunction}} -> {Module, HelpFunction};
	  false -> unknown_cmd
  end.


%%
%% Map command atom to command module and function
%% Specify the Module here, in case we want to break out
%%  UI functions into separate modules.
%%
cmd_atom_map() ->  
  [
    {cmd_create_block,     ?MODULE,  ui_create_block,   ui_create_block_help},
    {cmd_copy_block,       ?MODULE,  ui_copy_block,     ui_copy_block_help},
    {cmd_rename_block,     ?MODULE,  ui_rename_block,   ui_rename_block_help},
    {cmd_execute_block,    ?MODULE,  ui_execute_block,  ui_execute_block_help},
    {cmd_delete_block,     ?MODULE,  ui_delete_block,   ui_delete_block_help},
    {cmd_disable_block,    ?MODULE,  ui_disable_block,  ui_disable_block_help},
    {cmd_enable_block,     ?MODULE,  ui_enable_block,   ui_enable_block_help},
    {cmd_freeze_block,     ?MODULE,  ui_freeze_block,   ui_freeze_block_help},
    {cmd_thaw_block,       ?MODULE,  ui_thaw_block,     ui_thaw_block_help},
    {cmd_get_values,       ?MODULE,  ui_get_values,     ui_get_values_help},
    {cmd_set_value,        ?MODULE,  ui_set_value,      ui_set_value_help},
    {cmd_link_blocks,      ?MODULE,  ui_link_blocks,    ui_link_blocks_help},
    {cmd_unlink_blocks,    ?MODULE,  ui_unlink_blocks,  ui_unlink_blocks_help},
    {cmd_status,           ?MODULE,  ui_status,         ui_status_help},
    {cmd_valid_block_name, ?MODULE,  ui_valid_block_name, ui_valid_block_name_help},
    {cmd_load_blocks,      ?MODULE,  ui_load_blocks,    ui_load_blocks_help},
    {cmd_save_blocks,      ?MODULE,  ui_save_blocks,    ui_save_blocks_help},
    {cmd_node,             ?MODULE,  ui_node,           ui_node_help},
    {cmd_nodes,            ?MODULE,  ui_nodes,          ui_nodes_help},
    {cmd_connect,          ?MODULE,  ui_connect,        ui_connect_help},
    {cmd_hosts,            ?MODULE,  ui_hosts,          ui_hosts_help},
    {cmd_exit,             ?MODULE,  ui_exit,           ui_exit_help},
    {cmd_help,             ?MODULE,  ui_help,           ui_help_help}
  ].


%%
%% Expand function (called when the user presses TAB)
%% input: a reversed list with the row to left of the cursor
%% output: {yes|no, Expansion, ListofPossibleMatches}
%% where the atom no yields a beep
%% Expansion is a string inserted at the cursor
%% List... is a list that will be printed
%% Beep on prefixes that don't match and when the command filled in
%%
expand([$  | _]) ->
  {no, "", []};
expand(RevBefore) ->
  Before = lists:reverse(RevBefore),
    case longest_prefix(cmds_map(), Before) of
	    {prefix, P, [_]} -> {yes, P ++ " ", []};
	    {prefix, "", M}  -> {yes, "", M};
	    {prefix, P, _M}  -> {yes, P, []};
	    {none, _M}       -> {no, "", []}
    end.

%% longest prefix in a list, given a prefix
longest_prefix(List, Prefix) ->
  case [A || {A, _, _} <- List, lists:prefix(Prefix, A)] of
	  [] ->
	    {none, List};
	  [S | Rest] ->
	    NewPrefix0 =
		    lists:foldl(fun(A, P) ->
				              common_prefix(A, P, [])
			              end, S, Rest),
	    NewPrefix = nthtail(length(Prefix), NewPrefix0),
	    {prefix, NewPrefix, [S | Rest]}
    end.			


common_prefix([C | R1], [C | R2], Acc) ->
  common_prefix(R1, R2, [C | Acc]);
common_prefix(_, _, Acc) ->
  lists:reverse(Acc).

%% 
%% same as lists:nthtail(), but no badarg if n > the length of list
%%
nthtail(0, A)       -> A;
nthtail(N, [_ | A]) -> nthtail(N-1, A);
nthtail(_, _)       -> [].


%%
%% Get and set the current node this UI is connected to
%%
% TODO: Check if node exists before sending cmd to the node
curr_node() ->
  get(curr_node).

curr_node(Node) ->
  put(curr_node, Node).


%
% Display Strings
%

format_out(StringId) ->
  io:format(get_string(StringId)).

format_out(StringId, Args) ->
  io:format(get_string(StringId), Args).


%
% Get the string corresponding to the string ID
%
-spec get_string(StringId :: atom()) -> string().

get_string(StringId) ->
  case maps:get(StringId, strings_map()) of
    {badmap, StringsMap} ->  io:lib("Error: bad strings map: ~p~n", [StringsMap]);
    {badkey, StringId} -> io:lib("Error, string: ~p not found~n", [StringId]);
    String -> String
  end.


%
% Store the strings map and commands map in the process dictionary
%
strings_map(LangMod) ->
  put(mod_strings_map, LangMod:strings_map()).

strings_map() ->
  get(mod_strings_map).

cmds_map(LangMod) ->
  put(mod_cmds_map, LangMod:cmds_map()).

cmds_map() ->
  get(mod_cmds_map).


% Process block create command
% TODO: Add initial attrib values
ui_create_block(Params) ->
  case check_num_params(Params, 2, 3) of  
    low -> io:format("Enter block-type new-block-name <description>~n");

    ok ->
      case length(Params) of
        2 ->
          [BlockTypeStr, BlockNameStr] = Params,
          Description = "Description";
        3 ->
          [BlockTypeStr, BlockNameStr, DescriptionStr] = Params,
          Description = ui_utils:parse_value(DescriptionStr)
      end,
    
      BlockType = list_to_atom(BlockTypeStr),
      BlockName = list_to_atom(BlockNameStr),
      case linkblox_api:create_block(curr_node(), BlockType, BlockName, Description) of
        ok ->
          format_out(block_type_created, [BlockTypeStr, BlockNameStr]);

        {error, invalid_block_type} ->
          format_out(err_invalid_block_type, [BlockTypeStr]);

        {error, block_exists} ->
          format_out(err_block_already_exists, [BlockNameStr]);
        
        {error, Reason} -> 
          format_out(err_creating_block_type, [Reason, BlockTypeStr, BlockNameStr])
      end;

    high -> format_out(err_too_many_params)
  end.    


% Process block copy command
% TODO: Add initial attrib values
ui_copy_block(Params) ->
  case check_num_params(Params, 2, 3) of  
    low -> io:format("Enter source-block-name <dest-node-name> dest-block-name~n");

    ok ->
      case length(Params) of
        2 ->
          [SrcBlockNameStr, DstBlockNameStr] = Params,
          DstNode = curr_node();
        3 ->
          [SrcBlockNameStr, DstNodeStr, DstBlockNameStr] = Params,
          DstNode = list_to_atom(DstNodeStr)
      end,

      SrcBlockName = list_to_atom(SrcBlockNameStr),
      % Always get source block values from the current node
      case linkblox_api:get_block(curr_node(), SrcBlockName) of
        {ok, SrcBlockValues} ->
          DstBlockName = list_to_atom(DstBlockNameStr),
          case linkblox_api:copy_block(DstNode, DstBlockName, SrcBlockValues, []) of
            ok ->
              format_out(dest_block_created, [DstBlockNameStr]);

            {error, block_exists} ->
              format_out(err_dest_block_already_exists, [DstBlockNameStr]);
      
            {error, Reason} -> 
              format_out(err_creating_block,  [Reason, DstBlockNameStr])
          end;

        {error, block_not_found} ->
          format_out(err_source_block_does_not_exist, [SrcBlockNameStr])
      end;

    high -> format_out(err_too_many_params)
  end.    


% Process rename block command
% TODO: Just copied from copy block command still needs to be finished
ui_rename_block(Params) ->
  case check_num_params(Params, 2) of  
    low -> io:format("Enter current-block-name new-block-name~n");

    ok ->
      [SrcBlockNameStr, DstBlockNameStr] = Params,

      SrcBlockName = list_to_atom(SrcBlockNameStr),
      % Always get source block values from the current node
      case linkblox_api:get_block(curr_node(), SrcBlockName) of
        {ok, SrcBlockValues} ->
          DstBlockName = list_to_atom(DstBlockNameStr),
          case linkblox_api:copy_block(curr_node(), DstBlockName, SrcBlockValues, []) of
            ok ->
              format_out(dest_block_created, [DstBlockNameStr]);

            {error, block_exists} ->
              format_out(err_dest_block_already_exists, [DstBlockNameStr]);
      
            {error, Reason} -> 
              format_out(err_creating_block, [Reason, DstBlockNameStr])
          end;

        {error, block_not_found} ->
          format_out(err_source_block_does_not_exist, [SrcBlockNameStr])
      end;

    high -> format_out(err_too_many_params)
  end.    


% Process manual block execute command
ui_execute_block(Params) ->
   case check_num_params(Params, 1) of  
    low -> format_out(enter_block_name);

    ok -> 
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      case linkblox_api:execute_block(curr_node(), BlockName, manual) of
        ok -> 
          ok;
        {error, block_not_found} ->
          format_out(err_block_not_found, [BlockNameStr])  
      end;

    high -> format_out(err_too_many_params)
  end.


% Process block delete command
ui_delete_block(Params) ->
  % TODO: Add delete all command to delete all blocks at once
  % TODO: Ask the user if they really want to delete
   case check_num_params(Params, 1) of  
    low -> format_out(enter_block_name);

    ok -> 
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),  
      case linkblox_api:delete_block(curr_node(), BlockName) of
        ok -> 
          format_out(block_deleted, [BlockNameStr]);

        {error, block_not_found} ->
          format_out(err_block_not_found,  [BlockNameStr]);
   
        {error, Reason} ->
          format_out(err_deleting_block, [Reason, BlockNameStr]) 
      end;

    high -> format_out(err_too_many_params)
  end.
    
    
% Process disable block command
ui_disable_block(Params) ->
  case check_num_params(Params, 1) of

    low -> format_out(enter_block_name);
    
    ok -> 
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      case linkblox_api:set_value(curr_node(), BlockName, disable, true) of
        ok ->
          format_out(block_disabled, [BlockNameStr]);

        {error, block_not_found} ->
          format_out(err_block_not_found,  [BlockNameStr]);

        {error, Reason} ->
          format_out(err_disabling_block,  [Reason, BlockNameStr]) 
      end;
 
    high -> format_out(err_too_many_params)
  end.


% Process enable block command
ui_enable_block(Params) ->
 case check_num_params(Params, 1) of

    low -> format_out(enter_block_name);
    
    ok -> 
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      case linkblox_api:set_value(curr_node(), BlockName, disable, false) of
        ok ->
          format_out(block_enabled, [BlockNameStr]);

        {error, block_not_found} ->
          format_out(err_block_not_found,  [BlockNameStr]);

        {error, Reason} ->
          format_out(err_enabling_block,  [Reason, BlockNameStr]) 
      end;
 
    high -> format_out(err_too_many_params)
  end.
    
    
% Process freeze block command
ui_freeze_block(Params) ->
case check_num_params(Params, 1) of

    low -> format_out(enter_block_name);
    
    ok -> 
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      case linkblox_api:set_value(curr_node(), BlockName, freeze, true) of
        ok ->
          format_out(block_frozen, [BlockNameStr]);

        {error, block_not_found} ->
          format_out(err_block_not_found,  [BlockNameStr]);

        {error, Reason} ->
          format_out(err_freezing_block,  [Reason, BlockNameStr]) 
      end;
 
    high -> format_out(err_too_many_params)
  end.

    
% Process thaw block command
ui_thaw_block(Params) ->
case check_num_params(Params, 1) of

    low -> format_out(enter_block_name);
    
    ok -> 
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      case linkblox_api:set_value(curr_node(), BlockName, freeze, false) of
        ok ->
          format_out(block_thawed, [BlockNameStr]);

        {error, block_not_found} ->
          format_out(err_block_not_found,  [BlockNameStr]);

        {error, Reason} ->
          format_out(err_thawing_block,  [Reason, BlockNameStr]) 
      end;
 
    high -> format_out(err_too_many_params)
  end.


% Process block status command
ui_status(Params) ->
  case check_num_params(Params, 0) of  
    ok -> block_status();

    high -> format_out(err_too_many_params)
  end.
 
 
% Process the get values command
ui_get_values(Params) ->
  case length(Params) of  
    0 -> io:format("Enter block-name <value-name>~n");
    1 -> 
      [BlockNameStr] = Params,

      case BlockNameStr of
        "blocks" ->  % Just get the list of block names
          BlockNames = linkblox_api:get_block_names(curr_node()),
          io:format("~n"),
          lists:map(fun(BlockName) -> io:format("  ~p~n", [BlockName]) end, BlockNames);

        "types" ->  % Just get the list of block types information
          TypesInfo = linkblox_api:get_types_info(curr_node()),
          % Print the list of type names version
          io:fwrite("~n~-16s ~-8s ~-60s~n", 
                      ["Block Type", "Version", "Description"]),
          io:fwrite("~16c ~8c ~60c~n", [$-, $-, $-] ), 
   
          lists:map(fun({TypeName, Version, Description}) -> 
                    io:fwrite("~-16s ~-8s ~-60s~n", 
                              [string:left(TypeName, 16), 
                               string:left(Version, 8),
                               string:left(Description, 60)]) end, 
                              TypesInfo),
          TypesInfo = linkblox_api:get_types_info(curr_node());

       _ ->
          BlockName = list_to_atom(BlockNameStr),
          case linkblox_api:get_block(curr_node(), BlockName) of
            {ok, BlockState} -> 
              format_block_values(BlockState);
            {error, block_not_found} -> 
              format_out(err_block_not_found, [BlockNameStr]);
            Unknown ->
              format_out(err_unk_result_from_linkblox_api_get_block, [Unknown])
          end
        end;

    2 -> 
      case Params of
        [BlockNameStr, "raw"] ->
          % Display the unformated tuple and list values
          BlockName = list_to_atom(BlockNameStr),
          case linkblox_api:get_block(curr_node(), BlockName) of
            {ok, BlockState} -> 
              io:format("~n~p~n", [BlockState]);
            {error, block_not_found} -> 
              format_out(err_block_not_found, [BlockNameStr]);
            Unknown ->
              format_out(err_unk_result_from_linkblox_api_get_block, [Unknown])
          end;

        [BlockNameStr, ValueIdStr] ->
          BlockName = list_to_atom(BlockNameStr),
          case attrib_utils:str_to_value_id(ValueIdStr) of
            {ok, ValueId} ->
              case linkblox_api:get_value(curr_node(), BlockName, ValueId) of
                {ok, CurrentValue} ->
                  io:format("~n   ~p~n", [CurrentValue]);
              
                {error, block_not_found} ->
                  format_out(err_block_not_found, [BlockNameStr]);

                {error, value_not_found} ->
                  format_out(err_invalid_value_id, [ValueIdStr, BlockNameStr]);
                {error, Reason} ->
                  format_out(err_retrieving_value, [Reason, BlockNameStr, ValueIdStr]);

                Unknown ->
                  format_out(err_unk_result_from_linkblox_api_get_value, [Unknown]) 
              end;
            {error, invalid} ->
              format_out(err_invalid_value_id_str, [ValueIdStr])
          end
      end;
    _ -> format_out(err_too_many_params)
  end.    


% Display the block values in a readable format
format_block_values(BlockState) ->
  FormatAttribute = fun(BlockValue) -> format_attribute(BlockValue) end,

  case BlockState of
    {Config, Inputs, Outputs} ->
      format_out(config_str),
      lists:map(FormatAttribute, Config),

      format_out(inputs_str),
      lists:map(FormatAttribute, Inputs),

      format_out(outputs_str),
      lists:map(FormatAttribute, Outputs);

    _ ->
      format_out(inv_block_values)
  end.


% format an attribute name and value into a displayable string
format_attribute(BlockValue) ->
  case BlockValue of
    {ValueId, {Value}} ->
      format_value_id_value(ValueId, Value),
      format_newline();

    {ValueId, {Value, LinkOrRefs}} ->
      case LinkOrRefs of
        {} ->
          format_value_id_value(ValueId, Value),
          format_newline();

        [] ->
          format_value_id_value(ValueId, Value),
          format_newline();

        {OutAttribName} ->
          format_value_id_value(ValueId, Value),
          format_out(self_link_str, [OutAttribName]),
          format_newline();

        {BlockName, OutAttribName} ->
          format_value_id_value(ValueId, Value),
          format_out(block_link_str, [BlockName, OutAttribName]),
          format_newline();

        {NodeName, BlockName, OutAttribName} ->
          format_value_id_value(ValueId, Value),
          format_out(node_link_str, [NodeName, BlockName, OutAttribName]),
          format_newline();

        References ->
          format_value_id_value(ValueId, Value),
          format_out(reference_str, [References]),
          format_newline()
      end;

    {ValueName, ArrayValues} ->
      format_array_values(ValueName, 1, ArrayValues)
  end.


% Format one Value ID and value
format_value_id_value(ValueId, Value) ->
  case ValueId of
    last_exec ->
      io:format("  ~p:  ~s", [ValueId, format_last_exec(Value)]);
    _ ->
      io:format("  ~p:  ~p", [ValueId, Value])
  end.


% Format a value by itself
format_value(Value) ->
  io:format("  ~p", [Value]).


% Format new line
format_newline() ->
  io:format("~n").


% Format last_exec value
format_last_exec(Value) ->
  case Value of 
    null ->
      "null";
    {Hour, Minute, Second, Micro} ->
      io_lib:format("~2w:~2..0w:~2..0w.~6..0w", 
                    [Hour,Minute,Second,Micro]);
    _ ->
      "undef last_exec val"
  end.  


% Format array values
format_array_values(_ValueName, _Index, []) ->
  ok;

format_array_values(ValueName, Index, ArrayValues) ->
  io:format("  ~p[~w]:", [ValueName, Index]),
  [ArrayValue | RemainingArrayValues] = ArrayValues,

  case ArrayValue of
    {Value} ->
      format_value(Value),
      format_newline();

    {Value, LinkOrRefs}->
      case LinkOrRefs of
        {} ->
          format_value(Value),
          format_newline();

        [] ->
          format_value(Value),
          format_newline();

        {OutAttribName} ->
          format_value(Value),
          format_out(self_link_str, [OutAttribName]),
          format_newline();

        {BlockName, OutAttribName} ->
          format_value(Value),
          format_out(block_link_str, [BlockName, OutAttribName]),
          format_newline();

        {NodeName, BlockName, OutAttribName} ->
          format_value(Value),
          format_out(node_link_str, [NodeName, BlockName, OutAttribName]),
          format_newline();

        References ->
          format_value(Value),
          format_out(reference_str, [References]),
          format_newline()
      end
  end,
  format_array_values(ValueName, (Index + 1), RemainingArrayValues). 


% Process the set value command
ui_set_value(Params) ->
  case check_num_params(Params, 3) of

    low -> io:format("Enter block-name value-name value~n");
    
    ok -> 
      [BlockNameStr, ValueIdStr, ValueStr] = Params,

      BlockName = list_to_atom(BlockNameStr),
      case attrib_utils:str_to_value_id(ValueIdStr) of
        {ok, ValueId} ->
          Value = ui_utils:parse_value(ValueStr),
          case linkblox_api:set_value(curr_node(), BlockName, ValueId, Value) of
            ok ->
              format_out(block_value_set_to_str, [BlockNameStr, ValueIdStr, ValueStr]);

            {error, block_not_found} ->
              format_out(err_block_not_found,  [BlockNameStr]);

            {error, not_found} ->
              format_out(err_block_value_not_found,  [ValueIdStr]);

            {error, Reason} ->
              format_out(err_setting_block_value, [Reason, BlockNameStr, ValueIdStr, ValueStr]) 
          end;
            
        {error, invalid} ->
          format_out(err_invalid_value_id_str, [ValueIdStr])
      end;

    high -> format_out(err_too_many_params)
  end.    


% Process link blocks command
ui_link_blocks(Params) ->
  case check_num_params(Params, 3, 5) of
    low -> io:format("Enter input-block-name input-value-name <output-node-name <output-block-name>> output-value-name~n");

    ok ->
      % 1st parameter is block name
      InputBlockNameStr = lists:nth(1, Params),
      InputBlockName = list_to_atom(InputBlockNameStr),

      % 2nd parameter is the input value ID
      InputValueIdStr = lists:nth(2, Params),
      case attrib_utils:str_to_value_id(InputValueIdStr) of
        {ok, InputValueId} ->

          % The remaining params form the Link
          LinkParams = lists:nthtail(2, Params),
          case parse_link(LinkParams) of
            {ok, Link} ->
              case linkblox_api:set_link(curr_node(), InputBlockName, InputValueId, Link) of
                {error, Reason} ->
                  format_out(err_linking_input_to_output, 
                          [Reason, InputBlockNameStr, InputValueIdStr, Link]);
                ok ->
                  format_out(ui_block_input_linked_to_block_output, 
                              [InputBlockNameStr, InputValueIdStr, Link])
              end;
            {error, Reason} ->
              format_out(err_converting_to_link, [Reason, LinkParams])
          end;
        {error, Reason} ->
          format_out(err_converting_to_input_value_id, [Reason, InputValueIdStr])
      end;

    high -> format_out(err_too_many_params)
  end.


% Process unlink blocks command
ui_unlink_blocks(Params) ->
  case check_num_params(Params, 2) of
    low -> io:format("Enter block-name input-value-name~n");

    ok ->
      % 1st parameter is block name
      InputBlockNameStr = lists:nth(1, Params),
      InputBlockName = list_to_atom(InputBlockNameStr),

      % 2nd parameter is the input value ID
      InputValueIdStr = lists:nth(2, Params),
      case attrib_utils:str_to_value_id(InputValueIdStr) of
        {ok, InputValueId} ->
          % Set the input value link to an empty link: {}
          case linkblox_api:set_link(curr_node(), InputBlockName, InputValueId, {}) of
            {error, Reason} ->
              format_out(err_unlinking_input, [Reason, InputBlockNameStr, InputValueIdStr]);
              ok ->
                format_out(block_input_unlinked, [InputBlockNameStr, InputValueIdStr])
          end;
      
        {error, Reason} ->
          format_out(err_converting_to_input_value_id, [Reason, InputValueIdStr])
      end;

    high -> format_out(err_too_many_params)
  end.


% Parse the params into a Link tuple
parse_link(LinkParams) ->

  % Output Value ID is always the last parameter
  % So work backward to construct the link
  [OutputValueIdStr | OutputBlockNameAndNodeNameList] = lists:reverse(LinkParams),
  case attrib_utils:str_to_value_id(OutputValueIdStr) of
    {ok, OutputValueId} ->
      if 0 < length(OutputBlockNameAndNodeNameList) ->
        [OutputBlockNameStr | NodeNameList] = OutputBlockNameAndNodeNameList,
        OutputBlockName = list_to_atom(OutputBlockNameStr),
        if 0 < length(NodeNameList) ->
          [NodeNameStr] = NodeNameList,
          NodeName = list_to_atom(NodeNameStr),

          {ok, {NodeName, OutputBlockName, OutputValueId}};

        true ->
          {ok, {OutputBlockName, OutputValueId}}
        end;

      true ->
        {ok, {OutputValueId}}
      end;
   {error, Reason} -> {error, Reason}
  end.


% Process the load blocks command
ui_load_blocks(Params) ->
  %% Read a set of block values from a config file
  % TODO: Check for existence and validity
  case check_num_params(Params, 0, 1) of
    ok ->
      case length(Params) of  
        0 -> 
          format_out(enter_config_file_name),
          case ui_utils:get_input("") of
                  [] -> FileName = "LinkBloxConfig";
            FileName -> FileName
          end;
        1 ->
          % Use the entered parameter as the file name
          FileName = Params
      end,
      
      case linkblox_api:load_block_file(curr_node(), FileName) of
        ok ->
          format_out(block_config_file_loaded, [FileName]);

        {error, Reason} ->
          format_out(err_loading_block_config_file, [Reason, FileName])
      end;

    high -> format_out(err_too_many_params)
  end.

% TODO: Create command to get block data from one node and load it on another node



% Process the save blocks command
ui_save_blocks(Params) ->
  % Write the block values to a configuration file
  case check_num_params(Params, 0, 1) of
    ok ->
      case length(Params) of  
        0 -> 
          format_out(enter_config_file_name),
          case ui_utils:get_input("") of
                  [] -> FileName = "LinkBloxConfig";
            FileName -> FileName
          end;
        1 ->
          % Use the entered parameter as the file name
          FileName = Params
      end,

      format_out(config_file_overwrite_warning, [FileName]),
      case ui_utils:get_yes() of
        true ->
          case linkblox_api:save_blocks(curr_node(), FileName) of
            ok ->
              format_out(block_config_file_saved, [FileName]);

            {error, Reason} ->
              format_out(err_saving_block_config_file, [Reason, FileName])
          end;
        _ -> ok
      end;
    high -> format_out(err_too_many_params)
  end.


% Execute Erlang BIF node()
ui_node(_Params) ->
  format_out(node_prompt_str, [node()]).


% Execute Erlang BIF nodes()
ui_nodes(_Params) ->  
  format_out(nodes_prompt_str, [[node() | nodes()]]).


% Connect to another node
ui_connect(Params) ->
  case check_num_params(Params, 1) of
    low ->
      format_out(enter_node_name);

    ok ->
      case Params of
        ["local"] ->  % Just connect to the local node
          format_out(connecting_to_local_node),
          connect_to_node(node());

        [NodeStr] ->   
          Node = list_to_atom(NodeStr),
          connect_to_node(Node)
      end;
    high ->
      format_out(err_too_many_params),
      error
  end.

% Attempt to connect to the given node
connect_to_node(Node) ->
  case net_kernel:connect_node(Node) of
    true -> 
      curr_node(Node),
      format_out(connected_to_node, [Node]);

    _ ->
      format_out(unable_to_connect_to_node, [Node])
  end.


%%
%% Display status off each running block
%%
block_status() ->
  io:fwrite("~n~-16s ~-16s ~-12s ~-12s ~-12s ~-15s~n", 
            ["Block Type", "Block Name", "Output", "Status", "Exec Method", "Last Exec"]),
  io:fwrite("~16c ~16c ~12c ~12c ~12c ~15c~n", [$-, $-, $-, $-, $-, $-] ), 
  block_status(linkblox_api:get_block_names(curr_node())).
    
block_status([]) ->
  io:format("~n"), 
  ok;

block_status([BlockName | RemainingBlockNames]) ->
  {BlockTypeStr, _Version, _Description} = 
        linkblox_api:get_type_info(curr_node(), BlockName),

  % TODO: Create get_values() API call, get multiple values in one call 
  {ok, Value} = linkblox_api:get_value(curr_node(), BlockName, value),
  {ok, Status} = linkblox_api:get_value(curr_node(), BlockName, status),
  {ok, ExecMethod} = linkblox_api:get_value(curr_node(), BlockName, exec_method),
  
  case linkblox_api:get_value(curr_node(), BlockName, last_exec) of 
    {ok, null} ->
      LastExecuted = "null";
    {ok, {Hour, Minute, Second, Micro}} ->
      LastExecuted = io_lib:format("~2w:~2..0w:~2..0w.~6..0w", 
                                    [Hour,Minute,Second,Micro]);
    _ ->
      LastExecuted = "undef last_exec val"
  end,  
    
  io:fwrite("~-16s ~-16s ~-12s ~-12w ~-12w ~-15s~n", 
            [string:left(BlockTypeStr, 16), 
             string:left(atom_to_list(BlockName), 16), 
             string:left(io_lib:format("~w",[Value]), 12), 
             Status, ExecMethod, LastExecuted]),
  block_status(RemainingBlockNames).


% validate Params is one valid block name
ui_valid_block_name(Params) ->
  case check_num_params(Params, 1) of
    low ->
      format_out(enter_block_name),
      error;

    ok ->
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      % check if block name is an existing block
      case linkblox_api:is_block_name(curr_node(), BlockName) of
        true  -> 
          format_out(block_exists, [BlockNameStr]);
        false ->
          format_out(block_does_not_exist, [BlockNameStr])
      end;
 
    high ->
      format_out(err_too_many_params),
      error
  end.


%%
%% Print out the /etc/hosts file
%%
ui_hosts(_Params) ->
    {ok, Device} = file:open("/etc/hosts", [read]),
    try get_all_lines(Device)
      after file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line ->
          io:format("~s", [Line]),
          get_all_lines(Device)
    end.

%%
%% Exit UI
%%
ui_exit(_Params) ->
  done.

%%
%% Process the help command
%%
ui_help(Params) ->
  case check_num_params(Params, 0, 1) of
    ok ->
      
      CmdList = cmds_map(),
      case length(Params) of  
        0 ->
          io:format("~n     LinkBlox Help~n~n"),
          lists:map(fun(Cmd) -> 
                      {CmdStr, _CmdAtom, CmdParamStr} = Cmd,
                      io:format("~s ~s~n", [CmdStr, CmdParamStr])
                    end,
                    CmdList);
        1 ->
          % Use the entered parameter as the command Name
          [CmdHelp] = Params,
          CmdAtom = cmd_string_to_cmd_atom(CmdHelp),
          case cmd_atom_to_help_function(CmdAtom) of
            unknown_cmd ->
              io:format("No help for: ~s~n", [CmdHelp]);

            {Module, HelpFunction} -> 
              Module:HelpFunction()
          end
      end;
    high ->
      format_out(err_too_many_params),
      error
  end.

ui_create_block_help() ->
  io:format("~nCreate a new block with default values~n").

ui_copy_block_help() ->
  io:format("TODO: Insert copy help text here~n").
  
ui_rename_block_help() ->
  io:format("TODO: Insert rename help text here~n").
  
ui_execute_block_help() ->
  io:format("TODO: Insert execute help text here~n").
  
ui_delete_block_help() ->
  io:format("TODO: Insert delete help text here~n").
  
ui_disable_block_help() ->
  io:format("TODO: Insert disable help text here~n").
  
ui_enable_block_help() ->
  io:format("TODO: Insert enable help text here~n").
  
ui_freeze_block_help() ->
  io:format("TODO: Insert freeze help text here~n").
  
ui_thaw_block_help() ->
  io:format("TODO: Insert thaw help text here~n").
  
ui_get_values_help() ->
  io:format("TODO: Insert get help text here~n").
  
ui_set_value_help() ->
  io:format("TODO: Insert set help text here~n").
  
ui_link_blocks_help() ->
  io:format("TODO: Insert link help text here~n").
  
ui_unlink_blocks_help() ->
  io:format("TODO: Insert unlink help text here~n").
  
ui_status_help() ->
  io:format("TODO: Insert status help text here~n").

ui_valid_block_name_help() ->
  io:format("TODO: Insert validate block name help text here~n").
  
ui_load_blocks_help() ->
  io:format("TODO: Insert load help text here~n").
  
ui_save_blocks_help() ->
  io:format("TODO: Insert save help text here~n").
  
ui_node_help() ->
  io:format("TODO: Insert node help text here~n").
  
ui_nodes_help() ->
  io:format("TODO: Insert nodes help text here~n").
  
ui_connect_help() ->
  io:format("TODO: Insert connect help text here~n").

ui_hosts_help() ->
  io:format("Display the contents of the /etc/hosts file~n").

ui_exit_help() ->
   io:format("Exit the UI~n").

ui_help_help() ->
  io:format("Display the contents of the help screen~n").
  
  
%%
%% Check the number of parameters in the param list
%%
-spec check_num_params(Params :: list(string()),
                       Exact :: non_neg_integer()) -> low | ok | high.

check_num_params(Params, Exact) ->
  check_num_params(Params, Exact, Exact).

-spec check_num_params(Params :: list(string()),
                       Low :: non_neg_integer(),
                       High :: non_neg_integer()) -> low | ok | high.

check_num_params(Params, Low, High) ->
  NumParams = length(Params),
  if (NumParams < Low) -> low;
    true ->
      if (NumParams > High) -> high;
        true ->
          ok
      end
  end.

  
%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


% ====================================================================

-endif.