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
-export([start/1]).

% Functions must be exported in order to be invoked via Module:Function(Args)
-export([
          ui_create_block/2,
          ui_copy_block/2,
          ui_rename_block/2,
          ui_execute_block/2,
          ui_delete_block/2,
          ui_disable_block/2,
          ui_enable_block/2,
          ui_freeze_block/2,
          ui_thaw_block/2,
          ui_get_values/2,
          ui_set_value/2,
          ui_link_blocks/2,
          ui_unlink_blocks/2,
          ui_xlink_blocks/2,
          ui_xunlink_blocks/2,
          ui_status/2,
          ui_valid_block_name/2,
          ui_load_blocks/2,
          ui_save_blocks/2,
          ui_node/2,
          ui_nodes/2,
          ui_connect/2,
          ui_hosts/2,
          ui_exit/2,
          ui_help/2
        ]).


%%
%% Start the SSH daemon
%%
start(Options) ->
    SshPort = ui_utils:get_ssh_port(),
    logger:info(starting_SSH_CLI_user_interface_on_port, [SshPort]),
    crypto:start(),
    ssh:start(),
    ssh:daemon(any, SshPort, [{shell, fun(U, H) -> start_shell(U, H) end} | Options]).

%% 
%% Start user input loop
%%
start_shell(User, Peer) ->
  spawn(fun() ->
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
        {CmdAtom, ParamStrAtom, _HelpStrAtom} ->
          case cmd_atom_to_function(CmdAtom) of
            unknown_cmd ->
              format_out(unk_cmd_atom, [CmdAtom]);

            {Module, Function} ->
		          case catch Module:Function(Params, ParamStrAtom) of
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
%% This indirection allows command strings to be in different languages,
%%
cmd_string_to_cmd_atom(Command) ->
  case lists:keyfind(Command, 1, ui_utils:get_ui_cmds()) of
	  {Command, CmdAtom, ParamStrAtom, HelpStrAtom} -> {CmdAtom, ParamStrAtom, HelpStrAtom};
	  false -> unknown_cmd
  end.


%%
%% Translate command atom to command module and function
%%
cmd_atom_to_function(CmdAtom) ->
  case lists:keyfind(CmdAtom, 1, cmd_atom_map()) of
	  {CmdAtom, Module, Function} -> {Module, Function};
	  false -> unknown_cmd
  end.


%%
%% Map a command atom to a module and function here
%% We specify the Module here, in case we want to break out
%%  the UI functions into separate modules.
%%
cmd_atom_map() ->  
  [
    {cmd_create_block,     ?MODULE,  ui_create_block},
    {cmd_copy_block,       ?MODULE,  ui_copy_block},
    {cmd_rename_block,     ?MODULE,  ui_rename_block},
    {cmd_execute_block,    ?MODULE,  ui_execute_block},
    {cmd_delete_block,     ?MODULE,  ui_delete_block},
    {cmd_disable_block,    ?MODULE,  ui_disable_block},
    {cmd_enable_block,     ?MODULE,  ui_enable_block},
    {cmd_freeze_block,     ?MODULE,  ui_freeze_block},
    {cmd_thaw_block,       ?MODULE,  ui_thaw_block},
    {cmd_get_values,       ?MODULE,  ui_get_values},
    {cmd_set_value,        ?MODULE,  ui_set_value},
    {cmd_link_blocks,      ?MODULE,  ui_link_blocks},
    {cmd_unlink_blocks,    ?MODULE,  ui_unlink_blocks},
    {cmd_xlink_blocks,     ?MODULE,  ui_xlink_blocks},
    {cmd_xunlink_blocks,   ?MODULE,  ui_xunlink_blocks},
    {cmd_status,           ?MODULE,  ui_status},
    {cmd_valid_block_name, ?MODULE,  ui_valid_block_name},
    {cmd_load_blocks,      ?MODULE,  ui_load_blocks},
    {cmd_save_blocks,      ?MODULE,  ui_save_blocks},
    {cmd_node,             ?MODULE,  ui_node},
    {cmd_nodes,            ?MODULE,  ui_nodes},
    {cmd_connect,          ?MODULE,  ui_connect},
    {cmd_hosts,            ?MODULE,  ui_hosts},
    {cmd_exit,             ?MODULE,  ui_exit},
    {cmd_help,             ?MODULE,  ui_help}
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
    case longest_prefix(ui_utils:get_ui_cmds(), Before) of
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

show_params(ParamStrAtom) ->
  io:format("~s ~s~n", [get_string(enter_str), get_string(ParamStrAtom)]).


%
% Get the string corresponding to the string ID
%
-spec get_string(StringId :: atom()) -> string().

get_string(StringId) ->
  case maps:get(StringId, ui_utils:get_ui_strings()) of
    {badmap, StringsMap} ->  io_lib:format("Error: bad strings map: ~p~n", [StringsMap]);
    {badkey, StringId} -> io_lib:format("Error, string: ~p not found~n", [StringId]);
    String -> String
  end.


% Process block create command
% TODO: Add initial attrib values
ui_create_block(Params, ParamStrAtom) ->
  case check_num_params(Params, 2, 3) of  
    low -> show_params(ParamStrAtom);

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
ui_copy_block(Params, ParamStrAtom) ->
  case check_num_params(Params, 2, 3) of  
    low -> show_params(ParamStrAtom);

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
ui_rename_block(Params, ParamStrAtom) ->
  case check_num_params(Params, 2) of  
    low -> show_params(ParamStrAtom);

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
ui_execute_block(Params, ParamStrAtom) ->
   case check_num_params(Params, 1) of  
    low -> show_params(ParamStrAtom);

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
ui_delete_block(Params, ParamStrAtom) ->
  % TODO: Add delete all command to delete all blocks at once
  % TODO: Ask the user if they really want to delete
   case check_num_params(Params, 1) of  
    low -> show_params(ParamStrAtom);

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
ui_disable_block(Params, ParamStrAtom) ->
  case check_num_params(Params, 1) of

    low -> show_params(ParamStrAtom);
    
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
ui_enable_block(Params, ParamStrAtom) ->
 case check_num_params(Params, 1) of

    low -> show_params(ParamStrAtom);
    
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
ui_freeze_block(Params, ParamStrAtom) ->
case check_num_params(Params, 1) of

    low -> show_params(ParamStrAtom);
    
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
ui_thaw_block(Params, ParamStrAtom) ->
case check_num_params(Params, 1) of

    low -> show_params(ParamStrAtom);
    
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
ui_status(Params, _ParamStrAtom) ->
  case check_num_params(Params, 0) of  
    ok -> block_status();

    high -> format_out(err_too_many_params)
  end.
 
 
% Process the get values command
ui_get_values(Params, ParamStrAtom) ->
  case length(Params) of  
    0 -> show_params(ParamStrAtom);
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
      format_newline(),
      format_out(config_str),
      lists:map(FormatAttribute, Config),

      format_newline(),
      format_out(inputs_str),
      lists:map(FormatAttribute, Inputs),

      format_newline(),
      format_out(outputs_str),
      lists:map(FormatAttribute, Outputs);

    _ ->
      format_out(inv_block_values)
  end.


% format an attribute name and value into a displayable string
format_attribute(BlockValue) ->
  case BlockValue of
     % Array value attribute
    {ValueName, ArrayValues} when is_list(ArrayValues) ->
      format_array_values(ValueName, 1, ArrayValues);

    {ValueId, {Value}} -> % Config attribute
      format_value_id_value(ValueId, Value),
      format_newline();

    {ValueId, {Value, {DefValue}}} -> % Input attribute
      format_value_id_value_def_value(ValueId, Value, DefValue),
      format_newline();

    {ValueId, {Value, Links}} -> % Output attribute
      case Links of
        [] -> % No Links on this output value
          format_value_id_value(ValueId, Value),
          format_newline();

        _NotAnEmptyList -> % Must have links
          format_value_id_value(ValueId, Value),
          format_links(Links),
          format_newline()
      end
  end.


% Format array values
format_array_values(_ValueName, _Index, []) ->
    ok;
  
format_array_values(ValueName, Index, ArrayValues) ->
  io:format("  ~p[~w]:", [ValueName, Index]),
  [ArrayValue | RemainingArrayValues] = ArrayValues,

  case ArrayValue of
    {Value} -> % Config attribute
      format_value(Value),
      format_newline();

    {Value, {DefValue}}  -> % Input attribute
      format_value_def_value(Value, DefValue),
      format_newline();

    % Assume output if Links is a list and value is not a list.  
    {Value, Links} -> % Output attribute
    case Links of
      [] -> % No Links on this output 
        format_value(Value),
        format_newline();

      _NotAnEmptyList -> % Must have links
        format_value(Value),
        format_links(Links),
        format_newline()
    end
  end,
  format_array_values(ValueName, (Index + 1), RemainingArrayValues). 
  

% Format Value ID, value, and default value
format_value_id_value_def_value(ValueId, Value, DefValue) ->
  io:format("  ~p:  ~p  (~p)", [ValueId, Value, DefValue]).
  

% Format one Value ID and value
format_value_id_value(ValueId, Value) ->
  case ValueId of
    last_exec ->
      io:format("  ~p:  ~s", [ValueId, format_last_exec(Value)]);
    _ ->
      io:format("  ~p:  ~p", [ValueId, Value])
  end.


% Format value, and default value
format_value_def_value(Value, DefValue) ->
    io:format("  ~p  (~p)", [Value, DefValue]).
  

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

  format_links(Links) ->
    format_links(Links, "", 0).
  
  format_links([], LinkStr, _Count) -> 
    io:format("~s)", [LinkStr]);

  format_links([Link | Links], LinkStr, Count) ->
    case Count of
      0 -> NewLinkStr = " (";
      _ -> NewLinkStr = LinkStr ++ ", "
    end,
    format_links(Links, NewLinkStr ++ link_utils:format_link(Link), Count + 1).


% Process the set value command
ui_set_value(Params, ParamStrAtom) ->
  case check_num_params(Params, 3) of

    low -> show_params(ParamStrAtom);
    
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
ui_link_blocks(Params, ParamStrAtom) ->
  case check_num_params(Params, 4) of
    low -> show_params(ParamStrAtom);

    ok ->
      % 1st parameter is block name
      OutputBlockNameStr = lists:nth(1, Params),
      OutputBlockName = list_to_atom(OutputBlockNameStr),

      % 2nd parameter is the output value ID
      OutputValueIdStr = lists:nth(2, Params),
      case attrib_utils:str_to_value_id(OutputValueIdStr) of
        {ok, OutputValueId} ->

          % The remaining params form the Link
          LinkParams = lists:nthtail(2, Params),
          case parse_link(LinkParams) of
            {ok, Link} ->
              case linkblox_api:add_link(curr_node(), OutputBlockName, OutputValueId, Link) of
                {error, Reason} ->
                  format_out(err_linking_output_to_input, 
                          [OutputBlockNameStr, OutputValueIdStr, link_utils:format_link(Link), Reason]);
                ok ->
                  format_out(block_output_linked_to_block_input, 
                    [OutputBlockNameStr ++ ":" ++ OutputValueIdStr, link_utils:format_link(Link)])
              end;
            {error, Reason} ->
              format_out(err_converting_to_link, [Reason, LinkParams])
          end;
        {error, Reason} ->
          format_out(err_converting_to_output_value_id, [Reason, OutputValueIdStr])
      end;

    high -> format_out(err_too_many_params)
  end.


% Process unlink blocks command
ui_unlink_blocks(Params, ParamStrAtom) ->
  case check_num_params(Params, 4) of
    low -> show_params(ParamStrAtom);

    ok ->
      % 1st parameter is block name
      OutputBlockNameStr = lists:nth(1, Params),
      OutputBlockName = list_to_atom(OutputBlockNameStr),

      % 2nd parameter is the output value ID
      OutputValueIdStr = lists:nth(2, Params),
      case attrib_utils:str_to_value_id(OutputValueIdStr) of
        {ok, OutputValueId} ->
          % The remaining params form the Link
          LinkParams = lists:nthtail(2, Params),
          case parse_link(LinkParams) of
            {ok, Link} ->
              % Delete the output value link to an input, 
              case linkblox_api:del_link(curr_node(), OutputBlockName, OutputValueId, Link) of
                {error, Reason} ->
                  format_out(err_unlinking_output_from_input, 
                               [OutputBlockNameStr, OutputValueIdStr, link_utils:format_link(Link), Reason]);
                ok ->
                  format_out(block_output_unlinked_from_block_input, 
                               [OutputBlockNameStr ++ ":" ++ OutputValueIdStr, link_utils:format_link(Link)])
              end;

            {error, Reason} ->
              format_out(err_converting_to_link, [Reason, LinkParams])
          end;
        {error, Reason} ->
          format_out(err_converting_to_output_value_id, [Reason, OutputValueIdStr])
      end;

    high -> format_out(err_too_many_params)
  end.


% Process execution link blocks command
ui_xlink_blocks(Params, ParamStrAtom) ->
  case check_num_params(Params, 2) of
    low -> show_params(ParamStrAtom);

    ok ->
      % 1st parameter is executor block name
      ExecutorBlockNameStr = lists:nth(1, Params),
      ExecutorBlockName = list_to_atom(ExecutorBlockNameStr),

      % 2nd parameter is the executee block name
      ExecuteeBlockNameStr = lists:nth(2, Params),
      ExecuteeBlockName = list_to_atom(ExecuteeBlockNameStr),

      case linkblox_api:add_exec_link(curr_node(), ExecutorBlockName, ExecuteeBlockName) of
        {error, Reason} ->
          format_out(err_adding_execution_link_from_to, 
                      [ExecutorBlockNameStr, ExecuteeBlockNameStr, Reason]);
        ok ->
          format_out(block_execution_linked_to_block, [ExecutorBlockName, ExecuteeBlockName])
      end;
    high -> format_out(err_too_many_params)
  end.


% Process execution unlink blocks command
ui_xunlink_blocks(Params, ParamStrAtom) ->
  case check_num_params(Params, 2) of
    low -> show_params(ParamStrAtom);

    ok ->
      % 1st parameter is executor block name
      ExecutorBlockNameStr = lists:nth(1, Params),
      ExecutorBlockName = list_to_atom(ExecutorBlockNameStr),

      % 2nd parameter is the executee block name
      ExecuteeBlockNameStr = lists:nth(2, Params),
      ExecuteeBlockName = list_to_atom(ExecuteeBlockNameStr),

      case linkblox_api:del_exec_link(curr_node(), ExecutorBlockName, ExecuteeBlockName) of
        {error, Reason} ->
          format_out(err_deleting_execution_link_from_to, 
                          [ExecutorBlockName, ExecuteeBlockName, Reason]);
        ok ->
          format_out(block_execution_unlinked_from_block, [ExecutorBlockName, ExecuteeBlockName])
      end;
    high -> format_out(err_too_many_params)
  end.


% Parse the params into a Link tuple
parse_link(LinkParams) ->

  % Links should consist of a Block Name and Value ID
  case LinkParams of
    [BlockNameStr, ValueIdStr] ->
      BlockName = list_to_atom(BlockNameStr),

      case attrib_utils:str_to_value_id(ValueIdStr) of
        {ok, ValueId}   -> {ok, {BlockName, ValueId}};
        {error, Reason} -> {error, Reason}
      end;
    _Invalid ->
      {error, invalid}
  end.


% Process the load blocks command
ui_load_blocks(Params, _ParamStrAtom) ->
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
ui_save_blocks(Params, _ParamStrAtom) ->
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
ui_node(_Params, _ParamStrAtom) ->
  format_out(node_prompt_str, [node()]).


% Execute Erlang BIF nodes()
ui_nodes(_Params, _ParamStrAtom) ->  
  format_out(nodes_prompt_str, [[node() | nodes()]]).


% Connect to another node
ui_connect(Params, ParamStrAtom) ->
  case check_num_params(Params, 1) of
    low -> show_params(ParamStrAtom);

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
  
  case block_utils:is_string(Value) of
      true -> ValueStr = string:left(io_lib:format("\"~s\"", [Value]), 12);
         _ -> ValueStr = string:left(io_lib:format("~w",[Value]), 12)
  end,

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
             ValueStr, Status, ExecMethod, LastExecuted]),
  block_status(RemainingBlockNames).


% validate Params is one valid block name
ui_valid_block_name(Params, ParamStrAtom) ->
  case check_num_params(Params, 1) of
    low -> show_params(ParamStrAtom),
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
ui_hosts(_Params, _ParamStrAtom) ->
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
ui_exit(_Params, _ParamStrAtom) ->
  done.

%%
%% Process the help command
%%
ui_help(Params, _ParamStrAtom) ->
  case check_num_params(Params, 0, 1) of
    ok -> 
      CmdList = ui_utils:get_ui_cmds(),
      case length(Params) of  
        0 ->
          format_out(linkblox_help),
          lists:map(fun(Cmd) -> 
                      {CmdStr, _CmdAtom, _CmdParamAtom, CmdHelpAtom} = Cmd,
                      io:format("~s:  ~s~n", [CmdStr, get_string(CmdHelpAtom)])
                    end,
                    CmdList);
        1 ->
          % Use the entered parameter as the command Name
          [CmdHelp] = Params,
          case cmd_string_to_cmd_atom(CmdHelp) of
            unknown_cmd ->
              format_out(no_help_for, [CmdHelp]);

            {_CmdAtom, ParamStrAtom, HelpStrAtom} -> 
                io:format("~s:  ~s~n  ~s~n", [CmdHelp, get_string(HelpStrAtom), get_string(ParamStrAtom)])
          end
      end;
    high ->
      format_out(err_too_many_params),
      error
  end.

  
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