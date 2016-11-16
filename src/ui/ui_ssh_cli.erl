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
-export([start/1, start/2]).

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
          ui_block_types/1,     ui_block_types_help/0,
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
start(Port) ->
    start(Port, []).

start(Port, Options) ->
    error_logger:info_msg("Starting SSH CLI User Interface on port: ~p ~n", [Port]),
    crypto:start(),
    ssh:start(),
    ssh:daemon(any, Port, [{shell, fun(U, H) -> start_shell(U, H) end} | Options]).

%% 
%% Start user input loop
%%
start_shell(User, Peer) ->
  spawn(fun() ->
	  io:setopts([{expand_fun, fun(Bef) -> expand(Bef) end}]),
    io:format("~n   W E L C O M E  T O  L i n k B l o x !~n~n"),
		io:format("Enter command\n"),
		put(user, User),
		put(peer_name, Peer),
    % default current node to local node
    set_node(node()),
		shell_loop()
	end).


%% 
%% Get and Evaluate user input loop
%%
shell_loop() ->
  % use the current node for the UI prompt
  Prompt = atom_to_list(get(curr_node)) ++ "> ",
  % read user input
  Line = get_input(Prompt),

  Result = eval_input(Line),
  case Result of
	  done -> 
	    exit(normal);
	  _ -> 
	    shell_loop()
    end.


%% 
%% Evaluate user input
%%
eval_input(Line) ->
  case string:tokens(Line, " \n") of
	  [] -> [];
	  [Command | Params] ->
	    case cmd_string_to_cmd_atom(Command) of
        unknown_cmd ->
          io:format("~p Unknown command string~n", [Command]);
        CmdAtom ->
          case cmd_atom_to_function(CmdAtom) of
            unknown_cmd ->
              io:format("~p Unknown command atom~n", [CmdAtom]);

            {Module, Function} ->
		          case catch Module:Function(Params) of
			          {'EXIT', Error} ->
			            {error, Error}; % wrong_number_of_arguments};
			          Result ->
			            Result
		          end
          end
	    end
    end.


%% 
%% Translate a command string to an atom
%% This indirection allows command mapping using different languages,
%%
cmd_string_to_cmd_atom(Command) ->
  case lists:keysearch(Command, 1, ui_en_us:cmd_string_map()) of
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
    {cmd_block_types,      ?MODULE,  ui_block_types,    ui_block_types_help},
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


%%% our expand function (called when the user presses TAB)
%%% input: a reversed list with the row to left of the cursor
%%% output: {yes|no, Expansion, ListofPossibleMatches}
%%% where the atom no yields a beep
%%% Expansion is a string inserted at the cursor
%%% List... is a list that will be printed
%%% Here we beep on prefixes that don't match and when the command
%%% filled in

expand([$  | _]) ->
  {no, "", []};
expand(RevBefore) ->    
  Before = lists:reverse(RevBefore),
    case longest_prefix(ui_en_us:cmd_string_map(), Before) of
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
%% Get the current node this UI is connected to
%%
% TODO: Check if node exists before sending cmd to the node
get_node() ->
  get(curr_node).


%%
%% Set the node this UI is connected to
%%
set_node(Node) ->
  put(curr_node, Node). 


% Process block create command
% TODO: Add initial attrib values
ui_create_block(Params) ->
  case check_num_params(Params, 2) of  
    low -> io:format("Enter block-type new-block-name~n");

    ok -> 
      [BlockTypeStr, BlockNameStr] = Params,

      BlockType = list_to_atom(BlockTypeStr),
      BlockName = list_to_atom(BlockNameStr),
      case linkblox_api:create_block(get_node(), BlockType, BlockName, []) of
        ok ->
          io:format("Block ~s:~s Created~n", [BlockTypeStr, BlockNameStr]);

        {error, invalid_block_type} ->
          io:format("Error: Block type ~s is not a valid block type~n", [BlockTypeStr]);

        {error, block_exists} ->
          io:format("Error: Block ~s already exists~n", [BlockNameStr]);
        
        {error, Reason} -> 
          io:format("Error: ~p creating block ~s:~s ~n", [Reason, BlockTypeStr, BlockNameStr])
      end;

    high -> io:format("Error: Too many parameters~n")
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
          DstNode = get_node();
        3 ->
          [SrcBlockNameStr, DstNodeStr, DstBlockNameStr] = Params,
          DstNode = list_to_atom(DstNodeStr)
      end,

      SrcBlockName = list_to_atom(SrcBlockNameStr),
      % Always get source block values from the current node
      case linkblox_api:get_block(get_node(), SrcBlockName) of
        {ok, SrcBlockValues} ->
          DstBlockName = list_to_atom(DstBlockNameStr),
          case linkblox_api:copy_block(DstNode, DstBlockName, SrcBlockValues, []) of
            ok ->
              io:format("Dest Block ~s Created~n", [DstBlockNameStr]);

            {error, block_exists} ->
              io:format("Error: Dest Block ~s already exists~n", [DstBlockNameStr]);
      
            {error, Reason} -> 
              io:format("Error: ~p creating block ~s ~n",  [Reason, DstBlockNameStr])
          end;

        {error, block_not_found} ->
          io:format("Error: Source Block ~s does not exists~n", [SrcBlockNameStr])
      end;

    high -> io:format("Error: Too many parameters~n")
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
      case linkblox_api:get_block(get_node(), SrcBlockName) of
        {ok, SrcBlockValues} ->
          DstBlockName = list_to_atom(DstBlockNameStr),
          case linkblox_api:copy_block(get_node(), DstBlockName, SrcBlockValues, []) of
            ok ->
              io:format("Dest Block ~s Created~n", [DstBlockNameStr]);

            {error, block_exists} ->
              io:format("Error: Dest Block ~s already exists~n", [DstBlockNameStr]);
      
            {error, Reason} -> 
              io:format("Error: ~p creating block ~s ~n",  [Reason, DstBlockNameStr])
          end;

        {error, block_not_found} ->
          io:format("Error: Source Block ~s does not exists~n", [SrcBlockNameStr])
      end;

    high -> io:format("Error: Too many parameters~n")
  end.    


% Process manual block execute command
ui_execute_block(Params) ->
   case check_num_params(Params, 1) of  
    low -> io:format("Enter block name~n");

    ok -> 
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      case linkblox_api:execute_block(get_node(), BlockName) of
        ok -> 
          ok;
        {error, block_not_found} ->
          io:format("Error: block ~s not found~n", [BlockNameStr])  
      end;

    high -> io:format("Error: Too many parameters~n")
  end.


% Process block delete command
ui_delete_block(Params) ->
  % TODO: Add delete all command to delete all blocks at once
  % TODO: Ask the user if they really want to delete
   case check_num_params(Params, 1) of  
    low -> io:format("Enter block name~n");

    ok -> 
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),  
      case linkblox_api:delete_block(get_node(), BlockName) of
        ok -> 
          io:format("~p Deleted~n", [BlockNameStr]);
            
        {error, Reason} ->
          io:format("Error: ~p deleting ~p~n", [Reason, BlockNameStr]) 
      end;

    high -> io:format("Error: Too many parameters~n")
  end.
    
    
% Process disable block command
ui_disable_block(Params) ->
  case check_num_params(Params, 1) of

    low -> io:format("Enter block-name~n");
    
    ok -> 
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      case linkblox_api:set_value(get_node(), BlockName, disable, true) of
        ok ->
          io:format("~s Disabled~n", [BlockNameStr]);

        {error, block_not_found} ->
          io:format("Error: Block: ~s does not exist~n",  [BlockNameStr]);

        {error, Reason} ->
          io:format("Error: ~p Disabling: ~s~n",  [Reason, BlockNameStr]) 
      end;
 
    high -> io:format("Error: Too many parameters~n")
  end.


% Process enable block command
ui_enable_block(Params) ->
 case check_num_params(Params, 1) of

    low -> io:format("Enter block-name~n");
    
    ok -> 
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      case linkblox_api:set_value(get_node(), BlockName, disable, false) of
        ok ->
          io:format("~s Enabled~n", [BlockNameStr]);

        {error, block_not_found} ->
          io:format("Error: Block: ~s does not exist~n",  [BlockNameStr]);

        {error, Reason} ->
          io:format("Error: ~p Enabling: ~s~n",  [Reason, BlockNameStr]) 
      end;
 
    high -> io:format("Error: Too many parameters~n")
  end.
    
    
% Process freeze block command
ui_freeze_block(Params) ->
case check_num_params(Params, 1) of

    low -> io:format("Enter block-name~n");
    
    ok -> 
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      case linkblox_api:set_value(get_node(), BlockName, freeze, true) of
        ok ->
          io:format("~s Frozen~n", [BlockNameStr]);

        {error, block_not_found} ->
          io:format("Error: Block: ~s does not exist~n",  [BlockNameStr]);

        {error, Reason} ->
          io:format("Error: ~p Freezing: ~s~n",  [Reason, BlockNameStr]) 
      end;
 
    high -> io:format("Error: Too many parameters~n")
  end.

    
% Process thaw block command
ui_thaw_block(Params) ->
case check_num_params(Params, 1) of

    low -> io:format("Enter block-name~n");
    
    ok -> 
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      case linkblox_api:set_value(get_node(), BlockName, freeze, false) of
        ok ->
          io:format("~s Thawed~n", [BlockNameStr]);

        {error, block_not_found} ->
          io:format("Error: Block: ~s does not exist~n",  [BlockNameStr]);

        {error, Reason} ->
          io:format("Error: ~p Thawing: ~s~n",  [Reason, BlockNameStr]) 
      end;
 
    high -> io:format("Error: Too many parameters~n")
  end.


% Process block status command
ui_status(Params) ->
  case check_num_params(Params, 0) of  
    ok -> block_status();

    high -> io:format("Error: Too many parameters~n")
  end.
 
 
% Process the get values command
ui_get_values(Params) ->
  case length(Params) of  
    0 -> io:format("Enter block-name <value-name>~n");
    1 -> 
      [BlockNameStr] = Params,

      case BlockNameStr of
        "blocks" ->  % Just get the list of block names
          BlockNames = linkblox_api:get_block_names(get_node()),
          io:format("~n"),
          lists:map(fun(BlockName) -> io:format("  ~p~n", [BlockName]) end, BlockNames),
          io:format("~n");

        "types" ->  % Just get the list of block types information
          TypesInfo = linkblox_api:get_types_info(get_node()),
          io:format("~n"),
          io:format("Block Type Name   Version   Description~n"),
          io:format("---------------- --------- ----------------------------------------~n"),
          lists:map(fun(TypeInfo) ->
                      {TypeName, Version, Description} = TypeInfo, 
                      io:format("~s, ~s, ~s~n", [TypeName, Version, Description]) end, 
                      TypesInfo),
          io:format("~n");

       _ ->
          BlockName = list_to_atom(BlockNameStr),
          case linkblox_api:get_block(get_node(), BlockName) of
            {ok, BlockValues} -> 
              io:format("~n~p~n", [BlockValues]);
            {error, block_not_found} -> 
              io:format("Error: Block ~s does not exist~n", [BlockNameStr]);
            Unknown ->
              io:format("Error: Unkown result from linkblox_api:get_block(): ~p~n", Unknown)
          end
        end;

    2 -> 
      [BlockNameStr, ValueIdStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      case attrib_utils:str_to_value_id(ValueIdStr) of
        {ok, ValueId} ->
          case linkblox_api:get_value(get_node(), BlockName, ValueId) of
            {ok, CurrentValue} ->
              io:format("~n   ~p~n", [CurrentValue]);
          
            {error, block_not_found} ->
              io:format("Error: Block ~s does not exist~n", [BlockNameStr]);

            {error, value_not_found} ->
              io:format("Error: ~s is not a value of block: ~s~n", 
                         [ValueIdStr, BlockNameStr]);
            {error, Reason} ->
              io:format("Error: ~p retrieving value: ~s:~s~n", 
                           [Reason, BlockNameStr, ValueIdStr]);

            Unknown ->
              io:format("Error: Unkown result from linkblox_api:get_value(): ~p~n", 
                         [Unknown]) 
          end;
        {error, invalid} ->
          io:format("Error: Invalid Value Id string: ~s~n", [ValueIdStr])
      end;

    _ -> io:format("Error: Too many parameters~n")
  end.    
 
 
% Process the set value command
ui_set_value(Params) ->
  case check_num_params(Params, 3) of

    low -> io:format("Enter block-name value-name value~n");
    
    ok -> 
      [BlockNameStr, ValueIdStr, ValueStr] = Params,

      BlockName = list_to_atom(BlockNameStr),
      case attrib_utils:str_to_value_id(ValueIdStr) of
        {ok, ValueId} ->
          Value = parse_value(ValueStr),
          case linkblox_api:set_value(get_node(), BlockName, ValueId, Value) of
            ok ->
              io:format("~s:~s Set to: ~s~n", [BlockNameStr, ValueIdStr, ValueStr]);

            {error, block_not_found} ->
              io:format("Error: Block: ~s does not exist~n",  [BlockNameStr]);

            {error, not_found} ->
              io:format("Error: Attribute: ~s does not exist~n",  [ValueIdStr]);

            {error, Reason} ->
              io:format("Error: ~p Setting: ~s:~s to ~s~n",  
                         [Reason, BlockNameStr, ValueIdStr, ValueStr]) 
          end;
            
        {error, invalid} ->
          io:format("Error: Invalid Value Id string: ~s~n", [ValueIdStr])
      end;

    high -> io:format("Error: Too many parameters~n")
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
              case linkblox_api:set_link(get_node(), InputBlockName, InputValueId, Link) of
                {error, Reason} ->
                  io:format("Error: ~p Linking Input: ~s:~s to Output: ~p~n", 
                          [Reason, InputBlockNameStr, InputValueIdStr, Link]);
                ok ->
                  io:format("Block Input: ~s:~s Linked to Block Output: ~p~n", 
                              [InputBlockNameStr, InputValueIdStr, Link])
              end;
            {error, Reason} ->
              io:format("Error: ~p Converting ~p to a Link~n", 
                           [Reason, LinkParams])
          end;
        {error, Reason} ->
          io:format("Error: ~p Converting ~s to Input Value ID~n", 
                                        [Reason, InputValueIdStr])
      end;

    high -> io:format("Error: Too many parameters~n")
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
          case linkblox_api:set_link(get_node(), InputBlockName, InputValueId, {}) of
            {error, Reason} ->
              io:format("Error: ~p Unlinking Input: ~s:~s~n", 
                          [Reason, InputBlockNameStr, InputValueIdStr]);
              ok ->
                io:format("Block Input: ~s:~s Unlinked~n", 
                            [InputBlockNameStr, InputValueIdStr])
          end;
      
        {error, Reason} ->
          io:format("Error: ~p Converting ~s to Input Value ID~n", 
                                        [Reason, InputValueIdStr])
      end;

    high -> io:format("Error: Too many parameters~n")
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
          io:format("Enter file name, or press <Enter> for default: 'LinkBloxConfig': "),
          case get_input("") of
                  [] -> FileName = "LinkBloxConfig";
            FileName -> FileName
          end;
        1 ->
          % Use the entered parameter as the file name
          FileName = Params
      end,
      
      % file:consult() turns the contents of a text file into Erlang terms
      case file:consult(FileName) of
        {ok, BlockDefnList} ->
          create_blocks(BlockDefnList);
        {error, Reason} ->
          io:format("Error: ~p loading block config file: ~p~n", [Reason, FileName])
      end;

    high -> io:format("Error: Too many parameters~n")
  end.


% Create blocks from a list of block definitions
create_blocks([]) -> ok;

create_blocks(BlockDefnList) ->
  [BlockDefn | RemainingBlockDefnList] = BlockDefnList,
  {Config, _Inputs, _Outputs} = BlockDefn,
  BlockName = config_utils:name(Config),
  case linkblox_api:create_block(get_node(), BlockDefn) of
    ok -> 
      io:format("Block ~p Created on node: ~p~n", [BlockName, get_node()]);
    {error, Reason} -> 
      io:format("Error: ~p creating block ~p on node: ~p ~n", [Reason, BlockName, get_node()])
  end,
  create_blocks(RemainingBlockDefnList).    


% Process the save blocks command
ui_save_blocks(Params) ->
  % Write the block values to a configuration file
  case check_num_params(Params, 0, 1) of
    ok ->
      case length(Params) of  
        0 -> 
          io:format("Enter file name, or press <Enter> for default: 'LinkBloxConfig': "),
          case get_input("") of
                  [] -> FileName = "LinkBloxConfig";
            FileName -> FileName
          end;
        1 ->
          % Use the entered parameter as the file name
          FileName = Params
      end,

      io:format("This will overwrite ~s if the file exists. OK to continue? (Y/N): ", [FileName]),
      case get_yes() of
        true ->
          case linkblox_api:save_blocks(get_node(), FileName) of
            ok ->
              io:format("Block config file: ~s saved~n", [FileName]);

            {error, Reason} ->
              io:format("Error: ~p saving block conifg file: ~s~n", [Reason, FileName])
          end;
        _ -> ok
      end;
    high -> io:format("Error: Too many parameters~n")
  end.

-ifdef(CUT_IT_OUT).
      BlockValuesList = block_values(),
            
      Clean = fun(BlockValues) -> clean_block_values(BlockValues) end,

      CleanedBlockValuesList = lists:map(Clean, BlockValuesList),

      Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
      Text = lists:map(Format, CleanedBlockValuesList),
            
      case file:write_file(FileName, Text, [exclusive]) of
        ok -> 
          io:format("Block config file: ~s saved~n", [FileName]);

        {error, eexist} ->
          io:format("Error: ~s exists. Overwrite? (Y/N): ", [FileName]),
          case get_yes() of
            true ->
              case file:write_file(FileName, Text) of
                ok ->
                  io:format("Block config file: ~s saved~n", [FileName]);

                {error, Reason} ->
                  io:format("Error: ~p saving block conifg file: ~s~n", [Reason, FileName])
              end;
            _ -> ok
          end;
        {error, Reason} ->
          io:format("Error: ~p saving block config file: ~s~n", [Reason, FileName])
      end;
 
    
% Clean block values of linked Input and calculated Output values,
% to make the block values suitable for saving to a file 

clean_block_values({Config, Inputs, Outputs}) ->

  % At this point the Block Values are on the local node
  % so it is OK to call the local utility functions without
  % going through the linkblox_api.  
  % This also means the file will be saved on the local node
  EmptyInputs = link_utils:empty_linked_inputs(Inputs),
  EmptyOutputs = output_utils:update_all_outputs(Outputs, empty, empty),
  EmptyOutputs1 = output_utils:clear_output_refs(EmptyOutputs),
 
  % Cleaned block values
  {Config, EmptyInputs, EmptyOutputs1}.


%%
%% Get the list of block values for all of the blocks currently running
%% linkblox_api:get_block() does not return Private values
%%
block_values() ->
  block_values(linkblox_api:get_block_names(get_node()), []).
    
block_values([], BlockValuesList) -> 
  BlockValuesList;
 
block_values(BlockNames, BlockValuesList) ->
  [BlockName | RemainingBlockNames] = BlockNames,
  case linkblox_api:get_block(get_node(), BlockName) of
    {ok, BlockValues} ->
      block_values(RemainingBlockNames, [BlockValues | BlockValuesList]);

    {error, Reason} -> 
      io:format("Error: ~p getting block: ~p~n", [Reason, BlockName]),
      block_values(RemainingBlockNames, BlockValuesList)
  end.

-endif.

% Execute Erlang BIF node()
ui_node(_Params) ->
  io:format( "Node: ~p~n", [node()]).

% Execute Erlang BIF nodes()
ui_nodes(_Params) ->  
  io:format( "Nodes: ~p~n", [[node() | nodes()]]).

% Connect to another node
ui_connect(Params) ->
  case check_num_params(Params, 1) of
    low ->
      io:format("Enter node-name or local~n");

    ok ->
      case Params of
        ["local"] ->  % Just connect to the local node
          io:format("Connecting to local node~n"),
          connect_to_node(node());

        [NodeStr] ->   
          Node = list_to_atom(NodeStr),
          connect_to_node(Node)
      end;
    high ->
      io:format("Error: Too many parameters~n"),
      error
  end.

% Attempt to connect to the given node
connect_to_node(Node) ->
  case net_kernel:connect_node(Node) of
    true -> 
      set_node(Node),
      io:format("Connected to node: ~p~n", [Node]);

    _ ->
      io:format("Unable to connect to node: ~p~n", [Node])
  end.


%%
%% Display status off each running block
%%
block_status() ->
  io:fwrite("~n~-16s ~-16s ~-12s ~-12s ~-12s ~-15s~n", 
            ["Block Type", "Block Name", "Output", "Status", "Exec Method", "Last Exec"]),
  io:fwrite("~16c ~16c ~12c ~12c ~12c ~15c~n", [$-, $-, $-, $-, $-, $-] ), 
  block_status(linkblox_api:get_block_names(get_node())).
    
block_status([]) ->
  io:format("~n"), 
  ok;

block_status([BlockName | RemainingBlockNames]) ->
  {BlockTypeStr, _Version, _Description} = 
        linkblox_api:get_type_info(get_node(), BlockName),

  % TODO: Create get_values() API call, get multiple values in one call 
  {ok, Value} = linkblox_api:get_value(get_node(), BlockName, value),
  {ok, Status} = linkblox_api:get_value(get_node(), BlockName, status),
  {ok, ExecMethod} = linkblox_api:get_value(get_node(), BlockName, exec_method),
  
  case linkblox_api:get_value(get_node(), BlockName, last_exec) of 
    {ok, not_active} ->
      LastExecuted = "not_active";
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
      io:format("Enter block-name~n"),
      error;

    ok ->
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      % check if block name is an existing block
      case linkblox_api:is_block_name(get_node(), BlockName) of
        true  -> 
          io:format("Block: ~s exists~n", [BlockNameStr]);
        false ->
          io:format("Block ~p does not exist~n", [BlockNameStr])
      end;
 
    high ->
      io:format("Error: Too many parameters~n"),
      error
  end.


%%
%% Get list of the block type names and versions
%%
ui_block_types(_Params) ->
  BlockTypes = block_types:block_types_info(),
   
  % Print the list of type names version
  io:fwrite("~n~-16s ~-8s ~-60s~n", 
              ["Block Type", "Version", "Description"]),
  io:fwrite("~16c ~8c ~60c~n", [$-, $-, $-] ), 
   
  lists:map(fun({TypeName, Version, Description}) -> 
            io:fwrite("~-16s ~-8s ~-60s~n", 
                      [string:left(TypeName, 16), 
                       string:left(Version, 8),
                       string:left(Description, 60)]) end, 
                      BlockTypes),
  io:format("~n").


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
      
      CmdList = ui_en_us:cmd_string_map(),
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
      io:format("Error: Too many parameters~n"),
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

ui_block_types_help() ->
  io:format("TODO: Insert types help text here~n").

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


%%
%% Naive parse value function, i.e. take a stab at the value type
%%
parse_value(ValueStr) ->
  case ((lists:nth(1,ValueStr) == $") andalso
      (lists:last(ValueStr)  == $")) of
    true ->
      % ValueStr is surrounded by quotes
      % Remove the quotes and use the bare string
      [_FirstQuote | RemString] = ValueStr,
      lists:droplast(RemString);

    false ->
      case string:to_float(ValueStr) of
        {Float, []}       -> Float;

        {error, no_float} ->

          case string:to_integer(ValueStr) of
            {Integer, []}     -> Integer;
            
            {error, no_integer} ->
              % just turn the input into an atom
              list_to_atom(ValueStr);

            {_Integer, _Rest} -> ValueStr 
          end;

        {_Float, _Rest}   -> ValueStr
      end
  end.

  %
  % Get input, return 'true' if first char is 'Y' or 'y'
  %
  get_yes() ->
    case lists:nth(1, get_input("")) of
      $Y -> true;
      $y -> true;
      _  -> false
    end.


  %
  % Get user input, 
  % minus new line char, leading whitespace, 
  % and trailing whitespace
  %
  get_input(Prompt) ->
    Raw1 = io:get_line(Prompt),

    % In nerves environment, get_line() returns a binary.
    % Convert it to a string
    case is_binary(Raw1) of
      true  -> Raw2 = erlang:binary_to_list(Raw1);
      false -> Raw2  = Raw1 
    end, 
    % Remove new line char
    Raw3 = string:strip(Raw2, right, 10),
    % Remove leading and trailing whitespace
    string:strip(Raw3).




%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% ====================================================================
% Test parse_value()
% 
%   Test float good
parse_value_float_good_test() ->
  ExpectedResult = 12.345,
  ValueStr = "12.345",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

%   Test float bad
parse_value_float_bad_test() ->
  ExpectedResult = "12.345crap",
  ValueStr = "12.345crap",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

%   Test integer good
parse_value_integer_good_test() ->
  ExpectedResult = 12345,
  ValueStr = "12345",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

%   Test integer bad
parse_value_integer_bad_test() ->
  ExpectedResult = "12345crap",
  ValueStr = "12345crap",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

%   Test boolean true
parse_value_boolean_true_test() ->
  ExpectedResult = true,
  ValueStr = "true",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

%   Test boolean false
parse_value_boolean_false_test() ->
  ExpectedResult = false,
  ValueStr = "false",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

%   Test string good
parse_value_string_good_test() ->
  ExpectedResult = 'TestString',
  ValueStr = "TestString",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

% ====================================================================

-endif.