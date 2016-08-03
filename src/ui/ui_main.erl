%%%
%%% @doc 
%%% User Interface for LinkBlox app.
%%% @end
%%%

-module(ui_main).

-author("Mark Sebald").

-include("../block_state.hrl"). 


%% ====================================================================
%% UI functions
%% ====================================================================
-export([init/0]).

%%
%% Initialize node, before entering the UI loop
%%
init() ->
  % setup ets store for current node
  ets:new(node_store, [set, named_table]),
  % default node to local node
  set_node(node()),

  io:format("~n   W E L C O M E  T O  L i n k B l o x !~n~n"),

  % enter UI loop, and never return
  loop().


%%
%% Get the current node this UI is connected to
%%
% TODO: Check if node exists before sending
get_node() ->
  [{curr_node, Node}] = ets:lookup(node_store, curr_node),
  Node.


%%
%% Set the node this UI is connected to
%%
set_node(Node) ->
  ets:insert(node_store, {curr_node, Node}).


%%
%%  UI input loop
%%
loop() ->
  % use the current node for the UI prompt
  Prompt = atom_to_list(get_node()) ++ ">",
  Raw1 = io:get_line(Prompt),
  Raw2 = string:strip(Raw1, right, 10), % Remove new line char
  Raw3 = string:strip(Raw2), % Remove leading and trailing whitespace
    
  % Split up the string into command and parameter words
  CmdAndParams = string:tokens(Raw3, " "),  
    
  if 0 < length(CmdAndParams) ->
    [Cmd | Params] = CmdAndParams,
    CmdLcase = string:to_lower(Cmd),
        
    if CmdLcase /= "exit" ->
      case CmdLcase of
        "create"    -> ui_create_block(Params);
        "copy"      -> ui_copy_block(Params);
        "rename"    -> ui_rename_block(Params);
        "execute"   -> ui_execute_block(Params);
        "delete"    -> ui_delete_block(Params);
        "disable"   -> ui_disable_block(Params);
        "enable"    -> ui_enable_block(Params);
        "freeze"    -> ui_freeze_block(Params);
        "thaw"      -> ui_thaw_block(Params);
        "get"       -> ui_get_values(Params);
        "set"       -> ui_set_value(Params);
        "link"      -> ui_link_blocks(Params);
        "status"    -> ui_status(Params);
        "types"     -> ui_block_types(Params);
        "valid"     -> validate_block_name(Params);
        "load"      -> ui_load_blocks(Params);
        "save"      -> ui_save_blocks(Params);
        "node"      -> ui_node(Params);
        "nodes"     -> ui_nodes(Params);
        "connect"   -> ui_connect(Params);
        "help"      -> ui_help(Params);
            
        _Unknown    -> io:format("Error: Unknown command: ~p~n", [Raw3])
      end,
      loop();  % processed command keep looping

    true -> % user entered "exit", stop looping
      ok
    end;
  true -> % user just hit "Enter", keep looping 
    loop()
  end.
  

% Process block create command
% TODO: Add initial attrib values
ui_create_block(Params) ->
  case check_num_params(Params, 2) of  
    low -> io:format("Error: Enter block type and name~n");

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
    low -> io:format("Error: Enter source-block-name <dest-node-name> dest-block-name~n");

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
ui_rename_block(Params) ->
  case check_num_params(Params, 2) of  
    low -> io:format("Error: Enter source-block-name dest-block-name~n");

    ok ->
      [SrcBlockNameStr, DstBlockNameStr] = Params,

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


% Process manual block execute command
ui_execute_block(Params) ->
   case check_num_params(Params, 1) of  
    low -> io:format("Error: Enter block name~n");

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
    low -> io:format("Error: Enter block name~n");

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

    low -> io:format("Error: Enter block-name~n");
    
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

    low -> io:format("Error: Enter block-name~n");
    
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

    low -> io:format("Error: Enter block-name~n");
    
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

    low -> io:format("Error: Enter block-name~n");
    
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
    0 -> io:format("Error: Enter block-name [value-name]~n");
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

    low -> io:format("Error: Enter block-name value-name value~n");
    
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
    low -> io:format("Error: Enter input-block-name input-value-name <output-node-name <output-block-name>> output-value-name~n");

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
                  io:format("Error: ~p Linking Input: ~s:~s to Ouput: ~p~n", 
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
  case check_num_params(Params, 1) of
    low -> io:format("Error: Enter file name~n");
    ok ->
      FileName = Params,
	    case file:consult(FileName) of
    	  {ok, BlockValuesList} ->
			    create_blocks(BlockValuesList);
		    {error, Reason} ->
			    io:format("Error: ~p loading blocks file: ~p~n", [Reason, FileName])
      end;

    high -> io:format("Error: Too many parameters~n")
  end.


% Create blocks from a list of block values
create_blocks([]) -> ok;

create_blocks(BlockValuesList) ->
  [BlockValues | RemainingBlockValuesList] = BlockValuesList,
  {Config, _Inputs, _Outputs} = BlockValues,
  BlockName = config_utils:name(Config),
  case block_supervisor:create_block(BlockValues) of
    {ok, _Pid} -> 
      io:format("Block ~p Created~n", [BlockName]);
    {error, Reason} -> 
      io:format("Error: ~p creating block ~s ~n", [Reason, BlockName])
  end,
  create_blocks(RemainingBlockValuesList).    


% Process the save blocks command
ui_save_blocks(Params) ->
  %% Write the block values to a configuration file
  % TODO:  Add LinkBlox specific header to config file
   case check_num_params(Params, 1) of
    low -> io:format("Error: Enter file name~n");
    ok ->
      FileName = Params,
      % TODO: Check for existence

      BlockValuesList = block_values(),
            
      Clean = fun(BlockValues) -> clean_block_values(BlockValues) end,

      CleanedBlockValuesList = lists:map(Clean, BlockValuesList),

      Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
      Text = lists:map(Format, CleanedBlockValuesList),
            
      case file:write_file(FileName, Text) of
        ok -> 
          io:format("Blocks file: ~s saved~n", [FileName]);
        {error, Reason} ->
          io:format("Error: ~p saving blocks file: ~s~~n", [Reason, FileName])
      end;
 
    high -> io:format("Error: Too many parameters~n")
  end.

    
% Clean block values of Output and Private values
% To make the block values suitable for saving to a file 

clean_block_values({Config, Inputs, Outputs, _Private}) ->

  CleanInput = fun({ValueName, Value, Link}) ->
                  case Link of
                    ?EMPTY_LINK -> 
                      % fixed value, do nothing
                      {ValueName, Value, ?EMPTY_LINK};
                    {_NodeName, _BlockName, _ValueName} ->
                      % Linked to another block, set value to empty, before saving
                      {ValueName, empty, Link}
                   end
                 end,
                    
  NewInputs = lists:map(CleanInput, Inputs),
        
  % Set all output values to 'not_active' and delete any block link references
  CleanOutput = fun({ValueName, _Value, _LinkedBlocks}) ->
                      {ValueName, not_active, []} end,
                    
  NewOutputs = lists:map(CleanOutput, Outputs),
  
  
  % Private attributes are not saved in persistent storage
  % Just remove them
  % Cleaned block values
  {Config, NewInputs, NewOutputs}.    


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
      io:format("Error: Enter Node Name or local~n");

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


% Process the help command
ui_help(_Params) ->
  io:format("Not Implemented~n").

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
validate_block_name(Params) ->
  case check_num_params(Params, 1) of
    low ->
      io:format("Error: enter block-name~n"),
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
%% Get the list of block values for all of the blocks currently running
%%
block_values() ->
  block_values(linkblox_api:get_block_names(get_node()), []).
    
block_values([], BlockValuesList) -> 
  BlockValuesList;
 
block_values(BlockNames, BlockValuesList) ->
  [BlockName | RemainingBlockNames] = BlockNames,
  BlockValues = linkblox_api:get_block(get_node(), BlockName),
  block_values(RemainingBlockNames, [BlockValues | BlockValuesList]).


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
  case string:to_float(ValueStr) of
    {error, no_float} ->
      case string:to_integer(ValueStr) of
        {error, no_integer} ->
          case list_to_atom(ValueStr) of
            true       -> true;
            false      -> false;
            empty      -> empty;
            not_active -> not_active;
                     _ -> ValueStr
          end;
        {Integer, []} -> Integer;

        {_Integer, _Rest} -> ValueStr
      end;

    {Float, []} -> Float;

    {_Float, _Rest} ->  ValueStr  
  end.

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
  ExpectedResult = "TestString",
  ValueStr = "TestString",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

% ====================================================================

-endif.