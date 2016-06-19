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
-export([block_status/0, ui_init/0]).

%%
%% Initialize node, before entering the UI loop
%%
ui_init() ->
  % setup ets store for current node
  ets:new(node_store, [set, named_table]),
  % default node to local node
  set_node(node()),

  io:format("   W E L C O M E  T O  L i n k B l o x !~n~n"),

  % enter UI loop, and never return
  ui_loop().


%%
%% Get the current node this UI is connected to
%%
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
ui_loop() ->
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
        "execute"   -> ui_execute_block(Params);
        "delete"    -> ui_delete_block(Params);
        "disable"   -> ui_disable_block(Params);
        "enable"    -> ui_enable_block(Params);
        "freeze"    -> ui_freeze_block(Params);
        "thaw"      -> ui_thaw_block(Params);
        "get"       -> ui_get_values(Params);
        "set"       -> ui_set_value(Params);
        "link"      -> ui_link_blocks(Params);
        "add"       -> ui_add_attrib(Params);
        "remove"    -> ui_remove_attrib(Params);
        "status"    -> ui_status(Params);
        "types"     -> ui_block_types(Params);
        "load"      -> ui_load_blocks(Params);
        "save"      -> ui_save_blocks(Params);
        "node"      -> ui_node(Params);
        "nodes"     -> ui_nodes(Params);
        "connect"   -> ui_connect(Params);
        "help"      -> ui_help(Params);
            
        _Unknown    -> io:format("Error: Unknown command: ~p~n", [Raw3])
      end,
      ui_loop();  % processed command keep looping

    true -> % user entered "exit", stop looping
      ok
    end;
  true -> % user just hit "Enter", keep looping 
    ui_loop()
  end.
  

% Process block create command
ui_create_block(Params) ->
  case length(Params) of  
    0 -> io:format("Error: Enter block type and name~n");
    1 -> io:format("Error: Enter block type and name~n");
    2 -> 
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
    _ -> io:format("Error: Too many parameters~n")
  end.    


% Process manual block execute command
ui_execute_block(Params) ->
   case length(Params) of  
    0 -> io:format("Error: Enter block name~n");
    1 -> 
      [BlockNameStr] = Params,
      case linkblox_api:execute_block(get_node(), BlockNameStr) of
        ok -> 
          ok;
        {error, block_not_found} ->
          io:format("Error: block ~s not found~n", [BlockNameStr])  
      end;
     _ -> io:format("Error: Too many parameters~n")
  end.


% Process block delete command
ui_delete_block(Params) ->
  % TODO: Add delete all command to delete all blocks at once
  % TODO: Ask the user if they really want to delete
   case length(Params) of  
    0 -> io:format("Error: Enter block name~n");
    1 -> 
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),  
      case linkblox_api:delete_block(get_node(), BlockName) of
        ok -> 
          io:format("~p Deleted~n", [BlockNameStr]);
            
        {error, Reason} ->
          io:format("Error: ~p deleting ~p~n", [Reason, BlockNameStr]) 
      end;
    _ -> io:format("Error: Too many parameters~n")
  end.
    
    
% Process disable block command
ui_disable_block(Params) ->
  case validate_block_name(Params) of
    error     -> ok;  % Params was not a block name
    BlockNameStr ->  
      block_server:set_value(BlockNameStr, disable, true),
      ok
  end. 


% Process enable block command
ui_enable_block(Params) ->
  case validate_block_name(Params) of
    error     -> ok;  % Params was not a block name
    BlockNameStr ->  
      block_server:set_value(BlockNameStr, disable, false),
      ok
  end. 
    
    
% Process freeze block command
ui_freeze_block(Params) ->
  case validate_block_name(Params) of
    error     -> ok;  % Params was not a block name
    BlockNameStr ->  
      block_server:set_value(BlockNameStr, freeze, true),
      ok
  end. 

    
% Process thaw block command
ui_thaw_block(Params) ->
  case validate_block_name(Params) of
    error     -> ok;  % Params was not a block name
    BlockNameStr ->  
      block_server:set_value(BlockNameStr, freeze, false),
      ok
  end. 


% Process block status command
ui_status(Params) ->
  case length(Params) of  
    0 -> block_status();

    _ -> io:format("Error: Too many parameters~n")
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
      case str_to_value_id(ValueIdStr) of
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
  case length(Params) of  
    0 -> io:format("Error: Enter block-name value-name value~n");
    1 -> io:format("Error: Enter block-name value-name value~n");
    2 -> io:format("Error: Enter block-name value-name value~n");
    3 -> 
      [BlockNameStr, ValueNameStr, ValueStr] = Params,

      case linkblox_api:is_block_name(get_node(), BlockNameStr) of
        true -> 
          ValueName = list_to_atom(ValueNameStr),
          BlockName = list_to_atom(BlockNameStr),
          case block_server:get_value(BlockName, ValueName) of
            not_found ->
              io:format("Error: ~s is not a value of block ~s~n", 
                            [ValueNameStr, BlockNameStr]);
            _CurrentValue ->
              NewValue = list_to_atom(ValueStr),
              block_server:set_value(BlockName, ValueName, NewValue)        
          end;
        false -> 
          io:format("Error: Block ~s does not exist~n", [BlockNameStr])
      end;
    _ -> io:format("Error: Too many parameters~n")
  end.    


% Process link blocks command
ui_link_blocks(_Params) ->
  io:format("Not Implemented~n").
    
% Process add attribute command
ui_add_attrib(_Params) ->
  io:format("Not Implemented~n").
    
% Process remove attribute command
ui_remove_attrib(_Params) ->
  io:format("Not Implemented~n").

% Process the load blocks command
ui_load_blocks(Params) ->
  %% Read a set of block values from a config file
  % TODO: Check for existence and validity
  case length(Params) of
    0 -> io:format("Error: Enter file name~n");
    1 ->
      FileName = Params,
	    case file:consult(FileName) of
    	  {ok, BlockValuesList} ->
			    create_blocks(BlockValuesList);
		    {error, Reason} ->
			    io:format("Error: ~p loading blocks file: ~p~n", [Reason, FileName])
      end;

    _ -> io:format("Error: Too many parameters~n")
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
   case length(Params) of
    0 -> io:format("Error: Enter file name~n");
    1 ->
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
 
    _ -> io:format("Error: Too many parameters~n")
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
  case length(Params) of
    0 ->
      io:format("Error: Enter Node Name or local~n");

    1 ->
      case Params of
        ["local"] ->  % Just connect to the local node
          io:format("Connecting to local node~n"),
          connect_to_node(node());

        [NodeStr] ->   
          Node = list_to_atom(NodeStr),
          connect_to_node(Node)
      end;
    _ ->
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
    
  io:fwrite("~-16s ~-16s ~-12w ~-12w ~-12w ~-15s~n", 
            [string:left(BlockTypeStr, 16), 
             string:left(atom_to_list(BlockName), 16), 
             Value, Status, ExecMethod, LastExecuted]),
  block_status(RemainingBlockNames).


% validate Params is one valid block name
validate_block_name(Params) ->
  case length(Params) of
    1 ->
      [BlockNameStr] = Params,
      % check if block name is an existing block
      case linkblox_api:is_block_name(get_node(), BlockNameStr) of
        true  -> BlockNameStr;
        false ->
          io:format("Error: Block ~p does not exist~n", [BlockNameStr]),
          error
      end;
    0 ->
      io:format("Error: No block name specified~n"),
      error;
    _ ->
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
%% Convert a string to a value id. 
%% Need to handle the case of a value name string and
%% the case of a value name string plus an array index
%% Examples:
%%   "value"  -> value (atom type)
%%   "digit[1]"  -> {digit, 1} (atom and array index tuple)
%%
-spec str_to_value_id(ValueIdStr :: string()) -> {ok, value_id()} | {error, invalid}.

str_to_value_id(ValueIdStr) ->
  LeftBracket = string:chr(ValueIdStr, $[),
  if LeftBracket > 0 ->
    RightBracket = string:rchr(ValueIdStr, $]),
    if RightBracket > (LeftBracket + 1) ->
      ArrayIndexStr = string:substr(ValueIdStr, LeftBracket + 1, 
                                     RightBracket - LeftBracket - 1),
      case string:to_integer(ArrayIndexStr) of
        {error, _} -> 
          {error, invalid};

        {ArrayIndex, Rest} ->
          if length(Rest) == 0 ->
            if ArrayIndex > 0 -> 
              ValueNameStr = string:substr(ValueIdStr, 1, LeftBracket-1),
              ValueName = list_to_atom(ValueNameStr),
              {ok, {ValueName, ArrayIndex}};
            true -> % ArrayIndex =< 0
              {error, invalid}
            end;
          true -> % There are extra characters after the digit(s)
            {error, invalid}
          end
      end;

    true -> % left bracket found, without a corresponding right bracket
      {error, invalid}
    end;

  true ->  % No left bracket found, assume a non array value id
    {ok, list_to_atom(ValueIdStr)}
  end.

%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% ====================================================================
% Test str_to_value_id()
% 
%   Test good non-array value id
str_to_value_id_valuename_valid_test() ->
  Result = str_to_value_id("good_value"),
  ExpectedResult = {ok, good_value},
  ?assertEqual(ExpectedResult, Result).

%   Test good array value id 1
str_to_value_id_valuename_array_valid1_test() ->
  Result = str_to_value_id("good_array[10]"),
  ExpectedResult = {ok, {good_array, 10}},
  ?assertEqual(ExpectedResult, Result).

%   Test good array value id 2
str_to_value_id_valuename_array_valid2_test() ->
  Result = str_to_value_id("good_array[010]"),
  ExpectedResult = {ok, {good_array, 10}},
  ?assertEqual(ExpectedResult, Result).

%   Test bad array value id 2
str_to_value_id_valuename_array_invalid2_test() ->
  Result = str_to_value_id("bad_array[0]"),
  ExpectedResult = {error, invalid},
  ?assertEqual(ExpectedResult, Result).

%   Test bad array value id 3
str_to_value_id_valuename_array_invalid3_test() ->
  Result = str_to_value_id("bad_array[-123]"),
  ExpectedResult = {error, invalid},
  ?assertEqual(ExpectedResult, Result).

%   Test bad array value id 4
str_to_value_id_valuename_array_invalid4_test() ->
  Result = str_to_value_id("bad_array[12.34]"),
  ExpectedResult = {error, invalid},
  ?assertEqual(ExpectedResult, Result).

-endif.