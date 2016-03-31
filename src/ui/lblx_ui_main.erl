%%%
%%% @doc 
%%% User Interface for LinkBlox app.
%%% @end
%%%

-module(lblx_ui_main).

-author("Mark Sebald").

-include("../block_state.hrl"). 


%% ====================================================================
%% API functions
%% ====================================================================
-export([block_status/0, ui_loop/0]).


%%
%%  UI input loop
%%

ui_loop() ->
  Raw1 = io:get_line("LinkBlox> "),
  Raw2 = string:strip(Raw1, right, 10), % Remove new line char
  Raw3 = string:strip(Raw2), % Remove leading and trailing whitespace
    
  % Split up the string into command and parameter words
  CmdAndParams = string:tokens(Raw3, " "),  
    
  if 0 < length(CmdAndParams) ->
    [Cmd | Params] = CmdAndParams,
    CmdLcase = string:to_lower(Cmd),
        
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
      "help"      -> ui_help(Params);
      "exit"      -> ui_exit(Params);
            
      _Unknown    -> io:format("Error: Unknown command: ~p~n", [Raw3])
    end;
  true -> ok
  end,
  ui_loop().


% Process block create command
ui_create_block(Params) ->
  case length(Params) of  
    0 -> io:format("Error: Enter block type and name~n");
    1 -> io:format("Error: Enter block type and name~n");
    2 -> 
      [BlockTypeStr, BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      case is_block_type(BlockTypeStr) of
        true ->
          BlockModule = lblx_types:block_type_to_module(BlockTypeStr), 
          case is_block_name(BlockName) of
            false ->
              % TODO: Add a parameter for the block comment
              BlockValues = BlockModule:create(BlockName, "Test Comment"),
              case block_supervisor:create_block(BlockValues) of
                {ok, _Pid} -> 
                  io:format("Block ~s:~s Created~n", [BlockTypeStr, BlockNameStr]);
                {error, Reason} -> 
                  io:format("Error: ~p creating block ~s:~s ~n", [Reason, BlockTypeStr, BlockNameStr])
              end;
            true ->
              io:format("Error: Block ~s already exists~n", [BlockNameStr])
            end;
        false -> io:format("Error: Block type ~s is not a valid block type~n", [BlockTypeStr])
      end;
    _ -> io:format("Error: Too many parameters~n")
  end.    


% Process manual block execute command
ui_execute_block(Params) ->
  case validate_block_name(Params) of
    error     -> ok;  % Params was not a block name
    BlockName ->
      block_server:execute(BlockName),
      ok
  end.


% Process block delete command
ui_delete_block(Params) ->
  % TODO: Add delete * command to delete all blocks at once
  case validate_block_name(Params) of
    error     -> ok;  % Params was not a block name
    BlockName ->
      % TODO: Ask the user if they really want to delete
      case block_supervisor:delete_block(BlockName) of
        ok -> 
          io:format("~p Deleted~n", [BlockName]),
          ok;
            
        {error, Reason} ->
          io:format("Error: ~p deleting ~p~n", [Reason, BlockName]) 
      end
  end.
    
    
% Process disable block command
ui_disable_block(Params) ->
  case validate_block_name(Params) of
    error     -> ok;  % Params was not a block name
    BlockName ->  
      block_server:set_value(BlockName, disable, true),
      ok
  end. 


% Process enable block command
ui_enable_block(Params) ->
  case validate_block_name(Params) of
    error     -> ok;  % Params was not a block name
    BlockName ->  
      block_server:set_value(BlockName, disable, false),
      ok
  end. 
    
    
% Process freeze block command
ui_freeze_block(Params) ->
  case validate_block_name(Params) of
    error     -> ok;  % Params was not a block name
    BlockName ->  
      block_server:set_value(BlockName, freeze, true),
      ok
  end. 

    
% Process thaw block command
ui_thaw_block(Params) ->
  case validate_block_name(Params) of
    error     -> ok;  % Params was not a block name
    BlockName ->  
      block_server:set_value(BlockName, freeze, false),
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
      BlockName = list_to_atom(BlockNameStr),

      case is_block_name(BlockName) of
        true -> 
          BlockValues = block_server:get_values(BlockName),
          io:format("~n~p~n", [BlockValues]);
        false -> 
          io:format("Error: Block ~s does not exist~n", [BlockNameStr])
        end;

      2 -> 
        [BlockNameStr, ValueNameStr] = Params,
        BlockName = list_to_atom(BlockNameStr),
            
        case is_block_name(BlockName) of
          true -> 
            ValueName = list_to_atom(ValueNameStr),
            case block_server:get_value(BlockName, ValueName) of
              not_found ->
                io:format("Error: ~s is not a value of block ~s~n", 
                                      [ValueNameStr, BlockNameStr]);
              CurrentValue ->
                io:format("~n~p~n", [CurrentValue])        
            end;
          false -> 
            io:format("Error: Block ~s does not exist~n", [BlockNameStr])
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
      BlockName = list_to_atom(BlockNameStr),

      case is_block_name(BlockName) of
        true -> 
          ValueName = list_to_atom(ValueNameStr),
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
  BlockName = block_utils:name(Config),
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
  % TODO:  Add BlockPoint specific header to config file
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


% Process the help command
ui_help(_Params) ->
  io:format("Not Implemented~n").


% Process exit command
ui_exit(_Params) ->
  io:format("Not Implemented~n").


%%
%% Display status off each running block
%%
block_status() ->
  io:fwrite("~n~-16s ~-16s ~-12s ~-12s ~-12s ~-15s~n", 
            ["Block Type", "Block Name", "Output", "Status", "Exec Method", "Last Exec"]),
  io:fwrite("~16c ~16c ~12c ~12c ~12c ~15c~n", [$-, $-, $-, $-, $-, $-] ), 
  block_status(block_supervisor:block_names()).
    
block_status([]) ->
  io:format("~n"), 
  ok;

block_status([BlockName | RemainingBlockNames]) ->
  BlockType = block_server:get_value(BlockName, block_type),
  Value = block_server:get_value(BlockName, value),
  Status = block_server:get_value(BlockName, status),
  ExecMethod = block_server:get_value(BlockName, exec_method),
  
  case block_server:get_value(BlockName, last_exec) of 
    not_active ->
      LastExecuted = "not_active";
    {Hour, Minute, Second, Micro} ->
      LastExecuted = io_lib:format("~2w:~2..0w:~2..0w.~6..0w", 
                                    [Hour,Minute,Second,Micro]);
    _ ->
      LastExecuted = "undef last_exec val"
  end,  
    
  io:fwrite("~-16s ~-16s ~-12w ~-12w ~-12w ~-15s~n", 
            [string:left(BlockType, 16), 
             string:left(io_lib:write(BlockName), 16), 
             Value, Status, ExecMethod, LastExecuted]),
  block_status(RemainingBlockNames).


% validate Params is one valid block name
validate_block_name(Params) ->
  case length(Params) of
    1 ->
      [BlockNameStr] = Params,
      BlockName = list_to_atom(BlockNameStr),
      % check if block name is an existing block
      case is_block_name(BlockName) of
        true  -> BlockName;
        false ->
          io:format("Error: Block ~p does not exist~n", [BlockName]),
          error
      end;
    0 ->
      io:format("Error: No block name specified~n"),
      error;
    _ ->
      io:format("Error: Too many parameters~n"),
      error
  end.


% Is block name  an existing block
is_block_name(BlockName) -> 
  lists:member(BlockName, block_supervisor:block_names()).

% Is block type an existing block type
is_block_type(BlockTypeStr) -> 
  lists:member(BlockTypeStr, lblx_types:block_type_names()).


%%
%% Get the list of block values for all of the blocks currently running
%%
block_values() ->
  block_values(block_supervisor:block_names(), []).
    
block_values([], BlockValuesList) -> 
  BlockValuesList;
 
block_values(BlockNames, BlockValuesList) ->
  [BlockName | RemainingBlockNames] = BlockNames,
  BlockValues = block_server:get_values(BlockName),
  block_values(RemainingBlockNames, [BlockValues | BlockValuesList]).


%%
%% Get list of the block type names and versions
%%
ui_block_types(_Params) ->
  BlockTypes = lblx_types:block_types_info(),
   
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
        
    
    
%get_module_attribute(Module,Attribute) ->
%
%    case beam_lib:chunks(Module, [attributes]) of
%
%        { ok, { _, [ {attributes,Attributes} ] } } ->
%            case lists:keysearch(Attribute, 1, Attributes) of
%                { value, {Attribute,[Value]} } -> Value;
%                false                          -> { error, no_such_attribute }
%            end;
%
%        { error, beam_lib, { file_error, _, enoent} } ->
%            { error, no_such_module }
%
%    end.     
