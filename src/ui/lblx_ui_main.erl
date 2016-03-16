%%%
%%% @doc 
%%% User Interface for LinkBlox app.
%%% @end
%%%

-module('lblx_ui_main').

-author("Mark Sebald").


%% ====================================================================
%% API functions
%% ====================================================================
-export([block_names/0, block_status/0, ui_loop/0]).


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
            "execute"   -> ui_execute_block(Params);
            "delete"    -> ui_delete_block(Params);
            "disable"   -> ui_disable_block(Params);
            "enable"    -> ui_enable_block(Params);
            "freeze"    -> ui_freeze_block(Params);
            "thaw"      -> ui_thaw_block(Params);
            "set"       -> ui_set_value(Params);
            "status"    -> ui_status(Params);
            "types"     -> block_types();
            
            _Unknown    -> io:format("Error: Unknown command: ~p~n", [Raw3])
        end,
        ui_loop();
    true ->
        ui_loop()
    end.


% Process manual block execute command
 ui_execute_block(Params) ->
    case is_block_name(Params) of
        error     -> ok;  % Params was not a block name
        BlockName ->
             block_server:execute(BlockName),
            ok
    end.


% Process block delete command
 ui_delete_block(Params) ->
    case is_block_name(Params) of
        error     -> ok;  % Params was not a block name
        BlockName ->
            % TODO: Ask the user if they really want to delete
            block_server:delete(BlockName),
            ok
    end.
    
    
% Process disable block command
ui_disable_block(Params) ->
    case is_block_name(Params) of
        error     -> ok;  % Params was not a block name
        BlockName ->  
            block_server:set_value(BlockName, disable, true),
            ok
    end. 


% Process enable block command
ui_enable_block(Params) ->
    case is_block_name(Params) of
        error     -> ok;  % Params was not a block name
        BlockName ->  
            block_server:set_value(BlockName, disable, false),
            ok
    end. 
    
    
% Process freeze block command
ui_freeze_block(Params) ->
    case is_block_name(Params) of
        error     -> ok;  % Params was not a block name
        BlockName ->  
            block_server:set_value(BlockName, freeze, true),
            ok
    end. 

    
% Process thaw block command
ui_thaw_block(Params) ->
    case is_block_name(Params) of
        error     -> ok;  % Params was not a block name
        BlockName ->  
            block_server:set_value(BlockName, freeze, false),
            ok
    end. 


% Process block status command
ui_status(Params) ->
    if length(Params) == 0 ->  
        block_status();
    true ->
        io:format("Error: Extraneous parameters in status command~n") 
    end.
 
 % Process the set value command
ui_set_value(_Params) -> ok.
    
%%
%% Display status off each running block
%%
block_status() ->
    io:fwrite("~n~-16s ~-16s ~-12s ~-12s ~-12s ~-15s~n", 
                  ["Block Type", "Block Name", "Output", "Status", "Exec Method", "Last Exec"]),
    io:fwrite("~16c ~16c ~12c ~12c ~12c ~15c~n", [$-, $-, $-, $-, $-, $-] ), 
    block_status(block_names()).
    
block_status([]) ->
    io:format("~n"), 
    ok;

block_status([BlockName | RemainingBlockNames]) ->
    BlockType = block_server:get_value(BlockName, block_type),
    Value = block_server:get_value(BlockName, value),
    Status = block_server:get_value(BlockName, status),
    ExecMethod = block_server:get_value(BlockName, exec_method),
    {Hour, Minute, Second, Micro} = block_server:get_value(BlockName, last_exec),
    
    LastExecuted = io_lib:format("~2w:~2..0w:~2..0w.~6..0w", [Hour,Minute,Second,Micro]),
    
    io:fwrite("~-16s ~-16s ~-12w ~-12w ~-12w ~-15s~n", 
              [io_lib:write(BlockType), io_lib:write(BlockName), 
               Value, Status, ExecMethod, LastExecuted]),
    block_status(RemainingBlockNames).


% validate a block name
is_block_name(Params) ->
    case length(Params) of
        1 ->
            [BlockNameStr] = Params,
            BlockName = list_to_atom(BlockNameStr),
            % Validate block name is an existing block
            case lists:member(BlockName, block_names()) of
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


%% 
%% get the block names of currently running processes
%%
block_names() -> 
    block_names(block_supervisor:block_processes(), []).    
    
block_names([], BlockNames) -> 
    BlockNames;
    
block_names([BlockProcess | RemainingProcesses], BlockNames) ->
    % Only return block names of processes that are running
    case element(2, BlockProcess) of
        restarting -> NewBlockNames = BlockNames;
        undefined  -> NewBlockNames = BlockNames;
        _Pid       ->
            BlockName = element(1, BlockProcess),
            NewBlockNames = [BlockName | BlockNames]
    end,
    block_names(RemainingProcesses, NewBlockNames).


% Get list of the block type names
block_types() ->
    erlang:loaded().


