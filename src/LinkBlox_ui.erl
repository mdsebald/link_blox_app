%%%
%%% @doc 
%%% User Interface for LinkBlox app.
%%% @end
%%%

-module('LinkBlox_ui').

-author("Mark Sebald").


%% ====================================================================
%% API functions
%% ====================================================================
-export([block_names/0, block_status/0, ui_loop/0]).


%%
%%  UI input loop
%%
ui_loop() ->
    InputStr = io:get_line("LinkBlox> "),
    case InputStr of
        "status"   -> block_status();
        UnknownCmd -> io:format("Unknown Command: ~p entered ~n", UnknownCmd)
    end,
    ui_loop().

%% 
%% get the  block names of currently running processes
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

