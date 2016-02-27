%%
%% @author Mark
%% @doc Block functions common to all block types 
%% 


-module(block_common).

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([configs/3, inputs/0, outputs/0, private/0, initialize/3, delete/2]).


%%
%% Common Config Attributes
%%
configs(Name, Type, Version) ->
    Config = 
    [ 
      {block_name, Name},
	  {block_type, Type},
	  {version, Version}
    ],
    
    log_state("Creating", Config),
    Config.


%%
%% Common Input Attributes
%%
inputs() ->
    [
      {enable, true, ?EMPTY_LINK},      % Block will execute as long as enable input is true
      
      {execute_in, empty, ?EMPTY_LINK},     % Link to block allowed execute this block.
                                                % If this list contains one or more block names, 
                                                % then execute only on receiving an execute command message
                                                % i.e. Implement Control Flow 
      
      {execute_interval, 0, ?EMPTY_LINK}   % If > 0, execute block every 'execute_interval' milliseconds.
                                                % This is used to execute a block at fixed intervals
                                                % instead of being executed by other blocks.
                                                % or executed on change of input value  
    ].
    
    
%%
%% Common Output Attributes
%%
outputs() ->
    [ 
      {execute_out, false, []},
      {status, created, []},
      {value, not_active, []}
    ].
    
    
%%
%% Common Private Attributes
%%
private() ->
    [ 
      {exec_count, 0},
	  {last_exec, not_active},
      {timer_ref, empty}
    ].
       



%%
%% Common block initialization funcion
%% TODO: Fold this function into custom block code, use or block set timer function from block server module

initialize(Config, Inputs, Private) ->

    log_state("Initializing", Config),
    setup_execute_timer(Config, Inputs, Private).

%%
%%  Common block delete function
%%
delete(Config, Private) ->
    log_state("Deleting", Config),    
    % Cancel execution timer if it exists
    TimerReferenceValue = block_utils:get_value(Private, timer_ref),
    case TimerReferenceValue of
        empty -> empty;
        
        _ ->  timer:cancel(TimerReferenceValue)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

setup_execute_timer(Config, Inputs, Private) ->
    ExecuteInterval = block_utils:get_value(Inputs, execute_interval),
    if (ExecuteInterval > 0 ) ->
        BlockName = block_utils:get_value(Config, block_name),
        % Setup timer to execute block after timer expires
        case timer:apply_after(ExecuteInterval, block_server, timer_execute, [BlockName]) of
            {ok, TimerReferenceValue} -> 
                NewPrivate = block_utils:set_value(Private, timer_ref, TimerReferenceValue);
         
            {error, Reason} -> 
                NewPrivate = block_utils:set_value(Private, timer_ref, empty),
                io:format("~p Error: ~p Setting execution timer ~n", [BlockName, Reason])
        end;
    true ->  % Timeout value is less than or equal to zero, don't set up execution via timer
        NewPrivate = Private
    end,
    NewPrivate.

%% print the indicated state to the shell
log_state (State, Config) ->
    BlockName = block_utils:get_value(Config, block_name),
    BlockType = block_utils:get_value(Config, block_type),
    
    io:format("~s: ~p Type: ~p~n", [State, BlockName, BlockType]).
    