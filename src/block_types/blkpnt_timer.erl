%% @author Mark Sebald
%% @doc Timer block function


-module(blkpnt_timer).


%% ====================================================================
%% API functions
%% ====================================================================
-export([initiate/1, evaluate/1, terminate/1, create_values/1, type_name/0, version/0]).


initiate(BlockValues) ->
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	
	io:format("~p Type: Timer, initiate() ~n", [BlockName]),
    
    % TODO: Only do this if ExecuteTimer is > 0
    ExecuteTimerValue = blkpnt_utils:get_param_value(Params, 'Timeout'),
    
    % Setup block to be executed after timer expires
    case timer:apply_after(ExecuteTimerValue, 'BlockPoint_srv', execute, [BlockName]) of
        {ok, TimerReferenceValue} -> 
            NewInterms = blkpnt_utils:set_interm_value(Interms, 'TimerReference', TimerReferenceValue);
         
        {error, Reason} -> 
            NewInterms= blkpnt_utils:set_interm_value(Interms, 'TimerReference', empty),
            io:format("Error: ~p Setting execute timer ~n", [Reason])
    end,

	{BlockName, BlockModule, Params, Inputs, Outputs, NewInterms}.


evaluate(BlockValues) ->
	
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,

	%io:format("~p Type: Timer, evaluate() ~n", [BlockName]),

	case blkpnt_utils:get_input_value(Inputs, 'Enable') of
		true ->
		    case blkpnt_utils:get_output_value(Outputs, 'Value') of
                true -> 	
                    NewOutputs = blkpnt_utils:update_common_outputs(Outputs, false, normal);
                false ->
                	NewOutputs = blkpnt_utils:update_common_outputs(Outputs, true, normal);
                not_active ->
                    NewOutputs = blkpnt_utils:update_common_outputs(Outputs, true, normal)
            end;               

		false ->	% Block is Disabled
			NewOutputs = blkpnt_utils:update_common_outputs(Outputs, not_active, disabled)
	end,
    
    % TODO: Make this a common function
    % TODO: Only do this if ExecuteTimer is > 0
    ExecuteTimerValue = blkpnt_utils:get_param_value(Params, 'Timeout'),

    % Reset the execution timer
    case timer:apply_after(ExecuteTimerValue, 'BlockPoint_srv', execute, [BlockName]) of
        {ok, TimerReferenceValue} -> 
            NewInterms = blkpnt_utils:set_interm_value(Interms, 'TimerReference', TimerReferenceValue);
         
        {error, Reason} -> 
            NewInterms= blkpnt_utils:set_interm_value(Interms, 'TimerReference', empty),
            io:format("Error: ~p Setting execute timer", [Reason])
    end,

    {BlockName, BlockModule, Params, Inputs, NewOutputs, NewInterms}.

	
terminate(BlockValues) ->
	{BlockName, _BlockModule, _Params, _Inputs, _Outputs, _IntTerms} = BlockValues,
    % TODO cancel the timer, if created
	io:format("~p Type: Timer, terminate() ~n", [BlockName]).


create_values(BlockName)->
	BlockValues = blkpnt_setup:common_values(BlockName, ?MODULE),
	{BlockName, BlockModule, Params, Inputs, Outputs, Interms} = BlockValues,
	
	NewParams = [{'Timeout', 500} | Params],
    NewInterms = [{'TimerReference', empty} | Interms],

	{BlockName, BlockModule, NewParams, Inputs, Outputs, NewInterms}.
 


type_name()-> 'Timer'.

version() -> "0.1.0".

%% ====================================================================
%% Internal functions
%% ====================================================================
