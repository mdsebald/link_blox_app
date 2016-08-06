%% @doc 
%%% Handle block links     
%%%               
%%% @end 

-module(link_utils).

-author("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
          link_blocks/2,
          evaluate_link/5, 
          unlink_inputs/2,
          empty_linked_inputs/1,
          unlink_input/3,
          unlink_output/3,
          add_ref/3,
          delete_ref/3,
          update_linked_input_values/3
]).


%%
%% Send a link message to each block linked to the inputs of this block
%%
-spec link_blocks(BlockName :: block_name(),
                  Inputs :: list(input_attr())) -> list(input_attr()).

link_blocks(BlockName, Inputs)->
  link_blocks(BlockName, Inputs, Inputs).

link_blocks(_BlockName, [], UpdatedInputs)->
  UpdatedInputs;

link_blocks(BlockName, [Input | RemainingInputs], UpdatedInputs) ->

  case Input of	
    % Non-array value
    {ValueName, {Value, Link}} ->
      NewUpdatedInputs = evaluate_link(BlockName, ValueName, Value, Link, UpdatedInputs);
      
    % Array value  
    {ValueName, ArrayValues} ->
      NewUpdatedInputs = 
          process_array_input(BlockName, ValueName, 1, ArrayValues, UpdatedInputs)
  end,                 
  link_blocks(BlockName, RemainingInputs, NewUpdatedInputs).

%%
%%  Check the link on each value of an input array value
%%
-spec process_array_input(BlockName :: block_name(),
                          ValueName :: value_name(),
                          ArrayIndex :: pos_integer(),
                          ArrayValues :: attr_value_array(),
                          UpdatedInputs :: list(input_attr())) -> list(input_attr()).

process_array_input(_BlockName, _ValueName, _ArrayIndex, [], UpdatedInputs) ->
  UpdatedInputs;
  
process_array_input(BlockName, ValueName, ArrayIndex, [ArrayValue| RemainingArrayValues], Inputs) ->
  {Value, Link} = ArrayValue,
  UpdatedInputs = evaluate_link(BlockName, {ValueName, ArrayIndex}, Value, Link, Inputs),
  process_array_input(BlockName, ValueName, (ArrayIndex+1), RemainingArrayValues, UpdatedInputs).


%%
%% Evaluate the link on this input attribute, and decide what to do
%%
-spec evaluate_link(BlockName ::block_name(), 
                    ValueId :: value_id(),
                    Value :: value(),
                    Link :: input_link(), 
                    Inputs :: list(input_attr())) -> list(input_attr()).
                  
evaluate_link(BlockName, ValueId, Value, Link, Inputs) ->

  case Link of
    ?EMPTY_LINK ->
      % Input is not linked to another block, nothing to do
      Inputs;
      
    {LinkBlockName, LinkValueId} ->
      case whereis(LinkBlockName) of
        undefined  ->
          % Log warning, user may have misspelled the Linked block name
          error_logger:warning_msg("Linked Block: ~p Does not exist.~n", 
                        [LinkBlockName]),

          % If linked block is not running, input value should be empty
          case attrib_utils:set_value(Inputs, ValueId, empty) of
            {ok, UpdatedInputs} -> UpdatedInputs;

            _ -> Inputs % Set value failed, return Inputs, unchanged
          end;

        _Pid  ->
          % Linked block is running,
          % if the block input value is empty, 
          if Value == empty ->
            % send a link message to the linked block, get current linked value back
            UpdatedValue = block_server:link(LinkBlockName, LinkValueId, BlockName),
            
            {ok, UpdatedInputs} = 
                      attrib_utils:set_value(Inputs, ValueId, UpdatedValue),
            
            error_logger:info_msg("Block Input: ~p:~p Linked to Block Output: ~p:~p~n", 
                        [BlockName, ValueId, LinkBlockName, LinkValueId]),
                        
            UpdatedInputs;
          true ->
            % Value is not empty, link must already be established, nothing to do
            Inputs
          end
      end;
      
    %TODO: Handle other link types, i.e. links to other nodes      
    UnhandledLink -> 
      error_logger:error_msg("Unhandled Link Type: ~p~n", [UnhandledLink]),
      Inputs
  end.


%%
%% Send an unlink message to each block linked to the inputs of this block
%%
-spec unlink_inputs(BlockName :: block_name(),
                    Inputs :: list(input_attr())) -> ok.

unlink_inputs(_BlockName, [])-> ok;

unlink_inputs(BlockName, [Input | RemainingInputs])->

  case Input of
    % Non-array value
    {ValueName, {_Value, Link}} ->
      unlink_input(BlockName, ValueName, Link);
      
    % Array value  
    {ValueName, ArrayValues} ->
      unlink_array_input(BlockName, ValueName, ArrayValues)
  end,

  unlink_inputs(BlockName, RemainingInputs).


%%
%%  Unlink each value of an input array value
%%
-spec unlink_array_input(BlockName :: block_name(),
                         ValueName :: value_name(),
                         ArrayValues :: attr_value_array()) -> ok.

unlink_array_input(_BlockName, _ValueName, []) -> ok;
  
unlink_array_input(BlockName, ValueName, [ArrayValue | RemainingArrayValues]) ->
  {_Value, Link} = ArrayValue,
  unlink_input(BlockName, ValueName, Link),
  unlink_array_input(BlockName, ValueName, RemainingArrayValues).

  
%%
%% Unlink one input value link
%%
-spec unlink_input(BlockName :: block_name(),
                   ValueName :: value_name(),
                   Link :: input_link()) -> ok.  
  
unlink_input(BlockName, ValueName, Link) ->

  case Link of
    % Input is not linked, do nothing
    ?EMPTY_LINK -> ok;
       
    {LinkBlockName, LinkValueId} ->
      case whereis(LinkBlockName) of
        undefined  ->
          error_logger:warning_msg("unlink_input(): Linked Block: ~p Does not exist.~n", 
                                    [LinkBlockName]),
          ok;
        _Pid ->
          % Linked block is running
          error_logger:info_msg("Block Input: ~p:~p Unlinked from Block Output: ~p:~p~n", 
                            [BlockName, ValueName, LinkBlockName, LinkValueId]),
          block_server:unlink(LinkBlockName, LinkValueId, BlockName),
          ok
      end;

    %TODO: Handle other link types, i.e. links to other nodes      
    UnhandledLink -> 
      error_logger:error_msg("unlink_input(): Unhandled Link Type: ~p~n", [UnhandledLink]),
      ok
  end.


%%
%% Set the value of each input linked to another block, to 'empty'
%%
-spec empty_linked_inputs(Inputs :: list(input_attr())) -> list(input_attr()).

empty_linked_inputs(Inputs) ->
  lists:map(
    fun(Input) ->
      case Input of 
        {ValueName, {Value, Link}} ->
          case Link of
            ?EMPTY_LINK   -> {ValueName, {Value, Link}};
            _NonEmptyLink -> {ValueName, {empty, Link}}
          end;
        {ValueName, ArrayValues} ->
          {ValueName, empty_array_values(ArrayValues)}      
      end  
    end,
    Inputs).


%%
%% Set the value of each linked input in ArrayValues, to empty
%%
-spec empty_array_values(ArrayValues :: list(attr_value_array())) -> 
                          list(attr_value_array()).
                                
empty_array_values(ArrayValues) ->
  lists:map(
    fun({Value, Link}) -> 
      case Link of
        ?EMPTY_LINK   -> {Value, Link};
        _NonEmptyLink -> {empty, Link}
      end
    end, 
    ArrayValues).



%%
%% Set all input values linked to this output to 'empty'
%%
-spec unlink_output(BlockName :: block_name(),
                    ValueName :: value_name(),
                    Refs :: link_refs()) -> ok.  
  
unlink_output(BlockName, ValueName, Refs) ->

  block_common:update_linked_inputs(BlockName, ValueName, empty, Refs).
 

%%
%% Add 'ToBlockName' to the list of referenced blocks 
%% in the ValueName output attribute
%%
-spec add_ref(Outputs :: list(output_attr()), 
              ValueId :: value_id(), 
              ToBlockName :: block_name()) -> list(output_attr()).

add_ref(Outputs, ValueId, ToBlockName) ->

  case attrib_utils:get_attribute(Outputs, ValueId) of

    {error, not_found} ->
      % This block doesn't have an output 'ValueId'
      % Just return the original Outputs list
			error_logger:error_msg("add_ref() Error. ~p Doesn't exist for this block~n", 
                             [ValueId]),
			Outputs;
		
    % Non-array value
		{ok, {ValueName, {Value, Refs}}} ->

			% add 'ToBlockName' to list of block references for this output
      % ToBlockName can be added more than once, 
      % if multiple inputs of the same block are linked to this output.
      % Return updated Outputs list
			NewRefs = [ToBlockName | Refs],
			NewOutput = {ValueName, {Value, NewRefs}},
			attrib_utils:replace_attribute(Outputs, ValueName, NewOutput);

    % Array value
    {ok, {ValueName, ArrayValues}} ->
      % if this is an array value, the ValueName from get_attribute()
      % will match ValueName in the ValueId tuple
      {ValueName, ArrayIndex} = ValueId,
      if (0 < ArrayIndex) andalso (ArrayIndex =< length(ArrayValues)) ->
        {Value, Refs} = lists:nth(ArrayIndex, ArrayValues),
        NewRefs = [ToBlockName | Refs],
        NewArrayValue = {Value, NewRefs},
        NewArrayValues = attrib_utils:replace_array_value(ArrayValues, ArrayIndex, NewArrayValue),
        NewOutput = {ValueName, NewArrayValues}, 
        attrib_utils:replace_attribute(Outputs, ValueName, NewOutput);
      true ->
        error_logger:error_msg("add_ref() Error. Invalid array index ~p~n",
                             [ValueId]),
        Outputs
      end
  end.


%%
%% Delete 'ToBlockName' reference from the list of linked blocks 
%% in the ValueName output attribute
%%
-spec delete_ref(Outputs :: list(output_attr()), 
                 ValueId :: value_id(), 
                 ToBlockName :: block_name()) -> list(output_attr()).

delete_ref(Outputs, ValueId, ToBlockName) ->

  case attrib_utils:get_attribute(Outputs, ValueId) of

    {error, not_found} ->
      % This block doesn't have an output 'ValueId'
      % Just return the original Outputs list
      error_logger:error_msg("delete_ref() Error. ~p Doesn't exist for this block~n", 
                              [ValueId]),
      Outputs;
		
    % Non-Array value
    {ok, {ValueName, {Value, Refs}}} ->  
      % Delete the 'ToBlockName' from the list of linked blocks, if it exists
      % This deletes the first element matching ToBlockName, 
      % ToBlockName could be in the list more than once
      NewRefs = lists:delete(ToBlockName, Refs),
		  NewOutput = {ValueName, {Value, NewRefs}},
      attrib_utils:replace_attribute(Outputs, ValueName, NewOutput);

    % Array value  
    {ok, {ValueName, ArrayValues}} ->
      % if this is an array value, the ValueName from get_attribute()
      % will match ValueName in the ValueId tuple
      {ValueName, ArrayIndex} = ValueId,
      if (0 < ArrayIndex) andalso (ArrayIndex =< length(ArrayValues)) ->
        {Value, Refs} = lists:nth(ArrayIndex, ArrayValues),
        % Delete the 'ToBlockName' from the list of linked blocks, if it exists
        % This deletes the first element matching ToBlockName, 
        % ToBlockName could be in the list more than once
        NewRefs = lists:delete(ToBlockName, Refs),
        NewArrayValue = {Value, NewRefs},
        NewArrayValues = attrib_utils:replace_array_value(ArrayValues, ArrayIndex, NewArrayValue),
        NewOutput = {ValueName, NewArrayValues}, 
        attrib_utils:replace_attribute(Outputs, ValueName, NewOutput);
      true ->
        error_logger:error_msg("delete_ref() Error. Invalid array index ~p~n",
                                [ValueId]),
        Outputs
      end
  end.


%%    
%% Update the value of every input in this block linked to the 
%% 'NodeName:FromBlockName:ValueName' output value
%%
-spec update_linked_input_values(Inputs :: list(input_attr()),
                                 TargetLink :: input_link(),
                                 NewValue :: input_value()) -> list(input_attr()).

update_linked_input_values(Inputs, TargetLink, NewValue) ->

  % Update the value of each input record pointing at the given value 
  lists:map(
    fun(Input) -> 
      case Input of
        % Non-array value
        {ValueName, {_Value, Link}} ->
          case Link =:= TargetLink of
            % Matching links, update the input attribute value
            true  -> {ValueName, {NewValue, Link}};
            % Links don't match don't change the input value 
            false -> Input	
          end;
        % Array value  
        {ValueName, ArrayValues} ->
          {ValueName, 
           update_linked_array_values(ArrayValues, TargetLink, NewValue)}
      end
		end, 
    Inputs).


%%
%% Update array values with a link matching TargetLink
%%
-spec  update_linked_array_values(ArrayValues :: list(), 
                                  TargetLink :: input_link(), 
                                  NewValue :: value()) -> list(). 
                                  
update_linked_array_values(ArrayValues, TargetLink, NewValue) ->
  lists:map(
    fun({Value, Link}) ->
      case Link =:= TargetLink of
        % Matching links, update the value
        true  -> {NewValue, Link};
        % Links don't match, don't change the array value
        false -> {Value, Link}
      end
    end,
    ArrayValues).
