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
-export([link_blocks/2, unlink_blocks/2, unlink/2]).
-export([add_ref/3, delete_ref/3]).
-export([update_linked_input_values/3]). %, TODO: delete? set_input_link/3]).

-ifdef(INCLUDE_OBSOLETE).
%%
%% Return the name of the first block connected to this block, that is not registered, 
%% Return 'ok' if all bocks connected to this block are registered / running
%%
%% TODO: Delete?  not needed anymore
-spec unregistered_blocks(BlockInputs :: list(input_attr())) -> block_name() | ok.

unregistered_blocks([])->
  ok;
unregistered_blocks(BlockInputs)->
  
  [Input | RemainingInputs] = BlockInputs,
  
  {_ValueName, {_Value, Link}} = Input,
  case Link of 
    ?EMPTY_LINK -> 
      % Not linked to another block, do nothing
      unregistered_blocks(RemainingInputs);
 
    {LinkBlockName, _LinkValueName} ->
      % get the components of this input value link
      case whereis(LinkBlockName) of
        undefined  ->  
          % Linked block is not running, return error
          {error, LinkBlockName};
        _Pid  ->
          % Linked block is running, do nothing
          unregistered_blocks(RemainingInputs)
      end;

    %TODO: Handle other link types, i.e. links to other nodes      
    UnhandledLink -> 
      error_logger:error_msg("Unhandled Link Type: ~p~n", [UnhandledLink]) 
  end.
-endif.


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
          % If linked block is not running, input value should be empty
          if Value /= empty ->
            {ok, UpdatedInputs} = 
                      attrib_utils:set_value(Inputs, ValueId, empty),   
            UpdatedInputs;
          true ->
            Inputs
          end;

        _Pid  ->
          % Linked block is running,
          % if the block input value is empty, 
          if Value == empty ->
            % send a link message to the linked block, get current linked value back
            UpdatedValue = block_server:link(LinkBlockName, LinkValueId, BlockName),
            
            {ok, UpdatedInputs} = 
                      attrib_utils:set_value(Inputs, ValueId, UpdatedValue),
            
            error_logger:info_msg("Link Output <~p:~p> To Input <~p:~p>~n", 
                        [LinkBlockName, LinkValueId, BlockName, ValueId]),
                        
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
-spec unlink_blocks(BlockName :: block_name(),
                    BlockInputs :: list(input_attr())) -> {ok, integer()}.

unlink_blocks(BlockName, BlockInputs)->
  unlink_blocks(BlockName, BlockInputs, 0).

unlink_blocks(_BlockName, [], LinksRequested)->
  {ok, LinksRequested};

unlink_blocks(BlockName, BlockInputs, LinksRequested)->
	
  [Input | RemainingBlockInputs] = BlockInputs,

  {ValueName, {_Value, Link}} = Input,
  case Link of
    ?EMPTY_LINK ->
      % Input is not linked, do nothing
       unlink_blocks(BlockName, RemainingBlockInputs, LinksRequested);
       
     {LinkBlockName, LinkValueName} -> 
      % if the block name of this link is not null
      if LinkBlockName /= null ->
        error_logger:info_msg("Unlink Output <~p:~p> From Input <~p:~p>~n", 
                            [LinkBlockName, LinkValueName, BlockName, ValueName]),
        block_server:unlink(LinkBlockName, LinkValueName, BlockName),
        unlink_blocks(BlockName, RemainingBlockInputs, LinksRequested + 1);
      true ->
        unlink_blocks(BlockName, RemainingBlockInputs, LinksRequested)
      end;

    %TODO: Handle other link types, i.e. links to other nodes      
    UnhandledLink -> 
      error_logger:error_msg("Unhandled Link Type: ~p~n", [UnhandledLink]) 
  end.

  
%%
%% Unlink one input value link
%% TODO: Make unlink_blocks call this function
%%
-spec unlink(BlockName :: block_name(),
             Input :: input_attr()) -> ok.  
  
unlink(BlockName, Input) ->
  {ValueName, {_Value, Link}} = Input,
  case Link of
    % Input is not linked, do nothing
    ?EMPTY_LINK -> ok;
       
    {LinkBlockName, LinkValueName} -> 
      % if the block name of this link is not null
      if LinkBlockName /= null ->
        error_logger:info_msg("Unlink Output <~p:~p> From Input <~p:~p>~n", 
                            [LinkBlockName, LinkValueName, BlockName, ValueName]),
        block_server:unlink(LinkBlockName, LinkValueName, BlockName),
        ok;
      true ->
        ok
      end;

    %TODO: Handle other link types, i.e. links to other nodes      
    UnhandledLink -> 
      error_logger:error_msg("Unhandled Link Type: ~p~n", [UnhandledLink]) 
  end.



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
		
		{ok, {ValueName, {Value, Refs}}} ->  
			case lists:member(ToBlockName, Refs) of
			  true ->
          % TODO: Are we sure we want to do this?
          %    If a block has an array of inputs, and more than one of the inputs in the array
          %    is linked to this output value, and the block is reconfigured, reducing the number of inputs,
          %    When the input is deleted, the link to this output should be deleted, but 
          %    another input in that block could still be linked to this output
          %    May want to allow multiple refs to the same block, filter dupes in update_block()
          %    More than one reference to this will serve as a reference counter.
				  % This output is already linked to block 'ToBlockName' 
				  % Just return the original Outputs list
				  Outputs;
			  false ->
				  % add 'ToBlockName' to list of block references for this output
          % Return updated Outputs list
				  NewRefs = [ToBlockName | Refs],
				  NewOutput = {ValueName, {Value, NewRefs}},
				  attrib_utils:replace_attribute(Outputs, ValueName, NewOutput)
			end;
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
		
    {ok, {ValueName, {Value, Refs}}} ->  
      % Delete the 'ToBlockName' from the list of linked blocks, if it exists
      NewRefs = lists:delete(ToBlockName, Refs),
		  NewOutput = {ValueName, {Value, NewRefs}},
      attrib_utils:replace_attribute(Outputs, ValueName, NewOutput);
      
    {ok, {ValueName, ArrayValues}} ->
      % if this is an array value, the ValueName from get_attribute()
      % will match ValueName in the ValueId tuple
      {ValueName, ArrayIndex} = ValueId,
      if (0 < ArrayIndex) andalso (ArrayIndex =< length(ArrayValues)) ->
        {Value, Refs} = lists:nth(ArrayIndex, ArrayValues),
        NewRefs = lists:delete(ToBlockName, Refs),
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


-ifdef(INCLUDE_OBSOLETE).
%%
%% Update the input Link for the input value 'ValueName'
%% TODO: Do we need this? not used right now
%%
-spec set_input_link(BlockValues :: block_state(),
                     ValueName :: value_name(),
                     NewLink :: input_link()) -> block_state().
                     
set_input_link(BlockValues, ValueName, NewLink) ->
	
  {Config, Inputs, Outputs, Private} = BlockValues,

  case attrib_utils:get_attribute(Inputs, ValueName) of
    {error, not_found} ->
      BlockName = config_utils:name(Config),
      error_logger:error_msg("~p set_input_link() Error.  ~p not found in Input values.~n", 
                             [BlockName, ValueName]),
      % Input value not found, just return the BlockValues unchanged
      BlockValues;
      
    {ok, {ValueName, {Value, _Link}}} ->
      NewInput = {ValueName, {Value, NewLink}},
      NewInputs = attrib_utils:replace_attribute(Inputs, ValueName, NewInput),
      {Config, NewInputs, Outputs, Private}
  end.
 -endif.