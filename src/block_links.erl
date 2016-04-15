%% @doc 
%%% Handle block links     
%%%               
%%% @end 

-module(block_links).

-author("Mark Sebald").

-include("block_state.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([unregistered_blocks/1, link_blocks/2, unlink_blocks/2]).
-export([add_link/3, delete_link/3]).
-export([update_linked_input_values/5]).


%%
%% Return the name of the first block connected to this block, that is not registered, 
%% Return 'ok' if all bocks connected to this block are registered / running
%%
%% TODO: Delete?  not needed anymore
-spec unregistered_blocks(BlockInputs :: list()) -> atom() | ok.

unregistered_blocks([])->
  ok;
unregistered_blocks(BlockInputs)->
  
  [Input | RemainingInputs] = BlockInputs,
  
  {_ValueName, _Value, Link} = Input,
  case Link of 
    ?EMPTY_LINK -> 
      % Not linked to another block, do nothing
      unregistered_blocks(RemainingInputs);
 
    %TODO: Handle getting values from other nodes
    {_LinkNodeName, LinkBlockName, _LinkValueName} ->
      % get the components of this input value link
      case whereis(LinkBlockName) of
        undefined  ->  
          % Linked block is not running, return error
          {error, LinkBlockName};
        _Pid  ->
          % Linked block is running, do nothing
          unregistered_blocks(RemainingInputs)
      end
  end.


%%
%% Send a link message to each block linked to the inputs of this block
%%
-spec link_blocks(BlockName :: atom(),
                  Inputs :: list()) -> list().

link_blocks(BlockName, Inputs)->
  link_blocks(BlockName, Inputs, Inputs).

link_blocks(_BlockName, [], UpdatedInputs)->
  UpdatedInputs;

link_blocks(BlockName, Inputs, UpdatedInputs)->

  [Input | RemainingInputs] = Inputs,
	
  {ValueName, Value, Link} = Input,
  case Link of
    ?EMPTY_LINK ->
      % Input is not linked to another block, nothing to do
      link_blocks(BlockName, RemainingInputs, UpdatedInputs);
      
    %TODO: Handle getting values from other nodes
    {_LinkNodeName, LinkBlockName, LinkValueName} ->
      case whereis(LinkBlockName) of
        undefined  ->
          % If linked block is not running input value better be empty
          % TODO: Set input value to empty here. 
          %     Would require returning list of updated inputs
          %     Belt and suspenders design
          %     Should not have to do this, if everything is perfect
          if Value /= empty ->
            error_logger:error_msg("~p Error: linked input value not empty~n", 
                                    [BlockName]); 
          true -> ok
          end,

          % Linked block is not running, nothing to do
          link_blocks(BlockName, RemainingInputs, UpdatedInputs);

        _Pid  ->
          % Linked block is running,
          % if the block input value is empty, 
          if Value == empty ->
            % send a link message to the linked block, get current linked value back
            UpdatedValue = block_server:link(LinkBlockName, LinkValueName, BlockName),
            
            NewUpdatedInputs = block_utils:set_value(UpdatedInputs, ValueName, UpdatedValue),
            
            error_logger:info_msg("Link Output <~p:~p> To Input <~p:~p>~n", 
                        [LinkBlockName, LinkValueName, BlockName, ValueName]),
                        
            link_blocks(BlockName, RemainingInputs, NewUpdatedInputs);
          true ->
            % Value is not empty, link must already be established
            link_blocks(BlockName, RemainingInputs, UpdatedInputs)
          end
      end
  end.


%%
%% Send an unlink message to each block linked to the inputs of this block
%%
-spec unlink_blocks(BlockName :: atom(),
                    BlockInputs :: list()) -> {ok, integer()}.

unlink_blocks(BlockName, BlockInputs)->
  unlink_blocks(BlockName, BlockInputs, 0).

unlink_blocks(_BlockName, [], LinksRequested)->
  {ok, LinksRequested};

unlink_blocks(BlockName, BlockInputs, LinksRequested)->
	
  [Input | RemainingBlockInputs] = BlockInputs,

  {ValueName, _Value, Link} = Input,
  case Link of
    ?EMPTY_LINK ->
      % Input is not linked, do nothing
       unlink_blocks(BlockName, RemainingBlockInputs, LinksRequested);
       
    %TODO: Handle getting values from other nodes
    {_LinkNodeName, LinkBlockName, LinkValueName} -> 
      % if the block name of this link is not null
      if LinkBlockName /= null ->
        error_logger:info_msg("Unlink Output <~p:~p> From Input <~p:~p>~n", 
                            [LinkBlockName, LinkValueName, BlockName, ValueName]),
        block_server:unlink(LinkBlockName, LinkValueName, BlockName),
        unlink_blocks(BlockName, RemainingBlockInputs, LinksRequested + 1);
      true ->
        unlink_blocks(BlockName, RemainingBlockInputs, LinksRequested)
      end 
  end.


%%
%% Add 'ToBlockName' to the list of linked blocks in the ValueName output attribute
%%
-spec add_link(Outputs :: list(), 
               ValueName :: atom(), 
               ToBlockName :: atom()) -> list().

add_link(Outputs, ValueName, ToBlockName) ->

  case block_utils:get_attribute(Outputs, ValueName) of

    not_found ->
      % This block doesn't have an output called 'ValueName'
      % Just return the original Outputs list
			error_logger:error_msg("add_link() Error. ~p Doesn't exist for this block~n", 
                             [ValueName]),
			Outputs;
		
		{ValueName, Value, Links} ->  
			case lists:member(ToBlockName, Links) of
			  true ->
				  % This output is already linked to block 'ToBlockName' 
				  % Just return the original Outputs list
				  Outputs;
			  false ->
				  % add 'ToBlockName' to list of links for this output
          % Return updated Outputs list
				  NewLinks = [ToBlockName | Links],
				  NewOutput = {ValueName, Value, NewLinks},
				  block_utils:replace_attribute(Outputs, ValueName, NewOutput)
			end;
    Unknown ->
      error_logger:error_msg("add_link() Error. Unknown output value record  ~p~n",
                             [Unknown]),
      Outputs
  end.


%%
%% Delete 'ToBlockName' from the list of linked blocks in the ValueName output attribute
%%
-spec delete_link(Outputs :: list(), 
                  ValueName :: atom(), 
                  ToBlockName :: atom()) -> list().

delete_link(Outputs, ValueName, ToBlockName) ->

  case block_utils:get_attribute(Outputs, ValueName) of

    not_found ->
      % This block doesn't have an output called 'ValueName'
      % Just return the original Outputs list
      error_logger:error_msg("delete_link() Error. ~p Doesn't exist for this block~n", 
                              [ValueName]),
      Outputs;
		
    {ValueName, Value, Links} ->  
      % Delete the 'ToBlockName' from the list of linked blocks, if it exists
      NewLinks = lists:delete(ToBlockName, Links),
		  NewOutput = {ValueName, Value, NewLinks},
      block_utils:replace_attribute(Outputs, ValueName, NewOutput);

    Unknown ->
      error_logger:error_msg("delete_link() Error. Unknown output value record  ~p~n", [Unknown]),
      Outputs
  end.


%%    
%% Update the value of every input in this block linked to the 
%% 'NodeName:FromBlockName:ValueName' output value
%%
-spec update_linked_input_values(Inputs :: list(),
                                 NodeName :: atom(), 
                                 FromBlockName :: atom(), 
                                 NewValueName :: atom(),
                                 NewValue :: term()) -> list().

update_linked_input_values(Inputs, NodeName, FromBlockName, NewValueName, NewValue) ->

  TargetLink = {NodeName, FromBlockName, NewValueName},

  % Update the value of each input record pointing at the given value 
  lists:map(
    fun(Input) -> 
      {ValueName, _Value, Link} = Input,
      case Link =:= TargetLink of
        true  -> {ValueName, NewValue, Link};
        false -> Input	% This block input is not linked to the target block output. 
                        % Don't change the input value 
      end
		end, 
    Inputs).
 