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
-export([add_link/3, delete_link/3, delete_references/1, dereference_links/2]).
-export([update_linked_input_values/5]).


%%
%% Return the name of the first block connected to this block, that is not registered, 
%% Return 'ok' if all bocks connected to this block are registered / running
%%
-spec unregistered_blocks(BlockInputs :: list()) -> atom() | ok.

unregistered_blocks([])->
	ok;
unregistered_blocks(BlockInputs)->
	
	[Input | RemainingInputs] = BlockInputs,

	{_ValueName, _Value, Link} = Input,
	% get the value name, block name, and node name components of this input value link
	
	{_LinkValueName, LinkBlockName, _LinkNodeName} = Link,   %TODO: Handle getting values from other nodes
	
	% if this input is not a fixed value
	if Link /= ?EMPTY_LINK ->
		% if the block name of this link is not null
		if LinkBlockName /= null ->
			case whereis(LinkBlockName) of
				undefined  -> {error, LinkBlockName};
		        	 _Pid  -> unregistered_blocks(RemainingInputs)
			end;
		true ->
			unregistered_blocks(RemainingInputs)
		end;
	true -> 
		unregistered_blocks(RemainingInputs)
	end.


%%
%% Send a link message to each block linked to the inputs of this block
%%
-spec link_blocks(BlockName :: atom(), BlockInputs :: list()) -> {ok, integer()}.

link_blocks(BlockName, BlockInputs)->
	link_blocks(BlockName, BlockInputs, 0).

link_blocks(_BlockName, [], LinksRequested)->
	{ok, LinksRequested};

link_blocks(BlockName, BlockInputs, LinksRequested)->
	
	[Input | RemainingBlockInputs] = BlockInputs,
	
	{ValueName, Value, Link} = Input,
	{LinkValueName, LinkBlockName, _LinkNodeName} = Link, %TODO: Handle getting values from other nodes
	
	% if this input is not a fixed value
	if Link /= ?EMPTY_LINK ->
		   
		% if the block name of this link is not null
		if LinkBlockName /= null ->
			   
			%if the block input value is still empty, send a link message to the block linked to this input
			if Value == empty ->
				error_logger:info_msg("Link Output <~p:~p> To Input <~p:~p>~n", 
                                        [LinkBlockName, LinkValueName, BlockName, ValueName]),
				block_server:link(LinkBlockName, LinkValueName, BlockName),
				link_blocks(BlockName, RemainingBlockInputs, LinksRequested + 1);
			true ->
				link_blocks(BlockName, RemainingBlockInputs, LinksRequested)
			end;
		true ->
			link_blocks(BlockName, RemainingBlockInputs, LinksRequested)
		end;
	true -> 
		link_blocks(BlockName, RemainingBlockInputs, LinksRequested)
	end.


%%
%% Send an unlink message to each block linked to the inputs of this block
%%
-spec unlink_blocks(BlockName :: atom(), BlockInputs :: list()) -> {ok, integer()}.

unlink_blocks(BlockName, BlockInputs)->
	unlink_blocks(BlockName, BlockInputs, 0).

unlink_blocks(_BlockName, [], LinksRequested)->
	{ok, LinksRequested};

unlink_blocks(BlockName, BlockInputs, LinksRequested)->
	
	[Input | RemainingBlockInputs] = BlockInputs,
	
	{ValueName, _Value, Link} = Input,
	{LinkValueName, LinkBlockName, _LinkNodeName} = Link, %TODO: Handle getting values from other nodes
	
	% if this input is not a fixed value
	if Link /= ?EMPTY_LINK ->
		   
		% if the block name of this link is not null
		if LinkBlockName /= null ->
		    error_logger:info_msg("Unlink Output <~p:~p> From Input <~p:~p>~n", 
                                           [LinkBlockName, LinkValueName, BlockName, ValueName]),
		    block_server:unlink(LinkBlockName, LinkValueName, BlockName),
			unlink_blocks(BlockName, RemainingBlockInputs, LinksRequested + 1);
		true ->
			unlink_blocks(BlockName, RemainingBlockInputs, LinksRequested)
		end;
	true -> 
		unlink_blocks(BlockName, RemainingBlockInputs, LinksRequested)
	end.


%%
%% Add 'ToBlockName' to the list of linked blocks in the ValueName output attribute
%% Returns updated Outputs list
%%
-spec add_link(Outputs :: list(), AttributeName :: atom(), ToBlockName :: atom()) -> list().

add_link(Outputs, AttributeName, ToBlockName) ->
	 
	case block_utils:get_attribute(Outputs, AttributeName) of
		
		not_found ->
			% This block doesn't have an output called 'AttributeName'
			% Just return the original Outputs list
			error_logger:error_msg("add_link() Error. ~p Doesn't exist for this block~n", [AttributeName]),
			Outputs;
		
		{AttributeName, Value, Links} ->  
			case lists:member(ToBlockName, Links) of
			    true ->
				    % This output is already linked to block 'ToBlockName' 
				    % Just return the original Outputs list
				    Outputs;
			    false ->
				    % add 'ToBlockName' to list of links for this output
                    % Return updated Outputs list
				    NewLinks = [ToBlockName | Links],
				    NewOutput = {AttributeName, Value, NewLinks},
				    block_utils:replace_attribute(Outputs, AttributeName, NewOutput)
			end;
		Unknown ->
			error_logger:error_msg("add_link() Error. Unknown output value record  ~p~n", [Unknown]),
			Outputs
	end.

%%
%% Delete 'ToBlockName' from the list of linked blocks in the ValueName output attribute
%% Returns updated Outputs list
%%
-spec delete_link(Outputs :: list(), AttributeName :: atom(), ToBlockName :: atom()) -> list().

delete_link(Outputs, AttributeName, ToBlockName) ->
	 
	case block_utils:get_attribute(Outputs, AttributeName) of
		
		not_found ->
			% This block doesn't have an output called 'AttributeName'
			% Just return the original Outputs list
			error_logger:error_msg("delete_link() Error. ~p Doesn't exist for this block~n", [AttributeName]),
			Outputs;
		
		{AttributeName, Value, Links} ->  
            % Delete the 'ToBlockName' from the list of linked blocks, if it exists
			NewLinks = lists:delete(ToBlockName, Links),
		    NewOutput = {AttributeName, Value, NewLinks},
			block_utils:replace_attribute(Outputs, AttributeName, NewOutput);
			
		Unknown ->
			error_logger:error_msg("delete_link() Error. Unknown output value record  ~p~n", [Unknown]),
			Outputs
	end.

%% 
%% Scan all of the blocks and delete any references to the deleted block in all input links
%%
-spec delete_references(DeleteBlockName :: atom()) -> ok.

delete_references(DeleteBlockName)->
    BlockNames = 'LinkBlox_ui':block_names(),
    delete_references(DeleteBlockName, BlockNames).
    
delete_references(_DeleteBlockName, []) -> 
    ok;
  
delete_references(DeleteBlockName, [BlockName | RemainingBlockNames]) ->
    block_server:dereference(BlockName, DeleteBlockName),
    delete_references(DeleteBlockName, RemainingBlockNames).
   
   
%%
%% Scan all of the inputs of this block and delete any links that reference DeleteBlockName
%% Set input value to empty, return updated BlockInput attributes, and  
%%   
-spec dereference_links(DeleteBlockName :: atom(), BlockInputs :: list()) -> {list(), integer()}.

dereference_links(DeleteBlockName, BlockInputs) ->
	dereference_links(DeleteBlockName, BlockInputs, [],  0).

dereference_links(_DeleteBlockName, [], NewBlockInputs, ReferenceCount) ->
    {lists:reverse(NewBlockInputs), ReferenceCount};

dereference_links(DeleteBlockName, BlockInputs, NewBlockInputs, ReferenceCount) ->	

	[Input | RemainingBlockInputs] = BlockInputs,
	
	{ValueName, _Value, Link} = Input,
	{_LinkValueName, LinkBlockName, _LinkNodeName} = Link, %TODO: Handle getting values from other nodes
	
	% if the link on this input matches the DeleteBlockName
	if LinkBlockName == DeleteBlockName ->
        % Delete the link and set the input value to empty
        NewInput = {ValueName, empty, ?EMPTY_LINK},
        dereference_links(DeleteBlockName, RemainingBlockInputs, 
                                         [NewInput | NewBlockInputs], ReferenceCount + 1);
        
	true -> % The link on this input, doesn't reference DeleteBlockName, don't modify it,
		dereference_links(DeleteBlockName, RemainingBlockInputs, [Input | NewBlockInputs], ReferenceCount)
	end.
        
        
%%    
%% Update the value of every input in this block linked to the 
%% 'ValueName:FromBlockName:NodeName' output value
%% Return the updated list of Input values
%% TODO: Switch to passing around a target link {NodeName, BlockName, ValueName}
%%
-spec update_linked_input_values(Inputs :: list(), NewValueName :: atom(), 
                FromBlockName :: atom(), NodeName :: atom(), NewValue :: term()) -> list().

update_linked_input_values(Inputs, NewValueName, FromBlockName, NodeName, NewValue) ->

	TargetLink = {NewValueName, FromBlockName, NodeName},

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
 