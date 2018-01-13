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
          format_link/1,
          unlink_input/1,
          unlink_block/1,
          unlink_outputs/3,
          unlink_outputs_block/3,
          validate_link/1,
          add_link/4,
          del_link/4
]).


%%
%% Translate a Link tuple into a string
%%
-spec format_link(Link :: link_def()) -> string().

format_link({BlockName, {ValueName, Index}}) ->
  ValueNameStr = ui_utils:get_attrib_string(ValueName),
  io_lib:format("~p:~s[~w]", [BlockName, ValueNameStr, Index]);
  
format_link({BlockName, ValueName}) ->
  ValueNameStr = ui_utils:get_attrib_string(ValueName),
  io_lib:format("~p:~s", [BlockName, ValueNameStr]);

format_link(InvalidLink) ->
  io_lib:format("Invalid link: ~p", [InvalidLink]).

  
%%
%% Send a message to all blocks to remove this Link
%%   from their output values Links list
%%
-spec unlink_input(Link :: link_def()) -> ok.  
  
unlink_input(Link) ->

  BlockNames = block_supervisor:block_names(),

  lists:foreach(fun(BlockName) ->
                  block_server:unlink_input(BlockName, Link)
                end,
                BlockNames).


%%
%% Send a message to all blocks to remove any Links to this BlockName
%%   from their output values list of Links
%%
-spec unlink_block(InputBlockName :: block_name()) -> ok.  

unlink_block(InputBlockName) ->

  BlockNames = block_supervisor:block_names(),

  lists:foreach(fun(BlockName) ->
                  block_server:unlink_block(BlockName, InputBlockName)
                end,
                BlockNames).


%%
%% Search list of output attributres and remove the given Link
%%   from their list of links
%%
-spec unlink_outputs(BlockName :: block_name(),
                     Outputs :: output_attribs(),
                     Link :: link_def()) -> output_attribs().

unlink_outputs(BlockName, Outputs, Link) ->

  lists:map(fun(Output) ->
              case Output of 
                {ValueName, {Value, Links}} ->
                  case lists:member(Link, Links) of
                    true ->
                      logger:info(block_output_unlinked_from_block_input, 
                            [format_link({BlockName, ValueName}), format_link(Link)]),
                      % Return updated output attribute
                      {ValueName, {Value, lists:delete(Link, Links)}};

                    false -> % Does not contain target link, return attribute unchanged
                      Output
                  end;

                {ValueName, ArrayValues} -> % Array type value
                  {NewArrayValues, _Index} = 
                    lists:mapfoldl(fun({Value, Links}, Index) -> {
                      case lists:member(Link, Links) of
                        true ->
                          logger:info(block_output_unlinked_from_block_input, 
                                [format_link({BlockName, {ValueName, Index}}), format_link(Link)]),
                          {Value, lists:delete(Link, Links)};
    
                        false -> % Does not contain target link, return value unchanged
                          {Value, Links}
                      end,
                      Index + 1}
                    end,
                    1,
                    ArrayValues),
                  % Return updated array value attribute 
                  {ValueName, NewArrayValues}
              end
            end,
            Outputs).


%%
%% Search list of output attributes and remove any link
%%   from their list of links, that uses the given block
%%
-spec unlink_outputs_block(BlockName :: block_name(),
                           Outputs :: output_attribs(),
                           LinkBlockName :: block_name()) -> output_attribs().

unlink_outputs_block(BlockName, Outputs, LinkBlockName) ->

  lists:map(fun(Output) ->
              case Output of 
                {ValueName, {Value, Links}} ->
                  NewLinks = filter_links(BlockName, ValueName, Links, LinkBlockName),
                  {ValueName, {Value, NewLinks}};

                {ValueName, ArrayValues} -> % Array type value
                  {NewArrayValues, _Index} = 
                      lists:mapfoldl(fun({Value, Links}, Index) ->
                        NewLinks = filter_links(BlockName, {ValueName, Index}, Links, LinkBlockName),
                        {{Value, NewLinks}, Index + 1}
                      end,
                      1,
                      ArrayValues),

                  {ValueName, NewArrayValues}
              end
            end,
            Outputs).


%%
%% Filter out links that use the LinkBlockName
%%
-spec filter_links(BlockName :: block_name(),
                   ValueId :: value_id(),
                   Links :: link_defs(),
                   LinkBlockName :: block_name()) -> link_defs().

filter_links(BlockName, ValueId, Links, LinkBlockName) ->
  lists:filter(fun(Link) ->
        {BlockNameInLink, _ValueIdInLink} = Link,
        case (BlockNameInLink == LinkBlockName) of
          true ->
            logger:info(block_output_unlinked_from_block_input, 
                [format_link({BlockName, ValueId}), format_link(Link)]),
            false;  % remove this link, it uses LinkBlockName

          false ->  % Keep this link, 
            true
        end
    end,
    Links).


%%
%% Check if the link is valid for the given block and output value ID
%% Support for the linkblox_api module
%%
-spec validate_link(Link :: link_def()) -> ok | {error, atom()}.

validate_link(Link) ->
  case Link of
    {LinkBlockName, LinkValueId} ->
      case block_supervisor:is_block(LinkBlockName) of
        true ->
          % Linked block exists,
          % Check that the value id is a valid input of block "LinkBlockName" 
          case block_server:get_input_value(LinkBlockName, LinkValueId) of
            {ok, _Value} -> ok;
            {error, Reason} -> {error, Reason}
          end;
        _False ->
          {error, linked_block_does_not_exist}
      end;

    _InvalidLink ->
      {error, invalid_link}
  end.


%%
%% Add a new link to the list of links to block inputs 
%% in this ValueId output attribute
%%
-spec add_link(BlockName :: block_name(),
               Outputs :: output_attribs(), 
               ValueId :: value_id(), 
               Link :: link_def()) -> {ok, output_attribs()} | {error, atom()}.

add_link(BlockName, Outputs, ValueId, Link) ->

  case attrib_utils:get_attribute(Outputs, ValueId) of

    {error, not_found} ->
      % This block doesn't have an output 'ValueId'
      {error, not_found};

    % Non-array value
    {ok, {ValueName, {Value, Links}}} ->

      % Add Link to list of Links for this output
      % If not already added, Return updated Outputs list
      case lists:member(Link, Links) of 
        false ->
          NewLinks = [Link | Links],
          NewOutput = {ValueName, {Value, NewLinks}},
          logger:info(block_output_linked_to_block_input,
                      [format_link({BlockName, ValueName}), format_link(Link)]),
          {ok, attrib_utils:replace_attribute(Outputs, ValueName, NewOutput)};          
        true ->
          {error, already_exists}
      end;

    % Array value
    {ok, {ValueName, ArrayValues}} ->
      % if this is an array value, the ValueName from get_attribute()
      % will match ValueName in the ValueId tuple
      {ValueName, ArrayIndex} = ValueId,
      if (0 < ArrayIndex) andalso (ArrayIndex =< length(ArrayValues)) ->
        {Value, Links} = lists:nth(ArrayIndex, ArrayValues),
        % Add Link to list of block Links for this output
        % If not already added, Return updated Outputs list
        case lists:member(Link, Links) of
          false ->
            NewLinks = [Link | Links],
            NewArrayValue = {Value, NewLinks},
            NewArrayValues = attrib_utils:replace_array_value(ArrayValues, ArrayIndex, NewArrayValue),
            NewOutput = {ValueName, NewArrayValues}, 
            logger:info(block_output_linked_to_block_input, 
                     [format_link({BlockName, {ValueName, ArrayIndex}}), format_link(Link)]),
            {ok, attrib_utils:replace_attribute(Outputs, ValueName, NewOutput)};
          true ->
            {error, already_exists}
        end;
      true ->
        {error, invalid_array_index}
      end
  end.


%%
%% Delete Link from the list of Links to block inputs 
%% in the ValueId output attribute
%%
-spec del_link(BlockName :: block_name(),
               Outputs :: output_attribs(), 
               ValueId :: value_id(), 
               Link :: link_def()) -> {ok, output_attribs()} | {error, atom()}.

del_link(BlockName, Outputs, ValueId, Link) ->

  case attrib_utils:get_attribute(Outputs, ValueId) of

    {error, not_found} ->
      % This block doesn't have an output 'ValueId'
      {error, not_found};

    % Non-Array value
    {ok, {ValueName, {Value, Links}}} ->  
      % Delete the Link from the list of Links to block inputs, if it exists
      case lists:member(Link, Links) of 
        true ->
          NewLinks = lists:delete(Link, Links),
          NewOutput = {ValueName, {Value, NewLinks}},
          logger:info(block_output_unlinked_from_block_input, 
                      [format_link({BlockName, ValueName}), format_link(Link)]),
          {ok, attrib_utils:replace_attribute(Outputs, ValueName, NewOutput)};
        false ->
          {error, not_linked}
      end;

    % Array value  
    {ok, {ValueName, ArrayValues}} ->
      % if this is an array value, the ValueName from get_attribute()
      % will match ValueName in the ValueId tuple
      {ValueName, ArrayIndex} = ValueId,
      if (0 < ArrayIndex) andalso (ArrayIndex =< length(ArrayValues)) ->
        {Value, Links} = lists:nth(ArrayIndex, ArrayValues),
        % Delete the Link from the list of Links to block inputs, if it exists
        case lists:member(Link, Links) of 
          true ->
            NewLinks = lists:delete(Link, Links),
            NewArrayValue = {Value, NewLinks},
            NewArrayValues = attrib_utils:replace_array_value(ArrayValues, ArrayIndex, NewArrayValue),
            NewOutput = {ValueName, NewArrayValues}, 
            logger:info(block_output_unlinked_from_block_input, 
                [format_link({BlockName, {ValueName, ArrayIndex}}), format_link(Link)]),
            {ok, attrib_utils:replace_attribute(Outputs, ValueName, NewOutput)};
          false ->
            {error, not_linked}
        end;
      true ->
        {error, invalid_array_index}
      end
  end.


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% ====================================================================
% Test set_link()
% 
%   Test set_link() non-array input ok
% set_link_non_array_ok_test() ->
%   InputAttribs = test_data:attrib_utils_input_attribs1(),
%   InputValueId = number_in,
%   Link = {node_name, link_block, link_output},
%   ExpectedResult = {number_in, {empty, {node_name, link_block, link_output}}},
  
%   {ok, NewInputAttribs} = set_link(unit_test, InputAttribs, InputValueId, Link),
%   {ok, Result} = attrib_utils:get_attribute(NewInputAttribs, InputValueId), 
%   ?assertEqual(ExpectedResult, Result).

% % Test set_link() non-array input ok
% set_link_non_array_bad_test() ->
%   InputAttribs = test_data:attrib_utils_input_attribs1(),
%   InputValueId = invalid_input_name,
%   Link = {node_name, link_block, link_output},
%   ExpectedResult = {error, not_found},
  
%   Result = set_link(unit_test, InputAttribs, InputValueId, Link),
%   ?assertEqual(ExpectedResult, Result).

%   %   Test set_link() array input ok
% set_link_array_ok_test() ->
%   InputAttribs = test_data:attrib_utils_input_attribs1(),
%   InputValueId = {integer_array_in, 3},
%   Link = {node_name, link_block, link_output},
%   ExpectedResult = {integer_array_in, [{234,{}}, {456,{}}, 
%                                      {empty,{node_name, link_block, link_output}}]},
  
%   {ok, NewInputAttribs} = set_link(unit_test, InputAttribs, InputValueId, Link),
%   {ok, Result} = attrib_utils:get_attribute(NewInputAttribs, InputValueId), 
%   ?assertEqual(ExpectedResult, Result).

% % Test set_link() array input bad
% set_link_array_bad_test() ->
%   InputAttribs = test_data:attrib_utils_input_attribs1(),
%   InputValueId = {bool_array_in, 0}, % valid input name, invalid index
%   Link = {node_name, link_block, link_output},
%   ExpectedResult = {error, invalid_index},
  
%   Result = set_link(unit_test, InputAttribs, InputValueId, Link),
%   ?assertEqual(ExpectedResult, Result).



-endif.