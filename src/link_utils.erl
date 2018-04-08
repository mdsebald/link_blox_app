%%% @doc 
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
  lists:flatten(io_lib:format("~p:~s[~w]", [BlockName, ValueNameStr, Index]));
  
format_link({BlockName, ValueName}) ->
  ValueNameStr = ui_utils:get_attrib_string(ValueName),
  lists:flatten(io_lib:format("~p:~s", [BlockName, ValueNameStr]));

format_link(InvalidLink) ->
  lists:flatten(io_lib:format("Invalid link: ~p", [InvalidLink])).

  
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
                BlockNames),

  block_supervisor:del_block_exec_links(InputBlockName).


%%
%% Search list of output attributes and remove the given Link
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

  case output_utils:get_links(Outputs, ValueId) of
      {ok, Links} ->
        case lists:member(Link, Links) of 
          false ->
            NewLinks = [Link | Links],
            logger:info(block_output_linked_to_block_input, 
                    [format_link({BlockName, ValueId}), format_link(Link)]),
            output_utils:replace_links(Outputs, ValueId, NewLinks);
          true ->
            {error, already_linked}
        end;
      {error, Reason} ->
        {error, Reason}
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

  case output_utils:get_links(Outputs, ValueId) of
    {ok, Links} ->
      case lists:member(Link, Links) of 
        true ->
          NewLinks = lists:delete(Link, Links),
          logger:info(block_output_unlinked_from_block_input, 
                  [format_link({BlockName, ValueId}), format_link(Link)]),
          output_utils:replace_links(Outputs, ValueId, NewLinks);
        false ->
          {error, not_linked}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

 
%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% format_link/1,
% unlink_input/1,
% unlink_block/1,
% unlink_outputs/3,
% unlink_outputs_block/3,
% validate_link/1,
% add_link/4,
% del_link/4


% ====================================================================
% Test format_link()
% 
format_link_non_array_ok_test() ->
  ExpectedResult = "unit_test:input",
  Result = format_link({unit_test, input}),
  ?assertEqual(ExpectedResult, Result).

format_link_array_ok_test() ->
  ExpectedResult = "unit_test:input[1]",
  Result = format_link({unit_test, {input, 1}}),
  ?assertEqual(ExpectedResult, Result).

format_link_invalid_test() ->
  ExpectedResult = "Invalid link: bad_link",
  Result = format_link(bad_link),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================


% ====================================================================
% Test add_link()
% 
add_link_non_array_value_id_doesnt_exist_test() ->
  Outputs = test_data:output_attribs5(),
  ExpectedResult = {error, not_found},
  Result = add_link(test_block_name, Outputs, bad_value_id, {doesnt, matter}),
  ?assertEqual(ExpectedResult, Result).

add_link_array_value_id_doesnt_exist_test() ->
  Outputs = test_data:output_attribs5(),
  ExpectedResult = {error, not_found},
  Result = add_link(test_block_name, Outputs, {bad_value_id, 1}, {doesnt, matter}),
  ?assertEqual(ExpectedResult, Result).

add_link_non_array_already_linked_test() ->
  Outputs = test_data:output_attribs5(),
  ExpectedResult = {error, already_linked},
  Result = add_link(test_block_name, Outputs, value_with_links, {test1, input1}),
  ?assertEqual(ExpectedResult, Result).

add_link_array_already_linked_test() ->
  Outputs = test_data:output_attribs5(),
  ExpectedResult = {error, already_linked},
  Result = add_link(test_block_name, Outputs, {integer_array_out, 3}, {test2, input2}),
  ?assertEqual(ExpectedResult, Result).

add_link_non_array_valid_test() ->
  Outputs = test_data:output_attribs5(),
  ExpectedResult = {ok, test_data:output_attribs10()},
  Result = add_link(test_block_name, Outputs, value, {test1, input1}),
  ?assertEqual(ExpectedResult, Result).

add_link_array_valid_test() ->
  Outputs = test_data:output_attribs5(),
  ExpectedResult = {ok, test_data:output_attribs11()},
  Result = add_link(test_block_name, Outputs, {integer_array_out, 3}, {test3, input1}),
  ?assertEqual(ExpectedResult, Result).
% ====================================================================

% ====================================================================
% Test del_link()
% 
del_link_non_array_value_id_doesnt_exist_test() ->
  Outputs = test_data:output_attribs5(),
  ExpectedResult = {error, not_found},
  Result = del_link(test_block_name, Outputs, bad_value_id, {doesnt, matter}),
  ?assertEqual(ExpectedResult, Result).

del_link_array_value_id_doesnt_exist_test() ->
  Outputs = test_data:output_attribs5(),
  ExpectedResult = {error, not_found},
  Result = del_link(test_block_name, Outputs, {bad_value_id, 1}, {doesnt, matter}),
  ?assertEqual(ExpectedResult, Result).

del_link_non_array_not_linked_test() ->
  Outputs = test_data:output_attribs5(),
  ExpectedResult = {error, not_linked},
  Result = del_link(test_block_name, Outputs, value_with_links, {not_linked_block_name, input1}),
  ?assertEqual(ExpectedResult, Result).

del_link_array_not_linked_test() ->
  Outputs = test_data:output_attribs5(),
  ExpectedResult = {error, not_linked},
  Result = del_link(test_block_name, Outputs, {integer_array_out, 3}, {not_linked_block_name, input2}),
  ?assertEqual(ExpectedResult, Result).

del_link_non_array_valid_test() ->
  Outputs = test_data:output_attribs5(),
  ExpectedResult = {ok, test_data:output_attribs12()},
  Result = del_link(test_block_name, Outputs, value_with_links, {test2, input2}),
  ?assertEqual(ExpectedResult, Result).

del_link_array_valid_test() ->
  Outputs = test_data:output_attribs5(),
  ExpectedResult = {ok, test_data:output_attribs13()},
  Result = del_link(test_block_name, Outputs, {integer_array_out, 3}, {test1, input1}),
  ?assertEqual(ExpectedResult, Result).

% ====================================================================



-endif.