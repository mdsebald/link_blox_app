%%% @doc
%%% 
%%% User Interface utilities for LinkBlox app.
%%%
%%% @end

-module(ui_utils).

-author("Mark Sebald").

-export([
          get_yes/0,
          parse_value/1,
          get_input/1,
          parse_cli_params/1
]).


%%
%% Get input, return 'true' if first char is 'Y' or 'y'
%%
-spec get_yes() -> true | false.

get_yes() ->
  case lists:nth(1, get_input("")) of
    $Y -> true;
    $y -> true;
    _  -> false
  end.


%%
%% Naive parse value function, i.e. take a stab at the value type
%%
-spec parse_value(string()) -> string() | float() | integer() | atom().

parse_value(ValueStr) ->
  case is_open_and_close_quoted(ValueStr) of
    true ->
      % ValueStr is surrounded by quotes
      % Remove the quotes and use the bare string
      [_FirstQuote | RemString] = ValueStr,
      lists:droplast(RemString);

    false ->
      case string:to_float(ValueStr) of
        {Float, []}       -> Float;

        {error, no_float} ->

          case string:to_integer(ValueStr) of
            {Integer, []}     -> Integer;
            
            {error, no_integer} ->
              % just turn the input into an atom
              list_to_atom(ValueStr);

            {_Integer, _Rest} -> ValueStr 
          end;

        {_Float, _Rest}   -> ValueStr
      end
  end.


%%
%% Get user input, 
%% minus new line char, leading whitespace, and trailing whitespace
%%
-spec get_input(Prompt :: string()) -> string().

get_input(Prompt) ->
  Raw1 = io:get_line(Prompt),

  % In nerves environment, get_line() returns a binary.
  % Convert it to a string
  case is_binary(Raw1) of
    true  -> Raw2 = erlang:binary_to_list(Raw1);
    false -> Raw2 = Raw1 
  end, 
  % Remove new line char
  Raw3 = string:strip(Raw2, right, 10),
  % Remove leading and trailing whitespace
  string:strip(Raw3).


%%
%% Parse command line input into words and quoted strings
%%
-spec parse_cli_params(Line :: string()) -> {ok, list(string())} | {error, list(string())}.

parse_cli_params(Line) ->
  % Split command line on spaces into list of words
  CmdLineWords = string:tokens(Line, " "),
  parse_cli_params(CmdLineWords, [], "").

% Normal parsing completed
parse_cli_params([], Params, "") -> 
  {ok, lists:reverse(Params)};

% Error: Open quote without a matching closing quote, or
%        Close quote before open quote
parse_cli_params([], Params, _QuotedStr) -> 
  {error, lists:reverse(Params)};

% Process command line words when not inside a quoted string
parse_cli_params([Word | RemWords], Params, "") ->
  case is_open_and_close_quoted(Word) of
    true  ->
       % QuotedStr = end_quoted_str(begin_quoted_str(Word)),
      parse_cli_params(RemWords, [Word | Params], "");
    false ->
      case is_open_quoted(Word) of
        true  -> 
          % QuotedStr = begin_quoted_str(Word),
          parse_cli_params(RemWords, Params, Word);
        false ->
          case is_close_quoted(Word) of
            true -> % closing quote before opening quote is detected, force error
              parse_cli_params([], [Word | Params], Word);
            false -> % just a plain unquoted word, add to list of parsed words
              parse_cli_params(RemWords, [Word | Params], "")
          end
      end
  end;

% Process command line words when inside a quoted string
parse_cli_params([Word | RemWords], Params, QuotedStr) ->
  case is_open_and_close_quoted(Word) of
    true  -> % Quoted word inside quoted string, 
             % let higher functions determine if this is an error
      % SubQuotedStr = end_quoted_str(begin_quoted_str(Word)),       
      NewQuotedStr = add_to_quoted_str(QuotedStr, Word),
      parse_cli_params(RemWords, Params, NewQuotedStr);
    false ->
      case is_open_quoted(Word) of
        true  -> % second opening quote before closing quote detected, force error
          NewQuotedStr = add_to_quoted_str(QuotedStr, Word),
          parse_cli_params([], [NewQuotedStr | Params], NewQuotedStr);
        false ->
          case is_close_quoted(Word) of
            true  -> % finished this quoted string
              NewQuotedStr = add_to_quoted_str(QuotedStr, Word),
              parse_cli_params(RemWords, [NewQuotedStr | Params], "");
            false -> % just a plain unquoted word
              NewQuotedStr = add_to_quoted_str(QuotedStr, Word),
              parse_cli_params(RemWords, Params, NewQuotedStr)
          end
      end
  end.

is_open_and_close_quoted(Word) -> (is_open_quoted(Word) andalso is_close_quoted(Word)).

is_open_quoted(Word) -> lists:prefix([$"], Word).

is_close_quoted(Word) -> lists:suffix([$"], Word).
  
add_to_quoted_str(QuotedStr, Word) -> QuotedStr ++ " " ++ Word.





%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% ====================================================================
% Test parse_value()
% 
%   Test float good
parse_value_float_good_test() ->
  ExpectedResult = 12.345,
  ValueStr = "12.345",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

%   Test float bad
parse_value_float_bad_test() ->
  ExpectedResult = "12.345crap",
  ValueStr = "12.345crap",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

%   Test integer good
parse_value_integer_good_test() ->
  ExpectedResult = 12345,
  ValueStr = "12345",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

%   Test integer bad
parse_value_integer_bad_test() ->
  ExpectedResult = "12345crap",
  ValueStr = "12345crap",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

%   Test boolean true
parse_value_boolean_true_test() ->
  ExpectedResult = true,
  ValueStr = "true",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

%   Test boolean false
parse_value_boolean_false_test() ->
  ExpectedResult = false,
  ValueStr = "false",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

%   Test string good
parse_value_string_good_test() ->
  ExpectedResult = 'TestString',
  ValueStr = "TestString",
  Result = parse_value(ValueStr),
  ?assertEqual(ExpectedResult, Result).

% ====================================================================

% ====================================================================
% Test parse_cli_params()
% 
% Test one param, no quotes
parse_cli_params_one_word_no_quotes_test() ->
  Line = "Test",
  ExpectedResult = {ok, ["Test"]},
  Result = parse_cli_params(Line),
  ?assertEqual(ExpectedResult, Result).

% Test one param, whitespace, no quotes
parse_cli_params_one_param_whitespace_no_quotes_test() ->
  Line = "  Test  ",
  ExpectedResult = {ok, ["Test"]},
  Result = parse_cli_params(Line),
  ?assertEqual(ExpectedResult, Result).

% Test two params, whitespace, no quotes
parse_cli_params_two_params_whitespace_no_quotes_test() ->
  Line = "  Test One ",
  ExpectedResult = {ok, ["Test", "One"]},
  Result = parse_cli_params(Line),
  ?assertEqual(ExpectedResult, Result).

% Test three params, whitespace, no quotes
parse_cli_params_three_params_whitespace_no_quotes_test() ->
  Line = "  Test One Two ",
  ExpectedResult = {ok, ["Test", "One", "Two"]},
  Result = parse_cli_params(Line),
  ?assertEqual(ExpectedResult, Result).

% Test three params, one param quoted string
parse_cli_params_three_params_one_param_quoted_test() ->
  Line = "Test \"One Two Three\" Four",
  ExpectedResult = {ok, ["Test", "\"One Two Three\"", "Four"]},
  Result = parse_cli_params(Line),
  ?assertEqual(ExpectedResult, Result).

% Test embedded quoted word in embedded quoted string
parse_cli_params_embedded_quoted_word_in_quoted_string_test() ->
  Line = "Test \"One \"Two\" Three\" Four",
  ExpectedResult = {ok, ["Test", "\"One \"Two\" Three\"", "Four"]},
  Result = parse_cli_params(Line),
  ?assertEqual(ExpectedResult, Result).

% Test two params, one param missing close quote string
parse_cli_params_two_params_missing_close_quote_test() ->
  Line = "Test \"One Two",
  ExpectedResult = {error, ["Test"]},
  Result = parse_cli_params(Line),
  ?assertEqual(ExpectedResult, Result).

% Test two params, one param missing open quote string
parse_cli_params_two_params_missing_open_quote_test() ->
  Line = "Test One Two\"",
  ExpectedResult = {error, ["Test", "One", "Two\""]},
  Result = parse_cli_params(Line),
  ?assertEqual(ExpectedResult, Result).

% Test two params, both strings
parse_cli_params_two_string_params_test() ->
  Line = "\"Test One\" \"Two Three\"",
  ExpectedResult = {ok, ["\"Test One\"", "\"Two Three\""]},
  Result = parse_cli_params(Line),
  ?assertEqual(ExpectedResult, Result).

-endif.