%%% @doc 
%%% Format date time values according to a Format Specification string
%%% Format specifications are modeled after the .NET DateTime formats
%%% See:
%%%   https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings
%%%   https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings
%%%   https://msdn.microsoft.com/en-us/library/hc4ky857(v=vs.71).aspx
%%%   https://msdn.microsoft.com/en-us/library/8kb3ddd4(v=vs.71).aspx
%%%               
%%% @end 

-module(time_utils). 

-author("Mark Sebald").


-export([
          format_local/1,
          format_utc/1,
          format_time/3,
          get_format_defn/1
]).


%%
%% Format the local system time according to the Format specification or 
%% Format string and Parameter definitions
%%
-spec format_local(FormatStrParamDefs :: {string(), list(atom())}) -> string();
                  (FormatSpec :: string()) -> string().

format_local({FormatStr, ParamDefs}) ->
  TimeStamp = {_, _, MicroSec} = os:timestamp(),
  format_time({FormatStr, ParamDefs}, calendar:now_to_local_time(TimeStamp), MicroSec);

format_local(FormatSpec) ->
  case get_format_defn(FormatSpec) of
    {error, Reason} -> "Error: " ++ atom_to_list(Reason);

    {FormatStr, ParamDefs} -> format_local({FormatStr, ParamDefs});

    Error ->  io_lib:format("~p", [Error])
  end.


%%
%% Format the UTC time according to the Format specification or 
%% Format string and Parameter definitions
%%
-spec format_utc(FormatStrParamDefs :: {string(), [atom()]}) -> string();
                (FormatSpec :: string()) -> string().

format_utc({FormatStr, ParamDefs}) ->
  TimeStamp = {_, _, MicroSec} = os:timestamp(),
  format_time({FormatStr, ParamDefs}, calendar:now_to_universal_time(TimeStamp), MicroSec);

format_utc(FormatSpec) ->
  case get_format_defn(FormatSpec) of
    {error, Reason} -> "Error: " ++ atom_to_list(Reason);

    {FormatStr, ParamDefs} -> format_utc({FormatStr, ParamDefs});

    Error ->  io_lib:format("~p", [Error])
  end.


%%
%% Format the date and time, according to the Format specification or 
%% Format string and Parameter definitions
%%
-spec format_time(FormatStrParamDefs :: {string(), [atom()]},
                  DateTime :: {{pos_integer(), pos_integer(), pos_integer()},
                  {non_neg_integer(), non_neg_integer(), non_neg_integer()}},
                  MicroS :: non_neg_integer()) -> string();
                 (FormatSpec :: string(),
                  DateTime :: {{pos_integer(), pos_integer(), pos_integer()},
                  {non_neg_integer(), non_neg_integer(), non_neg_integer()}},
                  MicroS :: non_neg_integer()) -> string().

format_time({FormatStr, ParamDefs}, DateTime, MicroS) ->
  ParamVals = get_param_values(ParamDefs, ui_utils:get_calendar_locale(), DateTime, MicroS),
  lists:flatten(io_lib:format(FormatStr, ParamVals));
  
format_time(FormatSpec, DateTime, MicroS) ->
  case get_format_defn(FormatSpec) of
    {error, Reason} -> "Error: " ++ atom_to_list(Reason);

    {FormatStr, ParamDefs} ->
      format_time({FormatStr, ParamDefs}, DateTime, MicroS);

    Error ->  io_lib:format("~p", [Error])
  end.


%%
%% Translate a list of date/time Parameter definitions (atoms) 
%% into a list of actual date/time values.
%% Used to create a list of values to be inserted into a date/time format string.
%%
-spec get_param_values(ParamDefs :: [atom()],
                       CalendarStrs :: [{atom(), list()}],
                       DateTime :: {{pos_integer(), pos_integer(), pos_integer()},
                                    {non_neg_integer(), non_neg_integer(), non_neg_integer()}},
                       MicroS :: non_neg_integer()) -> [term()].

get_param_values(ParamDefs, CalendarStrs, {{Year, Month, Day}, {Hour, Minute, Second}}, MicroS) ->

  lists:map(fun(ParamDef) -> 
    case ParamDef of
      year2 -> Year rem 100;
      year3 -> Year rem 1000;
      year -> Year;

      month -> Month;
      month_str -> 
        get_calendar_string(CalendarStrs, months_strs, Month);
      month_abbr -> 
        get_calendar_string(CalendarStrs, months_abbr, Month);        

      day -> Day;
      day_str -> 
        get_calendar_string(CalendarStrs, days_strs, 
                            calendar:day_of_the_week(Year, Month, Day));
      day_abbr -> 
        get_calendar_string(CalendarStrs, days_abbr, 
                            calendar:day_of_the_week(Year, Month, Day));

      hour -> Hour;
      hour12 when Hour > 12 -> Hour - 12;
      hour12 -> Hour;

      am_pm when Hour >= 12 -> get_calendar_string(CalendarStrs, pm_str);
      am_pm -> get_calendar_string(CalendarStrs, am_str);
      
      am_pm1 when Hour >= 12 -> 
        [FirstChar | _Rest] = get_calendar_string(CalendarStrs, pm_str),
        [FirstChar];

      am_pm1 -> 
        [FirstChar | _Rest] = get_calendar_string(CalendarStrs, am_str),
        [FirstChar];

      minute -> Minute;
      second -> Second;

      fract_1 -> MicroS div 100000;
      fract_2 -> MicroS div 10000;
      fract_3 -> MicroS div 1000;
      fract_4 -> MicroS div 100;
      fract_5 -> MicroS div 10;
      fract_6 -> MicroS;

      fract_nz_1 -> non_zero_or_no_string(MicroS, 100000);
      fract_nz_2 -> non_zero_or_no_string(MicroS, 10000);
      fract_nz_3 -> non_zero_or_no_string(MicroS, 1000);
      fract_nz_4 -> non_zero_or_no_string(MicroS, 100);
      fract_nz_5 -> non_zero_or_no_string(MicroS, 10);
      fract_nz_6 -> non_zero_or_no_string(MicroS, 1);

      date_sep -> get_calendar_string(CalendarStrs, date_sep_str);

      time_sep -> get_calendar_string(CalendarStrs, time_sep_str);

      tz_offset_1 -> get_tz_offset_1();
      tz_offset_2 -> get_tz_offset_2();
      tz_offset_3 -> get_tz_offset_3();

      tz_name -> get_tz_name();

      era -> get_calendar_string(CalendarStrs, era_str);

      Undefined -> "Undef token: " + atom_to_list(Undefined)
    end end, ParamDefs).


%%  
%% Create an Erlang format time string and parameter defintion list
%% Following C# .NET time formatting standard
%%
-spec get_format_defn(FormatSpec :: string()) -> {string(), [atom()]} | {error, atom()}.

% Default format, same as 'G' format
get_format_defn([]) ->
    CalendarStrs = ui_utils:get_calendar_locale(),
    {DateFormatStr, DateParamDefs} = get_calendar_string(CalendarStrs, short_date_format),
    {TimeFormatStr, TimeParamDefs} = get_calendar_string(CalendarStrs, long_time_format),
    {DateFormatStr ++ " " ++ TimeFormatStr, DateParamDefs ++ TimeParamDefs};

% Standard formats (One character format definition)
get_format_defn(FormatSpec) when length(FormatSpec) == 1 ->
  CalendarStrs = ui_utils:get_calendar_locale(),
  case FormatSpec of
    "d" -> % Date, short date
      get_calendar_string(CalendarStrs, short_date_format);

    "D" -> % Date, long date
      get_calendar_string(CalendarStrs, long_date_format);

    "f" -> % Full, long date / short time
      {DateFormatStr, DateParamDefs} = get_calendar_string(CalendarStrs, long_date_format),
      {TimeFormatStr, TimeParamDefs} = get_calendar_string(CalendarStrs, short_time_format),
      {DateFormatStr ++ " " ++ TimeFormatStr, DateParamDefs ++ TimeParamDefs};

    "F" -> % Full, long date / long time
      {DateFormatStr, DateParamDefs} = get_calendar_string(CalendarStrs, long_date_format),
      {TimeFormatStr, TimeParamDefs} = get_calendar_string(CalendarStrs, long_time_format),
      {DateFormatStr ++ " " ++ TimeFormatStr, DateParamDefs ++ TimeParamDefs};

    "g" -> % General, short date / short time
      {DateFormatStr, DateParamDefs} = get_calendar_string(CalendarStrs, short_date_format),
      {TimeFormatStr, TimeParamDefs} = get_calendar_string(CalendarStrs, short_time_format),
      {DateFormatStr ++ " " ++ TimeFormatStr, DateParamDefs ++ TimeParamDefs};

    "G" -> % General, short date / long time
      {DateFormatStr, DateParamDefs} = get_calendar_string(CalendarStrs, short_date_format),
      {TimeFormatStr, TimeParamDefs} = get_calendar_string(CalendarStrs, long_time_format),
      {DateFormatStr ++ " " ++ TimeFormatStr, DateParamDefs ++ TimeParamDefs};
      
    "m" -> % Month Day
      get_calendar_string(CalendarStrs, month_day_format);

    "M" ->  % Month Day
      get_calendar_string(CalendarStrs, month_day_format);

    "o" -> % Round Trip
      round_trip_format();
                                              
    "O" -> % Round Trip
      round_trip_format();

    "r" -> % RFC1123
      rfc1123_format();

    "R" -> % RFC1123
      rfc1123_format();

    "s" -> % Sortable
      {"~4b-~2..0b-~2..0wT~2..0b:~2..0b:~2..0b", [year, month, day, hour, minute, second]};

    "t" -> % Short time
      get_calendar_string(CalendarStrs, short_time_format);

    "T" -> % Long time
      get_calendar_string(CalendarStrs, long_time_format);

    "u" -> % Universal sortable
      {"~4b-~2..0b-~2..0w ~2..0b:~2..0b:~2..0bZ", [year, month, day, hour, minute, second]};

    "U" -> % Universal full
      get_calendar_string(CalendarStrs, universal_full_format);

    "y" -> % Month year
      get_calendar_string(CalendarStrs, month_year_format);  

    "Y" -> % Month year
      get_calendar_string(CalendarStrs, month_year_format);  
  
    _ -> {error, invalid_format}
  end;

% Custom formats (Multi character format definition)
get_format_defn(FormatSpec) ->
  {LastFormatStr, LastFormat, LastParamDefs, LastParamDef, _, _} =
    lists:foldl(
      fun(FormatChar, {FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}) -> 
        case FormatChar of
          $y -> parse_year({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $M -> parse_month({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $d -> parse_day({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $h -> parse_12hour({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $H -> parse_24hour({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $m -> parse_minute({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $s -> parse_second({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $t -> parse_am_pm({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $f -> parse_fract({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $F -> parse_fract_nz({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $z -> parse_tz_offset({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $Z -> parse_tz_name({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $/ -> parse_date_sep({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $: -> parse_time_sep({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});
          $g -> parse_era({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count});

          % 2nd Backslash char, add literal Backslash
          $\\ when LastChar == $\\ -> 
            add_next_format_and_char(FormatStr, NextFormat, ParamDefs, NextParamDef, $\\);

          % Backslash char, literal char to follow
          $\\ ->
            {FormatStr ++ NextFormat, "", ParamDefs ++ NextParamDef, [],  $\\, 1};

          % Percent char, single char field definition may follow
          $% ->
            {FormatStr ++ NextFormat, "", ParamDefs ++ NextParamDef, [],  $%, 1};

          % Treat any character not already consumed, as a literal character    
          LiteralChar ->
            add_next_format_and_char(FormatStr, NextFormat, ParamDefs, NextParamDef, LiteralChar)
        end 
      end, 
      init_accum(), 
      FormatSpec),
  
  {LastFormatStr ++ LastFormat, LastParamDefs ++ LastParamDef}.


%% ====================================================================
%% Helper functions for building custom date/time format string and 
%%  parameter definitions list
%% ====================================================================

% Used more than once
round_trip_format() -> 
  {"~4b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.~6..0b", [year, month, day, hour, minute, second, fract_6]}.

rfc1123_format() -> 
  {"~s, ~b ~s ~4b ~2..0b:~2..0b:~2..0b GMT", [day_abbr, day, month_abbr, year, hour, minute, second]}.

%
% Setup the initial accumulator tuple for building 
% the Format string, and Parameter definition list
%
init_accum() ->
  FormatStr = "",
  NextFormat = "",
  ParamDefs = [],
  NextParamDef = [],
  PrevChar = null,
  CharCount = 0,
  {FormatStr, NextFormat, ParamDefs, NextParamDef, PrevChar, CharCount}.

% Year 'y' 
parse_year({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}) ->
  case LastChar of 
    $y when (Count == 2) ->
      {FormatStr, "~3..0b", ParamDefs, [year3], LastChar, Count + 1};

    $y when (Count >= 3) ->
      NewCount = Count + 1,
      NewNextFormat = lists:flatten(io_lib:format("~c~b\..0b", [$~, NewCount])),
      {FormatStr, NewNextFormat, ParamDefs, [year], LastChar, NewCount};

    _ -> % Switch to standard 2 digit formatting
      parse_2digit({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}, $y, [year2])
  end.

% Month 'M'
parse_month({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}) ->
  case LastChar of 
    $M when (Count == 2) ->
      {FormatStr, "~s", ParamDefs, [month_abbr], LastChar, Count + 1};

    $M when (Count == 3) ->
      {FormatStr, NextFormat, ParamDefs, [month_str], LastChar, Count + 1};
        
    $M when (Count > 3) -> % Format is not changing, just consume the chars
      {FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count};

    _ -> % Switch to standard 2 digit formatting
      parse_2digit({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}, $M, [month])
  end.

% Day 'd'
parse_day({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}) ->
  case LastChar of
    $d when (Count == 2) ->
      {FormatStr, "~s", ParamDefs, [day_abbr], LastChar, Count + 1};

    $d when (Count == 3) ->
      {FormatStr, NextFormat, ParamDefs, [day_str], LastChar, Count + 1};
      
    $d when (Count > 3) -> % Format is not changing, just consume the chars
      {FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count};

    _ -> % Switch to standard 2 digit formatting
      parse_2digit({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}, $d, [day])
  end.

% 12 Hour 'h'
parse_12hour({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}) ->
  parse_2digit({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}, $h, [hour12]).

% 24 Hour 'H'
parse_24hour({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}) ->
  parse_2digit({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}, $H, [hour]).

% Minute 'm'
parse_minute({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}) ->
  parse_2digit({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}, $m, [minute]).

% Second 's'
parse_second({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}) ->
  parse_2digit({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}, $s, [second]).

% Formatting rules are the same for standard 2 digit numbers:
%   2 digit year, month number, day number, 12/24 hour, minute, and second
parse_2digit({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}, FormatChar, ParamDef) ->
  case LastChar of
    $\\ ->
      add_next_format_and_char(FormatStr, NextFormat, ParamDefs, NextParamDef, FormatChar);

    $% -> 
      add_single_char_format(FormatStr, "~b", ParamDefs, ParamDef);

    FormatChar when (Count == 1) ->
      {FormatStr, "~2..0b", ParamDefs, NextParamDef, LastChar, Count + 1};

    FormatChar when (Count > 1) -> % Format is not changing, just consume the chars
      {FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count};

    _ ->
      {FormatStr ++ NextFormat, "~b", ParamDefs ++ NextParamDef, ParamDef, FormatChar, 1}
  end.

% AM/PM 't'
parse_am_pm({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}) ->
  case LastChar of
    $\\ ->
      add_next_format_and_char(FormatStr, NextFormat, ParamDefs, NextParamDef, $t);

    $% ->
      add_single_char_format(FormatStr, "~s", ParamDefs, [am_pm1]);

    $t when (Count == 1) ->
      {FormatStr, NextFormat, ParamDefs, [am_pm], LastChar, Count + 1};

    $t when (Count > 1) -> % Format is not changing, just consume the chars
      {FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count};

    _ ->
      {FormatStr ++ NextFormat, "~s", ParamDefs ++ NextParamDef, [am_pm1], $t, 1}
  end.

% Fraction of second 'f', with leading zeros
parse_fract({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}) ->
  case LastChar of
    $\\ ->
      add_next_format_and_char(FormatStr, NextFormat, ParamDefs, NextParamDef, $f);
    
    $% ->
      add_single_char_format(FormatStr, "~b", ParamDefs, [fract_1]);

    $f when (1 =< Count) andalso (Count =< 4) ->
      NewCount = Count + 1,
      NewNextFormat =  lists:flatten(io_lib:format("~c~b\..0b", [$~, NewCount])),
      NewParamDef = list_to_atom(lists:flatten(io_lib:format("fract_~b", [NewCount]))),
      {FormatStr, NewNextFormat, ParamDefs, [NewParamDef], LastChar, NewCount};

    $f -> % Allow maximum of 6 consecutive "f"s, (full microSecond resolution).
      {FormatStr ++ "~6..0b", "", ParamDefs ++ [fract_6], [], null, 0};

    _ ->
      {FormatStr ++ NextFormat, "~b", ParamDefs ++ NextParamDef, [fract_1], $f, 1}
  end.

% Fraction of second 'F', without leading zeros
parse_fract_nz({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}) ->
  case LastChar of
    $\\ ->
      add_next_format_and_char(FormatStr, NextFormat, ParamDefs, NextParamDef, $F);

    $% ->
      add_single_char_format(FormatStr, "~s", ParamDefs, fract_nz_1);

    $F when (1 =< Count) andalso (Count =< 4) ->
      NewCount = Count + 1,
      NewParamDef = list_to_atom(lists:flatten(io_lib:format("fract_nz_~b", [NewCount]))),
      {FormatStr, NextFormat, ParamDefs, [NewParamDef], LastChar, NewCount};

    $F -> % Allow maximum of 6 consecutive "F"s, (full microSecond resolution).
      {FormatStr ++ "~s", "", ParamDefs ++ [fract_nz_6], [], null, 0};
    
    _ ->
      {FormatStr ++ NextFormat, "~s", ParamDefs ++ NextParamDef, [fract_nz_1], $F, 1}
  end.

% Time zone offset 'z'
parse_tz_offset({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}) ->
  case LastChar of
    $\\ ->
      add_next_format_and_char(FormatStr, NextFormat, ParamDefs, NextParamDef, $z);

    $% ->
      add_single_char_format(FormatStr, "~s", ParamDefs, [tz_offset_1]);

    $z when (Count == 1) ->
      {FormatStr, NextFormat, ParamDefs, [tz_offset_2], LastChar, Count + 1};

    $z when (Count == 2) ->
      {FormatStr, NextFormat, ParamDefs, [tz_offset_3], LastChar, Count + 1};

    $z when (Count > 2) -> % Format is not changing, just consume the chars.
      {FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count};
    
    _ ->
      {FormatStr ++ NextFormat, "~s", ParamDefs ++ NextParamDef, [tz_offset_1], $z, 1}
  end.

% Time zone name 'Z'
parse_tz_name({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, _Count}) ->
  case LastChar of
    $\\ -> 
      add_next_format_and_char(FormatStr, NextFormat, ParamDefs, NextParamDef, $Z);
    $% ->
      add_single_char_format(FormatStr, "~s", ParamDefs, [tz_name]);      
    _ ->
      {FormatStr ++ NextFormat ++ "~s", "", ParamDefs ++ NextParamDef ++ [tz_name], [], null, 0}
  end.

% Date Separator '/'
parse_date_sep({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, _Count}) ->
  case LastChar of
    $\\ -> 
      add_next_format_and_char(FormatStr, NextFormat, ParamDefs, NextParamDef, $/);
    $% ->
      add_single_char_format(FormatStr, "~s", ParamDefs, [date_sep]);      
    _ ->
      {FormatStr ++ NextFormat ++ "~s", "", ParamDefs ++ NextParamDef ++ [date_sep], [], null, 0}
  end.

% Time Separator ':'
parse_time_sep({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, _Count}) ->
  case LastChar of
    $\\ -> 
      add_next_format_and_char(FormatStr, NextFormat, ParamDefs, NextParamDef, $:);
    $% ->
      add_single_char_format(FormatStr, "~s", ParamDefs, [time_sep]);      
    _ ->
      {FormatStr ++ NextFormat ++ "~s", "", ParamDefs ++ NextParamDef ++ [time_sep], [], null, 0}
  end.

% Era (i.e. "A.D.") 'g'
parse_era({FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count}) ->
case LastChar of
    $\\ ->
      add_next_format_and_char(FormatStr, NextFormat, ParamDefs, NextParamDef, $g);

    $% ->
      add_single_char_format(FormatStr, "~s", ParamDefs, [era]);

    $z when (Count > 0) -> % Format is not changing, just consume the chars.
      {FormatStr, NextFormat, ParamDefs, NextParamDef, LastChar, Count};
    
    _ ->
      {FormatStr ++ NextFormat, "~s", ParamDefs ++ NextParamDef, [era], $z, 1}
  end.

%
% Add the next Format and the next Parameter definition to the
% current Format String and Parameter definitions list respectively.
% then add a literal character to the Format string.
% The next Format, and next Parameter definition may be blank, that is OK.
% Reset the remaining parameters in the accumulater tuple.
%
add_next_format_and_char(FormatStr, NextFormat, ParamDefs, NextParamDef, Char) ->
  {FormatStr ++ NextFormat ++ [Char], "", ParamDefs ++ NextParamDef, [], null, 0}.

%
% Add a format and associated parameter definition to the 
% current Format String and Parameter definitions list respectively. 
% Do this when we know there is no pending NextFormat/NextParamDef
% and the format is defined by only one character.
% In this case we don't need to wait for the next character.
% Reset the remaining parameters in the accumulater tuple.
%
add_single_char_format(FormatStr, Format, ParamDefs, ParamDef) ->
  {FormatStr ++ Format, "", ParamDefs ++ ParamDef, [], null, 0}.

%
% Pick a calendar string based on the string group name and index
%
get_calendar_string(CalendarStrs, Group) ->
  get_calendar_string(CalendarStrs, Group, 1).

get_calendar_string(CalendarStrs, Group, Index) ->
  case lists:keyfind(Group, 1, CalendarStrs) of
    {Group, GroupStrs} -> lists:nth(Index, GroupStrs);

    _Error -> "Invalid calendar strings"
  end.

%
% Return a digit string, if the result of the 
% integer division > zero
%
non_zero_or_no_string(MicroS, Divisor) ->
  case MicroS div Divisor of
    0 -> [];
    N -> lists:flatten(io_lib:format("~b", [N]))
  end.


%
% Get Time zone info, via OS command
%
get_tz_offset_1() ->
  TzOffset = string:trim(os:cmd("date +%:::z")),
  case lists:nth(2, TzOffset) of
    $0 -> lists:delete($0, TzOffset);
     _ -> TzOffset
  end.

get_tz_offset_2() ->
  string:trim(os:cmd("date +%:::z")).

get_tz_offset_3() ->
  string:trim(os:cmd("date +%:z")).

get_tz_name() ->
  string:trim(os:cmd("date +%Z")).


%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% ====================================================================
% Test format_time()

% Test local and utc
format_time_local_and_utc_test() ->
  {Value, _Rest} = string:to_integer(format_local("%H")),
  {Expected, _Rest} = string:to_integer(format_utc("%H")),
  ?assertEqual((Expected-5), Value).

% Test default
format_time_default_test() ->
  Value = format_time("", {{2018, 3, 25}, {19,19,19}}, 0),
  Expected = "3/25/2018 7:19:19 PM",
  ?assertEqual(Expected, Value).

% Test short date
format_time_short_date_test() ->
  Value = format_time("d", {{2018, 3, 22}, {9,9,9}}, 0),
  Expected = "3/22/2018",
  ?assertEqual(Expected, Value).

% Test long date
format_time_long_date_test() ->
  Value = format_time("D", {{2018, 3, 22}, {9,9,9}}, 0),
  Expected = "Thursday, March 22, 2018",
  ?assertEqual(Expected, Value).

% Test full date short time
format_time_full_date_short_time_test() ->
  Value = format_time("f", {{2018, 3, 23}, {19,19,19}}, 0),
  Expected = "Friday, March 23, 2018 7:19 PM",
  ?assertEqual(Expected, Value).

% Test full date long time
format_time_full_date_long_time_test() ->
  Value = format_time("F", {{2018, 3, 24}, {19,19,19}}, 0),
  Expected = "Saturday, March 24, 2018 7:19:19 PM",
  ?assertEqual(Expected, Value).

% Test general date short time
format_time_general_date_short_time_test() ->
  Value = format_time("g", {{2018, 3, 23}, {19,19,19}}, 0),
  Expected = "3/23/2018 7:19 PM",
  ?assertEqual(Expected, Value).

% Test general date long time
format_time_general_date_long_time_test() ->
  Value = format_time("G", {{2018, 3, 24}, {19,19,19}}, 0),
  Expected = "3/24/2018 7:19:19 PM",
  ?assertEqual(Expected, Value).

% Test month day 'm'
format_time_month_date_m_test() ->
  Value = format_time("m", {{2018, 3, 23}, {19,19,19}}, 0),
  Expected = "March 23",
  ?assertEqual(Expected, Value).

% Test month day 'M'
format_time_month_day_M_test() ->
  Value = format_time("M", {{2018, 3, 2}, {19,19,19}}, 0),
  Expected = "March 2",
  ?assertEqual(Expected, Value).

% Test round trip 'o'
format_time_round_trip_o_test() ->
  Value = format_time("o", {{2018, 3, 22}, {9,9,9}}, 123456),
  Expected = "2018-03-22T09:09:09.123456",
  ?assertEqual(Expected, Value).

% Test round trip 'O'
format_time_round_trip_O_test() ->
  Value = format_time("O", {{2018, 3, 22}, {9,9,9}}, 123456),
  Expected = "2018-03-22T09:09:09.123456",
  ?assertEqual(Expected, Value).

% Test RFC1123 'r'
format_time_rfc1123_r_test() ->
  Value = format_time("r", {{2018, 3, 22}, {9,9,9}}, 123456),
  Expected = "Thu, 22 Mar 2018 09:09:09 GMT",
  ?assertEqual(Expected, Value).

% Test RFC1123 'R'
format_time_rfc1123_R_test() ->
  Value = format_time("R", {{2018, 3, 22}, {9,9,9}}, 123456),
  Expected = "Thu, 22 Mar 2018 09:09:09 GMT",
  ?assertEqual(Expected, Value).

% Test sortable 's'
format_time_sortable_test() ->
  Value = format_time("s", {{2018, 3, 23}, {19,19,19}}, 0),
  Expected = "2018-03-23T19:19:19",
  ?assertEqual(Expected, Value).

% Test short time 't'
format_time_short_time_test() ->
  Value = format_time("t", {{2018, 3, 22}, {9,9,9}}, 0),
  Expected = "9:09 AM",
  ?assertEqual(Expected, Value).

% Test long time 'T'
format_time_long_time_test() ->
  Value = format_time("T", {{2018, 3, 22}, {9,9,9}}, 0),
  Expected = "9:09:09 AM",
  ?assertEqual(Expected, Value).

% Test universal 'u'
format_time_universal_test() ->
  Value = format_time("u", {{2018, 3, 23}, {19,19,19}}, 0),
  Expected = "2018-03-23 19:19:19Z",
  ?assertEqual(Expected, Value).

% Test universal full 'U'
format_time_universal_full_test() ->
  Value = format_time("U", {{2018, 3, 23}, {19,19,19}}, 0),
  Expected = "Friday, March 23, 2018 7:19:19 PM",
  ?assertEqual(Expected, Value).

% Test custom format 
format_time_custom_format_test() ->
  Value = format_time("Year: y, yy, yyy, yyyy, yyyyy \\Mon\\t\\h: M, MM, MMM, MMMM Da\\y: d, dd, ddd, dddd \\Hour: h, hh, H, HH \\Minu\\te: m, mm Secon\\d: s, ss t tt", 
                        {{2108, 3, 18}, {3, 3, 3}}, 0),
  Expected = "Year: 8, 08, 108, 2108, 02108 Month: 3, 03, Mar, March Day: 18, 18, Sun, Sunday Hour: 3, 03, 3, 03 Minute: 3, 03 Second: 3, 03 A AM",
  ?assertEqual(Expected, Value).

format_time_custom_format2_test() ->
  Value = format_time("U\\sec: f, ff, fff, ffff, fffff, ffffff, ffffffffffff", 
                        {{2108, 3, 18}, {3, 3, 3}}, 123456),
  Expected = "Usec: 1, 12, 123, 1234, 12345, 123456, 123456123456",
  ?assertEqual(Expected, Value). 

format_time_custom_format3_test() ->
  Value = format_time("u\\Sec: f, ff, fff, ffff, fffff, ffffff, ffffffffffff", 
                        {{2108, 3, 18}, {3, 3, 3}}, 6),
  Expected = "uSec: 0, 00, 000, 0000, 00000, 000006, 000006000006",
  ?assertEqual(Expected, Value).

format_time_custom_format4_test() ->
  Value = format_time("uSec: F, FF, FFF, FFFF, FFFFF, FFFFFF, FFFFFFFFFFFF", 
                        {{2108, 3, 18}, {3, 3, 3}}, 6),
  Expected = "uSec: , , , , , 6, 66",
  ?assertEqual(Expected, Value). 

format_time_custom_format5_test() ->
  Value = format_time("T\\Z O\\f\\f\\se\\t: z, zz, zzz, %z T\\Z Na\\me: Z, %Z", 
                        {{2108, 3, 18}, {3, 3, 3}}, 6),
  Expected = "TZ Offset: -5, -05, -05:00, -5 TZ Name: DST, DST",
  ?assertEqual(Expected, Value). 

format_time_custom_format6_test() ->
  Value = format_time("Era: g, gg, ggg, %g", 
                        {{2108, 3, 18}, {3, 3, 3}}, 6),
  Expected = "Era: A.D., A.D., A.D., A.D.",
  ?assertEqual(Expected, Value). 

% ====================================================================

% ====================================================================
% Test get_format_defn()

get_format_str_year_test() ->
  Value = get_format_defn("y, yy, yyy, yyyy, yyyyyyyyyyy"),
  Expected = {"~b, ~2..0b, ~3..0b, ~4..0b, ~11..0b", [year2, year2, year3, year, year]},
  ?assertEqual(Expected, Value).

get_format_str_month_test() ->
  Value = get_format_defn("M, MM, MMM, MMMM, MMMMMMMMMMM"),
  Expected = {"~b, ~2..0b, ~s, ~s, ~s", [month, month, month_abbr, month_str, month_str]},
  ?assertEqual(Expected, Value).

get_format_str_day_test() ->
  Value = get_format_defn("d, dd, ddd, dddd, ddddddddd"),
  Expected = {"~b, ~2..0b, ~s, ~s, ~s", [day, day, day_abbr, day_str, day_str]},
  ?assertEqual(Expected, Value).
    
get_format_str_hour12_test() ->
  Value = get_format_defn("h, hh, hhhh,"),
  Expected = {"~b, ~2..0b, ~2..0b,", [hour12, hour12, hour12]},
  ?assertEqual(Expected, Value).
    
get_format_str_hour_test() ->
  Value = get_format_defn("H, HH, HHHHH,"),
  Expected = {"~b, ~2..0b, ~2..0b,", [hour, hour, hour]},
  ?assertEqual(Expected, Value).

get_format_str_am_pm_test() ->
  Value = get_format_defn("t, tt, tttt,"),
  Expected = {"~s, ~s, ~s,", [am_pm1, am_pm, am_pm]},
  ?assertEqual(Expected, Value).

get_format_str_minute_test() ->
  Value = get_format_defn("m, mm, mmmmmmmm,"),
  Expected = {"~b, ~2..0b, ~2..0b,", [minute, minute, minute]},
  ?assertEqual(Expected, Value).

get_format_str_second_test() ->
  Value = get_format_defn("s, ss, sssssssss,"),
  Expected = {"~b, ~2..0b, ~2..0b,", [second, second, second]},
  ?assertEqual(Expected, Value).

get_format_str_fract_test() ->
    Value = get_format_defn("f, ff, fff, ffff, fffff, ffffff, ffffffffff"),
    Expected = {"~b, ~2..0b, ~3..0b, ~4..0b, ~5..0b, ~6..0b, ~6..0b~4..0b", [fract_1, fract_2, fract_3, fract_4, fract_5, fract_6, fract_6, fract_4]},
    ?assertEqual(Expected, Value).

get_format_str_fract_nz_test() ->
  Value = get_format_defn("F, FF, FFF, FFFF, FFFFF, FFFFFF, FFFFFFFFFF"),
  Expected = {"~s, ~s, ~s, ~s, ~s, ~s, ~s~s", [fract_nz_1, fract_nz_2, fract_nz_3, fract_nz_4, fract_nz_5, fract_nz_6, fract_nz_6, fract_nz_4]},
  ?assertEqual(Expected, Value).

get_format_str_tz_name_test() ->
  Value = get_format_defn("Z, %Z, \\Z "),
  Expected = {"~s, ~s, Z ", [tz_name, tz_name]},
  ?assertEqual(Expected, Value).

get_format_str_tz_offset_test() ->
  Value = get_format_defn("z, zz, zzz, zzzz, \\z, %z "),
  Expected = {"~s, ~s, ~s, ~s, z, ~s ", [tz_offset_1, tz_offset_2, tz_offset_3, tz_offset_3, tz_offset_1]},
  ?assertEqual(Expected, Value).

get_format_str_date_time_sep_test() ->
  Value = get_format_defn("//, ::, /:/: "),
  Expected = {"~s~s, ~s~s, ~s~s~s~s ", [date_sep, date_sep, time_sep, time_sep, date_sep, time_sep, date_sep, time_sep]},
  ?assertEqual(Expected, Value).

get_format_str_era_test() ->
  Value = get_format_defn("g, gg, ggg, %g, \\g"),
  Expected = {"~s, ~s, ~s, ~s, g", [era, era, era, era]},
  ?assertEqual(Expected, Value).


get_format_str_literal_test() ->
  Value = get_format_defn("abceijklnopqruvwx 1234567890 ABCDEGIJKLNOPQRSTUVWXY `~!@#$^&*()_-+={}[];\"'"),
  Expected = {"abceijklnopqruvwx 1234567890 ABCDEGIJKLNOPQRSTUVWXY `~!@#$^&*()_-+={}[];\"'", []},
  ?assertEqual(Expected, Value).

get_format_str_escape_test() ->
  Value = get_format_defn("\\M\\d\\h\\H\\t\\m\\s\\f\\F\\Z\\z\\g\\/\\:"),
  Expected = {"MdhHtmsfFZzg/:", []},
  ?assertEqual(Expected, Value).

% ====================================================================


% ====================================================================
% Test get_tz_offset()
get_tz_offest_z_test() ->
  Value = get_tz_offset_1(),
  Expected = "-5",
  ?assertEqual(Expected, Value).

get_tz_offest_zz_test() ->
  Value = get_tz_offset_2(),
  Expected = "-05",
  ?assertEqual(Expected, Value).

get_tz_offest_zzz_test() ->
  Value = get_tz_offset_3(),
  Expected = "-05:00",
  ?assertEqual(Expected, Value).

get_tz_name_test() ->
  Value = get_tz_name(),
  Expected = "DST",
  ?assertEqual(Expected, Value).

% ====================================================================


-endif.