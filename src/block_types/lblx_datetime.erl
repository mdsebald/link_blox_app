%%% @doc 
%%% BLOCKTYPE
%%% Get Date and Time, Output formatted string and component values.
%%% DESCRIPTION
%%% Get Date Time value, Output component values and as formatted string
%%% Source may be local time, utc time, timestamp input, or component inputs
%%% Default format configuration string is "F" 
%%% Example Output: "Monday, June 15, 2009 1:45:30 PM"
%%% Format configuration follows .NET Date/Time formatting rules.  See links below.
%%% The calendar_locale() structure in the language module may be modified for other locales.
%%% LINKS
%%%   https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings
%%%   https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings
%%%   https://msdn.microsoft.com/en-us/library/hc4ky857(v=vs.71).aspx
%%%   https://msdn.microsoft.com/en-us/library/8kb3ddd4(v=vs.71).aspx
%%% @end 

-module(lblx_datetime).  
  
-author("Mark Sebald").

-include("../block_state.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================

-export([groups/0, version/0]). 
-export([create/2, create/4, create/5, upgrade/1, initialize/1, execute/2, delete/1]).

groups() -> [time].

version() -> "0.1.0".


%% Merge the block type specific, Config, Input, and Output attributes
%% with the common Config, Input, and Output attributes, that all block types have
 
-spec default_configs(BlockName :: block_name(),
                      Description :: string()) -> config_attribs().

default_configs(BlockName, Description) -> 
  attrib_utils:merge_attribute_lists(
    block_common:configs(BlockName, ?MODULE, version(), Description), 
    [
      {source, {local}}, %| enum | local | local, utc, timestamp, component |
      {format, {"F"}} %| string | "" |  Standard and Custom Date Time format string |
    ]).


-spec default_inputs() -> input_attribs().

default_inputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:inputs(),
    [
      {timestamp_in, {empty, {empty}}},  %| composite | empty | N/A |
      {year_in, {empty, {empty}}},  %| integer | empty | 0..9999 |
      {month_in, {empty, {empty}}}, %| integer | empty | 1..12 |
      {day_in, {empty, {empty}}}, %| integer | empty | 1..31 |
      {hour_in, {empty, {empty}}}, %| integer | empty | 0..23 |
      {minute_in, {empty, {empty}}}, %| integer | empty | 0..59 |
      {second_in, {empty, {empty}}}, %| integer | empty | 0..59 |
      {micro_sec_in, {empty, {empty}}} %| integer | empty | 0..999999 |
    ]). 


-spec default_outputs() -> output_attribs().
                            
default_outputs() -> 
  attrib_utils:merge_attribute_lists(
    block_common:outputs(),
    [
      {timestamp_out, {null, []}},  %| composite | null | N/A |
      {year_out, {null, []}},  %| integer | null | 0..9999 |
      {month_out, {null, []}}, %| integer | null | 1..12 |
      {day_out, {null, []}}, %| integer | null | 1..31 |
      {dow_out, {null, []}}, %| integer | null | 1..7 |
      {hour_out, {null, []}}, %| integer | null | 0..23 |
      {minute_out, {null, []}}, %| integer | null | 0..59 |
      {second_out, {null, []}}, %| integer | null | 0..59 |
      {micro_sec_out, {null, []}} %| integer | null | 0..999999 |
    ]). 


%%  
%% Create a set of block attributes for this block type.  
%% Init attributes are used to override the default attribute values
%% and to add attributes to the lists of default attributes
%%
-spec create(BlockName :: block_name(),
             Description :: string()) -> block_defn().

create(BlockName, Description) -> 
  create(BlockName, Description, [], [], []).

-spec create(BlockName :: block_name(),
             Description :: string(),  
             InitConfig :: config_attribs(), 
             InitInputs :: input_attribs()) -> block_defn().
   
create(BlockName, Description, InitConfig, InitInputs) -> 
  create(BlockName, Description, InitConfig, InitInputs, []).

-spec create(BlockName :: block_name(),
             Description :: string(), 
             InitConfig :: config_attribs(), 
             InitInputs :: input_attribs(), 
             InitOutputs :: output_attribs()) -> block_defn().

create(BlockName, Description, InitConfig, InitInputs, InitOutputs) ->

  % Update Default Config, Input, Output, and Private attribute values 
  % with the initial values passed into this function.
  %
  % If any of the intial attributes do not already exist in the 
  % default attribute lists, merge_attribute_lists() will create them.
    
  Config = attrib_utils:merge_attribute_lists(default_configs(BlockName, Description), InitConfig),
  Inputs = attrib_utils:merge_attribute_lists(default_inputs(), InitInputs), 
  Outputs = attrib_utils:merge_attribute_lists(default_outputs(), InitOutputs),

  % This is the block definition, 
  {Config, Inputs, Outputs}.


%%
%% Upgrade block attribute values, when block code and block data versions are different
%% 
-spec upgrade(BlockDefn :: block_defn()) -> {ok, block_defn()} | {error, atom()}.

upgrade({Config, Inputs, Outputs}) ->

  ModuleVer = version(),
  {BlockName, BlockModule, ConfigVer} = config_utils:name_module_version(Config),
  BlockType = type_utils:type_name(BlockModule),

  case attrib_utils:set_value(Config, version, version()) of
    {ok, UpdConfig} ->
      logger:info(block_type_upgraded_from_ver_to, 
                            [BlockName, BlockType, ConfigVer, ModuleVer]),
      {ok, {UpdConfig, Inputs, Outputs}};

    {error, Reason} ->
      logger:error(err_upgrading_block_type_from_ver_to, 
                            [Reason, BlockName, BlockType, ConfigVer, ModuleVer]),
      {error, Reason}
  end.


%%
%% Initialize block values
%% Perform any setup here as needed before starting execution
%%
-spec initialize(BlockState :: block_state()) -> block_state().

initialize({Config, Inputs, Outputs, Private}) ->

  Private1 = attrib_utils:merge_attribute_lists(Private, [{format_def, {empty}}]),

  case attrib_utils:get_value(Config, format) of
    {ok, Format} ->
      case time_utils:get_format_defn(Format) of
        {FormatStr, ParamDefs} when is_list(ParamDefs) ->
          {ok, Private2} = attrib_utils:set_value(Private1, format_def, {FormatStr, ParamDefs}),
          case attrib_utils:get_value(Config, source) of
            {ok, Source} ->
              case lists:member(Source, [local, utc, timestamp, component]) of
                true ->
                  Value = null, Status = initialed;
                false ->
                  {Value, Status} = config_utils:log_error(Config, source, invalid_value)
              end;

            {error, Reason} ->
              {Value, Status} = config_utils:log_error(Config, format, Reason)
          end;

        {error, Reason} ->
          Private2 = Private1,
          {Value, Status} = config_utils:log_error(Config, format, Reason)
      end;

    {error, Reason} ->
      Private2 = Private1,
      {Value, Status} = config_utils:log_error(Config, format, Reason)
  end,

  {ok, Outputs1} = attrib_utils:set_values(Outputs, 
  [
    {value, Value}, {status, Status},
    {timestamp_out, Value}, {year_out, Value}, {month_out, Value}, {day_out, Value}, {dow_out, Value},
    {hour_out, Value}, {minute_out, Value}, {second_out, Value}, {micro_sec_out, Value}
  ]),
  
  % This is the block state
  {Config, Inputs, Outputs1, Private2}.


%%
%%  Execute the block specific functionality
%%
-spec execute(BlockState :: block_state(), 
              ExecMethod :: exec_method()) -> block_state().

execute({Config, Inputs, Outputs, Private}, _ExecMethod) ->

  {ok, FormatDef} = attrib_utils:get_value(Private, format_def),

  case attrib_utils:get_value(Config, source) of

    {ok, local} -> % Get local time
      Status = normal,
      TimeStamp = {_, _, MicroSec} = os:timestamp(),
      {{Year, Month, Day},{Hour, Minute, Second}} = calendar:now_to_local_time(TimeStamp);
 
    {ok, utc} -> % Get UTC time
      Status = normal,
      TimeStamp = {_, _, MicroSec} = os:timestamp(),
      {{Year, Month, Day},{Hour, Minute, Second}} = calendar:now_to_universal_time(TimeStamp);
  
    {ok, timestamp} ->
      case input_utils:get_any_type(Inputs, timestamp_in) of
        {ok, TimeStamp} ->
          Status = normal,
          {_, _, MicroSec} = TimeStamp,
          {{Year, Month, Day},{Hour, Minute, Second}} = calendar:now_to_local_time(TimeStamp);

        {error, Reason} ->
          TimeStamp = Year = Month = Day = Hour = Minute = Second = MicroSec = null, 
          {null, Status} = input_utils:log_error(Config, timestamp_in, Reason)
      end;
    
    {ok, component} ->
      TimeStamp = null,
      case input_utils:get_integer_range(Inputs, year_in, 1, 9999) of
        {ok, Year} ->
          case input_utils:get_integer_range(Inputs, month_in, 1, 12) of
            {ok, Month} ->
              case input_utils:get_integer_range(Inputs, day_in, 1, 31) of
                {ok, Day} ->
                  case input_utils:get_integer_range(Inputs, hour_in, 0, 23) of
                    {ok, Hour} ->
                      case input_utils:get_integer_range(Inputs, minute_in, 0, 59) of
                        {ok, Minute} ->
                          case input_utils:get_integer_range(Inputs, second_in, 0, 59) of
                            {ok, Second} ->
                              case input_utils:get_integer_range(Inputs, micro_sec_in, 0, 999999) of
                                {ok, MicroSec} ->
                                  case calendar:valid_date(Year, Month, Day) of 
                                    true ->
                                      Status = normal;
                                    false ->
                                      {null, Status} = input_utils:log_error(Config, day_in, invalid_date)
                                  end;
                                {error, Reason} ->
                                  MicroSec = null, 
                                  {null, Status} = input_utils:log_error(Config, micro_sec_in, Reason)
                              end;  
                            {error, Reason} ->
                              Second = MicroSec = null, 
                              {null, Status} = input_utils:log_error(Config, second_in, Reason)
                          end;  
                        {error, Reason} ->
                          Minute = Second = MicroSec = null, 
                          {null, Status} = input_utils:log_error(Config, minute_in, Reason)
                      end;   
                    {error, Reason} ->
                      Hour = Minute = Second = MicroSec = null, 
                      {null, Status} = input_utils:log_error(Config, hour_in, Reason)
                  end;
                {error, Reason} ->
                  Day = Hour = Minute = Second = MicroSec = null, 
                  {null, Status} = input_utils:log_error(Config, day_in, Reason)
              end;
            {error, Reason} ->
              Month = Day = Hour = Minute = Second = MicroSec = null, 
              {null, Status} = input_utils:log_error(Config, month_in, Reason)
          end;
        {error, Reason} ->
          Year = Month = Day = Hour = Minute = Second = MicroSec = null, 
          {null, Status} = input_utils:log_error(Config, year_in, Reason)
      end
  end,

  case Status of
    normal ->
      DayOfWeek = calendar:day_of_the_week(Year, Month, Day),
      Formatted = time_utils:format_time(FormatDef, {{Year, Month, Day},{Hour, Minute, Second}}, MicroSec),
      {ok, Outputs1} = attrib_utils:set_values(Outputs, 
      [
        {value, Formatted}, {status, Status},
        {timestamp_out, TimeStamp}, {year_out, Year}, {month_out, Month}, {day_out, Day}, {dow_out, DayOfWeek},
        {hour_out, Hour}, {minute_out, Minute}, {second_out, Second}, {micro_sec_out, MicroSec}
      ]);

    _NotNormal ->
      {ok, Outputs1} = attrib_utils:set_values(Outputs, 
      [
        {value, null}, {status, Status},
        {timestamp_out, null}, {year_out, null}, {month_out, null}, {day_out, null}, {dow_out, null},
        {hour_out, null}, {minute_out, null}, {second_out, null}, {micro_sec_out, null}
      ])
  end,

  % Return updated block state
  {Config, Inputs, Outputs1, Private}.


%% 
%%  Delete the block
%%	
-spec delete(BlockState :: block_state()) -> block_defn().

delete({Config, Inputs, Outputs, _Private}) -> 
  
  {Config, Inputs, Outputs}.


%% ====================================================================
%% Internal functions
%% ====================================================================



%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("block_io_test_gen.hrl").

test_sets() ->
  [
    % test invalid config
    {[{source, bad}], [], [{status, config_err}, {value, null}, {timestamp_out, null}, {year_out, null}, {month_out, null}, {day_out, null}, {dow_out, null},
                          {hour_out, null}, {minute_out, null}, {second_out, null}, {micro_sec_out, null}]},
    {[{source, local}, {format, "K"}], [], [{status, config_err}, {value, null}, {timestamp_out, null}, {year_out, null}, {month_out, null}, {day_out, null}, {dow_out, null},
                          {hour_out, null}, {minute_out, null}, {second_out, null}, {micro_sec_out, null}]},

    % test invalid input
    {[{source, component}, {format, "F"}], [{year_in, -1}], [{status, input_err}, {value, null}, {timestamp_out, null}, {year_out, null}, {month_out, null}, {day_out, null}, {dow_out, null},
                              {hour_out, null}, {minute_out, null}, {second_out, null}, {micro_sec_out, null}]},

    {[{year_in, 2018}, {month_in, 20}], [{status, input_err}, {value, null}, {timestamp_out, null}, {year_out, null}, {month_out, null}, {day_out, null}, {dow_out, null},
                              {hour_out, null}, {minute_out, null}, {second_out, null}, {micro_sec_out, null}]},
    {[{month_in, 3}, {day_in, 32}], [{status, input_err}, {value, null}, {timestamp_out, null}, {year_out, null}, {month_out, null}, {day_out, null}, {dow_out, null},
                              {hour_out, null}, {minute_out, null}, {second_out, null}, {micro_sec_out, null}]},
    {[{day_in, 16}, {hour_in, 24}], [{status, input_err}, {value, null}, {timestamp_out, null}, {year_out, null}, {month_out, null}, {day_out, null}, {dow_out, null},
                              {hour_out, null}, {minute_out, null}, {second_out, null}, {micro_sec_out, null}]},
    {[{hour_in, 23}, {minute_in, -2}], [{status, input_err}, {value, null}, {timestamp_out, null}, {year_out, null}, {month_out, null}, {day_out, null}, {dow_out, null},
                              {hour_out, null}, {minute_out, null}, {second_out, null}, {micro_sec_out, null}]},
    {[{minute_in, 56}, {second_in, 66}], [{status, input_err}, {value, null}, {timestamp_out, null}, {year_out, null}, {month_out, null}, {day_out, null}, {dow_out, null},
                              {hour_out, null}, {minute_out, null}, {second_out, null}, {micro_sec_out, null}]},
    {[{second_in, 10}, {micro_sec_in, 1234567}], [{status, input_err}, {value, null}, {timestamp_out, null}, {year_out, null}, {month_out, null}, {day_out, null}, {dow_out, null},
                              {hour_out, null}, {minute_out, null}, {second_out, null}, {micro_sec_out, null}]},
    % test valid input
    {[{micro_sec_in, 123456}], [{status, normal}, {value, "Friday, March 16, 2018 11:56:10 PM"}, {timestamp_out, null}, {year_out, 2018}, {month_out, 3}, {day_out, 16}, 
                        {dow_out, 5}, {hour_out, 23}, {minute_out, 56}, {second_out, 10}, {micro_sec_out, 123456}]},
    % don't know date or time the test will be run, just verify status is normal
    {[{source, local}], [], [{status, normal}]},
    % don't know date or time the test will be run, just verify status is normal   
    {[{source, utc}], [], [{status, normal}]},
    {[{source, timestamp}], [{timestamp_in, {1522,1695,72085}}], [{status, normal}, {value, "Sunday, March 25, 2018 1:14:55 PM"}, {timestamp_out, {1522,1695,72085}}, {year_out, 2018}, 
                        {month_out, 3}, {day_out, 25}, {dow_out, 7}, {hour_out, 13}, {minute_out, 14}, {second_out, 55}, {micro_sec_out, 72085}]}
  ].

-endif.
