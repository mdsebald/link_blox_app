%%% @doc
%%% Common logging server module.
%%% All LinkBlox log messages are routed through here.
%%%
%%% @end

-module(logger).

-author("Mark Sebald").

%% ====================================================================
%% API functions
%% ====================================================================

-export([
          error/1,
          error/2,
          warning/1,
          warning/2,
          info/1,
          info/2,
          debug/1,
          debug/2
]).

% TODO: Add calls to configure logging, i.e. to file, turn off tty, etc

%%
%% Log error messages
%%
-spec error(LogMsgId :: atom()) -> ok.

error(LogMsgId) ->
  log_message(error, self(), LogMsgId).


-spec error(LogMsgId :: atom(),
            Args :: list(term())) -> ok.

error(LogMsgId, Args) -> 
  log_message(error, self(), LogMsgId, Args).


%%
%% Log warning messages
%%
-spec warning(LogMsgId :: atom()) -> ok.

warning(LogMsgId) -> 
  log_message(warning, self(), LogMsgId).


-spec warning(LogMsgId :: atom(),
              Args :: list(term())) -> ok.

warning(LogMsgId, Args) ->
  log_message(warning, self(), LogMsgId, Args).


%%
%% Log info messages
%%
-spec info(LogMsgId :: atom()) -> ok.

info(LogMsgId) ->
  log_message(info, self(), LogMsgId).


-spec info(LogMsgId :: atom(),
           Args :: list(term())) -> ok.

info(LogMsgId, Args) -> 
  log_message(info, self(), LogMsgId, Args).

%%
%% Log debug messages
%% For debugging purposes, don't use string map, just pass in string
%%
-spec debug(LogMsg :: string()) -> ok.

debug(LogMsg) ->
  lager:log(debug, self(), LogMsg).


-spec debug(LogMsg :: string(),
            Args :: list(term())) -> ok.

debug(LogMsg, Args) ->
  lager:log(debug, self(), LogMsg, Args).


%% ====================================================================
%% Internal functions
%% ====================================================================


log_message(LogLevel, Pid, LogMsgId) ->
  LogMsg = ui_utils:get_log_string(LogMsgId),
  lager:log(LogLevel, Pid, LogMsg).

log_message(LogLevel, Pid, LogMsgId, Args) ->
  LogMsg = ui_utils:get_log_string(LogMsgId),
  lager:log(LogLevel, Pid, LogMsg, Args).
