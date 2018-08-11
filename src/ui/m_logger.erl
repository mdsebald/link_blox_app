%%% @doc
%%% Common logging server module.
%%% All LinkBlox log messages are routed through here.
%%%
%%% @end

-module(m_logger).

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
-spec error(LogMsg :: string() | atom()) -> ok.

error(LogMsg) ->
  log_message(error, self(), LogMsg).


-spec error(LogMsg :: string() | atom(),
            Args :: list(term())) -> ok.

error(LogMsg, Args) -> 
  log_message(error, self(), LogMsg, Args).


%%
%% Log warning messages
%%
-spec warning(LogMsg :: string() | atom()) -> ok.

warning(LogMsg) -> 
  log_message(warning, self(), LogMsg).


-spec warning(LogMsg :: string() | atom(),
              Args :: list(term())) -> ok.

warning(LogMsg, Args) ->
  log_message(warning, self(), LogMsg, Args).


%%
%% Log info messages
%%
-spec info(LogMsg :: string() | atom()) -> ok.

info(LogMsg) ->
  log_message(info, self(), LogMsg).


-spec info(LogMsg :: string() | atom(),
           Args :: list(term())) -> ok.

info(LogMsg, Args) -> 
  log_message(info, self(), LogMsg, Args).

%%
%% Log debug messages
%%
-spec debug(LogMsg :: string() | atom()) -> ok.

debug(LogMsg) ->
  log_message(debug, self(), LogMsg).


-spec debug(LogMsg :: string() | atom(),
            Args :: list(term())) -> ok.

debug(LogMsg, Args) ->
  log_message(debug, self(), LogMsg, Args).


%% ====================================================================
%% Internal functions
%% ====================================================================

%%
%% If LogMsgId is an atom, look up the string in the log_string Map
%% Otherwise assume LogMsg is already a string
%%
log_message(LogLevel, Pid, LogMsgId) when is_atom(LogMsgId) ->
  LogMsg = ui_utils:get_log_string(LogMsgId),
  lager:log(LogLevel, Pid, LogMsg);

log_message(LogLevel, Pid, LogMsg) ->
  lager:log(LogLevel, Pid, LogMsg).
    

log_message(LogLevel, Pid, LogMsgId, Args) when is_atom(LogMsgId) ->
  LogMsg = ui_utils:get_log_string(LogMsgId),
  lager:log(LogLevel, Pid, LogMsg, Args);

log_message(LogLevel, Pid, LogMsg, Args) ->
  lager:log(LogLevel, Pid, LogMsg, Args).
