%%% @doc
%%% Common logging server module.
%%% All LinkBlox log messages are routed through here.
%%%
%%% @end

-module(logger).

-author("Mark Sebald").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

-export([
          start/0,
          stop/0,
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
%% Start logging server
%%
-spec start() -> {ok, pid()} | ignore | {error, term()}.

start() ->
  gen_server:start_link({local, logger}, ?MODULE, self(), []).


%%
%% Stop logging server
%%
-spec stop() -> ok.

stop() -> gen_server:call(?MODULE, stop).


%%
%% Log error messages
%%
-spec error(LogMsgId :: atom()) -> ok.

error(LogMsgId) -> 
  gen_server:cast(?MODULE, {error, self(), LogMsgId}).


-spec error(LogMsgId :: atom(),
            Args :: list(term())) -> ok.

error(LogMsgId, Args) -> 
  gen_server:cast(?MODULE, {error, self(), LogMsgId, Args}).


%%
%% Log warning messages
%%
-spec warning(LogMsgId :: atom()) -> ok.

warning(LogMsgId) -> 
  gen_server:cast(?MODULE, {warning, self(), LogMsgId}).


-spec warning(LogMsgId :: atom(),
              Args :: list(term())) -> ok.

warning(LogMsgId, Args) -> 
  gen_server:cast(?MODULE, {warning, self(), LogMsgId, Args}).


%%
%% Log info messages
%%
-spec info(LogMsgId :: atom()) -> ok.

info(LogMsgId) -> 
  gen_server:cast(?MODULE, {info, self(), LogMsgId}).


-spec info(LogMsgId :: atom(),
           Args :: list(term())) -> ok.

info(LogMsgId, Args) -> 
  gen_server:cast(?MODULE, {info, self(), LogMsgId, Args}).

%%
%% Log debug messages
%% For debugging purposes, don't use string map, just pass in string
%%
-spec debug(LogMsg :: string()) -> ok.

debug(LogMsg) -> 
  gen_server:cast(?MODULE, {debug, self(), LogMsg}).


-spec debug(LogMsg :: string(),
            Args :: list(term())) -> ok.

debug(LogMsg, Args) -> 
  gen_server:cast(?MODULE, {debug, self(), LogMsg, Args}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================


%% ====================================================================
%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
  Result :: {ok, State}
      | {ok, State, Timeout}
      | {ok, State, hibernate}
      | {stop, Reason :: term()}
      | ignore,
  State :: term(),
  Timeout :: non_neg_integer() | infinity.

init(Pid) ->
  lager:start(),
  LangMod = ui_utils:get_lang_mod(),
  StringsMap = ui_utils:get_log_strings(),
  String = get_string(starting_logger, StringsMap),
  lager:log(info, Pid, String),
  {ok, StringsMap}.


%% ====================================================================
%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
  Result :: {reply, Reply, NewState}
      | {reply, Reply, NewState, Timeout}
      | {reply, Reply, NewState, hibernate}
      | {noreply, NewState}
      | {noreply, NewState, Timeout}
      | {noreply, NewState, hibernate}
      | {stop, Reason, Reply, NewState}
      | {stop, Reason, NewState},
  Reply :: term(),
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity,
  Reason :: term().

handle_call(stop, _From, StringsMap) ->
  {stop, normal, ok, StringsMap};

handle_call(Msg, _From, StringsMap) ->
  String = get_string(unknown_logger_call_msg, StringsMap),
  lager:log(error, self(), String, [Msg]),
  {noreply, StringsMap}.


%% ======================================================================
%% handle_cast/2
%% ======================================================================
-spec handle_cast(Request :: term(), State :: term()) -> Result when
  Result :: {noreply, NewState}
     | {noreply, NewState, Timeout}
     | {noreply, NewState, hibernate}
     | {stop, Reason, NewState},
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity,
  Reason :: term().

handle_cast({debug, Pid, String}, StringsMap) ->
  lager:log(debug, Pid, String),
  {noreply, StringsMap};

handle_cast({debug, Pid, String, Args}, StringsMap) ->
  lager:log(debug, Pid, String, Args),
  {noreply, StringsMap};

handle_cast({LogLevel, Pid, StringId}, StringsMap) ->
  
  String = get_string(StringId, StringsMap),
  lager:log(LogLevel, Pid, String),
  {noreply, StringsMap};

handle_cast({LogLevel, Pid, StringId, Args}, StringsMap) ->

  String = get_string(StringId, StringsMap),
  lager:log(LogLevel, Pid, String, Args),
  {noreply, StringsMap};

handle_cast(Msg, StringsMap) ->
  String = get_string(unknown_logger_cast_msg, StringsMap),
  lager:log(warning, self(), String, [Msg]),
  {noreply, StringsMap}.


%% ========================================================================
%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
  Result :: {noreply, NewState}
      | {noreply, NewState, Timeout}
      | {noreply, NewState, hibernate}
      | {stop, Reason :: term(), NewState},
  NewState :: term(),
  Timeout :: non_neg_integer() | infinity.

handle_info(Msg, StringsMap) ->
  String = get_string(unknown_logger_info_msg, StringsMap),
  lager:log(warning, self(), String, [Msg]),
  {noreply, StringsMap}.


%% ====================================================================
%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
  Reason :: normal
      | shutdown
      | {shutdown, term()}
      | term().

terminate(normal, _StringsMap) ->
  ok;
    
terminate(Reason, StringsMap) ->
  String = get_string(logger_abnormal_termination, StringsMap),
  lager:log(error, self(), String, [Reason]),
  ok.


%% ====================================================================
%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
  Result :: {ok, NewState :: term()} | {error, Reason :: term()},
  OldVsn :: Vsn | {down, Vsn},
  Vsn :: term().

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


%
% Get the string corresponding to the string ID
%
-spec get_string(StringId :: atom(),
                 StringsMap :: map()) -> string().

get_string(StringId, StringsMap) ->
  case maps:get(StringId, StringsMap) of
    {badmap, StringsMap} ->  io_lib:format("Error: bad strings map: ~p~n", [StringsMap]);
    {badkey, StringId} -> io_lib:format("Error, string: ~p not found~n", [StringId]);
    String -> String
  end.
