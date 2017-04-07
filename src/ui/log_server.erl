%%% @doc
%%% Common logging server module.
%%% All LinkBlox log messages are routed through here.
%%%
%%% @end

-module(log_server).

-author("Mark Sebald").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

-export([
          start/1,
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
-spec start(LangMod :: module()) -> {ok, pid()} | ignore | {error, term()}.

start(LangMod) ->
  gen_server:start_link({local, log_server}, ?MODULE, LangMod, []).


%%
%% Log error messages
%%
-spec error(LogMsgId :: atom()) -> ok.

error(LogMsgId) ->
  gen_server:cast(?MODULE, {error, LogMsgId}).


-spec error(LogMsgId :: atom(),
            Args :: list(term())) -> ok.

error(LogMsgId, Args) ->
  gen_server:cast(?MODULE, {error, LogMsgId, Args}).


%%
%% Log warning messages
%%
-spec warning(LogMsgId :: atom()) -> ok.

warning(LogMsgId) ->
  gen_server:cast(?MODULE, {warning, LogMsgId}).


-spec warning(LogMsgId :: atom(),
              Args :: list(term())) -> ok.

warning(LogMsgId, Args) ->
  gen_server:cast(?MODULE, {warning, LogMsgId, Args}).


%%
%% Log info messages
%%
-spec info(LogMsgId :: atom()) -> ok.

info(LogMsgId) ->
  gen_server:cast(?MODULE, {info, LogMsgId}).


-spec info(LogMsgId :: atom(),
           Args :: list(term())) -> ok.

info(LogMsgId, Args) ->
  gen_server:cast(?MODULE, {info, LogMsgId, Args}).

%%
%% Log debug messages
%% For debugging purposes, don't use string map, just pass in string
%%
-spec debug(LogMsg :: string()) -> ok.

debug(LogMsg) ->
  gen_server:cast(?MODULE, {debug, LogMsg}).


-spec debug(LogMsg :: string(),
            Args :: list(term())) -> ok.

debug(LogMsg, Args) ->
  gen_server:cast(?MODULE, {debug, LogMsg, Args}).


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

init(LangMod) ->
  StringsMap = LangMod:strings_map(),
  String = get_string(starting_log_server, StringsMap),
  error_logger:info_msg(String, [LangMod]),
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

handle_call(Msg, _From, StringsMap) ->
  String = get_string(unknown_log_server_call_msg, StringsMap),
  error_logger:error_msg(String, [Msg]),
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

handle_cast({error, StringId}, StringsMap) ->
  String = get_string(StringId, StringsMap),
  error_logger:error_msg(String),
  {noreply, StringsMap};

handle_cast({error, StringId, Args}, StringsMap) ->
  String = get_string(StringId, StringsMap),
  error_logger:error_msg(String, Args),
  {noreply, StringsMap};

handle_cast({warning, StringId}, StringsMap) ->
  String = get_string(StringId, StringsMap),
  error_logger:warning_msg(String),
  {noreply, StringsMap};

handle_cast({warning, StringId, Args}, StringsMap) ->
  String = get_string(StringId, StringsMap),
  error_logger:warning_msg(String, Args),
  {noreply, StringsMap};

handle_cast({info, StringId}, StringsMap) ->
  String = get_string(StringId, StringsMap),
  error_logger:info_msg(String),
  {noreply, StringsMap};

handle_cast({info, StringId, Args}, StringsMap) ->
  String = get_string(StringId, StringsMap),
  error_logger:info_msg(String, Args),
  {noreply, StringsMap};

% Prefix string with "[debug] " to differentiate from info messages
handle_cast({debug, String}, StringsMap) ->
  error_logger:info_msg("[debug] " ++ String),
  {noreply, StringsMap};

handle_cast({debug, String, Args}, StringsMap) ->
  DebugString = "[debug] " ++ String,
  error_logger:info_msg(DebugString, Args),
  {noreply, StringsMap};


handle_cast(Msg, StringsMap) ->
  String = get_string(unknown_log_server_cast_msg, StringsMap),
  error_logger:warning_msg(String, [Msg]),
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
  String = get_string(unknown_log_server_info_msg, StringsMap),
  error_logger:warning_msg(String, [Msg]),
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
  String = get_string(log_server_abnormal_termination, StringsMap),
  error_logger:error_msg(String, [Reason]),
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
    {badmap, StringsMap} ->  io:lib("Error: bad strings map: ~p~n", [StringsMap]);
    {badkey, StringId} -> io:lib("Error, string: ~p not found~n", [StringId]);
    String -> String
  end.
