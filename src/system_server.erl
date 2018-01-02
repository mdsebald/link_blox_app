%%% @doc
%%% System server 
%%% Reply to inquiries about the LinkBlox instance 
%%%
%%% @end

-module(system_server).

-author("Mark Sebald").

-behaviour(gen_server).

-include("block_state.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

-export([
          start/0,
          is_block/1,
          is_block/2
]).

%%
%% Start system server
%%
-spec start() -> {ok, pid()} | ignore | {error, term()}.

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, null, []).


%%
%% Is BlockName a valid block?
%%
-spec is_block(BlockName :: block_name()) -> boolean().

is_block(BlockName)->
  gen_server:call(?MODULE, {is_block, BlockName}).

-spec is_block(Node :: node(),
               BlockName :: block_name()) -> boolean().

is_block(Node, BlockName)->
  gen_server:call({?MODULE, Node}, {is_block, BlockName}).


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

init(null) ->
  logger:info(starting_system_server),
  {ok, []}.


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



handle_call({is_block, BlockName}, _From, State)->
  IsMember = lists:member(BlockName, block_supervisor:block_names()),
  {reply, IsMember, State};

handle_call(Msg, _From, State) ->
  logger:warning(unknown_system_server_call_msg, [Msg]),
  {noreply, State}.


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

handle_cast(Msg, State) ->
  logger:warning(unknown_system_server_cast_msg,[Msg]),
  {noreply, State}.


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

handle_info(Msg, State) ->
  logger:warning(unknown_system_server_info_msg, [Msg]),
  {noreply, State}.


%% ====================================================================
%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
  Reason :: normal
      | shutdown
      | {shutdown, term()}
      | term().

terminate(normal, _State) ->
  ok;
    
terminate(Reason, _State) ->
  logger:warning(system_server_abnormal_termination, [Reason]),
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
