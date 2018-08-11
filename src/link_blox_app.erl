%%% @doc
%%% LinkBlox application
%%%
%%% @end


-module(link_blox_app).

-author("Mark Sebald").

-behaviour(application).

-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================


%% ====================================================================
%% Behavioural functions
%% ====================================================================


%% ====================================================================
%% start/2
%% ====================================================================
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
  {ok, Pid :: pid()} | {error, Reason :: term()}.

start(normal, Options) ->
  case linkblox_supervisor:start_link(Options) of
    {ok, Pid} ->
      m_logger:info(linkblox_startup_complete),
      {ok, Pid};

    Error ->
      m_logger:error("~p Starting LinkBlox", [Error]),
      Error
  end.


%% ====================================================================
%% stop/1
%% ====================================================================
-spec stop(State :: term()) ->  ok.

stop(_State) ->
  ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
