%%% @doc
%%% LinkBlox application
%%%
%%% @end


-module('LinkBlox_app').

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

start(normal, [BlockValuesFile, LangMod]) ->
  case linkblox_supervisor:start_link([BlockValuesFile, LangMod]) of
    {ok, Pid} ->
      logger:info(linkblox_startup_complete),
      {ok, Pid};

    Error ->
      logger:error(err_starting_linkblox, [Error]),
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
