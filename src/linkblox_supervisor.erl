%%%
%%% @doc 
%%% LinkBlox Application Main Supervisor
%%% @end
%%%

-module(linkblox_supervisor).

-author("Mark Sebald").

-behaviour(supervisor).

-export([
          init/1,
          start_link/1
]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link([BlockValuesFile, LangMod]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [BlockValuesFile, LangMod]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% ====================================================================
%% init/1
%% ====================================================================
-spec init(Args :: term()) -> Result when
  Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
  SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
  RestartStrategy :: one_for_all
           | one_for_one
           | rest_for_one
           | simple_one_for_one,
  ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
  StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
  RestartPolicy :: permanent
           | transient
           | temporary,
  Modules :: [module()] | dynamic.

init([BlockValuesFile, LangMod]) ->

  log_server:start(LangMod),
  start_user_interface(LangMod),
  log_server:info(host_name, [net_adm:localhost()]),
  
  % Listen for nodes connecting an disconnecting
  node_watcher:start(),

  % System Server Spec
  SystemServerSpec = #{id => system_server, 
                       restart => transient,
                       start => {system_server, start, []},
                       type => worker},

  % API Server Spec
  ApiServerSpec = #{id => linkblox_api, 
                    restart => transient,
                    start => {linkblox_api, start, []},
                    type => worker},
  
  % Block Supervisor Spec
  BlockSupervisorSpec = #{id => block_supervisor, 
                          restart => transient,
                          start => {block_supervisor, start_link, [BlockValuesFile]},
                          type => supervisor},
                   
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
          
  {ok, {SupFlags, SystemServerSpec, [ApiServerSpec, BlockSupervisorSpec]}}.


%% ====================================================================
%% Internal functions
%% ====================================================================

-ifdef(STANDALONE).

% If this is the Nerves embedded version, 
% don't start the SSH command line interface
start_user_interface(_LangMod) -> ok.

-else.

% Hosted version, start up the SSH command line interface
start_user_interface(LangMod) ->
  % TODO: Should be configurable,
  %       SSH port number,
  %       system_dir,
  %       Used because we get a nice shell UI experience, 
  %       (i.e command history, line editing, tab completion, etc)
  ui_ssh_cli:start(1111, LangMod, [{system_dir, "/home/mark/ssh_host"}]).

-endif.
