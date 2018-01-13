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

  ui_utils:create_config(),
  ui_utils:set_lang_mod(LangMod),
  ui_utils:set_ssh_port(1111),

  logger:info(starting_linkblox_lang_mod, [LangMod]),

  ui_ssh_cli:start([{system_dir, "/etc/ssh"}]),

  logger:info(host_name, [net_adm:localhost()]),
  
  % Listen for nodes connecting an disconnecting
  node_watcher:start(),

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
          
  {ok, {SupFlags, [ApiServerSpec, BlockSupervisorSpec]}}.


%% ====================================================================
%% Internal functions
%% ====================================================================

