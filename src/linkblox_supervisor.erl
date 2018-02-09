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

start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Options).

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

init([BaseNodeName, LangMod, SSH_Port, LogLevel]) ->   
  lager:set_loglevel(lager_console_backend, LogLevel),

  ui_utils:create_config(),
  ui_utils:set_lang_mod(LangMod),
  ui_utils:set_ssh_port(SSH_Port),

  logger:info(starting_linkblox_options, [BaseNodeName, LangMod, SSH_Port, LogLevel]),
 

  HostName = net_adm:localhost(),
  logger:info(host_name, [HostName]),

  % This app should crash if node not started
  {ok, NodeName} = start_node(BaseNodeName, 1),

  BlockValuesFile = atom_to_list(NodeName) ++ "Config",
  logger:info(block_values_file, [BlockValuesFile]),
  
  start_ssh_cli(),
  
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

-spec start_node(BaseNodeName :: atom(),
                 Index :: pos_integer()) -> {ok, atom()}.

-ifdef(STANDALONE). % i.e. if Nerves build

% Node is already started
start_node(BaseNodeName, _Index) -> {ok, BaseNodeName}.

% Don't start SSH command line interface
start_ssh_cli() -> ok.

-else.

start_node(BaseNodeName, Index) ->
  IndexStr = io_lib:format("~2..0w", [Index]),
  BaseNodeNameStr = atom_to_list(BaseNodeName),
  NodeName = list_to_atom(lists:flatten(BaseNodeNameStr ++ IndexStr)),
  logger:info("Node Name: ~p", [NodeName]),

  case net_kernel:start([NodeName, shortnames]) of
    {ok, _Pid} -> 
      logger:info(distributed_node_started, [NodeName]),
      {ok, NodeName};
    {error, {already_started, _Pid}} ->
      logger:debug("~p Already started", [NodeName]),
      start_node(BaseNodeName, Index + 1);
    {error, Error} ->
      logger:error("~p Starting: ~p", [Error, NodeName]),
      start_node(BaseNodeName, Index + 1)
  end.


start_ssh_cli() ->
  ui_ssh_cli:start([{system_dir, "/etc/ssh"}]).

-endif.

