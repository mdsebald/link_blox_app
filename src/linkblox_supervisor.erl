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

init([BaseNodeName, LangMod, SSH_Port, LogLevel, Cookie]) ->   
  lager:set_loglevel(lager_console_backend, LogLevel),

  ui_utils:create_config(),
  ui_utils:set_lang_mod(LangMod),
  ui_utils:set_ssh_port(SSH_Port),

  m_logger:info(starting_linkblox_options, [BaseNodeName, LangMod, SSH_Port, LogLevel]),
 
  HostName = net_adm:localhost(),
  m_logger:info(host_name, [HostName]),

  % This app should crash if node not started
  
  {ok, NodeName} = start_node(BaseNodeName, 1),

  if (node() /= nonode@nohost) ->
    erlang:set_cookie(node(), Cookie),
    m_logger:debug("Node cookie set to: ~p", [erlang:get_cookie()]);
  true ->
    m_logger:debug("Node not started")
  end,

  BlockValuesFile = atom_to_list(NodeName) ++ "Config",
  m_logger:info(block_values_file, [BlockValuesFile]),
  
  start_ssh_cli(),

  start_ntpd(),
  
  % Listen for nodes connecting an disconnecting
  node_watcher:start(),

  % API Supervisor Spec
  ApiSupervisorSpec = #{id => api_supervisor, 
                    restart => transient,
                    start => {api_supervisor, start_link, []},
                    type => supervisor},
  
  % Block Supervisor Spec
  BlockSupervisorSpec = #{id => block_supervisor, 
                          restart => transient,
                          start => {block_supervisor, start_link, [BlockValuesFile]},
                          type => supervisor},
                   
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
          
  {ok, {SupFlags, [ApiSupervisorSpec, BlockSupervisorSpec]}}.


%% ====================================================================
%% Internal functions
%% ====================================================================

-spec start_node(BaseNodeName :: atom(),
                 Index :: pos_integer()) -> {ok, atom()}.

-ifdef(STANDALONE). % i.e. if Embedded system build

% Node is already started
start_node(BaseNodeName, _Index) -> {ok, BaseNodeName}.

% Don't start SSH command line interface
start_ssh_cli() -> ok.

% TODO: Start ntpd via system config value
start_ntpd() -> ok.
%    'Nerves.Ntp.Worker':start_link().

-else.

-ifdef(TEST). % Testing don't need to start node

start_node(BaseNodeName, _Index) -> {ok, BaseNodeName}.

% Don't start SSH command line interface
start_ssh_cli() -> ok.

start_ntpd() -> ok.
% ntpd starts by itself
%    'Nerves.Ntp.Worker':start_link().

-else.

%% limit number of nodes we try to start
start_node(BaseNodeName, Index) when (Index =< 10) ->

  % make sure epmd is running
  case net_adm:names() of
    {ok, _} -> %% Epmd is running
      ok;
    {error, address} ->
      Epmd = os:find_executable("epmd"),
      os:cmd(Epmd ++ " -daemon")
  end,

  IndexStr = io_lib:format("~2..0w", [Index]),
  BaseNodeNameStr = atom_to_list(BaseNodeName),
  NodeName = list_to_atom(lists:flatten(BaseNodeNameStr ++ IndexStr)),
  m_logger:info("Node Name: ~p", [NodeName]),

  case net_kernel:start([NodeName, shortnames]) of
    {ok, _Pid} -> 
      m_logger:info(distributed_node_started, [NodeName]),
      {ok, NodeName};
    {error, {already_started, _Pid}} ->
      m_logger:debug("~p Already started", [NodeName]),
      start_node(BaseNodeName, Index + 1);
    {error, Error} ->
      m_logger:error("~p Starting: ~p", [Error, NodeName]),
      start_node(BaseNodeName, Index + 1)
  end;

start_node(_BaseNodeName, _Index) -> 
  m_logger:debug("Unable to start node"),
  {error, starting_node}.


start_ssh_cli() ->
  ui_ssh_cli:start([{system_dir, "/etc/ssh"}]).

start_ntpd() -> ok.

-endif.
-endif.

