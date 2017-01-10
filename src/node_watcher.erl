%%% @doc 
%%% Node Watcher, Indicate when other LinkBlox nodes connect and disconnect
%%%
%%% @end

-module(node_watcher).

-author("Mark Sebald").

-export([start/0, listen_start/0]).



-spec start() -> pid().

start() ->
  spawn(node_watcher, listen_start, []).


%
% Listen for nodeup and nodedown messages
%
listen_start() ->
  error_logger:info_msg("Starting Node Watcher~n"),
  net_kernel:monitor_nodes(true),  
  listen_loop().


listen_loop() ->
  receive 
    {nodeup, Node} ->
      error_logger:info_msg("Node ~p Has Connected~n", [Node]),
      % Configure all blocks on this node, in case any are linked
      % to blocks on the node that just connected
      block_common:configure_all_blocks();

    {nodedown, Node} ->
      error_logger:info_msg("Node ~p Has Disconnected~n", [Node]);

    Unexpected ->
      error_logger:warning_msg("Received unexpected message: ~p in Node Watcher", [Unexpected])

  end,
  listen_loop().