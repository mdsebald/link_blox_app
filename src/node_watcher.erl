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
  logger:info(starting_node_watcher),
  net_kernel:monitor_nodes(true),  
  listen_loop().


listen_loop() ->
  receive 
    {nodeup, Node} ->
      logger:info(node_has_connected, [Node]),
      % update all blocks on this node, in case any are linked
      % to blocks on the node that just connected
      block_utils:update_all_blocks();

    {nodedown, Node} ->
      logger:info(node_has_disconnected, [Node]);

    Unexpected ->
      logger:warning(node_watcher_received_unexpected_msg, [Unexpected])

  end,
  listen_loop().