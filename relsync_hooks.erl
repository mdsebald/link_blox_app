-module(relsync_hooks).

-export([presync/0, postsync/0]).

presync() ->
    io:format("Handling presync~n").

postsync() ->
    io:format("Handling postsync~n").
