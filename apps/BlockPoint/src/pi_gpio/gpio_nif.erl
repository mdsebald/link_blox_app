
%% Erlang side of NIF Implementation that waits for interrupts from GPIO pins

-module(gpio_nif).
-export([init_interrupt/2]).
-on_load(init/0).

init() ->
   ok = erlang:load_nif("../c_src/gpio_nif", 0). 

init_interrupt(_X, _Y) ->
    exit(nif_library_not_loaded).