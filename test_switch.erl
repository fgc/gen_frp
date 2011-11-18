-module(test_switch).

-behaviour(gen_frp).

-export([init/1, terminate/2, code_change/2]).

init(_Args) ->
    gen_frp:formatE(gen_frp:oneE("Here we go")),
    
    EvStream = gen_frp:mapE(gen_frp:timerE(5000),fun(X) -> gen_frp:mapE(gen_frp:timerE(1000), fun (_X) -> X end) end),
    gen_frp:formatE(EvStream),
    gen_frp:formatE(gen_frp:switchE(EvStream)),

    {ok, []}.

terminate(_Args, State) ->
    {ok, State}.

code_change(_This,_That) ->
    bleh.
