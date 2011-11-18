-module(test_merge).

-behaviour(gen_frp).

-export([init/1, terminate/2, code_change/2]).

init(_Args) ->
    gen_frp:formatE(gen_frp:oneE("Here we go")),
    Ticker1 = gen_frp:mapE(gen_frp:timerE(1000), fun(_X) -> "tick1" end), %% tick every second
    Ticker2 = gen_frp:mapE(gen_frp:timerE(2000), fun(_X) -> "tick2" end), %% tick every two seconds
    gen_frp:formatE(gen_frp:mergeE([Ticker1, Ticker2])), %% merge both ticks n a single stream and print them out
    {ok, []}.

terminate(_Args, State) ->
    {ok, State}.

code_change(_This,_That) ->
    bleh.
