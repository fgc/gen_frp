-module(test_frp).

-behaviour(gen_frp).

-export([init/1, terminate/2, code_change/2]).

init(_Args) ->
    Timer = gen_frp:timerE(1000),                                             %% every second
    gen_frp:formatE(Timer),                                                   %% print the timestamp
    gen_frp:formatE(gen_frp:collectE(Timer, 0, fun(_X,Acc) -> Acc + 1 end)),  %% and the number of seconds elapsed
    {ok, []}.

terminate(_Args, State) ->
    {ok, State}.

code_change(_This,_That) ->
    bleh.
