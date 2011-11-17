-module(test_frp).

-behaviour(gen_frp).

-export([init/1, terminate/2, code_change/2]).

init(_Args) ->
    {ok, []}.

terminate(_Args, State) ->
    {ok, State}.

code_change(_This,_That) ->
    bleh.
