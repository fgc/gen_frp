-module(frp).

-compile(export_all).

timerE(Interval) ->
    D = case get(graph) of
	    undefined -> 
		new_graph();
	    Other -> Other
	end,
    EventID = now(),
    Event = {event, {timer_e,EventID, fun(X) -> X end}},
    digraph:add_vertex(D, Event),
    spawn(?MODULE, timer_loop, [Interval, fun () -> propagate(Event,tstamp(),D) end]),
    Event.

timer_loop(Interval, F) ->
    timer:sleep(Interval),
    F(),
    timer_loop(Interval, F).

tstamp() ->
    {Mega, Sec, M} = now(),
    Mega * 1000000000 + Sec * 1000 + M.


mapE(Event, F) ->
    NewEventID = now(),
    NewEvent = {event,{map_e, NewEventID, F}},
    add_observer(NewEvent, Event),
    NewEvent.
    
collectE(Event, Init, Fold) ->
    FakeID = now(),
    put(FakeID, Init),
    mapE(Event, fun(N) ->
			io:format("N is: ~p, FakeID is: ~p, Total is: ~p~n", [N, FakeID, get(FakeID)]),
			Next = Fold(N, get(FakeID)),
			put(FakeID, Next),
			Next
		end).

formatE(Event) ->
    NewEventID = now(),
    F = fun(InVal) ->
		io:format("~p~n", [InVal])
	end,
    
    NewEvent = {event,{format_e, NewEventID, F}},
    add_observer(NewEvent, Event),
    NewEvent.
    
new_graph() ->
    G = digraph:new(),
    put(graph, G),
    G.

add_observer(Observer, Parent) ->
    D = get(graph),
    digraph:add_vertex(D, Observer),
    digraph:add_edge(D, Parent, Observer).

propagate(Event,InVal,Graph) ->
    [propagate(E, F(InVal), Graph) || {event, {_,_,F}} = E <- digraph:out_neighbours(Graph, Event)].
    
lift(F) ->
    fun(Behaviours) ->  apply(F,Behaviours) end.

test1() ->
    formatE(timerE(100)).

test2() ->
    T = timerE(1000),
    M = mapE(T, fun(X) -> X rem 10 end),
    formatE(M).

test3() ->
    T = timerE(1000),
    formatE(T),
    formatE(collectE(T,0,fun(_N, Acc) -> Acc + 1 end)).
