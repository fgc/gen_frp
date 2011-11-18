-module(gen_frp).

%% An otp behaviour that allows declaring and executing a functional reactive program within a process.

-export([behaviour_info/1]).

-export([start/1, start_link/1, stop/1]).

-export([init_it/6]).

-export([zeroE/0, oneE/1, timerE/1, 
	 formatE/1, mapE/2, 
	 mergeE/1, switchE/1, collectE/3]).

-export([timer_loop/3, tstamp/0]).

-export_type([event/0,start_ret/0]).

-import(error_logger, [error_msg/2]).



-define(reply(X), From ! {element(2,Tag), X}).

%%==============================================================================
%%%  API
%%==============================================================================

%% gen_frp:start() -> {ok, Pid} | {error, What}
%% gen_frp:stop(Pid) -> ok

%% -callback init(InitArgs :: term()) ->
%%     {ok, State :: term()}.

%% -callback terminate(Args :: (term() | {stop, Reason :: term()} |
%%                              stop | remove_handler |
%%                              {error, {'EXIT', Reason :: term()}} |
%%                              {error, term()}),
%%                     State :: term()) ->
%%     term().

%% -callback code_change(OldVsn :: (term() | {down, term()}),
%%                       State :: term(), Extra :: term()) ->
%%     {ok, NewState :: term()}.

%%------------------------------------------------------------------------------

behaviour_info(callbacks) ->
    [{init, 1}, {terminate, 2}, {code_change, 2}];
behaviour_info(_) ->
    undefined.

%%------------------------------------------------------------------------------

-type event() :: {event, {atom(), term(), term()}}.
-type start_ret() :: {'ok', pid()} | {'error', term()}.

%%------------------------------------------------------------------------------

-spec start(atom()) -> start_ret().
start(Mod) ->
    gen:start(?MODULE, nolink, Mod, [], []).

start_link(Mod) ->	       
    gen:start(?MODULE, link, Mod, [], []).

-spec stop(pid()) -> 'ok'.
stop(P) ->
    {ok, Reply} = gen:call(P, self(), stop, infinity),
    Reply.
		  

%% -spec init_it(pid(), 'self' | pid(), emgr_name(), module(), [term()], [_]) -> 
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, _Parent, _Name0, Mod, Args, _Options) ->
    process_flag(trap_exit, true), %%?
    case catch Mod:init(Args) of
	{ok, State} ->
	    proc_lib:init_ack(Starter, {ok, self()}),
	    loop(State);
	{stop, Reason} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	{'EXIT', Reason} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	ignore ->
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	Else ->
	    Error = {bad_return_value, Else},
	    proc_lib:init_ack(Starter, {error, Error}),
	    exit(Error)
    end.

loop(State) ->
    fetch_msg(State).

fetch_msg(State) ->
    receive
	{'EXIT', _Parent, Reason} ->
	    terminate_server(Reason);
	Msg ->
	    handle_msg(Msg, State)
    end.

handle_msg(Msg, State) ->
    case Msg of
	{event, Event, Val} ->
	    propagateE(Event,Val,get(graph)),
	    loop(State);
	{From, Tag, stop} ->
	    catch terminate_server(Tag),
	    ?reply(ok);
	Other ->
	    io:format("received unmatched msg: ~p~n", [Other]),
	    loop(State)
    end.

terminate_server(Reason) ->
    exit(Reason).


%%------------------------------------------------------------------------------
%% Events
%%------------------------------------------------------------------------------
zeroE() ->
    D = case get(graph) of
	    undefined -> 
		new_graph();
	    Other -> Other
	end,
    EventID = now(),
    Event = {event, {one_e,EventID, fun(_X) -> nil end}},
    digraph:add_vertex(D, Event),
    Event.
    
oneE(Val) ->
    D = case get(graph) of
	    undefined -> 
		new_graph();
	    Other -> Other
	end,
    EventID = now(),
    Event = {event, {one_e,EventID, fun(_X) -> Val end}},
    digraph:add_vertex(D, Event),
    self() ! {event, Event, Val},
    Event.

    
timerE(Interval) ->
    D = case get(graph) of
	    undefined -> 
		new_graph();
	    Other -> Other
	end,
    EventID = now(),
    Event = {event, {timer_e,EventID, fun(X) -> X end}},
    digraph:add_vertex(D, Event),
    spawn(?MODULE, timer_loop, [self(), Interval, Event]),
    Event.

timer_loop(Parent, Interval, Event) ->
    timer:sleep(Interval),
    Parent ! {event, Event, tstamp()},
    timer_loop(Parent, Interval, Event).

tstamp() ->
    {Mega, Sec, M} = now(),
    Mega * 1000000000 + Sec * 1000 + M.

mapE(Event, F) ->
    NewEventID = now(),
    NewEvent = {event,{map_e, NewEventID, F}},
    add_observer(NewEvent, Event),
    NewEvent.

mergeE(Events) ->
    NewEventID = now(),
    NewEvent = {event,{merge_e, NewEventID, fun(X) -> X end}},
    [add_observer(NewEvent, Event) || Event <- Events],
    NewEvent.

%%should cleanup the initial zeroE after the first switch
switchE(EventStream) ->    
    NewEventID = now(),
    AuxEvent = mapE(zeroE(), fun(X) -> X end),
    F = fun(Event) ->
		stop_observing(AuxEvent),
		add_observer(AuxEvent, Event),
	end,
    NewEvent = {event,{switch_e, NewEventID, F}},
    add_observer(NewEvent, EventStream),
    AuxEvent.
    
collectE(Event, Init, Fold) ->
    FakeID = now(),
    put(FakeID, Init),
    mapE(Event, fun(N) ->
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

%% remove_observer(Observer, Parent) ->
%%     D = get(graph),
%%     digraph:del_edge(D,digraph:edge(Parent,Observer)).

stop_observing(Observer) ->
    Graph = get(graph),
    [digraph:del_edge(Graph,E) || E <- digraph:in_edges(Graph,Observer)].

propagateE(Event,InVal,Graph) ->
    [propagateE(E, F(InVal), Graph) || {event, {_,_,F}} = E <- digraph:out_neighbours(Graph, Event)].
