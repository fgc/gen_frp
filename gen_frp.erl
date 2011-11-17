-module(gen_frp).

%% A behaviour that allows declaring and executing a functional reactive program within a process.

-export([behaviour_info/1]).

-export([start/1, start_link/1, stop/1]).

-export([init_it/6]).

-export_type([event/0,start_ret/0]).

-import(error_logger, [error_msg/2]).



-define(reply(X), From ! {element(2,Tag), X}).

%%================================================================================
%%%  API
%%================================================================================

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
init_it(Starter, _Parent, _Name0, _, _, _Options) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack(Starter, {ok, self()}),
    loop().

loop() ->
    fetch_msg().

fetch_msg() ->
    receive
	{'EXIT', _Parent, Reason} ->
	    terminate_server(Reason);
	Msg ->
	    handle_msg(Msg)
    end.

handle_msg(Msg) ->
    case Msg of
	{notify, Event} ->
	    io:format("notify received: ~p~n", Event),
	    loop();
	{From, Tag, stop} ->
	    catch terminate_server(Tag),
	    ?reply(ok)
    end.

terminate_server(Reason) ->
    exit(Reason).
