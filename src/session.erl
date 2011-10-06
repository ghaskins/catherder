-module(session).
-behavior(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2]).

-record(state, {}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, _State) ->
    throw(eimpl).

handle_cast(_Request, _State) ->
    throw(eimpl).

handle_info(_Info, _State) ->
    throw(eimpl).

terminate(_Reason, _State) ->
    ok.



