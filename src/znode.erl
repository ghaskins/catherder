-module(znode).
-behavior(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("catherder.hrl").

-record(data, {version=1, value}).
-record(children, {version=1, data=sets:new()}).
-record(stat, {version=1, extra}).
-record(state, {uuid, data=#data{}, children=#children{}, stat=#stat{}}).

start_link(Uuid) ->
    gen_server:start_link(?MODULE, [Uuid], []).

init(?ROOT_ZNODE) ->
    init_(?ROOT_ZNODE);
init(Uuid) ->
    try
	init_(Uuid)
    catch
	error:badarg -> {stop, normal}
    end.

init_(Uuid) ->
    true = gproc:add_global_name(znodeapi:uuid_to_name(Uuid)),
    {ok, #state{uuid=Uuid}}.    

handle_call({create, Uuid, Version}, _From, State) ->
    Children = State#state.children,
    if
	Children#children.version =/= Version ->
	    {reply, {error, stale, "Stale version"}, State};
	true ->
	    {ok, _} = znodeapi:create_actor(Uuid),
	    Data = sets:add_element(Uuid, Children#children.data),
	    NewChildren = Children#children{version=Children#children.version+1,
					    data=Data},
	    {reply, ok, State#state{children=NewChildren}}
    end;
handle_call({delete, Uuid, Version}, _From, State) ->
    Children = State#state.children,
    delete(Uuid,
	   Children#children.version, Version,
	   sets:is_element(Uuid, Children#children.data),
	   znodeapi:lookup(Uuid),
	   State);
handle_call(unlink, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, _State) ->
    throw(eimpl).

handle_cast(_Request, _State) ->
    throw(eimpl).

handle_info(_Info, _State) ->
    throw(eimpl).

terminate(_Reason, _State) ->
    ok.

delete(_, OurVersion, TheirVersion, _, _, State)
  when OurVersion =/= TheirVersion -> 
    {reply, {error, stale, "Stale version"}, State};
delete(_, _, _, IsPresent, Pid, State)
  when IsPresent =:= false; Pid =:= undefined ->
    {reply, {error, notfound, "Could not find node"}, State};
delete(Uuid, _, _, true, Pid, State) ->
    Children = State#state.children,
    ok = gen_server:call(Pid, unlink),
    Data = sets:del_element(Uuid, Children#children.data),
    NewChildren = Children#children{version=Children#children.version+1,
				    data=Data},
    {reply, ok, State#state{children=NewChildren}}.


    
    
