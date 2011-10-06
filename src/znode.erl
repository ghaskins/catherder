-module(znode).
-behavior(gen_server).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("catherder.hrl").

-record(data, {version=1, value = <<>>}).
-record(children, {version=1, data=sets:new()}).
-record(stat, {version=1, extra}).
-record(state, {uuid, data=#data{}, children=#children{}, stat=#stat{}}).

start_link(Uuid, Props) ->
    gen_server:start_link(?MODULE, {Uuid, Props}, []).

init({?ROOT_ZNODE, Props}) ->
    init_(?ROOT_ZNODE, Props);
init({Uuid, Props}) ->
    try
	init_(Uuid, Props)
    catch
	error:badarg -> {stop, normal}
    end.

init_(Uuid, Props) ->
    true = gproc:add_global_name(znodeapi:uuid_to_name(Uuid)),
    Data = proplists:get_value(data, Props, <<>>),
    {ok, #state{uuid=Uuid, data=#data{value=Data}}}.    

handle_call({create, Uuid, Data}, _From, State) ->
    Children = State#state.children,
    create(Uuid, 
	   sets:is_element(Uuid, Children#children.data),
	   Data,
	   State);
handle_call({delete, Uuid, Version}, _From, State) ->
    Children = State#state.children,
    delete(Uuid,
	   Children#children.version, Version,
	   sets:is_element(Uuid, Children#children.data),
	   znodeapi:lookup(Uuid),
	   State);
handle_call(unlink, _From, State) ->
    znodeapi:notify(State#state.uuid, unlink),
    {stop, normal, ok, State};
handle_call(get_children, _From, State) ->
    {reply, get_children(State), State};
handle_call(get_data, _From, State) ->
    Data = State#state.data,
    {reply, {ok, Data#data.version, Data#data.value}, State};
handle_call({set_data, Version, Payload}, _From, State) ->
    Data = State#state.data,
    set_data(Data#data.version, Version, Payload, State);
handle_call(_Request, _From, _State) ->
    throw(eimpl).

handle_cast(_Request, _State) ->
    throw(eimpl).

handle_info(_Info, _State) ->
    throw(eimpl).

terminate(_Reason, _State) ->
    ok.

get_children(State) ->
    Children = State#state.children,
    Data = [znodeapi:strip_rootznode(I) || 
	       I <- sets:to_list(Children#children.data)],
    {ok, Children#children.version, length(Data), Data}.

create(_, true, _, State) ->
    {reply, {error, exists, "Znode already exists"}, State};
create(Uuid, false, Payload, State) ->
    Children = State#state.children,
    {ok, _} = znodeapi:create_actor(Uuid, [{data, Payload}]),
    Data = sets:add_element(Uuid, Children#children.data),
    NewChildren = Children#children{version=Children#children.version+1,
				    data=Data},
    NewState = State#state{children=NewChildren},
    notify_children(NewState),
    {reply, ok, NewState}.

delete(_, OurVersion, TheirVersion, _, _, State)
  when OurVersion =/= TheirVersion -> 
    {reply, {error, stale, "Stale version"}, State};
delete(_, _, _, IsPresent, Pid, State)
  when IsPresent =:= false; Pid =:= undefined ->
    {reply, {error, notfound, "Could not find node"}, State};
delete(Uuid, _, _, true, Pid, State) ->
    Children = State#state.children,
    case gen_server:call(Pid, get_children) of
	{ok, _, 0, _} -> 
	    ok = gen_server:call(Pid, unlink),
	    Data = sets:del_element(Uuid, Children#children.data),
	    NewChildren = Children#children{version=Children#children.version+1,
					    data=Data},
	    NewState = State#state{children=NewChildren},
	    notify_children(NewState),
	    {reply, ok, NewState};
	{ok, _, Count, _} ->
	    {reply, {error, haschildren, "Has children"}, State}
    end.

set_data(OurVersion, TheirVersion, _, State)
  when OurVersion =/= TheirVersion -> 
    {reply, {error, stale, "Stale version"}, State};
set_data(_, _, Payload, State) ->
    Data = State#state.data,
    NewData = Data#data{version=Data#data.version+1, value=Payload},
    znodeapi:notify(State#state.uuid,
		    {data, NewData#data.version, NewData#data.value}),
    {reply, ok, State#state{data=NewData}}.

notify_children(State) ->
    {ok, Version, Count, Data} = get_children(State),
    znodeapi:notify(State#state.uuid, {children, Version, Count, Data}).
    


    
    

