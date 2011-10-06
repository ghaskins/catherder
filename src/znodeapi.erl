-module(znodeapi).

-include("catherder.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([uuid_to_name/1, uuid_to_key/1, lookup/1,
	 strip_rootznode/1,
	 find/1, create_actor/2,
	 create/2, create/3, delete/2, get_children/1,
	 get_data/1, set_data/3,
	 subscribe/1, notify/2]).

uuid_to_name(Uuid) -> lists:flatten("znode-" ++ Uuid).
uuid_to_key(Uuid) -> {n, g, uuid_to_name(Uuid)}.
uuid_to_fqn(Uuid) -> lists:flatten(?ROOT_ZNODE ++ Uuid).
    
lookup(Uuid) ->
    gproc:lookup_global_name(uuid_to_name(Uuid)).
    
find(Uuid) ->
    case lookup(Uuid) of
	undefined ->
	    % verify that the UUID is valid in the DB 
	    launch_actor(Uuid, 5);
	Pid ->
	    {ok, Pid}
    end.

launch_actor(_, 0) ->
    throw("Cannot start znode");
launch_actor(Uuid, Retries) ->
    create_actor(Uuid, []),
    try
	{Pid, _} = gproc:await(uuid_to_key(Uuid), 500),
	{ok, Pid}
    catch
	throw:timeout ->
	    launch_actor(Uuid, Retries-1)
    end.

create_actor(Uuid, Props) ->
    catherder_sup:start_child({erlang:make_ref(),
			       {znode, start_link, [Uuid, Props]},
			       transient,
			       brutal_kill,
			       supervisor,
			       [znode]}).

extract_parent(Uuid) ->
    Tokens = string:tokens(Uuid, "/"),
    SubTokens = lists:sublist(Tokens, erlang:length(Tokens)-1),
    Path = lists:map(fun(I) -> "/" ++ I end, SubTokens),
    lists:flatten(Path).

strip_rootznode(Fqn) ->
    [_ | Tokens] = string:tokens(Fqn, "/"),
    Path = lists:map(fun(I) -> "/" ++ I end, Tokens),
    lists:flatten(Path).

invoke_parent(Uuid, Version, Op) ->
    Parent = extract_parent(uuid_to_fqn(Uuid)),
    case lookup(Parent) of
	undefined ->
	    {error, bad_arguments, "Invalid parent"};
	Pid ->
	    gen_server:call(Pid, Op)
    end.

call_generic(Uuid, Msg) ->
    case lookup(uuid_to_fqn(Uuid)) of
	undefined ->
	    {error, bad_arguments, "Invalid znode"};
	Pid ->
	    gen_server:call(Pid, Msg)
    end.        

create(Uuid, Version) ->
    create(Uuid, Version, <<>>).

create(Uuid, Version, Data) ->
    invoke_parent(Uuid, Version, {create, uuid_to_fqn(Uuid), Version, Data}).
  
delete(Uuid, Version) ->
    invoke_parent(Uuid, Version, {delete, uuid_to_fqn(Uuid), Version}).

get_children(Uuid) ->
    call_generic(Uuid, get_children).

get_data(Uuid) ->
    call_generic(Uuid, get_data).

set_data(Uuid, Version, Data) ->
    call_generic(Uuid, {set_data, Version, Data}). 

subscribe(Uuid) ->
    Key = {p, g, {?MODULE, uuid_to_fqn(Uuid)}},
    gproc:reg(Key).

notify(Uuid, Msg) ->
    Key = {?MODULE, Uuid},
    gproc:send({p, g, Key}, {self(), Key, Msg}).

%----------------------------------------------------------
% unit tests
%----------------------------------------------------------

extract_parent_test() ->
    "/path/to/parent" = extract_parent("/path/to/parent/child").
  
strip_test() ->
    "/foo/bar" = strip_rootznode(uuid_to_fqn("/foo/bar")).
    
	
