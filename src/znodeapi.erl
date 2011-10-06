-module(znodeapi).

-include("catherder.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([uuid_to_name/1, uuid_to_key/1, find/1, create_actor/1, create/2]).

uuid_to_name(Uuid) -> lists:flatten("znode-" ++ Uuid).
uuid_to_key(Uuid) -> {n, g, uuid_to_name(Uuid)}.
    
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
    create_actor(Uuid),
    try
	{Pid, _} = gproc:await(uuid_to_key(Uuid), 500),
	{ok, Pid}
    catch
	throw:timeout ->
	    launch_actor(Uuid, Retries-1)
    end.

create_actor(Uuid) ->
    catherder_sup:start_child({erlang:make_ref(),
			       {znode, start_link, [Uuid]},
			       transient,
			       brutal_kill,
			       supervisor,
			       [znode]}).

parent_from_uuid(Uuid) ->
    Tokens = string:tokens(Uuid, "/"),
    SubTokens = lists:sublist(Tokens, erlang:length(Tokens)-1),
    Path = lists:map(fun(I) -> "/" ++ I end, SubTokens),
    lists:flatten(Path).

create(Uuid, Version) ->
    Fqn = ?ROOT_ZNODE ++ Uuid,
    Parent = parent_from_uuid(Fqn),
    case lookup(Parent) of
	undefined ->
	    {error, bad_arguments, "Invalid parent"};
	Pid ->
	    gen_server:call(Pid, {create, Fqn, Version})
    end.
    
%----------------------------------------------------------
% unit tests
%----------------------------------------------------------

parent_from_uuid_test() ->
    "/path/to/parent" = parent_from_uuid("/path/to/parent/child").
  

    
	
