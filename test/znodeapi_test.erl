-module(znodeapi_test).

-include_lib("eunit/include/eunit.hrl").

%----------------------------------------------------------
% unit tests
%----------------------------------------------------------
  
setup() ->
    application:set_env(gproc, gproc_dist, all),
    application:start(sasl),
    application:start(gproc),
    application:start(catherder),
    ok.
  
create_test_() ->
    {setup, fun setup/0, 
     ?_test(
	begin
	    ok = znodeapi:create("/foo"),
	    ok = znodeapi:create("/foo/bar"),
	    {ok, 2, 1, ["/foo/bar"]} = znodeapi:get_children("/foo"),
	    {error, exists, _} = znodeapi:create("/foo/bar"),
	    {error, bad_arguments, _} = znodeapi:create("/bar/foo")
	end)
    }.

data_test_() ->
    {setup, fun setup/0, 
     ?_test(
	begin
	    {ok, 1, <<>>} = znodeapi:get_data("/foo"),
	    ok = znodeapi:set_data("/foo", 1, <<"bar">>),
	    {ok, 2, <<"bar">>} = znodeapi:get_data("/foo")
	end)
    }.


delete_test_() ->
    {setup, fun setup/0, 
     ?_test(
	begin
	    {error, stale, _} = znodeapi:delete("/foo/bar", 1),
	    ok = znodeapi:delete("/foo/bar", 2),
	    {error, bad_arguments, _} = znodeapi:create("/foo/bar/baz")
	end)
    }.

deletechildren_test_() ->
    {setup, fun setup/0, 
     ?_test(
	begin
	    ok = znodeapi:create("/foo/bar"),
	    ok = znodeapi:create("/foo/bar/baz"),
	    {error, haschildren, _} = znodeapi:delete("/foo/bar", 4)
	end)
    }.

event_test_() ->
    {setup, fun setup/0, 
     ?_test(
	begin
	    znodeapi:subscribe("/foo/bar/baz"),
	    ok = znodeapi:delete("/foo/bar/baz", 2),
	    receive
		{_, _, unlink} -> ok
	    after
		1000 -> throw(timeout)
	    end,
	    receive
		{_, _, {data, _, _}} -> ok
	    after
		1000 -> throw(timeout)
	    end,
	    znodeapi:subscribe("/foo"),
	    znodeapi:subscribe("/foo/baz/foobar"),
	    ok = znodeapi:create("/foo/baz"),
	    receive
		{_, _, {children, _, _, _}} -> ok
	    after
		1000 -> throw(timeout)
	    end,
	    ok = znodeapi:create("/foo/baz/foobar"),
	    receive
		{_, _, {data, _, _}} -> ok
	    after
		1000 -> throw(timeout)
	    end
	end)
    }.

   
    
	
