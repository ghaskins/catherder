-module(catherder_app).
-behaviour(application).

-include("catherder.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    catherder_sup:start_link().
% 
%    gproc:await(znodeapi:uuid_to_key(?ROOT_ZNODE)),
%    
%    R.

stop(_State) ->
    ok.
