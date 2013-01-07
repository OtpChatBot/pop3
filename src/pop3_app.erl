-module(pop3_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
	% start main supervisor
    pop3_sup:start_link().

stop(_State) ->
    ok.
