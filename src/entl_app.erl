-module(entl_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("entl.hrl").

start(_Type, StartArgs) ->
	case entl_agent_sup:start_link(StartArgs) of
		{ok, Pid} -> {ok, Pid};
		Other -> {error, Other}
	end.

stop(_State) -> ok.