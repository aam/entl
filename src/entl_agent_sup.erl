%% @author Alexander Aprelev <alexander.aprelev@db.com>
%% @since Nov 6, 2011

-module(entl_agent_sup).

-behaviour(supervisor).

-export([start_link/0, start_link/1]).
-export([start_child/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(_Args) -> io:format("start_link~n", []), supervisor:start_link({local, ?SERVER}, ?MODULE, []).
start_link() -> start_link([]).
start_child(User) -> supervisor:start_child(?SERVER, [User]).

init(_Args) -> 
	Children =
		[
			{
				entl_agent, 
				{entl_agent, start_link, []},
				permanent, brutal_kill, worker, [entl_agent]
			}
		],
	{ok, {{simple_one_for_one, 10000, 1}, Children}}.
