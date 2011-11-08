-module(entl_test).

-export([init/0, localtest/0, remotetest/1, test/2]).

-define(NAGENTS, 10).
-define(NSAMPLES, 100).
-define(TIMEOUT, 5000). % 5 sec

init() ->
	entl:init(),
	entl_db:create_db(),

	entl:start(?NAGENTS).

localtest() -> test(node(), ?NSAMPLES).
remotetest(Node) -> test(Node, ?NSAMPLES).

test(ServerNode, NSAMPLES) ->
	entl_db:create_db(),
	
	First = now(),
	ParentPID = self(),
	lists:foldl(
		fun(X, _AccIn) ->
			rpc:call(
				ServerNode,
				entl,
				handle_request,
				[
					{"Check Balance", ["Bank Account #", X]},
					fun(_Param) ->
						ParentPID ! {done, self()}
					end
				]
			)
		end,
		0,
		lists:seq(1, NSAMPLES)
	),

	E = ets:new(processed, []),
	wait(E, NSAMPLES),
	Last = now(),
	ets:foldl(
		fun({AgentPID, Count}, _AccIn) -> 
			io:format("~p processed ~p items~n", [AgentPID, Count])
		end, 
		[],
		E),
	
	io:format("~p: ~p items in ~p secs, ~p mks/item~n", [self(), NSAMPLES,  timer:now_diff(Last, First)/1000/1000, timer:now_diff(Last, First) / 10000]).

wait(_E, 0) -> ok;
wait(E, N) ->
	receive
		{done, AgentPID} ->
			case ets:lookup(E, AgentPID) of
				[] -> ets:insert(E, {AgentPID, 1});
				[{AgentPID, C}] -> ets:insert(E, {AgentPID, C+1})
			end,
			wait(E, N-1)
		after 5000 -> 
			io:format("Timed out")
	end.