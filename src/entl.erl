%% @author Alexander Aprelev <alexander.aprelev@db.com>
%% @since Nov 6, 2011
-module(entl).

%%
%% Include files
%%
-include("entl.hrl").

%%
%% Exported Functions
%%
-export([init/0, start/1, handle_request/2, resubmit_new_requests/0]).

%%
%% API Functions
%%
init() ->
	application:start(log4erl),
	application:start(mnesia),
	application:start(entl).

start(NAgents) ->
	lists:foreach(
		fun(N) -> {ok, P} = entl_agent:create("Acc" ++ N) end,
		lists:seq(1, NAgents)
	).

%%
%% @spec handle_request(Permission::any(), Handler:: fun(any()) ) -> ok
%%
handle_request(R, Handler) ->
	ID = entl_db:place_request(R, Handler),
	
	lists:foreach(
		fun({_X, P, _W, _L}) ->
			gen_server:cast(P, {new_request, ID})
		end, 
		supervisor:which_children(entl_agent_sup)
	).

resubmit_new_requests() ->
	lists:foreach(
		fun(Req) ->
			lists:foreach(
				fun({_X, P, _W, _L}) ->
					gen_server:cast(P, {new_request, Req#request.id})
				end, 
				supervisor:which_children(entl_agent_sup)
			)
		end,
		entl_db:all_new_requests()
	).


%%
%% Local Functions
%%

