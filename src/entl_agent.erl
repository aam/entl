-module(entl_agent).

-include("entl.hrl").

-behaviour(gen_server).

-export([start_link/1, create/1, havePermission/2, delete/1]).

%%
%% gen_server callbacks
%%
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {user}).

start_link(User) -> gen_server:start_link(?MODULE, [User], []).

create(User) -> entl_agent_sup:start_child(User).

havePermission(Pid, Permission) -> gen_server:call(Pid, {havePermission, [Permission]}).

delete(Pid) -> gen_server:cast(Pid, delete).

init([User]) ->
	io:format("new agent ~p~n", [self()]),
	gen_server:cast(self(), recheckQueue),
	{ok, #state{user=User}}.

handle_call({havePermission, [_Permission]}, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(delete, State) ->
	{stop, normal, State};

handle_cast(recheckQueue, State) ->
	entl_db:revisit_what_is_left_off(self()),
	entl:resubmit_new_requests(),
	{noreply, State};

handle_cast({new_request, ID}, State) ->
	Result = entl_db:take_request(self(), ID),
	case Result of
		{atomic, ok} ->
%% 			case random:uniform(10) of
%% 				1 -> apples = oranges;
%% 				_ -> 
					{atomic, Request} = entl_db:done_request(ID),
					H = Request#request.handler,
					H({self(), true});
%% 			end;
		_ -> ok
	end,
	{noreply, State}.

handle_info(timeout, State) ->
	{stop, normal, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
