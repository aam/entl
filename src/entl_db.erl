%% Author: aam
%% Created: Nov 6, 2011
%% Description: TODO: Add description to entl_db
-module(entl_db).

%%
%% Include files
%%
-include("entl.hrl").

%%
%% Exported Functions
%%
-export([create_db/0]).
-export([add_entl/1, remove_entl/1]).
-export([place_request/2, take_request/2, done_request/1, revisit_what_is_left_off/1, all_new_requests/0]).

%%
%% API Functions
%%
create_db() ->
	case mnesia:delete_table(entl) of
		{atomic, ok} -> ok;
		{aborted, {no_exists, entl}} -> ok
	end,
	mnesia:create_table(entl, [{attributes, record_info(fields, entl)}]),
	case mnesia:delete_table(request) of
		{atomic, ok} -> ok;
		{aborted, {no_exists, request}} -> ok
	end,
	mnesia:create_table(request, [{attributes, record_info(fields, request)}]).

add_entl(E) ->
	mnesia:transaction(fun() -> mnesia:write(E) end).
remove_entl(E) ->
	mnesia:transaction(fun() -> mnesia:delete(E) end).

place_request(Permission, Handler) ->
	Ref = make_ref(),
	{atomic, ok} =
		mnesia:transaction(
			fun() ->
				mnesia:write(#request{id = Ref, permission = Permission, status = new, handler = Handler}) 
			end
		),
	Ref.

%%
%%	@spec take_request(ReqId) -> {ok, {failed, already_taken}} | {ok}
%%	where
%%		ReqId = ref()
%%
take_request(WorkerPID, ReqId) ->
	mnesia:transaction(
		fun() ->
			[Req] = mnesia:read(request, ReqId, write),
			case Req#request.status of
				completed -> {failed, already_taken};
				taken -> {failed, already_taken};
				new -> mnesia:write(Req#request{status = taken, workerPID = WorkerPID})
			end
		end).

done_request(ReqId) ->
	mnesia:transaction(
		fun() ->
			[Req] = mnesia:read(request, ReqId, write),
			taken = Req#request.status,
			mnesia:write(Req#request{status = completed}),
			Req
		end).

revisit_what_is_left_off(WorkerPID) ->
	mnesia:transaction(
		fun() ->
			lists:foreach(
				fun(Request) ->
					case is_process_alive(Request#request.workerPID) of
						true ->
							ok;
						false ->
							mnesia:write(Request#request{workerPID = WorkerPID, status = new})
					end
				end,
			 	mnesia:match_object({request, '_', '_', '_', taken, '_'})
			)
		end).

all_new_requests() ->
	{atomic, Result} = 
		mnesia:transaction(
			fun() ->
			 	mnesia:match_object({request, '_', '_', '_', new, '_'})
			end),
	Result.

%%
%% Local Functions
%%

