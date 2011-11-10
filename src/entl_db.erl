%% @author Alexander Aprelev <alexander.aprelev@db.com>
%% @since Nov 6, 2011
%% @doc Entitlements persistent layer.
%%		Persists permission requests in mnesia databases. 
%%		Provides layer of abstraction to accomodate new requests submission, retrieval. 
%% @end
-module(entl_db).

%%
%% Include files
%%
-include("entl.hrl").

%%
%% Exported Functions
%%
-export([create_db/0]).
-export([place_request/2, take_request/2, done_request/1, revisit_what_is_left_off/1, all_new_requests/0]).

%%
%% API Functions
%%

%%
%%	@doc Creates entitlements and requests database
%%
%%	@spec create_db() -> ok
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

%%
%%	@doc Agent calls this method to claim request
%%
%%	@spec place_request(Permission::any(), Handler::fun(any())) -> ref()
%%
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
%%	@doc Agent calls this method to claim request
%%
%%	@spec take_request(WorkerPID::pid(), ReqId::ref()) -> {atomic, {failed, already_taken}} | {atomic, ok}
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

%%
%%	@doc Agent calls this method to indicate that the request was processed
%%
%%	@spec done_request(ReqId::ref()) -> {atomic, Request::request}
%%
done_request(ReqId) ->
	mnesia:transaction(
		fun() ->
			[Req] = mnesia:read(request, ReqId, write),
			taken = Req#request.status,
			mnesia:write(Req#request{status = completed}),
			Req
		end).

%%
%%	@doc Agent calls this method on agent's startup to force rescan of 'taken' requests to make sure that 
%%		dead agents's requests can be reprocessed.
%%
%%	@spec revisit_what_is_left_off(WorkerPID::pid()) -> {atomic, ok}
%%
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

%%
%%	@doc Returns list of 'new' requests
%%
%%	@spec all_new_requests() -> list(Request)
%%
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

