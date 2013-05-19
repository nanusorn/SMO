%% Copyright (c) 2008-2013, Nanusorn Photpipat <nanusorn@photpipat.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.-module (ability_effect).
-module(smo_interval_activity).
-behaviour(gen_server).

-export([start_link/0]).
%% gen_event callbacks
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

start_link() ->
	ServerName = {local, smo_interval_activity}, % name gets register()ed
	Module = smo_interval_activity,
	Options = [],
	gen_server:start_link(ServerName, Module, noargs, Options).

%% gen_server callbacks
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call(_Request, _From, State) ->
	{noreply,State}.

handle_cast(Request, State) ->
	case Request of
		update_ccu -> start_update(300000);
		collect_inactive_arena -> start_collect(600000);
			
		_Any -> smo_logger:fmsg("receive Request ~p~n", [Request])
	end,
	{noreply,State}.

handle_info(Info, State) ->
	smo_logger:fmsg("report_handler: unknown message, info=~w", [Info]),
	{noreply, State}.

init(_Args) ->
	State = [],
	odbc:start(),
	DBConnectionType = [{auto_commit, on}, {timeout, infinity}, {scrollable_cursors, off}, {trace_driver, on}, {tuple_row, on}],
	{ok, Ref} = odbc:connect("DSN=InternalMySQL;UID=summoner;PWD=$umm0ns,f,6-", DBConnectionType),
	put(db_ref, Ref),
	{ok, State}.

terminate(Reason, _State) ->
	smo_logger:fmsg("report_handler: terminating, reason=~w", [Reason]),
	ok.
	
update_ccu() ->
	[_, _, _, {workers, Worker}] = supervisor:count_children(smo_player_sup),
	SQLCommand = "CALL smno.sp_update_ccu ('"++atom_to_list(node())++"', "++integer_to_list(Worker)++");",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p -----", [SQLCommand, QueryResult]),
	case QueryResult of
		{error, connection_closed} -> reconnect_db();
		{updated, _} -> smo_logger:msg("Update CCU Complete");
		_OtherCase -> smo_logger:fmsg("Update CCU Incomplete Case ~p~n", [_OtherCase])
	end.
		
			
reconnect_db() ->
	DBConnectionType = [{auto_commit, on}, {timeout, infinity}, {scrollable_cursors, off}, {trace_driver, on}, {tuple_row, on}],
	{ok, Ref} = odbc:connect("DSN=InternalMySQL;UID=summoner;PWD=$umm0ns,f,6-", DBConnectionType),
	put(db_ref, Ref).
	
start_update(Timeout) ->
    receive
    after Timeout ->
        update_ccu(),
		  gen_server:cast(self(), update_ccu)%start_update(Timeout)
    end.
	 
start_collect(Timeout) ->
	receive
    after Timeout ->
        collect_garbage_arena(), start_collect(Timeout)
    end.
	 
collect_garbage_arena() -> 
	AllRoom = lib_lobby_protocol:get_all_room().
	
	
check_inactive_arena([{RoomPid, RoomID}| Tail]) ->	
	case process_info(RoomPid) of
		_ -> []
	end.
