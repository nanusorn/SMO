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
-module(smo_database).
-behaviour(gen_server).
-created_by('Scrooge McDuck at playpal.co.th').

%% intermodule exports
-export([start_link/0]).
%%-export([start/1]).
-export([sql_query/1]).

%% gen_server callbacks
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

-record(state, {db_ref, db_type}).

-define(MYSQL_PORT, 3306).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% intermodule exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
start_link() ->
	smo_logger:msg("database start link"),
	ServerName = {local, smo_database}, % name gets register()ed
	Module = smo_database,
	Options = [],
	gen_server:start_link(ServerName, Module, noargs, Options).
	
sql_query(Query) ->
    gen_server:call(smo_database, {sql_query, Query}, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks
code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.

handle_call({sql_query, Query}, From, State) ->
    case smo_mysql:sql_query(Query) of
	% error returned by MySQL driver
		{error, "query timed out"} = Reply ->
			%%%smo_logger:msg("====================>>> Return Pos 1 <<<===================="),
			 {stop, timeout, Reply, State};
		% error returned by MySQL driver
		{error, "Failed sending data on socket"++_} = Reply ->
			%%%smo_logger:msg("====================>>> Return Pos 2 <<<===================="),
			 {stop, closed, Reply, State};
		Reply ->
			%%%smo_logger:msg("====================>>> Return Pos 3 <<<===================="),
			 {reply, Reply, State}
    end;

handle_call(Request, From, State) ->
	{noreply, State}.

handle_cast(Request, State) ->
	{noreply,State}.

handle_info(Info, State) ->
	{noreply, State}.

init(Args) ->
	odbc:start(),
	% DBConnectionType = [{auto_commit, on}, {timeout, infinity}, {scrollable_cursors, off}, {trace_driver, on}, {tuple_row, on}],
	% State =
	% case odbc:connect("DSN=MS_AID;UID=SMO_Check_UserAuthen;PWD=3nf8EYYSj4uj", DBConnectionType) of
		% {ok, Ref} -> smo_logger:fmsg("------------------------connect to @id SQL Server with = ~w~n", [Ref]),
			% register(aref, Ref),
			% [Ref];
		% {error, Reason} ->
			% smo_logger:fmsg("Can not initialize odbc to @id SQL Server with Reason = ~w",[Reason]),
			% []
	% end,
	smo_mysql:start(),
	%{ok, State}.
	{ok, []}.

%% called when handle_cast returns stop.
%% when a shutdown occurs, all sockets are brutally killed by smo_player_sup
terminate(Reason, State) ->
	ok.
