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
-module(lib_database).
-author('scrooge mcduck @ playpal.co.th').
-export([connect/0, disconnect/0]).
-export([sql_query/1, get_query/1]).

%% Internal exports - just for mysql_* modules
-export([log/4
	]).

%% Log messages are designed to instantiated lazily only if the logging level
%% permits a log message to be logged
-define(Log(LogFun,Level,Msg),
	LogFun(?MODULE,?LINE,Level,fun()-> {Msg,[]} end)).
-define(Log2(LogFun,Level,Msg,Params),
	LogFun(?MODULE,?LINE,Level,fun()-> {Msg,Params} end)).
			     
log(Module, Line, _Level, FormatFun) ->
    {Format, Arguments} = FormatFun(),
    io:format("~w:~b: "++ Format ++ "~n", [Module, Line] ++ Arguments).

connect() ->
	%LogFun1 = fun log/4,
	%{ok, Pid} = mysql_conn:start_link(	"10.100.1.200", 3306, "summoner", "$umm0ns,f,6-", "smno", LogFun1,	utf8, undefined),
	%Pid.
	
	%%mysql:start_link(p1, "10.100.1.200", undefined, "summoner", "$umm0ns,f,6-", "smno", undefined, utf8),
	%%connect(PoolId, Host, Port, User, Password, Database, Encoding, Reconnect)
	%%mysql:connect(p1, "10.100.1.200", undefined, "summoner", "$umm0ns,f,6-", "smno", utf8, true).
	case get(db_ref) of
		undefined ->
			DBConnectionType = [{auto_commit, on}, {timeout, infinity}, {scrollable_cursors, off}, {trace_driver, on}, {tuple_row, on}],
			case odbc:connect("DSN=InternalMySQL;UID=summoner;PWD=$umm0ns,f,6-", DBConnectionType) of
		%	case odbc:connect("DSN=Internal", DBConnectionType) of
				{ok, Ref} -> 
					CharSet = "SET NAMES utf8",
					odbc:sql_query(Ref, CharSet),
					link(Ref),
					put(db_ref, Ref);
				{error, Reason} ->
					io:format("Can not connect to db : ~p~n", [Reason])
			end;
		Ref -> link(Ref),	put(db_ref, Ref)
	end.
	
disconnect() ->
	DbRef = get(db_ref),
	odbc:disconnect(DbRef),
	garbage_collect(DbRef).
	
	
			
get_query(Statement) ->
	Pid = get(db_ref),
	%smo_logger:fmsg("++++++++++++++your PidDB ~p~n", [Pid]),
	%A = mysql_conn:smo_fetch(Pid, Statement, self(), infinity),
	%mysql_to_odbc(A).
	odbc:sql_query(Pid, Statement).
	
sql_query(Statement) ->
	Pid = smo_player_guardian:request_database_pid(self()),
	%smo_logger:fmsg("++++++++++++++your PidDB ~p~n", [Pid]),
	%A = mysql_conn:smo_fetch(Pid, Statement, self(), infinity),
	%mysql_to_odbc(A).
	%%mysql_to_odbc(mysql:fetch(p1, Statement, infinity)).
	odbc:sql_query(Pid, Statement).
	
%% Convert MySQL query result to Erlang ODBC result formalism
mysql_to_odbc({updated, MySQLRes}) ->
    {updated, mysql:get_result_affected_rows(MySQLRes)};
mysql_to_odbc({data, MySQLRes}) ->
    mysql_item_to_odbc(mysql:get_result_field_info(MySQLRes),
		       mysql:get_result_rows(MySQLRes));
mysql_to_odbc({error, MySQLRes}) when is_list(MySQLRes) ->
    {error, MySQLRes};
mysql_to_odbc({error, MySQLRes}) ->
    {error, mysql:get_result_reason(MySQLRes)}.

%% When tabular data is returned, convert it to the ODBC formalism
mysql_item_to_odbc(Columns, Recs) ->
	%% For now, there is a bug and we do not get the correct value from MySQL
	%% module:
	{selected,
		[element(2, Column) || Column <- Columns],
		[list_to_tuple(Rec) || Rec <- Recs]}.

