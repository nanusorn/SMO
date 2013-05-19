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
%%%-------------------------------------------------------------------
%%% File    : smo_mysql.erl
%%% Author  : Scrooge McDuck (nanusorn@playpal.co.th)
%%% Descrip.: Main entry for replace usage of Erlang's ODBC. 
%%% Created : 10/9/2008
%%%
%%% Note    : All ODBC call in ssmo will be replace and changed to call
%%%           smo_mysql instead, since only 2 function were called in
%%%           smo project, so this module created for served need of those
%%%           requirement.
%%% Copyright (c) 2008 PlayPal Co.,Ltd. All Right Reserved.
%%%
%%%-------------------------------------------------------------------
-module(smo_mysql).
-auther('nanusorn@playpal.co.th').
-export([start/0]).
-export([sql_query/1]).

start() ->
	%mysql:start_link(p1, "192.168.10.33", undefined, "summoner", "$umm0ns,f,6-", "smno", undefined, utf8),
	%mysql:connect(p1, "192.168.10.33", undefined, "summoner", "$umm0ns,f,6-", "smno", utf8, true).
	mysql:start_link(p1, "203.144.132.76", undefined, "summoner", "$umm0ns,f,6-", "smno", undefined, utf8),
	mysql:connect(p1, "203.144.132.76", undefined, "summoner", "$umm0ns,f,6-", "smno", utf8, true).
	
sql_query(Statement) ->
	mysql_to_odbc(mysql:fetch(p1, Statement, infinity)).
	
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
