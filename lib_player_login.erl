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
-module(lib_player_login).
-export([check_login/2]).
-import(lists, [sublist/3, nthtail/2]).

-export([check_duplicate_login/2, reject_login/2]).

% to_lower_case ([]) -> [];
% to_lower_case ([Chr | Text]) ->
	% if
		% Chr >= 65, Chr =< 90 ->
			% [Chr + 32] ++ to_lower_case (Text);
		% true ->
			% [Chr] ++ to_lower_case (Text)
	% end.

reject_login(SPlayer, Reason) ->
	case Reason of
		wrong_password -> gen_server:cast(SPlayer, {send , [16#00, 16#00, 16#00, 16#00]});
		duplicate_login -> gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#01]});
		
		wrong_version -> gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#03]});
		
		can_not_connect -> gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#05]});

		ban_user -> gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#06]});
		server_limited -> gen_server:cast(SPlayer, {send , [16#00, 16#00, 16#00, 16#07]})
	end,
	lib_database:disconnect(),
	garbage_collect(SPlayer).
	
check_login(SPlayer, [LoginSize | Data]) ->
	[_, _, _, {workers, Worker}] = supervisor:count_children(smo_player_sup),
	%LinksCount = gen_server:call(smo_player_guardian, {get_ccu}),
	case Worker > 500 of
		true -> 	reject_login(SPlayer, server_limited);
		_ ->
			smo_logger:fmsg("initiate check login with DATA ~p~n", [Data]),
			DataSize = lists:flatlength (Data),
			if DataSize >= LoginSize ->
				Login = sublist (Data, 1, LoginSize),
				DataRest = nthtail (LoginSize, Data),
				%smo_logger:fmsg("1st step check login ~p~n", [SPlayer]),
				check_password_data (SPlayer, Login, DataRest);
			 true ->
				smo_logger:msg("logger tracking"),
				reject_login(SPlayer, wrong_password)
			end
	end.
	
check_password_data(SPlayer, _, []) ->
	reject_login(SPlayer, cannot_connect);
check_password_data(SPlayer, Login, [PassSize | Data]) ->
	DataSize = lists:flatlength (Data),
	if DataSize >= PassSize ->
		Password = sublist (Data, 1, PassSize),
		DataRest = nthtail (PassSize, Data),
		%smo_logger:fmsg("2nd step check password data ~p~n", [SPlayer]),
		check_version_data(SPlayer, Login, Password, DataRest);
	true ->
		reject_login(SPlayer, wrong_password)
	end.

check_version_data(SPlayer, _, _, []) ->
	gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#03]});
check_version_data(SPlayer, Login, Password, [VersionSize | Data]) ->
	DataSize = lists:flatlength (Data),
	if DataSize =:= VersionSize ->
		VersionCheck = sublist (Data, 1, VersionSize),
		%smo_logger:fmsg("3rd step check version data ~p~n", [SPlayer]),
		%check_client_version(SPlayer, Login, Password, VersionCheck);
		check_user_password_aid(SPlayer, Login, Password, VersionCheck);
	true ->
		gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#03]})
	end.
	
check_user_password_aid(SPlayer, Login, Password, VersionCheck) ->
	case individual_connect_aid() of
		[] -> 
			check_client_version(SPlayer, Login, Password, VersionCheck);
			%check_user_password_internal_player(SPlayer, Login, Password);
		%gen_server:cast(SPlayer, {send , [16#00, 16#00, 16#00, 16#00]});%{reply, error, State};
		ARef -> 
				SQLCommand = "EXEC dbo.usp_UserPassword_SMOCheck '" ++ Login ++ "', '" ++ Password ++ "';",
				QueryResult = odbc:sql_query(ARef, SQLCommand, infinity),
				smo_logger:fmsg("~p <=:=> ~p on ~p~n", [SQLCommand, QueryResult, now()]),
				case QueryResult of
					{selected,_,[{_,0}]} -> 	
						odbc:disconnect(ARef),
						garbage_collect(ARef),
						check_first_time_login_version(SPlayer, Login, VersionCheck);%{reply, ok, State};
					{selected,_,[{_,1}]} -> 
						odbc:disconnect(ARef),
						garbage_collect(ARef),
						check_client_version(SPlayer, Login, Password, VersionCheck);%{reply, not_found, State};
					{selected,_,[{_,2}]} -> 
						odbc:disconnect(ARef),
						garbage_collect(ARef),
						gen_server:cast(SPlayer, {send , [16#00, 16#00, 16#00, 16#00]});%{reply, wrong_psw, State};
					 _ ->
					 	smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
						odbc:disconnect(ARef),
						garbage_collect(ARef),
						gen_server:cast(SPlayer, {send , [16#00, 16#00, 16#00, 16#00]})%{reply, error, State}
				end
	end.

check_client_version(SPlayer, Login, Password, VersionCheck) ->
	lib_database:connect(),
	SQLCommand = "SELECT smno.fc_chk_version ('" ++ VersionCheck ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p on ~p~n", [SQLCommand, QueryResult, now()]),
	case QueryResult of
		{selected, _, [{1}]} ->	check_user_password_internal_player(SPlayer, Login, Password);
		_ -> reject_login(SPlayer, wrong_version)
	end.
	
check_first_time_login_version(SPlayer, Login, VersionCheck) ->
	lib_database:connect(),
	SQLCommand = "SELECT smno.fc_chk_version ('" ++ VersionCheck ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p on ~p~n", [SQLCommand, QueryResult, now()]),
	case QueryResult of
		{selected, _, [{1}]} -> check_first_time_login(SPlayer, Login);
		_ -> reject_login(SPlayer, wrong_version)
	end.
	
check_first_time_login(SPlayer, Login) ->
	SQLCommand = "CALL smno.sp_chk_profile ('" ++ Login ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p on ~p~n", [SQLCommand, QueryResult, now()]),
	% smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
	case QueryResult of
		[{selected, _, [{3}]}, _] -> % Ban Users
			reject_login(SPlayer, ban_user);%gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#00]});
		[{selected, _, [{2}]}, _]->
			reject_login(SPlayer, wrong_password);%gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#03]});
		[{selected, _, [{1}]}, _] ->
			gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#02]});
		[{selected, _, [{0}]}, _] ->
			check_duplicate_login(SPlayer, Login);
		_R -> smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
			reject_login(SPlayer, can_not_connect)%gen_server:cast(SPlayer, {send , [16#00, 16#00, 16#00, 16#00]}),
			%io:format ("Check profile fault ~p~n", [R])
	end.
	
check_user_password_internal_player(SPlayer, Login, Password) ->
	SQLCommand = "CALL smno.sp_chk_login ('" ++ Login ++ "', '" ++ Password ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p on ~p~n", [SQLCommand, QueryResult, now()]),
	% smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
	%%smo_logger:fmsg("smo_database: parameter for query ~p, ~p, ~p", [Login, Password, VersionCheck]).
	case QueryResult of	
		[{selected, _, [{4}]}, _] ->
			reject_login(SPlayer, ban_user);
		[{selected, _, [{3}]}, _] ->
			gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#02]});%gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#03]});
		[{selected, _, [{1}]}, _] ->
			check_duplicate_login(SPlayer, Login);
		[{selected, _, [{0}]}, _] ->
			% incorrect password
			reject_login(SPlayer, wrong_password);%gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#00]});
		_R ->
			smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
%%			self() ! {db_connection_lost},
			reject_login(SPlayer, can_not_connect)%gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#00]}); %% incorrect password
	end.

check_duplicate_login(SPlayer, Login) ->
	case check_duplicate_login(Login) of
		{allow, LoginLower} ->
			query_command_gender(SPlayer, LoginLower);
		{disallow, Pid} ->
			%gen_server:cast(Pid, {other_duplicate_login, SPlayer, Login})
			gen_server:cast(Pid, {stop_process}),
			check_duplicate_login(SPlayer, Login)
			%reject_login(SPlayer, duplicate_login)%gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#01]})
	end.
	
check_duplicate_login(Login) ->
	%LowerLogin = to_lower_case(Login),
	%case lib_lobby_protocol:get_user_data_login(LowerLogin, process_id) of
	case lib_lobby_protocol:get_user_data_login(Login, process_id) of
		%error -> {allow, LowerLogin};
		error -> {allow, Login};
		Pid ->
			case process_info(Pid, status) of
				{status, waiting} -> {disallow, Pid};
				{status, running} -> {disallow, Pid};
				_Processing -> 
					smo_logger:fmsg("duplicate login case ~w", [_Processing]), 
					lib_lobby_protocol:remove_user_login(Login),
					{allow, Login}
			end
	end.

query_command_gender(SPlayer, LoginLower) ->
	SQLCommand = "CALL smno.sp_insertLogPlayer('"++ LoginLower ++"', 1);",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p on ~p~n", [SQLCommand, QueryResult, now()]),
	case QueryResult of
		[{selected, _, [{Gender}]}, _] ->
			request_player_data (SPlayer, LoginLower, Gender);
		_R ->	
			%%io:format ("Get gender error ~p ~n", [R]),
			smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
			reject_login(SPlayer, wrong_password)%gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#03]})
	end.	
	
request_player_data (SPlayer, LoginLower, Gender) ->
	%smo_logger:fmsg("SPlayer:~p and self():~p ~n", [SPlayer, self()]),
	SQLCommand = "CALL smno.sp_get_data_player('" ++ LoginLower ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p on ~p~n", [SQLCommand, QueryResult, now()]),
	case QueryResult of
		[{selected, _, []}, _] ->
			gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#00, 16#00]});
		[{selected, _, Result}, _] ->
			PidDB = get(db_ref),
			gen_server:cast(SPlayer, {update_state, Result, PidDB}),

			lib_lobby_protocol:add_player_data_to_mnesia(SPlayer, Result),

			[{_PlayerID, PNUTF, AvatarSet, AvatarID, _DeckUsed}] = Result,
			DBTime = db_server_time(),
			PNameLength = length(PNUTF),			
			gen_server:cast(SPlayer, {send, [16#0, 16#0, 16#1, PNameLength] ++ PNUTF ++ [list_to_integer(AvatarSet), list_to_integer(AvatarID)] ++ DBTime}),
			gen_server:cast(smo_player_guardian, {update_player_login, LoginLower}),
			smo_logger:fmsg("~p login successful with Data [~p, ~p, ~p] on ~p", [SPlayer, _PlayerID, LoginLower, PNUTF, now()]);

		_ErrReason ->
			smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
			reject_login(SPlayer, can_not_connect)
	end.
	
individual_connect_aid() ->
	DBConnectionType = [{auto_commit, on}, {timeout, infinity}, {scrollable_cursors, off}, {trace_driver, on}, {tuple_row, on}],
	case odbc:connect("DSN=MS_AID;UID=SMO_Check_UserAuthen;PWD=3nf8EYYSj4uj", DBConnectionType) of
		{ok, Ref} -> %smo_logger:fmsg("------------------------reconnect to @id SQL Server with = ~w~n", [Ref]),
			Ref;
		{error, Reason} ->
			smo_logger:fmsg("Can not initialize odbc to @id SQL Server with Reason = ~w",[Reason]),
			[]
	end.
	
db_server_time() ->
	SQLCommand = "SELECT smno.fc_get_time();",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p on ~p~n", [SQLCommand, QueryResult, now()]),
	case QueryResult of
		{selected,_,[{Time}]} -> Time;
		_ -> ""
	end.
