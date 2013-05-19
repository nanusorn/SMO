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
-module(smo_friend).
-behaviour(gen_server).
-created_by('Scroge McDuck at playpal.co.th').

-export([start_link/0]).

%% gen_server callbacks
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

%% intermodule export
-export([request_all_friend/1]).
-export([request_friend/2]).
-export([response_friend/2]).

%% used to start the gen_server
start_link() ->
	ServerName = {local, smo_friend}, % name gets register()ed
	Module = smo_friend,
	Options = [],
	gen_server:start_link(ServerName, Module, noargs, Options).

%%------------------------------------------------------------------------------------
%% intermodule export 
%%------------------------------------------------------------------------------------
request_all_friend(PlayerPid) ->
	gen_server:cast(smo_friend, {request_all_friend, PlayerPid}).
request_friend(PlayerPid, Data) ->
	gen_server:cast(smo_friend, {request_friend, PlayerPid, Data}).
response_friend(PlayerPid, Data) ->
	gen_server:cast(smo_friend, {response_friend, PlayerPid, Data}).

%%------------------------------------------------------------------------------------
%% gen_server callback 
%%------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.

handle_call(_Request, _From, State) ->
	{noreply,State}.
			
handle_cast({request_all_friend, PlayerPid}, State) ->
	case lib_lobby_protocol:get_user_data_pid (PlayerPid, [user_id]) of
		[LoginName] ->
			SQLCommand = "CALL smno.sp_show_friend ('" ++ LoginName ++ "');",
			QueryResult = lib_database:get_query(SQLCommand),
			smo_logger:fmsg("~p <<==>> ~p~n", [SQLCommand, QueryResult]),
			case QueryResult of
				[{selected, _, []}, _] ->
					gen_server:cast(PlayerPid, {send, [16#01, 16#06, <<0:32>>]});
				[{selected, _, FriendList}, _] ->
					FriendSize = lists:flatlength (FriendList),
					FriendData1 = get_online_status (FriendList),
					{FriendData2, FriendActivate} = order_frined_by_status (FriendData1, [1, 6, 0, 3, 2, 4, 5]),
					FLData = get_friend_list_data (FriendData2),
					gen_server:cast(PlayerPid, {send, [16#01, 16#06, <<FriendSize:32>>] ++ FLData}),
					send_activate_response_friend (PlayerPid, LoginName, FriendActivate);
				R -> io:format ("Return ~p~n", [R])
			end;		
		_ ->
			smo_logger:msg("Found nothing")
	end,	
	{noreply,State};
handle_cast({request_friend, PlayerPid, Data}, State) ->
	[RequestAction, FNameSize | FName] = Data,
	case lib_lobby_protocol:get_user_data_pid (PlayerPid, [user_id]) of
		[LoginName] ->
			NameSize = lists:flatlength (FName),
			case NameSize of
				FNameSize ->
					request_friend (PlayerPid, RequestAction, LoginName, FName);
				_ ->	smo_logger:fmsg("Friend name error ~w", [FName])
			end;			
		_ ->
			smo_logger:msg("request_friend : user_id not found...")
	end,
	{noreply,State};
handle_cast({response_friend, PlayerPid, Data}, State) ->
	[ResponseAction, FNameSize | FName] = Data,
	case lib_lobby_protocol:get_user_data_pid (PlayerPid, [user_id]) of
		[LoginName] ->
			NameSize = lists:flatlength (FName),
			case NameSize of
				FNameSize ->
					response_friend (PlayerPid, ResponseAction, LoginName, FName);
				_ ->	smo_logger:fmsg("response_friend : Friend name error ~w", [FName])
			end;
		_ ->
			smo_logger:msg("response_friend : user_id not found...")
	end,
	{noreply,State};
handle_cast({activate_player_response_friend, FPlayerName, LoginName, PlayerName}, State) ->
	%FPNAscii = ascii_unicode:unicode_to_ascii (FPlayerName),
	case lib_lobby_protocol:check_user_online_status (player_name, FPlayerName) of
		[] ->	
			smo_logger:msg ("Try to add offline friend");
		[{_FLoginName}] ->
			LSize = lists:flatlength (PlayerName),
			[PlayerPid] = lib_lobby_protocol:get_user_pid_by_name(FPlayerName),
			gen_server:cast(PlayerPid, {send, [16#81, 16#03, LSize] ++ PlayerName}),
			sp_activate_request(LoginName, FPlayerName)
	end,
	{noreply,State};
handle_cast({update_response_consider, _FLoginName, PlayerPid, _Consideration}, State) ->
	case lib_lobby_protocol:get_user_data_pid (PlayerPid, [player_name]) of
		[PlayerName] ->
			AcceptedData = [lists:flatlength(PlayerName)] ++ PlayerName;
			%%case find_process_id (FLoginName, L) of
			%%	{ok, ReturnPid} ->
			%%		send (ReturnPid, [16#81, 16#07, Consideration] ++ AcceptedData);
			%%	_ ->	io:format ("Can not find process id of ~p from ~p~n", [FLoginName, L])
			%%end;
		_ ->	io:format ("Get playername error from ~p~n", [PlayerPid])
	end,
	{noreply,State};
handle_cast(Request, State) ->
	{noreply,State}.

handle_info(Info, State) ->
	{noreply,State}.

init(Args) ->
	State = [],
	odbc:start(),
	DBConnectionType = [{auto_commit, on}, {timeout, infinity}, {scrollable_cursors, off}, {trace_driver, on}, {tuple_row, on}],
	{ok, Ref} = odbc:connect("DSN=InternalMySQL;UID=summoner;PWD=$umm0ns,f,6-", DBConnectionType),
	put(db_ref, Ref),
	{ok,  State}.

%% called when handle_cast returns stop.
terminate(Reason, State) ->
	ok.
	
%%------------------------------------------------------------------------------------
%% internal function
%%------------------------------------------------------------------------------------
get_online_status ([]) -> [];
get_online_status ([{FriendName, Status} | FriendList]) ->
	AsciiFN = ascii_unicode:unicode_to_ascii (FriendName),
	case Status of
		1 ->	case online_status_use (AsciiFN) of
				online ->
					[{FriendName, Status}] ++ get_online_status (FriendList);
				offline ->
					[{FriendName, 6}] ++ get_online_status (FriendList)
			end;
		_ ->	[{FriendName, Status}] ++ get_online_status (FriendList)
	end.

order_frined_by_status (_, []) -> {[], []};
order_frined_by_status (FriendData1, [OrderStatus | OrderList]) ->
	Friend = get_friend_order_status (FriendData1, OrderStatus),
	{Send, Activate} = order_frined_by_status (FriendData1 -- Friend, OrderList),
	case OrderStatus of
		0 ->	{Friend ++ Send, Friend ++ Activate};
		_ ->	{Friend ++ Send, Activate}
	end.

get_friend_order_status ([], _) -> [];
get_friend_order_status ([{FriendName, OrderStatus} | FriendList], OrderStatus) ->
	[{FriendName, OrderStatus}] ++ get_friend_order_status (FriendList, OrderStatus);
get_friend_order_status ([_ | FriendList], OrderStatus) ->
	get_friend_order_status (FriendList, OrderStatus).

get_friend_list_data ([]) -> [];
get_friend_list_data ([{FriendName, Status} | FriendList]) ->
	AsciiFN = ascii_unicode:unicode_to_ascii (FriendName),
	[lists:flatlength (AsciiFN), AsciiFN, 0, Status] ++ get_friend_list_data (FriendList).

online_status_use (AsciiFN) ->
	case lib_lobby_protocol:check_user_online_status (player_name, AsciiFN) of
		[] ->	offline;
		_ ->	online
	end.

send_activate_response_friend (_, _, []) -> all_activate;
send_activate_response_friend (PlayerPid, LoginName, [{FriendName, _} | Activate]) ->
	PlayerName = lib_lobby_protocol:get_user_data_login (LoginName, player_name),
	FPNAscii = ascii_unicode:unicode_to_ascii (FriendName),
	LSize = lists:flatlength (FPNAscii),
	%%psql_odbc ! {sp_activate_request, FriendName, PlayerName},
	sp_activate_request (FriendName, PlayerName),
	gen_server:cast(PlayerPid, {send, [16#81, 16#03, LSize] ++ FPNAscii}),
	send_activate_response_friend (PlayerPid, LoginName, Activate).

request_friend (PlayerPid, RequestAction, LoginName, FriendPlayerName) ->
	case RequestAction of
		0 ->	activate_player_response_friend (PlayerPid, LoginName, FriendPlayerName);
		1 ->	activate_player_delete_friend (LoginName, FriendPlayerName);
		2 ->	activate_player_block_friend (LoginName, FriendPlayerName);
		3 -> activate_player_unblock_friend (LoginName, FriendPlayerName)
	end.

activate_player_response_friend (PlayerPid, LoginName, FriendPlayerName) ->
	[FNameID] = lib_lobby_protocol:get_user_id_by_name(FriendPlayerName),
	%smo_logger:fmsg("Desire Freind ~p <<<==>>> ~p~n", [FriendPlayerName, FNameID]),
	SQLCommand = "CALL smno.sp_request_friend ('" ++ LoginName ++ "', '" ++ FNameID ++ "');",
	%smo_logger:fmsg("Request ~p <<<==>>> ..... ~n", [LoginName]),
	
	%smo_logger:fmsg("~p <<<==>>> ..... ~n", [SQLCommand]),
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <<<==>>> ~p ~n", [SQLCommand, QueryResult]),
	case QueryResult of
		[{selected, _, [{1}]}, _] ->
%			io:format ("Request to be new friend~n"),
			gen_server:cast(PlayerPid, {send , [16#01, 16#07, 16#01]}),
			PlayerName = lib_lobby_protocol:get_user_data_login (LoginName, player_name),
			gen_server:cast(self(), {activate_player_response_friend, FriendPlayerName, LoginName, PlayerName});
		[{selected, _, [{0}]}, _] ->
%			io:format ("Request to an old friend~n"),
			gen_server:cast(PlayerPid, {send, [16#01, 16#07, 16#02]});
		R -> io:format ("Activate player response friend ~p~n", [R])
	end.
	
activate_player_delete_friend(LoginName, FriendPlayerName) ->
	SQLCommand = "CALL smno.sp_delete_friend ('" ++ LoginName ++ "', '" ++ FriendPlayerName ++ "');",
	lib_database:get_query(SQLCommand).
%	io:format ("Delete ~p~n", [SQLCommand]),
%	case odbc:sql_query(SQLCommand) of
%		[{selected, _, [{1}]} | _] ->
%			io:format ("Player ~p delete ~p from friend list~n", [LoginName, FriendPlayerName]);
%		R ->	io:format ("Return data from delete name ~p~n", [R])
%	end.

activate_player_block_friend (LoginName, FriendPlayerName) ->
	SQLCommand = "CALL smno.sp_response_friend ('" ++ LoginName ++ "', '" ++ FriendPlayerName ++ "', 5);",
%	io:format ("Block ~p~n", [SQLCommand]),
	lib_database:get_query(SQLCommand).

activate_player_unblock_friend (LoginName, FriendPlayerName) ->
	SQLCommand = "CALL smno.sp_response_friend ('" ++ LoginName ++ "', '" ++ FriendPlayerName ++ "', 7);",
%	io:format ("Block ~p~n", [SQLCommand]),
	lib_database:get_query(SQLCommand).

sp_activate_request(LoginName, FriendPlayerName) ->
%	io:format ("Player ~p has activated ~p~n", [LoginName, FriendName]),
%	FriendName = ascii_unicode:ascii_to_unicode (FriendPlayerName),
	SQLCommand = "CALL smno.sp_activate_request ('" ++ LoginName ++ "', '" ++ FriendPlayerName ++ "');",
	io:format ("Activate request ~p~n", [SQLCommand]),
	lib_database:get_query(SQLCommand).

response_friend(PlayerPid, ResponseAction, LoginName, FriendPlayerName) ->
%	FPNAscii = ascii_unicode:unicode_to_ascii (FriendPlayerName),
	case lib_lobby_protocol:get_user_id_by_name(FriendPlayerName) of
		[FLoginName] ->
			update_response_friend(PlayerPid, ResponseAction, LoginName, FLoginName, FriendPlayerName);
		_ ->	io:format ("Error not found login from ~p~n", [FriendPlayerName])
	end.

update_response_friend (PlayerPid, ResponseAction, LoginName, FLoginName, FriendPlayerName) ->
	%FUTF = ascii_unicode:ascii_to_unicode (FriendPlayerName),
	case ResponseAction of
		0 ->	SQLCommand = "CALL smno.sp_response_friend ('" ++ LoginName ++ "', '" ++ FriendPlayerName ++ "', 2);";
		1 ->	SQLCommand = "CALL smno.sp_response_friend ('" ++ LoginName ++ "', '" ++ FriendPlayerName ++ "', 1);"
	end,
%	io:format ("Response query ~p~n", [SQLCommand]),
	case lib_database:get_query(SQLCommand) of
		[{selected, _, [{1}]}, _] ->
			%%%ChannelController ! {update_response_consider, FLoginName, PlayerPid, ResponseAction};
			gen_server:cast(self(), {update_response_consider, FLoginName, PlayerPid, ResponseAction});
		R -> io:format ("Response friend ~p~n", [R])
	end.
