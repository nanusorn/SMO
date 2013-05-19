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
-module(lib_friend).

-export([request_all_friend/1]).
-export([request_friend/2]).
-export([response_friend/2]).

request_all_friend(SPlayer) ->
	case lib_lobby_protocol:get_user_data_pid (SPlayer, [user_id]) of
		[LoginName] ->
			SQLCommand = "CALL smno.sp_show_friend ('" ++ LoginName ++ "');",
			QueryResult = lib_database:get_query(SQLCommand),
			smo_logger:fmsg("~p <<==>> ~p~n", [SQLCommand, QueryResult]),
			case QueryResult of
				[{selected, _, []}, _] ->
					gen_server:cast(SPlayer, {send, [16#01, 16#06, <<0:32>>]});
				[{selected, _, FriendList}, _] ->
					FriendSize = lists:flatlength(FriendList),
					FriendData1 = get_online_status(FriendList),
					{FriendData2, FriendActivate} = order_frined_by_status(FriendData1, [1, 6, 0, 3, 2, 4, 5]),
					FLData = get_friend_list_data(FriendData2),
					% FLData = [lists:flatlength(FriendName), FriendName, lists:flatlength(FUserID), FUserID, 0, Status] ++ [...]
					gen_server:cast(SPlayer, {send, [16#01, 16#06, <<FriendSize:32>>] ++ FLData}),
					send_activate_response_friend(SPlayer, LoginName, FriendActivate);
				R -> io:format ("Return ~p~n", [R])
			end;		
		_ ->
			smo_logger:msg("Found nothing")
	end.

request_friend(PlayerPid, Data) ->
	%FName คือ UserID ของ เป้าหมายของการร้องขอ
	[RequestAction, FNameSize | FName] = Data,
	case lib_lobby_protocol:get_user_data_pid(PlayerPid, [user_id]) of
		% คือ UserID ของ ผู้ร้องขอ
		[LoginName] ->
			NameSize = lists:flatlength (FName),
			case NameSize of
				FNameSize ->
					request_friend(PlayerPid, RequestAction, LoginName, FName);
				_ ->	smo_logger:fmsg("Friend name error ~w", [FName])
			end;			
		_ ->
			smo_logger:msg("request_friend : user_id not found...")
	end.
	
% LoginName = UserID ของผู้ Request
% FName อาจเป็น PlayerName หรือ LoginName ก็ได้
request_friend(PlayerPid, RequestAction, LoginName, FName) ->
	FreindPlayerName = FLoginName = FName,
	case RequestAction of
		0 ->	activate_player_response_friend(PlayerPid, LoginName, FreindPlayerName); % ใช้ PlayerName Client ส่งมาเป็น PlayerName
		1 ->	activate_player_delete_friend(LoginName, FLoginName);
		2 ->	activate_player_block_friend(LoginName, FLoginName);
		3 -> activate_player_unblock_friend(LoginName, FLoginName)
	end.
	
activate_player_response_friend(PlayerPid, LoginName, FriendPlayerName) ->
	[FLoginName] = lib_lobby_protocol:get_user_id_by_name(FriendPlayerName),
	SQLCommand = "CALL smno.sp_request_friend ('" ++ LoginName ++ "', '" ++ FLoginName ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	%smo_logger:fmsg("~p <<<==>>> ~p ~n", [SQLCommand, QueryResult]),
	case QueryResult of
		[{selected, _, [{1}]}, _] ->
%			io:format ("Request to be new friend~n"),
			gen_server:cast(PlayerPid, {send , [16#01, 16#07, 16#01]}),
			PlayerName = lib_lobby_protocol:get_user_data_login(LoginName, player_name),
			player_response_friend(FLoginName, LoginName, PlayerName);
		[{selected, _, [{0}]}, _] ->
%			io:format ("Request to an old friend~n"),
			gen_server:cast(PlayerPid, {send, [16#01, 16#07, 16#02]});
		R -> io:format ("Activate player response friend ~p~n", [R])
	end.
	
player_response_friend(FLoginName, LoginName, PlayerName) -> 
	case lib_lobby_protocol:get_user_pid_by_userid(FLoginName) of
		[FPid] ->
			sp_activate_request(LoginName, FLoginName),
			LSize = lists:flatlength(PlayerName),
			gen_server:cast(FPid, {send, [16#81, 16#03, LSize] ++ PlayerName});
		[] -> smo_logger:fmsg("~p Try to Add Offline Friend ~n", [FLoginName])
	end.
			
	% case lib_lobby_protocol:check_user_online_status(player_name, FPlayerName) of
		% [] ->	
			% smo_logger:msg ("Try to add offline friend");
		% [{FLoginName}] ->
			% LSize = lists:flatlength (PlayerName),
			% [PlayerPid] = lib_lobby_protocol:get_user_pid_by_name(FPlayerName),
			% gen_server:cast(PlayerPid, {send, [16#81, 16#03, LSize] ++ PlayerName}),
			% sp_activate_request(LoginName, FLoginName)
	% end.
	
activate_player_delete_friend(LoginName, FLoginName) ->
	SQLCommand = "CALL smno.sp_delete_friend ('" ++ LoginName ++ "', '" ++ FLoginName ++ "');",
	lib_database:get_query(SQLCommand).

activate_player_block_friend(LoginName, FLoginName) ->
	SQLCommand = "CALL smno.sp_response_friend ('" ++ LoginName ++ "', '" ++ FLoginName ++ "', 5);",
%	io:format ("Block ~p~n", [SQLCommand]),
	lib_database:get_query(SQLCommand).
	
activate_player_unblock_friend(LoginName, FLoginName) ->
	SQLCommand = "CALL smno.sp_response_friend ('" ++ LoginName ++ "', '" ++ FLoginName ++ "', 7);",
%	io:format ("Block ~p~n", [SQLCommand]),
	lib_database:get_query(SQLCommand).
	
response_friend(PlayerPid, Data) ->
	[ResponseAction, FNameSize | FName] = Data,
	case lib_lobby_protocol:get_user_data_pid(PlayerPid, [user_id]) of
		[LoginName] ->
			NameSize = lists:flatlength (FName),
			case NameSize of
				FNameSize ->
					response_friend(PlayerPid, ResponseAction, LoginName, FName);
				_ ->	smo_logger:fmsg("response_friend : Friend name error ~w", [FName])
			end;
		_ ->
			smo_logger:msg("response_friend : user_id not found...")
	end.
	
response_friend(PlayerPid, ResponseAction, LoginName, FriendPlayerName) ->
%	FPNAscii = ascii_unicode:unicode_to_ascii (FriendPlayerName),
	case lib_lobby_protocol:get_user_pid_name_by_name(FriendPlayerName) of
		[{FPid, FLoginName}] ->
			update_response_friend(PlayerPid, ResponseAction, LoginName, FLoginName, FPid);
		_ ->	io:format ("Error not found login from ~p~n", [FriendPlayerName])
	end.
	
update_response_friend(PlayerPid, ResponseAction, LoginName, FLoginName, FPid) ->
	%FUTF = ascii_unicode:ascii_to_unicode (FriendPlayerName),
	case ResponseAction of
		0 ->	SQLCommand = "CALL smno.sp_response_friend ('" ++ LoginName ++ "', '" ++ FLoginName ++ "', 2);";
		1 ->	SQLCommand = "CALL smno.sp_response_friend ('" ++ LoginName ++ "', '" ++ FLoginName ++ "', 1);"
	end,
%	io:format ("Response query ~p~n", [SQLCommand]),
	case lib_database:get_query(SQLCommand) of
		[{selected, _, [{1}]}, _] ->
			%%%ChannelController ! {update_response_consider, FLoginName, PlayerPid, ResponseAction};
			update_response_consider(FPid, PlayerPid, ResponseAction);
		R -> io:format ("Response friend ~p~n", [R])
	end.
	
update_response_consider(FPid, PlayerPid, Consideration) ->
	case lib_lobby_protocol:get_user_data_pid(PlayerPid, [player_name]) of
		[PlayerName] ->
			AcceptedData = [lists:flatlength(PlayerName)] ++ PlayerName,
			%%case find_process_id (FLoginName, L) of
			%%	{ok, ReturnPid} ->
			%%		send (ReturnPid, [16#81, 16#07, Consideration] ++ AcceptedData);
			%%	_ ->	io:format ("Can not find process id of ~p from ~p~n", [FLoginName, L])
			%%end;
			gen_server:cast(FPid, {send, [16#81, 16#07, Consideration] ++ AcceptedData});
		_ ->	io:format ("Get playername error from ~p~n", [PlayerPid])
	end.	
%%------------------------------------------------------------------------------------
%% internal function
%%------------------------------------------------------------------------------------
get_online_status([]) -> [];
get_online_status([{FUserId, FriendName, Status} | FriendList]) ->
	%AsciiFN = ascii_unicode:unicode_to_ascii (FriendName),
	%smo_logger:fmsg("User '~p' is ~p~n", [FUserId, Status]),
	case Status of
		1 ->	
			case online_status_use(FUserId) of
				online ->
					[{FUserId, FriendName, Status}] ++ get_online_status(FriendList);
				offline ->
					[{FUserId, FriendName, 6}] ++ get_online_status(FriendList)
			end;
		_ ->	[{FUserId, FriendName, Status}] ++ get_online_status(FriendList)
	end.

order_frined_by_status(_, []) -> {[], []};
order_frined_by_status(FriendData1, [OrderStatus | OrderList]) ->
	Friend = get_friend_order_status(FriendData1, OrderStatus),
	{Send, Activate} = order_frined_by_status(FriendData1 -- Friend, OrderList),
	case OrderStatus of
		0 ->	{Friend ++ Send, Friend ++ Activate};
		_ ->	{Friend ++ Send, Activate}
	end.

get_friend_order_status ([], _) -> [];
get_friend_order_status ([{FLoginName, FriendName, OrderStatus} | FriendList], OrderStatus) ->
	[{FLoginName, FriendName, OrderStatus}] ++ get_friend_order_status (FriendList, OrderStatus);
get_friend_order_status ([_ | FriendList], OrderStatus) ->
	get_friend_order_status (FriendList, OrderStatus).

get_friend_list_data([]) -> [];
get_friend_list_data([{FUserID, FriendName, Status} | FriendList]) ->
	%AsciiFN = ascii_unicode:unicode_to_ascii (FriendName),
	[lists:flatlength(FUserID), FUserID, lists:flatlength(FriendName), FriendName, 0, Status] ++ get_friend_list_data(FriendList).

online_status_use(LoginName) ->
	case lib_lobby_protocol:get_user_pid_by_userid(LoginName) of
		[] ->	offline;
		_ ->	online
	end.

send_activate_response_friend(_, _, []) -> all_activate;
send_activate_response_friend(PlayerPid, LoginName, [{FLoginName, FriendName, _} | Activate]) ->
	PlayerName = lib_lobby_protocol:get_user_data_login(LoginName, player_name),
	sp_activate_request(FLoginName, LoginName),
	LSize = lists:flatlength(PlayerName),	
	gen_server:cast(PlayerPid, {send, [16#81, 16#03, LSize] ++ PlayerName}),
	send_activate_response_friend(PlayerPid, LoginName, Activate).

sp_activate_request(LoginName, FriendLoginName) ->
	%smo_logger:fmsg("CALL smno.sp_activate_request ('~p', '~p' ) ~n", [LoginName, FriendLoginName]),
	SQLCommand = "CALL smno.sp_activate_request ('" ++ LoginName ++ "', '" ++ FriendLoginName ++ "');",
	lib_database:get_query(SQLCommand).