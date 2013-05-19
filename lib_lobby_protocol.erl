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
-module (lib_lobby_protocol).

-include_lib ("stdlib/include/qlc.hrl").
-include ("record.hrl").

-import (mnesia_table, [do/1]).
-import (lists, [foreach/2, sum/1, append/2, min/1]).

-compile (export_all).

to_lower_case ([]) -> [];
to_lower_case ([Chr | Text]) ->
	if
		Chr >= 65, Chr =< 90 ->
			[Chr + 32] ++ to_lower_case (Text);
		true ->
			[Chr] ++ to_lower_case (Text)
	end.
	
count_lists_size (L) ->
	count_list_size (L, 0).
count_list_size ([_|T], Size) ->
	count_list_size (T, Size + 1);
count_list_size ([], Size) -> Size.


request_room_info (SPlayer) ->
	QResult = request_room_infomation(),
	NumberOfRoom = count_lists_size(QResult),
	Reply = [<<NumberOfRoom:16>>|QResult],
	%smo_logger:fmsg("Reply request_room_info = ~w",[Reply]),
	
	gen_server:cast(SPlayer, {send,  [16#01, 16#00, Reply]}).
		
request_room_infomation() ->
	optimize_data_send(
		do(
			qlc:sort(query_room_infomation(), {order, ascending})
		)
	).
	
query_room_infomation() ->
	qlc:q([{
					X#room.room_id, 
					X#room.room_name, 
					X#room.password, 
					X#room.player_in_room,
					X#room.play_time, 
					X#room.room_status, 
					X#room.allow_observer, 
					X#room.sub_time, 
					X#room.rank} || X <- mnesia:table(room), X#room.rank =/= 2, X#room.rank =/= 3, X#room.is_ready =:= 1]).
		
get_all_room() ->
	do(qlc:q([{X#room.room_pid, X#room.room_id} || X <- mnesia:table(room), X#room.rank =/= 2])).
	
get_rank_room() ->
	do(qlc:q([X#room.room_id || X <- mnesia:table(room), X#room.rank =:= 2])).

%get_room_infomation (RoomID) ->
%	Query = qlc:q([{X#room.room_pid, X#room.rank, X#room.room_name, X#room.password, X#room.max_player_in_room, X#room.player_in_room,
%		X#room.play_time, X#room.room_status, X#room.allow_observer} || X <- mnesia:table(room), X#room.room_id =:= RoomID]),
get_room_infomation (RoomID) ->
	Query = 
		qlc:q([{
						X#room.room_pid, 
						X#room.rank, 
						X#room.mode, 
						X#room.room_name, 
						X#room.password, 
						X#room.max_player_in_room, 
						X#room.player_in_room,
						X#room.play_time, 
						X#room.room_status, 
						X#room.allow_observer, 
						X#room.sub_time, 
						X#room.is_ready} || X <- mnesia:table(room), X#room.room_id =:= RoomID]),
	case do(Query) of
		[Result] -> {ok, Result};
		[] -> {error}
	end.

get_room_infomation (RoomID, Field) ->
	case get_room_infomation (RoomID) of
		{ok, Result} -> get_info_field (Result, Field);
		{error} -> error
	end.

get_info_field (Result, Field) ->
	{RoomPid, Rank, Mode, RoomName, Password, MaxPIR, PlayerIR, PlayTime, RoomStatus, Observer, SubTime, IsReady} = Result,
	case Field of
		room_pid -> {ok, RoomPid};
		rank -> {ok, Rank};
		mode -> {ok, Mode};
		room_name -> {ok, RoomName};
		password -> {ok, Password};
		max_player_in_room -> {ok, MaxPIR};
		player_in_room -> {ok, PlayerIR};
		play_time -> {ok, PlayTime};
		room_status -> {ok, RoomStatus};
		allow_observer -> {ok, Observer};
		sub_time -> {ok, SubTime};
		is_ready -> {ok, IsReady};
		_ -> io:format("Get room field ~p out of range~n", [Field])		
	end.

get_room_join_infomation (RoomID) ->
	Query = qlc:q([{
										X#room.room_pid,
										X#room.room_name, 
										X#room.player_in_room,
										X#room.play_time, 
										X#room.room_status, 
										X#room.allow_observer,
										X#room.password, 
										X#room.sub_time}|| X <- mnesia:table(room), X#room.room_id =:= RoomID]),
	case do(Query) of
		[Result] -> {ok, Result};
		[] -> {error}
	end.
	
get_room_id(ArenaPid) ->
	 do(qlc:q([X#room.room_id|| X <- mnesia:table(room), X#room.room_pid =:= ArenaPid])).

% room_id, rank, room_name, password, max_player_in_room, player_in_room, play_time, room_status, allow_observer
set_room_infomation (RoomID, {RoomPid, Rank, Mode, RoomName, Password, MaxPIR, PlayerIR, PlayTime, RoomStatus, Observer, SubTime, IsReady}) ->
	Row = 
		#room{
						room_pid = RoomPid, 	
						room_id = RoomID, 
						rank = Rank, 
						mode = Mode, 
						room_name = 
						RoomName, 
						password = Password, 
						max_player_in_room = MaxPIR,
						player_in_room = PlayerIR, 
						play_time = PlayTime, 
						room_status = RoomStatus, 
						allow_observer = Observer, 
						sub_time = SubTime, 
						is_ready = IsReady},
	F = fun() ->
			mnesia:write(Row)
		end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			{ok, RoomID};
		{aborted, Reason} ->
			{error, Reason}
	end.

set_room_infomation(RoomID, Field, Data) ->
	case get_room_infomation (RoomID) of
		{ok, Result} ->	RoomUpdate = set_room(Result, Field, Data),
					set_room_infomation(RoomID, RoomUpdate);
		{error} -> io:format("Get room infomation error room id ~p~n", [RoomID])
	end.

set_room (Result, Field, Data) ->
	{RoomPid, Rank, Mode, RoomName, Password, MaxPIR, PlayerIR, PlayTime, RoomStatus, Observer, SubTime, IsReady} = Result,
	case Field of
		rank -> {RoomPid, Data, Mode, RoomName, Password, MaxPIR, PlayerIR, PlayTime, RoomStatus, Observer, SubTime, IsReady};
		mode -> {RoomPid, Rank, Data, RoomName, Password, MaxPIR, PlayerIR, PlayTime, RoomStatus, Observer, SubTime, IsReady};
		room_name -> {RoomPid, Rank, Mode, Data, Password, MaxPIR, PlayerIR, PlayTime, RoomStatus, Observer, SubTime, IsReady};
		password -> {RoomPid, Rank, Mode, RoomName, Data, MaxPIR, PlayerIR, PlayTime, RoomStatus, Observer, SubTime, IsReady};
		max_player_in_room -> {RoomPid, Rank, Mode, RoomName, Password, Data, PlayerIR, PlayTime, RoomStatus, Observer, SubTime, IsReady};
		player_in_room -> {RoomPid, Rank, Mode, RoomName, Password, MaxPIR, Data, PlayTime, RoomStatus, Observer, SubTime, IsReady};
		play_time -> {RoomPid, Rank, Mode, RoomName, Password, MaxPIR, PlayerIR, Data, RoomStatus, Observer, SubTime, IsReady};
		room_status -> {RoomPid, Rank, Mode, RoomName, Password, MaxPIR, PlayerIR, PlayTime, Data, Observer, SubTime, IsReady};
		allow_observer -> {RoomPid, Rank, Mode, RoomName, Password, MaxPIR, PlayerIR, PlayTime, RoomStatus, Data, SubTime, IsReady};
		sub_time -> {RoomPid, Rank, Mode, RoomName, Password, MaxPIR, PlayerIR, PlayTime, RoomStatus, Observer, Data, IsReady};
		is_ready -> {RoomPid, Rank, Mode, RoomName, Password, MaxPIR, PlayerIR, PlayTime, RoomStatus, Observer, SubTime, Data};
		_ -> io:format("Set room field ~p out of range~n", [Field])
	end.

% update_room_infomation(SPlayer, RoomID) ->
	% case get_room_infomation(RoomID) of
		% {ok, Result} -> update_infomation(SPlayer, RoomID, Result);
		% {error} -> error
	% end.

% update_infomation(SPlayer, RoomID, Result) ->
	% io:format("-------------------update_infomation--------------------------"),
	% {_RoomPid, _Rank, Mode, RoomName, Password, MaxPIR, PlayerInR, PlayTime, RStatus, Observer, SubTime, _IsReady} = Result,
	% StatusUpdate = 
	% case PlayerInR of
		% 0 -> -1;
		% MaxPIR ->
			% case RStatus of
				% 2 -> RStatus;
				% _ -> 1
			% end;
		% _ -> 0
	% end,
	% if
		% StatusUpdate >= 0 ->
			% RoomNSize = string:len(RoomName),
			% PWDSize = string:len(Password),
			% if 
				% PWDSize > 0 -> HavePWD = 1;
				% true -> HavePWD = 0
			% end,
			% set_room_infomation (RoomID, room_status, StatusUpdate),
% %			io:format("update room info ~p~n", [{RoomID, RoomNSize, RoomName, PlayerIR, PlayTime, StatusUpdate, Observer, HavePWD}]),
			% ChannelPid ! {update_room, RoomID, [RoomNSize, RoomName, PlayerIR, PlayTime, StatusUpdate, Observer, HavePWD, SubTime]};
		% true -> room_remove
	% end.
	

optimize_data_send(L) ->
	[optimize_data (X) || X <- L].
optimize_data (L) ->
	{RoomID, RName, Password, PlayerInRoom, PlayTime, RoomStatus, Observer, SubTime, Mode} = L,
	RNameSize = string:len(RName),
	PassSize = string:len(Password),
	if
		PassSize > 0 ->
			if
				Password =/= 0 ->
					HavePass = 1;
				true ->
					HavePass = 0
			end;
		true ->
			HavePass = 0
	end,
	% ข้อมูลของ RoomID ส่งขนาด 2 byte
	[<<RoomID:16>>, RNameSize, RName, PlayerInRoom, PlayTime, RoomStatus, Observer, HavePass, SubTime, Mode].

create_room(RoomPid, Rank, RoomName, Password, PlayTime, MaxPlayer, Observer, SubTime, IsReady, Mode) ->
	smo_logger:fmsg("call create_room with ~w ~w ~w ~w ~w ~w ~w", [Rank, RoomName, Password, PlayTime, MaxPlayer, Observer, SubTime]),
	RoomID = find_room_id_slot(),
	Row = #room{
										room_id = RoomID, 
										room_pid = RoomPid,
										rank = Rank,
										mode = Mode,
										room_name = RoomName, 
										password = Password, 
										max_player_in_room = MaxPlayer, 
										player_in_room = 1,
										play_time = PlayTime, 
										room_status = 0, 
										allow_observer = Observer,
										sub_time = SubTime,
										is_ready = IsReady},
	F = fun() ->
			mnesia:write(Row)
		end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			{ok, RoomID};
		{aborted, Reason} ->
			{error, Reason}
	end.

join_room(SPlayer, RoomID) ->
	room_can_join(SPlayer, RoomID).

print_character ([]) ->
	io:format ("~n");
print_character ([Chr | Result]) ->
	io:format ("~p~n", [Chr]),
	print_character (Result).
	
add_player_data_to_mnesia(PlayerPid, Result) ->
	[{PlayerID, PNUTF, AvatarSet, AvatarID, DeckUsed}] = Result,
	Row = #user_data{	user_id			= PlayerID,%PLAYERID,
	%%%Row = #user_data{	user_id			= PlayerID,
					player_name			= PNUTF,%NewPNASCII,
					player_name_utf		= PNUTF,%binary_to_list(PNUTF),
					playing_status		= 0,
					avatar				= {list_to_integer(AvatarSet), list_to_integer(AvatarID)},
					deck_used			= DeckUsed,
					process_id			= PlayerPid},
	F = fun() ->
			mnesia:write(Row)
		end,
	
	%%smo_logger:fmsg("Add player_name = ~w", [PLAYERID]),	
	
	mnesia:transaction(F).

set_user_data (PlayerPid, LoginName, UserData) ->
	{PNASCII, PlayerName, Status, Avatar, DeckUsed} = UserData,
	Row = #user_data{	user_id			= LoginName,
					player_name			= PNASCII,
					player_name_utf		= PlayerName,
					playing_status		= Status,
					avatar				= Avatar,
					deck_used			= DeckUsed,
					process_id			= PlayerPid},
	F = fun() ->
			mnesia:write(Row)
		end,
	mnesia:transaction(F).

get_avatar (LoginName) ->
	Query = qlc:q([X#user_data.avatar || X <- mnesia:table(user_data), X#user_data.user_id =:= LoginName]),
	do(Query).

change_player_data(PlayerPid, Field, Data) -> %% Status คือ สถานะผู้เล่นว่าตอนนี้อยู่ที่ได้ เล่นเกมอยู่ หรือว่า อยู่ในล็อบบี้ --
	Query = qlc:q([{	
										X#user_data.user_id,
										X#user_data.player_name, 
										X#user_data.player_name_utf, 
										X#user_data.playing_status, 
										X#user_data.avatar, 
										X#user_data.deck_used} || X <- mnesia:table(user_data), X#user_data.process_id =:= PlayerPid]),
	case do(Query) of
		[Result] ->
			{LoginName, PlayerNameASCII, PlayerName, Status, Avatar, DeckUsed} = Result,
			DataChange = change_data_field({PlayerNameASCII, PlayerName, Status, Avatar, DeckUsed}, Field, Data),
			set_user_data(PlayerPid, LoginName, DataChange);
		[] -> {error}
	end.
	
change_player_data (PlayerPid, LoginName, Field, Data) -> %% Status คือ สถานะผู้เล่นว่าตอนนี้อยู่ที่ได้ เล่นเกมอยู่ หรือว่า อยู่ในล็อบบี้ --
	Query = qlc:q([{	X#user_data.player_name, 
										X#user_data.player_name_utf, 
										X#user_data.playing_status, 
										X#user_data.avatar, 
										X#user_data.deck_used} || X <- mnesia:table(user_data), X#user_data.user_id =:= LoginName]),
	case do(Query) of
		[Result] ->
			DataChange = change_data_field (Result, Field, Data),
			set_user_data (PlayerPid, LoginName, DataChange);
		[] -> {error}
	end.

change_data_field (Result, Field, Data) ->
	{PlayerNameASCII, PlayerName, Status, Avatar, DeckUsed} = Result,
	case Field of
		player_name -> {Data, PlayerName, Status, Avatar, DeckUsed};
		player_name_utf -> {PlayerNameASCII, Data, Status, Avatar, DeckUsed};
		playing_status -> {PlayerNameASCII, PlayerName, Data, Avatar, DeckUsed};
		avatar -> {PlayerNameASCII, PlayerName, Status, Data, DeckUsed};
		deck_used -> {PlayerNameASCII, PlayerName, Status, Avatar, Data};
		_ -> io:format("Change user data error from select field ~p~n", [Field])
	end.

remove_player_from_mnesia (_SPlayer, LoginName) ->
	SQLCommand = "CALL smno.sp_insertLogPlayer ('" ++ LoginName ++ "', 2);",
	lib_database:sql_query(SQLCommand),
	
	Oid = {user_data, LoginName},
	F =	fun() ->
			mnesia:delete(Oid)
		end,
	mnesia:transaction(F).

search_pior_room () ->
	min(do(qlc:q([{X#room.room_id} || X <- mnesia:table(room), X#room.room_status =:= 0]))).

find_room_id_slot () ->
	Result = do(qlc:sort(qlc:q([{X#room.room_id} || X <- mnesia:table(room)]), {order, ascending})),
	find_empty_slot (Result, 1).

find_empty_slot ([H|T], FreeRoomID) ->
	{RoomID} = H,
	if
		RoomID =:= FreeRoomID ->
			find_empty_slot(T, FreeRoomID + 1);
		true ->
			FreeRoomID
	end;
find_empty_slot ([], FreeRoomID) -> FreeRoomID.

room_can_join(SPlayer, RoomID) ->
	L = do(qlc:q([{X#room.player_in_room, X#room.max_player_in_room} || X <- mnesia:table(room), X#room.room_id =:= RoomID])),
	{PlayerInRoom, MaxPlayer} = find_room_data (L),	
	%io:format("-----------------------~p~p~n", [PlayerInRoom, MaxPlayer]),
	if
		PlayerInRoom < MaxPlayer ->
			update_player_in_room(SPlayer, RoomID, add),
			{accepted};
		true ->
			{rejected}
	end.

find_room_data (L) ->
	case L of
		[{PlayerInRoom, MaxPlayer}] ->
			{PlayerInRoom, MaxPlayer};
		[] ->
			io:format("Not allow to join room~n"),			
			{2, 2}
	end.

update_player_in_room(SPlayer, RoomID, Update) ->
	Result =
		do(qlc:q([{
								X#room.room_pid, 
								X#room.rank, 
								X#room.mode, 
								X#room.room_name, 
								X#room.password, 
								X#room.max_player_in_room,
								X#room.player_in_room, 
								X#room.play_time, 
								X#room.room_status, 
								X#room.allow_observer, 
								X#room.sub_time} ||	X <- mnesia:table(room), X#room.room_id =:= RoomID])),
	case get_update_result(Result) of
		{RoomPid, Rank, Mode, RoomName, Password, MaxPlayer, Player, PlayTime, Status, Observer, SubTime} ->
			update_room(Update, RoomPid, RoomID, Rank, Mode, RoomName, Password, MaxPlayer, Player, PlayTime, Status, Observer, SubTime);
			%update_room_infomation(SPlayer, RoomID);
		{error} -> no_room
	end.

update_room(Update, RoomPid, RoomID, Rank, Mode, RoomName, Password, MaxPlayer, Player, PlayTime, Status, Observer, SubTime) ->
	case Update of
		add -> PlayerInRoom = Player + 1;
		remove -> PlayerInRoom = Player - 1
	end,
	Row = 
		#room{
						room_pid = RoomPid, 
						room_id = RoomID, 
						rank = Rank, 
						mode = Mode, 
						room_name = RoomName, 
						password = Password,
						max_player_in_room = MaxPlayer, 
						player_in_room = PlayerInRoom, 
						play_time = PlayTime, 
						room_status = Status, 
						allow_observer = Observer, 
						sub_time = SubTime},
	F = fun() ->
			mnesia:write(Row)
		end,
	mnesia:transaction(F).
	
get_room_rank(RoomPid) ->
	do(qlc:q([X#room.rank || X <- mnesia:table(room), X#room.room_pid =:= RoomPid])).

get_update_result(Result) ->
	case Result of
		[{RoomPid, Rank, Mode, RoomName, Password, MaxPlayer, Player, PlayTime, Status, Observer, SubTime}] -> 
			{RoomPid, Rank, Mode, RoomName, Password, MaxPlayer, Player, PlayTime, Status, Observer, SubTime};
		_ ->
			{error}
	end.

remove_room (RoomID) ->
	Oid = {room, RoomID},
	F =	fun() ->
			mnesia:delete(Oid)
		end,
	mnesia:transaction(F).

get_user_data_pid (PlayerPid, Field) ->
	Query = qlc:q([{	X#user_data.user_id, 
										X#user_data.player_name, 
										X#user_data.player_name_utf, 
										X#user_data.playing_status, 
										X#user_data.avatar, 
										X#user_data.deck_used, 
										X#user_data.process_id} || X <- mnesia:table(user_data), X#user_data.process_id =:= PlayerPid]),
	case do (Query) of
		[] ->	error;
		[Result] ->
			get_data (Result, Field)
	end.
	
remove_user_login(UserName) ->
	case get_user_pid_by_userid(UserName) of
		[] -> smo_logger:fmsg("User ~p is not in the game now~n", [UserName]);
		[_SPlayer] ->
			Oid = {user_data, UserName},
			F =	fun() -> mnesia:delete(Oid) end,
		mnesia:transaction(F)
	end.
		
get_user_data_login (LoginName, Field) ->
	Query = qlc:q([{	X#user_data.user_id, 
										X#user_data.player_name, 
										X#user_data.player_name_utf, 
										X#user_data.playing_status, 
										X#user_data.avatar, 
										X#user_data.deck_used, 
										X#user_data.process_id} || X <- mnesia:table(user_data), X#user_data.user_id =:= LoginName]),
	case do (Query) of
		[] ->	error;
		[Result] ->
			get_data (Result, Field)
	end.

get_data (Result, Field) when is_atom (Field) ->
	[FieldData] = get_data (Result, [Field]),
	FieldData;
get_data (_, []) -> [];
get_data (Result, [Field | Fields]) ->
	{LoginName, PlayerNameASCII, PlayerName, PlayingStatus, Avatar, DeckUsed, ProcessId} = Result,
	FieldData =
	case Field of
		user_id -> LoginName;
		player_name -> PlayerNameASCII;
		player_name_utf -> PlayerName;
		playing_status -> PlayingStatus;
		avatar -> Avatar;
		deck_used -> DeckUsed;
		process_id -> ProcessId
	end,
	[FieldData] ++ get_data (Result, Fields).

get_user_pid_by_name(PlayerName) ->
	do(qlc:q([X#user_data.process_id || X <- mnesia:table(user_data), X#user_data.player_name =:= PlayerName])).
get_user_pid_name_by_name(PlayerName) ->
	do(qlc:q([{X#user_data.process_id, X#user_data.user_id} || X <- mnesia:table(user_data), X#user_data.player_name =:= PlayerName])).
get_user_id_by_name(PlayerName) ->
	do(qlc:q([X#user_data.user_id || X <- mnesia:table(user_data), X#user_data.player_name =:= PlayerName])).
get_user_pid_by_userid(LoginName) ->
	do(qlc:q([X#user_data.process_id || X <- mnesia:table(user_data), X#user_data.user_id =:= LoginName])).
	
check_user_online_status (CheckFrom, CheckData) ->
	case CheckFrom of
		login_name ->
			do(qlc:q([{	X#user_data.user_id} || X <- mnesia:table(user_data), X#user_data.user_id =:= CheckData]));
		player_name ->
			do(qlc:q([{	X#user_data.user_id} || X <- mnesia:table(user_data), X#user_data.player_name =:= CheckData]))
	end.