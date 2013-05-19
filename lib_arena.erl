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
-module(lib_arena).
-compile(export_all).
-auther('nanusorn@playpal.co.th').
-import(lists, [sublist/3, nthtail/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Create arena function 
%%%% required necessary parameter by followed :-
%%%%	SPlayer : player process id.
%%%%	LoginName : player_id.
%%%%	PlayerName : player_name
%%%%	RoomData which contain...
%%%%		Rank, RNameSize, RoomName, Password, HavePwd, PlayTime, NumberOfPlayer, AllowObserver...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_arena(SPlayer, [Mode|Data]) ->
	io:format("create room mode ~p~n", [Mode]),
	io:format("create room ~p~n", [Data]),
	case Mode of
		0 -> 
			[0, 0|RoomData] = Data,
			%io:format("Argument Count is ~p~n", [Arg]),
			create_ordinary_arena(SPlayer, [0] ++ RoomData ++ [Mode]);
		1 -> 
			[0, 1, DeckID|RoomData] = Data,
			io:format("RoomData ~p~n", [RoomData]),
			case lib_lobby_protocol:get_user_data_pid(SPlayer, [user_id, player_name_utf]) of
				[LoginName, PlayerName] -> create_mode_room(SPlayer, LoginName, PlayerName, [1] ++ RoomData ++ [{1, [DeckID]}]);
				_Error -> smo_logger:fmsg("Can not find player name from ~p case ~p ~n", [SPlayer, _Error])
			end
		%_ ->
			% [Arg0, Arg1|AllData] = Data,
			% <<ArgCount:16>> = list_to_binary([Arg0, Arg1]),
			% {ok, OptionArg, RoomData} = get_data(AllData, ArgCount),
	end.

create_ordinary_arena(SPlayer, Data) ->
	io:format("Data ~p~n", [Data]),
	case lib_lobby_protocol:get_user_data_pid(SPlayer, [user_id, player_name_utf]) of
		[LoginName, PlayerName] ->
			case check_valid_deck(LoginName, "1") of
				{deck_valid} ->
					create_mode_room(SPlayer, LoginName, PlayerName, Data);
					%create_room(SPlayer, LoginName, PlayerName, Data);
				{deck_invalid} ->
					gen_server:cast(SPlayer, {send, [16#01, 16#02, 16#00]})
			end;			
		_Error ->	
			smo_logger:fmsg("Can not find player name from ~p case ~p ~n", [SPlayer, _Error])
	end.

check_valid_deck(LoginName, PlayType) ->
	SQLCommand = "SELECT smno.fc_deck_valid('" ++ LoginName ++ "', " ++ PlayType ++ ");",
	%smo_logger:fmsg("SQLCommand ~p~n", [SQLCommand]),
	QueryResult = lib_database:sql_query(SQLCommand),
	%smo_logger:fmsg("~p <===> ~p~n", [SQLCommand, QueryResult]),
	case QueryResult of
		{selected, _, [{1}]} -> 
			{deck_valid};
		{selected, _, Other} -> 
			{deck_invalid};
		{error, Error} -> 
			{deck_invalid}
	end.
	
create_mode_room(SPlayer, LoginName, PlayerName, Data) ->
	case get_data(Data) of
		{ok, RMode, Data2} -> get_mode_roomname_size(SPlayer, LoginName, PlayerName, RMode, Data2);
			%get_mode_room_name(SPlayer, LoginName, PlayerName, 0, RNameSize, Data2);
		{error} ->	
			smo_logger:msg("Can not create room - Rank error")
	end.
	
get_mode_roomname_size(SPlayer, LoginName, PlayerName, RMode, Data) ->
	case get_data(Data) of
		{ok, RNameSize, Data2} -> get_mode_room_name(SPlayer, LoginName, PlayerName, RMode, RNameSize, Data2);
		{error} ->	io:format("Can not create room - Room name size error~n")		
	end.
	
get_mode_room_name(SPlayer, LoginName, PlayerName, RMode, RNameSize, Data) ->
	case get_data (Data, RNameSize) of
		{ok, RoomName, Data2} -> 
%			io:format ("Room name ~p~n", [RoomName]),
			get_mode_pwd_size(SPlayer, LoginName, PlayerName, RMode, RNameSize, RoomName, Data2);
		{error} ->	io:format("Can not create room - Room name error~n")		
	end.

get_mode_pwd_size(SPlayer, LoginName, PlayerName, RMode, RNameSize, RoomName, Data) ->
	case get_data(Data) of
		{ok, PwdSize, Data2} -> get_mode_pwd(SPlayer, LoginName, PlayerName, RMode, RNameSize, RoomName, PwdSize, Data2);
		{error} ->	io:format("Can not create room - Pwd size error~n")		
	end.
	
get_mode_pwd(SPlayer, LoginName, PlayerName, RMode, RNameSize, RoomName, PwdSize, Data) ->
	case get_data(Data, PwdSize) of
		{ok, "", Data2} -> get_mode_play_time(SPlayer, LoginName, PlayerName, RMode, RNameSize, RoomName, "", 0, Data2);
		{ok, Pwd, Data2} -> get_mode_play_time(SPlayer, LoginName, PlayerName, RMode, RNameSize, RoomName, Pwd, 1, Data2);
		{error} ->	io:format("Can not create room - Pwd error~n")		
	end.

get_mode_play_time(SPlayer, LoginName, PlayerName, RMode, RNameSize, RoomName, Pwd, HavePwd, Data) ->
	case get_data(Data) of
		{ok, PlayTime, Data2} -> get_mode_sub_time(SPlayer, LoginName, PlayerName, RMode, RNameSize, RoomName, Pwd, HavePwd, PlayTime, Data2);
		{error} ->	io:format("Can not create room - Play Time error~n")		
	end.
	
get_mode_sub_time(SPlayer, LoginName, PlayerName, RMode, RNameSize, RoomName, Pwd, HavePwd, PlayTime, Data) ->
	case get_data(Data) of
		{ok, ST, Precon} -> io:format("mode ~p~n", [Precon]), 
			create(SPlayer, LoginName, PlayerName, RMode, RNameSize, RoomName, Pwd, HavePwd, PlayTime, 2, 0, ST, Precon);
		{error} -> io:format("Can not create room - Sub Time error ~n")		
	end.
%------------------------------------------------------------------
create_room(SPlayer, LoginName, PlayerName, Data) ->
	case get_data(Data) of
		{ok, Rank, Data2} -> 
			get_roomname_size(SPlayer, LoginName, PlayerName, Rank, Data2);
		{error} ->	
			smo_logger:msg("Can not create room - Rank error")
	end.
	
get_roomname_size(SPlayer, LoginName, PlayerName, RMode, Data) ->
	case get_data(Data) of
		{ok, RNameSize, Data2} -> get_room_name(SPlayer, LoginName, PlayerName, RMode, RNameSize, Data2);
		{error} ->	io:format("Can not create room - Room name size error~n")		
	end.
	
get_room_name (SPlayer, LoginName, PlayerName, Rank, RNameSize, Data) ->
	case get_data (Data, RNameSize) of
		{ok, RoomName, Data2} -> 
%			io:format ("Room name ~p~n", [RoomName]),
			get_pwd_size (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Data2);
		{error} ->	io:format("Can not create room - Room name error~n")		
	end.
	
get_pwd_size (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Data) ->
	case get_data(Data) of
		{ok, PwdSize, Data2} -> get_pwd (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, PwdSize, Data2);
		{error} ->	io:format("Can not create room - Pwd size error~n")		
	end.

get_pwd (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, PwdSize, Data) ->
	case get_data (Data, PwdSize) of
		{ok, "", Data2} -> get_play_time (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, "", 0, Data2);
		{ok, Pwd, Data2} -> get_play_time (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Pwd, 1, Data2);
		{error} ->	io:format("Can not create room - Pwd error~n")		
	end.

get_play_time (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Pwd, HavePwd, Data) ->
	case get_data(Data) of
		{ok, PlayTime, Data2} -> get_number_of_player (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Pwd, HavePwd, PlayTime, Data2);
		{error} ->	io:format("Can not create room - Play Time error~n")		
	end.

get_number_of_player (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Pwd, HavePwd, PlayTime, Data) ->
	case get_data(Data) of
		{ok, NOP, Data2} -> get_observer_allow (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Pwd, HavePwd, PlayTime, NOP, Data2);
		{error} ->	io:format("Can not create room - Number of Player error~n")		
	end.

get_observer_allow (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Pwd, HavePwd, PlayTime, NOP, Data) ->
	case get_data(Data) of
%		{ok, AO, _} -> create (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Pwd, HavePwd, PlayTime, NOP, AO);
		{ok, AO, Data2} -> get_sub_time (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Pwd, HavePwd, PlayTime, NOP, AO, Data2);
		{error} ->	io:format("Can not create room - Allow Observer error~n")		
	end.
	
get_sub_time (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Pwd, HavePwd, PlayTime, NOP, AO, Data) ->
	case get_data(Data) of
		{ok, ST, Precon} -> io:format("mode ~p~n", [Precon]), 
			create (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Pwd, HavePwd, PlayTime, NOP, AO, ST, Precon);
		{error} -> io:format("Can not create room - Sub Time error ~n")		
	end.

create (SPlayer, LoginName, PlayerName, Rank, RNameSize, RoomName, Password, HavePwd, PlayTime, NumberOfPlayer, AllowObserver, SubTime, Precon) ->
	%%smo_logger:fmsg("R - ~w ~w ~w ~w ~w ~w ~w ~w ~w ~w",[SPlayer, LoginName, Rank, RNameSize, RoomName, Password, HavePwd, PlayTime, NumberOfPlayer, AllowObserver]),
	%%smo_logger:msg("McDuck Said : We're here. =0="),
	%%%room_controller ! {create_room, SPlayer, LoginName, PlayerName, [Rank, RNameSize, RoomName, Password, HavePwd, PlayTime, NumberOfPlayer, AllowObserver]}.
	Avatar = lib_lobby_protocol:get_user_data_pid(SPlayer, avatar),
	smo_arena:start([
											SPlayer, 
											LoginName, 
											PlayerName,
											Avatar,
											0,
											Rank, 
											RNameSize, 
											RoomName, 
											Password, 
											HavePwd, 
											PlayTime, 
											NumberOfPlayer, 
											AllowObserver, 
											SubTime, 
											whereis(smo_arena_guardian), 
											whereis(smo_player_guardian), 
											whereis(smo_dispatcher), 
											undefined, 
											Precon]).

	
get_data (L) ->
	case L of
		[] -> {error};
		[H|T] -> {ok, H, T}
	end.

get_data (L, Size) ->
	ListSize = lists:flatlength (L),
	if	ListSize < Size -> {error};
		Size =:= 0 -> {ok, "", L};
		true ->	SubData = sublist(L, 1, Size),
				T = nthtail(Size, L),
				{ok, SubData, T}
	end.
	
leave_room(SPlayer) ->
	%% need remove specific player (SPlayer) from room instance
	smo_logger:fmsg("player leave room ~p~n", [SPlayer]),
	gen_server:cast(SPlayer, {send, [16#83, 16#ff, 1]}).

join_arena(SPlayer, [RoomID1, RoomID2, PassCnt1, PassCnt2| PassOption]) ->
	<<PassArg:16>> = list_to_binary([PassCnt1, PassCnt2]),
	{ok, Password, Option} = get_data(PassOption, PassArg),
	%io:format("Password ~p~n", [Password]),
	smo_logger:fmsg("lib_arena:join_arena receive data ~p~n", [[RoomID1, RoomID2, PassCnt1, PassCnt2| PassOption]]),
	<<RoomID:16>> = list_to_binary([RoomID1, RoomID2]),
	case lib_lobby_protocol:get_user_data_pid(SPlayer, [user_id]) of
		[LoginName] ->
			case lib_lobby_protocol:join_room(SPlayer, RoomID) of
				{accepted} ->
					case lib_lobby_protocol:get_room_infomation(RoomID, room_pid) of
						{error} ->
							smo_logger:msg("can not join room"),
							gen_server:cast(SPlayer, {send, [16#01, 16#02, 16#02]}),
							gen_server:cast(SPlayer, {send, [16#81, 0, <<RoomID:16>>, 1]});
							%smo_player_guardian:update_remove_arena(RoomID);
						{ok, ArenaPid} ->
							case check_join(ArenaPid) of
								can_join ->
									case Option of
										[1, Precon] ->
											{ok, ActualRoomPassword} = lib_lobby_protocol:get_room_infomation(RoomID, password),
											case ActualRoomPassword=:=Password of
												true ->
													%gen_server:cast(ArenaPid, {join_arena, SPlayer, LoginName, RoomID, 1, 2});
													%Precon = function_utility:random_select(1, [1,2, 3,4]),
													Avatar = lib_lobby_protocol:get_user_data_pid(SPlayer, avatar),
													gen_server:cast(ArenaPid, {join_arena, SPlayer, LoginName, Avatar, RoomID, 1, 2, undefined, [{1, [Precon]}]});
													% case process_info(ArenaPid, status) of
														% undefined -> gen_server:cast(SPlayer, {send, [16#83, 16#ff, 0]});
														% _ -> gen_server:call(ArenaPid, {join_arena, SPlayer, LoginName, RoomID, 1, 2})
													% end;
												false ->
													gen_server:cast(SPlayer, {send, [16#01, 16#02, 16#03]})
											end;
										_ -> 
											case check_valid_deck (LoginName, "1") of
												{deck_valid} ->
													{ok, ActualRoomPassword} = lib_lobby_protocol:get_room_infomation(RoomID, password),
													case ActualRoomPassword=:=Password of
														true ->
															%gen_server:cast(ArenaPid, {join_arena, SPlayer, LoginName, RoomID, 1, 2});
															Avatar = lib_lobby_protocol:get_user_data_pid(SPlayer, avatar),
															gen_server:cast(ArenaPid, {join_arena, SPlayer, LoginName, Avatar, RoomID, 1, 2, undefined, 0});
															% case process_info(ArenaPid, status) of
																% undefined -> gen_server:cast(SPlayer, {send, [16#83, 16#ff, 0]});
																% _ -> gen_server:call(ArenaPid, {join_arena, SPlayer, LoginName, RoomID, 1, 2})
															% end;
														false ->
															gen_server:cast(SPlayer, {send, [16#01, 16#02, 16#03]})
													end;
												{deck_invalid} ->
													smo_logger:fmsg("~p SPlayer Deck invalid ~n", [SPlayer]),
													gen_server:cast(SPlayer, {send, [16#01, 16#02, 16#00]})
											end
									end;
								cannot_join -> smo_logger:msg("Room Cannot Join case Joined")%gen_server:cast(SPlayer, {send, [16#01, 16#02, 16#02]})
							end
					end;
				_Reject -> 
					smo_logger:fmsg("Room Cannot Join case Joined case ~p~n", [_Reject]),
					gen_server:cast(SPlayer, {send, [16#01, 16#02, 16#02]}),
					gen_server:cast(SPlayer, {send, [16#81, 0, <<RoomID:16>>, 1]})
			end;
		_ ->	
			smo_logger:fmsg("Can not find player name from ~w", [SPlayer])
	end.

check_join(ArenaPid) ->
	GetResult = gen_server:call(ArenaPid, {room_can_join}, infinity),
	smo_logger:fmsg("------check room can join result of ~p~n", [GetResult]),
	case GetResult of
		ok -> can_join;
		_ -> cannot_join
	end.
