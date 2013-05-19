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
-module(lib_tournament).
-compile(export_all).
-export ([compatitor_name_status/3]).

reject_action(SPlayer, Reason) ->
	RejectReason =
	case Reason of
		no_valid_tournament -> 0;
		inadequate_fee_requirement -> 1;
		inadequate_rank_requirement -> 10;
		no_room -> 11;
		too_late -> 12;
		no_compatitor_info -> 2;
		no_user_scoreline -> 3;
		cannot_found_next_match_info -> 4;
		no_tournament_information -> 5;
		you_late_game_is_over -> 6;
		cannot_found_game_info -> 7;
		cannot_found_match_schedule -> 8;
		too_early -> 9;
		your_compatitor_late -> 10;
		you_are_not_yet_ready_game_is_over -> 11;
		_ -> 99
	end,
	gen_server:cast(SPlayer, {send, [16#ff, 16#aa, RejectReason]}).
	
reject_register(SPlayer, Reject) ->
	SendReject = 
	case Reject of
		inadequate_fee_requirement -> 2;
		inadequate_rank_requirement -> 3;
		register_close -> 4;
		no_room -> 5;
		ban_user -> 6;
		overlap_period -> 7;
		cancel_success -> 8;
		cannot_cancel_register -> 9;
		unsuccess_register -> 10
	end,
	gen_server:cast(SPlayer, {send, [16#81, 16#a0, SendReject]}).
%------------------------------------------------------------------
% ส่วนการตรวจสอบ Tournament ทีเปิดรับสมัครและ User นี้ยังไม่ได้สมัคร
request_valid_tournament(SPlayer) ->
	LoginName = lib_lobby_protocol:get_user_data_pid(SPlayer, [user_id]),
	SQLCommand = "CALL smno.sp_league_open(" ++ integer_to_list(0) ++", '"++ LoginName ++"');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
	case QueryResult of
		[{selected, _, [{0}]}, _] -> gen_server:cast(SPlayer, {send, [16#81, 16#a6, 0]});
		[{selected, _, ValidTRM}, _] ->	transform_valid(SPlayer, ValidTRM);
		_ -> reject_action(SPlayer, no_valid_tournament)
	end.
%------------------------------------------------------------------
% ส่วนการตรวจสอบ Tournament ทีเปิดรับสมัครและ User นี้สมัครไปแล้ว
request_registered_tournament(SPlayer) ->
	LoginName = lib_lobby_protocol:get_user_data_pid(SPlayer, [user_id]),
	SQLCommand = "CALL smno.sp_league_open(" ++ integer_to_list(1) ++", '"++ LoginName ++"');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
	case QueryResult of
		[{selected, _, []}, _] -> gen_server:cast(SPlayer, {send, [16#81, 16#a7, 0]});
		[{selected, _, ValidTRM}, _] ->	transform_registed(SPlayer, ValidTRM);
		_ -> reject_action(SPlayer, no_valid_tournament)
	end.
%------------------------------------------------------------------
transform_to_binary([]) -> [];
transform_to_binary([{STRMID}|ValidTRM]) ->
	TRMID = list_to_integer(STRMID),
	[<<TRMID:16>>] ++ transform_to_binary(ValidTRM);
transform_to_binary([{STRMID, Status}|ValidTRM]) ->
	TRMID = list_to_integer(STRMID),
	[<<TRMID:16>>, Status] ++ transform_to_binary(ValidTRM).
	
% ส่วนการ Register เพื่อเข้่าร่วม Tournament
transform_valid(SPlayer, ValidTRM) ->
	DataSize = length(ValidTRM),
	TRM = transform_to_binary(ValidTRM),
	io:format("TRM ~p~n", [TRM]),
	gen_server:cast(SPlayer, {send, [16#81, 16#a6, DataSize] ++ TRM}).
	
transform_registed(SPlayer, ValidTRM) ->
	DataSize = length(ValidTRM),
	TRM = transform_to_binary(ValidTRM),
	io:format("TRM ~p~n", [TRM]),
	gen_server:cast(SPlayer, {send, [16#81, 16#a7, DataSize] ++ TRM}).
%------------------------------------------------------------------
cancel_tournament_register(SPlayer, TnmID) ->
	io:format("TournamentID is ~p~n", [TnmID]),
	<<TID:16>> = list_to_binary(TnmID),
	LoginName = lib_lobby_protocol:get_user_data_pid(SPlayer, [user_id]),
	SQLCommand = "CALL smno.sp_league_register (" ++ integer_to_list(0) ++ ","++integer_to_list(TID)++ ",'" ++ LoginName ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
	case QueryResult of
		[{selected, _, [{1}]}, _] -> gen_server:cast(SPlayer, {send, [16#81, 16#a8, 1]});
		_ -> gen_server:cast(SPlayer, {send, [16#81, 16#a8, 0]})
	end.

request_register(SPlayer, TnmID) ->
	io:format("TournamentID is ~p~n", [TnmID]),
	<<TID:16>> = list_to_binary(TnmID),
	LoginName = lib_lobby_protocol:get_user_data_pid(SPlayer, [user_id]),
	smo_logger:fmsg("~p ~n", [LoginName]),
	SQLCommand = "CALL smno.sp_league_chk_requirement ("++integer_to_list(TID)++ ",'" ++ LoginName ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
	case QueryResult of
		[{selected, _, [{1}]}, _] -> register_success(SPlayer, TID, LoginName);
		[{selected, _, [{2}]}, _] -> reject_register(SPlayer, inadequate_fee_requirement);
		[{selected, _, [{3}]}, _] -> reject_register(SPlayer, inadequate_rank_requirement);
		[{selected, _, [{4}]}, _] -> reject_register(SPlayer, register_close);
		[{selected, _, [{5}]}, _] -> reject_register(SPlayer, no_room);
		[{selected, _, [{6}]}, _] -> reject_register(SPlayer, ban_user);
		[{selected, _, [{7}]}, _] -> reject_register(SPlayer, overlap_period);
		_ -> reject_action(SPlayer, other)
	end.
% -----------------------------------------------------------------
request_match_player_information(SPlayer, TnmID) ->
	io:format("TournamentID is ~p~n", [TnmID]),
	[A, B|_] = TnmID,
	<<TID:16>> = list_to_binary([A, B]),
	LoginName = lib_lobby_protocol:get_user_data_pid(SPlayer, [user_id]),
	case compatitor_name_status(SPlayer, LoginName, TID)  of
		[{_, ComLogin, _, _, StartTime, _}] -> % return เป็น ชุดข้อมูล ได้แก่ .... UserName ของคู่แข่ง, วันเวลา, ระยะเวลาของ match
			 ONameData =  request_player_data(ComLogin),
			 SStat = request_player_statistic(TID, LoginName),
			 OStat = request_player_statistic(TID, ComLogin),
			 manage_statistic(SPlayer, TnmID, SStat, ONameData, OStat, StartTime);
		_ -> gen_server:cast(SPlayer, {send, [16#81, 16#a1, 0]})
	end.
		 
	
request_player_data(LoginName) ->
	SQLCommand = "CALL smno.sp_get_data_player('" ++ LoginName ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p on ~p~n", [SQLCommand, QueryResult, now()]),
	case QueryResult of
		[{selected, _, []}, _] -> {0, [], 0};
		[{selected, _, Result}, _] ->
			[{_PlayerID, PNUTF, Avatar1, Avatar2, _DeckUsed}] = Result,
			Avatar_1 = list_to_integer(Avatar1),
			Avatar_2 = list_to_integer(Avatar2),
			{length(PNUTF), PNUTF, Avatar_1, Avatar_2};
		_ -> {0, [], 0}
	end.
	
request_player_statistic(TID, LoginName) ->
	SQLCommand = "CALL smno.sp_league_get_data_player (" ++integer_to_list(TID)++ ", '" ++ LoginName ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	%total_point,  num_win, num_lose, num_draw, num_leave
	smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
	case QueryResult of
		[{selected,_, OtherResult},_] -> OtherResult;
		_ -> []
	end.
	
manage_statistic(SPlayer, TnmID, SelfResult, {ONameSize, OName, OAvatar1, OAvatar2}, OtherResult, StartTime) ->
	case {SelfResult, OtherResult} of
		{[], []} -> "";
		{[], _} -> "";
		{_, []} -> "";
		_ ->
			[{_STotalP, SWin, Sloss, SDraw, SLeave, SMatchRem}] = SelfResult,
			[{_OTotalP, OWin, Oloss, ODraw, OLeave, OMatchRem}] = OtherResult,
			A = [<<SWin:8>>, <<Sloss:8>>, <<SDraw:8>>, <<SLeave:8>>, <<SMatchRem:8>>], 
			B = [<<OWin:8>>, <<Oloss:8>>, <<ODraw:8>>, <<OLeave:8>>, <<OMatchRem:8>>],
			TimeSize = length(StartTime),
		%	gen_server:cast(SPlayer, {send, [16#81, 16#a1, 1] ++ TnmID ++  A ++ [ONameSize] ++ OName ++ [0] ++B ++ [TimeSize]++StartTime})
			gen_server:cast(SPlayer, {send, [16#81, 16#a1, 1] ++ [TnmID, ONameSize, OName, OAvatar1, OAvatar2, TimeSize, StartTime]})
	end.
	
register_success(SPlayer, TID, LoginName) ->
	SQLCommand = "CALL smno.sp_league_register (" ++ integer_to_list(1) ++ ", "++integer_to_list(TID)++ ",'" ++ LoginName ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
	case QueryResult of
		[{selected, _, [{1}]}, _] -> gen_server:cast(SPlayer, {send, [16#81, 16#a0, 1]});
		_ -> reject_register(SPlayer, unsuccess_register)
	end.
%------------------------------------------------------------------	
compatitor_name_status(_SPlayer, LoginName, TID) ->
	%%SQLCommand = "CALL smno.sp_league_get_challenger (" ++integer_to_list(TID)++ ", '" ++ LoginName ++ "');",
	SQLCommand = "CALL smno.sp_league_get_challenger (" ++integer_to_list(TID)++ ", '" ++ LoginName ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
	case QueryResult of
		[{selected, _, Match}, _] -> Match; % return เป็น ชุดข้อมูล ได้แก่ .... UserName ของคู่แข่ง, วันเวลา, ระยะเวลาของ match
		_ -> []
	end.
%------------------------------------------------------------------
% ส่วนการร้องขอ ข้อมูลการแข่งขัน ของ User นั้น
request_player_scoreline(SPlayer) ->
	LoginName = lib_lobby_protocol:get_user_data_pid(SPlayer, [user_id]),
	SQLCommand = "SELECT smno.request_player_scoreline ('" ++ LoginName ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
	case QueryResult of
		{selected, _, PlayerInfo} -> transform_scoreline_info(SPlayer, PlayerInfo);
		_ -> reject_action(SPlayer, no_user_scoreline)
	end.

transform_scoreline_info(SPlayer, PlayerInfo) ->
	gen_server:cast(SPlayer, {send, [16#ff, 16#aa, PlayerInfo]}).
%------------------------------------------------------------------
% ส่วนการร้องขอ ข้อมูลของ Torunament
request_tournament_info(SPlayer) ->
	LoginName = lib_lobby_protocol:get_user_data_pid(SPlayer, [user_id]),
	SQLCommand = "SELECT smno.request_tournament_info ('" ++ LoginName ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
	case QueryResult of
		{selected, _, ComInfo} -> transform_tournament_info(SPlayer, ComInfo);
		_ -> reject_action(SPlayer, no_tournament_information)
	end.

transform_tournament_info(SPlayer, ComInfo) ->
	gen_server:cast(SPlayer, {send, [16#ff, 16#aa, ComInfo]}).
%------------------------------------------------------------------
% การยกเลิกสถานะพร้อมแข่งจะทำได้หลังจากที่ระบบได้เปลี่ยนสถานะเป็นพร้อมแข่งแล้ว
cancel_player_ready(SPlayer, _TnmID) ->
	lib_lobby_protocol:change_player_data(SPlayer, playing_status, 0),
	case get(counter_pid) of
		undefined -> do_nothing;
		CountPid -> 
			unlink(CountPid),
			exit(CountPid, unused) 
	end,
	
%	put(cancle_clicked, 1),
%	OverTime = get(overtime),

%	if OverTime =:= 1 ->
%		put(ready_clicked, 0);
%	true ->
%		put(waiting, 0)
%	end,

	gen_server:cast(SPlayer, {send, [16#81, 16#a3]}), % ส่งไปบอกว่าระบบได้ยกเลิกสถานะความะพร้อมของคุณแล้ว
	exit.
	
% ส่วนการจับคู่
response_player_send_ready(SPlayer, Reason) ->
	ReasonSend =
	case Reason of
		compatitor_is_ready -> [1, 1];
		compatitor_is_not_ready -> [1, 2];
		connot_find_match_schedule -> [1, 3];
		waiting_for_next_match -> [1, 4];
		{too_early, TimeEarlier} -> [5, 5] ++ TimeEarlier;
		{too_late, TimeLate} -> [5, 6] ++ TimeLate;
		compatitor_late_you_win -> [1, 7];
		you_are_not_yet_ready_for_match -> [1, 8]
	end,
	gen_server:cast(SPlayer, {send, [16#81, 16#a2] ++ ReasonSend}).
	
player_ready(SPlayer, TnmID) ->
%	Room_Cancle = get(cancle_clicked),
%	if Room_Cancle =:= 1 ->
%		put(ready_to_rejoin, 1);
%	true ->
%		put(ready_to_rejoin, 0)
%	end,
%	ReJoin = get(ready_to_rejoin),
		
%	OverTime = get(overtime),
%	if OverTime =:= 1 ->
%		put(ready_clicked, 1);
%	true ->
%		put(ready_clicked, 2)
%	end,
	
	
%	ReadyClick = get(ready_clicked),
	
	io:format("Tournament ID is ~p~n", [TnmID]),
	<<TID:16>> = list_to_binary(TnmID), 
	% 1. หาว่าคู่แข่งเป็นใคร ใน ComInfo (ต้องมี user_id ของดู่แข่งด้วย)
	SLogin = lib_lobby_protocol:get_user_data_pid(SPlayer, [user_id]),
		% 2. เช็คว่าเกมส์ นี้ต้่องแข่งกับใครและ เกมส์จบไปหรือยัง 
		case compatitor_name_status(SPlayer, SLogin, TID) of
			% เกมส์ยังไม่จบ ภายใต้ condition ว่า Db จะ Return 1 ก็ต่อเมื่อ เกมส์ยังไม่ได้รับรายงานว่าจบไปแล้ว
			[{MID, ComLogin, _, _, _, 1}] ->
				%%เติม MID ด้วยหน้า ComLogin ถ้าจะใช้ชุด2
				put(match_ID, MID),
				
%				if ReadyClick =:= 1 ->
					PStatusPid = lib_lobby_protocol:get_user_data_login(ComLogin, [playing_status, process_id]),
					io:format("PlayeStatus and ProcessID are ~p~n", [PStatusPid]),
					case PStatusPid of
					% 3.1 ถ้าคู่แข่งพร้อมแล้ว ให้สามารถแข่งได้เลย
						[4, ComPid] -> 
							check_schedule_time_remain(SPlayer, SLogin, ComPid, ComLogin, TID, ready);
						% 3.2 ถ้าคู่แข่งยังไม่พร้อม (กรณี ที่คู่แข่งเข้ามาในระบบแล้ว)
						[_OtherNo, ComPid] -> 
							%response_player_send_ready(ComPid, compatitor_is_ready),
							%response_player_send_ready(SPlayer, compatitor_is_not_ready),
							check_schedule_time_remain(SPlayer, SLogin, ComPid, ComLogin, TID, not_ready); % ส่งไปเตือนว่าคู่แข่งได้เข้ามาในระบบแล้ว
						% 3.2 ถ้าคู่แข่งยังไม่พร้อม (กรณี ที่คู่แข่งยังไม่เข้ามาในระบบ)
						_Other ->
							%response_player_send_ready(SPlayer, compatitor_is_not_ready),
							check_schedule_time_remain(SPlayer, SLogin, [], ComLogin, TID, other_case)
					end;
%					if OverTime =/= 1 ->
%							put(waiting, 1);
%					true ->
%						""
%					end;
					
%				true ->
%					if ReadyClick =:= 0 ->
%						"";
%					true ->
%						if Room_Cancle =/= 1; ReJoin =/= 0 ->
%							put(ready_to_rejoin, 0),
%							% 3. และดูว่าคู่แข่งอยู่ในสถานะพร้อมแข่งหรือยัง
%							PStatusPid = lib_lobby_protocol:get_user_data_login(ComLogin, [playing_status, process_id]),
%							io:format("PlayeStatus and ProcessID are ~p~n", [PStatusPid]),
%							case PStatusPid of
%								% 3.1 ถ้าคู่แข่งพร้อมแล้ว ให้สามารถแข่งได้เลย
%								[4, ComPid] -> 
%									check_schedule_time_remain(SPlayer, SLogin, ComPid, ComLogin, TID, ready);
%								% 3.2 ถ้าคู่แข่งยังไม่พร้อม (กรณี ที่คู่แข่งเข้ามาในระบบแล้ว)
%								[_OtherNo, ComPid] -> 
%									%response_player_send_ready(ComPid, compatitor_is_ready),
%									%response_player_send_ready(SPlayer, compatitor_is_not_ready),
%									check_schedule_time_remain(SPlayer, SLogin, ComPid, ComLogin, TID, not_ready); % ส่งไปเตือนว่าคู่แข่งได้เข้ามาในระบบแล้ว
%								% 3.2 ถ้าคู่แข่งยังไม่พร้อม (กรณี ที่คู่แข่งยังไม่เข้ามาในระบบ)
%								_Other ->
%									%response_player_send_ready(SPlayer, compatitor_is_not_ready),
%									check_schedule_time_remain(SPlayer, SLogin, [], ComLogin, TID, other_case)
%							end,
%							if OverTime =/= 1 ->
%								put(waiting, 1);
%							true ->
%								""
%							end;
%						true ->  
%							put(ready_to_rejoin,1)
%						end
%					end
%				end;

			error -> response_player_send_ready(SPlayer, connot_find_match_schedule);
				% เกมส์จบแล้ว ผู้ที่เข้ามาตอนนั้นหมายถึง มาช้ากว่าเวลาที่กำหนด แจ้งผู้เล่นว่ายังไม่ถึงเวลาแข่งขันเกมส์ใหม่ หรือเกมส์ก่อนหน้านี้จบไปแล้ว
				% เกมส์ยังจบ ภายใต้ condition ว่า Db จะ Return 0 ก็ต่อเมื่อ เกมส์จบไปแล้วและการ Query นี้กระทำหลังจากเกมส์จบไปไม่เกิน 30 นาที
			_Other ->
				% กรณี Other ได้แก่ เกมส์จบแล้ว ...เนื่องจากผู้เล่นมารายงานตัวช้า ถูกแบน หรือ เกมส์จบไปแล้วจริงๆ ต้องเพิ่มและบอก เหตูผลกับ Client ด้วย
				response_player_send_ready(SPlayer, waiting_for_next_match) % You Lose
		end.
%		put(cancle_clicked, 0).
		

			% 3.2.1 เช็คเวลาว่าถึงเวลาที่จะแข่งตามกำหนดการหรือยัง
				% 3.2.1 ถ้าถึงเวลาแล้ว เช็คต่อว่า เลยเวลาหรือยัง 
					% 3.2.1.1 ถ้าเลยแล้วให้แจ้งว่า ผู้เล่นนี้ชนะ และเก็บข้อมูลลง Db
					% 3.2.1.2 ถ้ายังไม่เลยให้แจ้งว่าคู่แข่งยังไม่พร้อม และนับเวลา ถ้าเวลาหมดแล้วคู่แข่งยังไม่พร้อมให้ ผู้เล่นนี้ชนะ แต่ถ้าครบเวลาแล้ว ให้ตรวจดูว่าผู้เล่นเปลี่ยนสถานะเป็น play หรือยัง
						% 3.2.1.2.1 ถ้าเปลี่ยนแล้ว ก็ไม่ต้องทำอะไร
						% 3.2.1.2.2 ถ้ายังไม่เปลี่ยนแสดงว่ายังไม่มีการแข่งขัน และให้ผู้เล่นนี้ชนะ
				% 3.2.2 ถ้ายังไมถึงเวลาให้ แจ้งว่ายังไม่ถึงเวลาแข่ง และนับเวลา ถ้าเวลาหมดแล้วคู่แข่งยังไม่พร้อมให้ ผู้เล่นนี้ชนะ แต่ถ้าครบเวลาแล้ว ให้ตรวจดูว่าผู้เล่นเปลี่ยนสถานะเป็น play หรือยัง
					% 3.2.2.1 ถ้าเปลี่ยนแล้ว ก็ไม่ต้องทำอะไร
					% 3.2.2.2 ถ้ายังไม่เปลี่ยนแสดงว่ายังไม่มีการแข่งขัน และให้ผู้เล่นนี้ชนะ
check_schedule_time_remain(SPlayer, LoginName, ComPid, ComName, TID, Case) -> 
	Match_ID = get(match_ID),
%	Waiting = get(waiting),
	SQLCommand = "CALL smno.sp_league_chk_status_game (" ++ integer_to_list(Match_ID) ++ ");",
	%%SQLCommand = "CALL smno.sp_league_chk_status_game (" ++ integer_to_list(TID) ++ ", '" ++ LoginName ++ "', '" ++ ComName ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
	case QueryResult of
		[{selected, _, MatchSchedule}, _] ->
			case MatchSchedule of
				[{0, TimeEarlier}]   -> 
					ETime = round(TimeEarlier/1000),
					response_player_send_ready(SPlayer, {too_early, [<<ETime:32>>]}); % บอกผู้เล่นว่าให้กลับมาอีกในกี่นาทีข้างหน้า
%					put(overtime, 0);
				%	 case get(counter_pid) of
				%		undefined -> do_nothing;
				%		CountPid -> 
				%			unlink(CountPid),
				%			exit(CountPid, unused)
				%	end,
				%	
				%	CounterPid = counter_spawn(SPlayer, LoginName, ComPid, ComName, TID, TimeEarlier),
				%	put(counter_pid, CounterPid);
				[{1, TimeRemain}] ->
%					put(overtime, 1),
%					if Waiting =/= 0 ->
						case Case of
							ready ->
								Rank = 3, % ส่ง Rank เป็น 3 คือการสร้างห้องเพื่อแข่งแบบ Tournament 
								% เมื่อเริ่มเล่นแล้วต้องไปเปลี่ยนค่าใน DB ว่าคู่นี้เริ่มแข่งแล้วเพื่อได้ผลถูกต้องเมื่อเช็ค remain_time_countdown
								gen_server:cast(ComPid, {arena_to_create, SPlayer, Rank, TID});
%								put(overtime, 0);
							_ ->
								% 1. เปลี่ยนสถานะของผู้แข่งให้เป็น 4 ก่อน, Status ของผู้เล่นเป็น 4 คือ ผู้เล่นพร้อมแข่ง Tournament
								lib_lobby_protocol:change_player_data(SPlayer, playing_status, 4),
								% เมื่อเข้ามาถึงส่วนนี้แล้วผู้เล่นจะจึงจะสามารถกด Cancel เพื่อออกไปทำอย่างอื่นได้ หรือถ้่าไม่กดก็รอจนกว่าจะหมดเวลา หริือว่า ผู้เล่นอีกคนเข้ามากด Ready
								%ส่งไปบอกผู้เล่นว่าระบบได้เปลี่ยนสถานะของผู้เล่นเป็นพร้อมแข่งแล้ว (ถ้ายังไม่ส่งอันนี้ไป ต้องให้ Client ทำปุ่มเป็นสีเทาไว้ก่อน เพื่อไม่ให้กดยกเลิก)
								%gen_server:cast(SPlayer, {send, []}),
								% และเริ่มนับเวลาจนถึงหมดเวลาของ Match
								case is_pid(ComPid) of
									true -> 	response_player_send_ready(ComPid, compatitor_is_ready);
									_ -> ""
								end,
								response_player_send_ready(SPlayer, compatitor_is_not_ready),
								case get(counter_pid) of
									undefined -> do_nothing;
									CountPid -> 
										unlink(CountPid),
										exit(CountPid, unused) 
								end,
								CounterPid = counter_spawn(SPlayer, LoginName, ComPid, ComName, TID, TimeRemain),
								put(counter_pid, CounterPid)
						end;
%					true ->
%						""
%					end;
				[{2, TimeLate}] -> LTime = round(TimeLate/1000),
					response_player_send_ready(SPlayer, {too_late, [<<LTime:32>>]}) % You Lose
			end;
		_ -> response_player_send_ready(SPlayer, connot_find_match_schedule)
	end.	
	
% 3.2.1.2 ถ้ายังไม่เลยให้แจ้งว่าคู่แข่งยังไม่พร้อม และนับเวลา ถ้าเวลาหมดแล้วคู่แข่งยังไม่พร้อมให้ ผู้เล่นนี้ชนะ แต่ถ้าครบเวลาแล้ว ให้ตรวจดูว่าผู้เล่นเปลี่ยนสถานะเป็น play หรือยัง
remain_time_countdown(SPlayer, LoginName, ComPid, ComName, TID, TimeRemain) ->
	lib_database:connect(),
	time_countdown(SPlayer, LoginName, ComPid, ComName, TID, TimeRemain).
	
time_countdown(SPlayer, LoginName, ComPid, ComName, TID, TimeRemain) ->	
	case compatitor_name_status(SPlayer, LoginName, TID)  of
	[{MID, _, _, _, _, _}] -> % return เป็น ชุดข้อมูล ได้แก่ .... UserName ของคู่แข่ง, วันเวลา, ระยะเวลาของ match
		put(match_ID, MID);
		_ -> gen_server:cast(LoginName, {send, [16#81, 16#a1, 0]})
	end,
	Match_ID = get(match_ID),
	receive
	after TimeRemain ->
		%%SQLCommand = "SELECT smno.fc_league_chk_status (" ++ integer_to_list(TID) ++ ", '" ++ LoginName ++ "', '" ++ ComName ++ "');",
		SQLCommand = "SELECT smno.fc_league_chk_status (" ++ integer_to_list(Match_ID) ++ ");",
		QueryResult = lib_database:get_query(SQLCommand),
		smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]),
		case QueryResult of
			% 3.2.1.2.1 ถ้าเปลี่ยนแล้ว ก็ไม่ต้องทำอะไร
			{selected, _, [{1}]} -> 
			% 3.2.1.2.2 ถ้ายังไม่เปลี่ยนแสดงว่ายังไม่มีการแข่งขัน และให้ผู้เล่นนี้ชนะ ก็ต่อเมื่อ ผู้เล่นนี้ยังอยู้ในสถานะพร้อมแข่ง
				case lib_lobby_protocol:get_user_data_pid(SPlayer, [playing_status]) of
					% ผู้เล่นยังอยู่ในสถานะพร้อมแข่ง
					[4] ->
						lib_lobby_protocol:change_player_data(SPlayer, playing_status, 0),
						% เก็บผลการแข่งขันเข้าสู่ DB
						collect_match_result(LoginName, ComName, TID, "4"),
						response_player_send_ready(SPlayer, compatitor_late_you_win), % You Win
						case is_pid(ComPid) of
							true -> response_player_send_ready(ComPid, you_are_not_yet_ready_for_match);
							_ -> ""
						end;
					% ผู้เล่นไม่อยู่ในสถานะพร้อมแข่งแล้ว อาจมีการยกเลิกออกไปก่อน
					Other -> smo_logger:fmsg("check user playing status is ~p~n", [Other])
						% response_player_send_ready(SPlayer, you_are_not_yet_ready_for_match),
						% case is_pid(ComPid) of
						%	 true -> response_player_send_ready(ComPid, you_are_not_yet_ready_for_match);
						%	 _ -> ""
						% end
				end;
			_ -> do_nothing
		end
	end.

counter_spawn(SPlayer, LoginName, ComPid, ComName, TnmID, TimeRemain) ->
	spawn_link(lib_tournament, remain_time_countdown, [SPlayer, LoginName, ComPid, ComName, TnmID, TimeRemain]).
	
	
collect_match_result(LoginName, ComName, TID, Result) ->
	Match_ID = get(match_ID),
	%%UpdateCommand = "CALL smno.sp_league_update_logGame (" ++ integer_to_list(TID) ++  ", '" ++ LoginName ++ "', '" ++ ComName ++ "', "++ Result ++");",
	UpdateCommand = "CALL smno.sp_league_update_logGame ("++ integer_to_list(Match_ID) ++" , '" ++ LoginName ++ "', '" ++ ComName ++ "', "++ Result ++");",
	UpdateResult = lib_database:get_query(UpdateCommand),
	smo_logger:fmsg("~p <=:=> ~p~n", [UpdateCommand, UpdateResult]).
%%	SQLCommand = "CALL smno.sp_league_get_challenger (" ++integer_to_list(TID)++ ", '" ++ LoginName ++ "');",
%%	QueryResult = lib_database:get_query(SQLCommand),
%%	smo_logger:fmsg("~p <=:=> ~p~n", [SQLCommand, QueryResult]).