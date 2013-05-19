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
-module(lib_rank_play).
-export([
						request_rank_play/1,
						cancel_rank_play/1,
						request_rank_info/1,
						all_temp_invalid/0
					]).
					
all_temp_invalid() -> temporary_invalid_id(20) ++ temporary_invalid_tid(5) ++ temporary_invalid_did(20) ++ temporary_invalid_jow(200).
	
temporary_invalid_id(0) -> [];
temporary_invalid_id(X) ->
	if 
		X =< 9 -> ["smn0" ++ integer_to_list(X)] ++ temporary_invalid_id(X -1 );
		X =< 99  -> ["smn" ++ integer_to_list(X)] ++ temporary_invalid_id(X -1 )
		% true -> ["rod_" ++ integer_to_list(X)] ++ temporary_invalid_id(X -1 )
	end.

temporary_invalid_tid(0) -> [];
temporary_invalid_tid(X) ->
	if 
		X =< 9 -> ["smo_t0" ++ integer_to_list(X)] ++ temporary_invalid_tid(X -1 )
	end.

temporary_invalid_did(0) -> [];
temporary_invalid_did(X) ->
	if 
		X =< 9 -> ["smo_d0" ++ integer_to_list(X)] ++ temporary_invalid_did(X -1 );
		X =< 99  -> ["smo_d" ++ integer_to_list(X)] ++ temporary_invalid_did(X -1 )
	end.

temporary_invalid_jow(0) -> [];
temporary_invalid_jow(X) ->
	if 
		X =< 9 -> ["jow_00" ++ integer_to_list(X)] ++ temporary_invalid_jow(X -1 );
		X =< 99  -> ["jow_0" ++ integer_to_list(X)] ++ temporary_invalid_jow(X -1 );
		true  -> ["jow_" ++ integer_to_list(X)] ++ temporary_invalid_jow(X -1 )
	end.
	
	
check_rank_band_card(LoginName) ->
	% BandedCard = [{1,1,1}, {2,1,1}, {3,1,1}, {4,1,1}, {5,1,1}, 
									% {6,1,1}, {7,1,1}, {8,1,1}, {9,1,1}, {10,1,1},
									% {11,1,1}, {12,1,1}, {14,1,1}, {14,1,1}, {15,1,1},
	% {103, 2, 0}, {100, 3, 0}],
	BandedCard = [],
	BandedSort = function_utility:qsort(BandedCard),
	PlayerCard = function_utility:qsort(lib_deck_edit:query_player_deck(LoginName)),
	%io:format("PlayerCard ~p~n", [PlayerCard]),
	
	DontHave = BandedSort -- PlayerCard,
	BandHad = BandedSort -- DontHave,
	
	BandCount = band_had_count(BandHad, PlayerCard),
	%io:format("BandCount :~p~n", [BandCount]),
	case BandHad of
		[] -> {deck_valid};
		_ -> {deck_invalid, BandCount}
	end.

band_had_count(BandHad, PlayerCard) -> band_had_count(BandHad, PlayerCard, []).
	
band_had_count([], _, Count) -> Count;
% ถ้า Head ของ การ์ด Band ที่ผู้เล่นมี เหมือนกับ Head ของการ์ดผู้เล่น
band_had_count([{ID, Set, Type}|OtherBand], [{ID, Set, Type}|PlayerCard], Count) ->
	case PlayerCard of
		% ถ้า Head ของ Tail ของการ์ดที่ผู้เล่นมีเหมือนกับ การ์ด Band ใบปัจจุบัน
		[{ID, Set, Type}|_] -> 
			% ยังใช้ การ์ด Band ใบเดิม ตรวจสอบอยู่
			case Count of
				[ID, Set, Type, CardCount|PreviousCount] -> 
					band_had_count([{ID, Set, Type}|OtherBand], PlayerCard, [ID, Set, Type, CardCount+1] ++ PreviousCount);
				_ -> band_had_count([{ID, Set, Type}|OtherBand], PlayerCard, [ID, Set, Type, 1] ++ Count)
			end;
		% ถ้า Head ของ Tail ของการ์ดที่ผู้เล่นมีไม่เหมือนกับ การ์ด Band ใบปัจจุบัน
		_ ->
			case Count of
				[ID, Set, Type, CardCount|PreviousCount] -> 
					band_had_count(OtherBand, PlayerCard, [ID, Set, Type, CardCount+1] ++ PreviousCount);
				_ -> band_had_count(OtherBand, PlayerCard, [ID, Set, Type, 1] ++ Count)
			end
	end;
band_had_count([{ID, Set, Type}|OtherBand], [{CardID, CardSet, CardType}|PlayerCard], Count) -> band_had_count([{ID, Set, Type}|OtherBand], PlayerCard ++ [{CardID, CardSet, CardType}], Count).
	
		
request_rank_play(SPlayer) ->
	[LoginName] = lib_lobby_protocol:get_user_data_pid(SPlayer, [user_id]),
	AllInvalid = all_temp_invalid(),
	case [LoginName] -- AllInvalid of
		[] -> smo_logger:fmsg("UserName is in forbidden ID ~p~n", [LoginName]),
			gen_server:cast(SPlayer, {send, [16#81, 16#0b, 0]});
		_ ->
			case lib_arena:check_valid_deck(LoginName, "1") of
				{deck_valid} ->
					% Check Banded Card
					case check_rank_band_card(LoginName) of
						{deck_valid} ->
							Rating = player_rating(LoginName),
							Class = calculate_class(Rating),
							smo_ranking_guardian:connected(SPlayer, Class);
						{deck_invalid, BandedCard} ->	
							io:format("send Banded Card ~p~n", [[16#81, 16#0b,2] ++ BandedCard]),
							gen_server:cast(SPlayer, {send, [16#81, 16#0b,2] ++ BandedCard})
					end;
					% case smo_ranking_guardian:connected(SPlayer, Class) of
						% wait -> request_rank_play(SPlayer);%gen_server:cast(SPlayer, {send, [16#00, 16#00, 16#09]});
						% {FirstQPid, RanQPid} -> gen_server:cast(smo_ranking_guardian, {both_player_play, FirstQPid, RanQPid})
					% end;
				{deck_invalid} -> gen_server:cast(SPlayer, {send, [16#81, 16#0b, 1]})
			end
	end.

calculate_class(Rating) ->
	if
		%%Rating < 1200 -> "G";
		%%Rating > 1200, Rating < 1400 -> "F";
		%%Rating > 1400, Rating < 1600 -> "E";
		%%Rating > 1600, Rating < 1800 -> "D";
		%%Rating > 1800, Rating < 2000 -> "C";
		%%Rating > 2000, Rating < 2200 -> "B";
		%%Rating > 2200, Rating < 2400 -> "A";
		%%Rating > 2400 -> "S";
		true -> io:format("Rating Error is ~p~n", [Rating]), "G"
	end.
	
cancel_rank_play(PlayerPid) ->
	smo_ranking_guardian:disconnected(PlayerPid).

player_rating(LoginName) ->
	SQLCommand = "SELECT  smno.fc_get_rating('" ++ LoginName ++ "');",
	case lib_database:sql_query(SQLCommand) of
		{selected,_,[{Rating}]} -> Rating;
		{error, Reason} ->	io:format("Query error : ~p~n", [Reason])
	end.
	
request_rank_info(SPlayer) ->
	UserId = lib_lobby_protocol:get_user_data_pid(SPlayer, user_id),
	AllRankRoom = lib_lobby_protocol:get_rank_room(),
	RankRoomCount = length(AllRankRoom),
	SQLCommand = "CALL smno.sp_get_player_rank ('" ++ UserId ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	PlayerDena = player_dena(UserId),
	PlayerAurius = player_aurius(UserId),
	smo_logger:fmsg("~p <=:=> ~p ~n", [SQLCommand, QueryResult]),
	case  QueryResult of
		%%[{selected, _, [{Rank, Rating, Win, Lose, Draw}]}, _] -> gen_server:cast(self(), {send, [16#81, 16#08, <<RankRoomCount:16>>, <<Rating:16>>, <<Rank:16>>, <<Win:16>>, <<Draw:16>>, <<Lose:16>>]});
		[{selected, _, [{Rating, Rank, Win, Draw, Lose}]}, _] -> gen_server:cast(self(), {send, [16#81, 16#08, <<Rating:32>>, <<Rank:32>>, <<Win:16>>, <<Draw:16>>, <<Lose:16>>, <<PlayerDena:32>>, <<PlayerAurius:32>>]});
		_ -> gen_server:cast(self(), {send, [16#81, 16#08, <<0:32>>, <<0:32>>, <<0:16>>, <<0:16>>, <<0:16>>, <<0:32>>, <<0:32>>]})
	end.
	
player_dena(LoginName) ->
	SQLCommand = "CALL smno.sp_get_cash('" ++ LoginName ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p ~n", [SQLCommand, QueryResult]),
	case  QueryResult of
		[{selected, _, [{_, Denarian}]}, _] -> Denarian;
		{error, Reason} ->	io:format("Query error : ~p~n", [Reason])
	end.
	
player_aurius(LoginName) ->
	SQLCommand = "CALL smno.sp_get_cash('" ++ LoginName ++ "');",
	QueryResult = lib_database:get_query(SQLCommand),
	smo_logger:fmsg("~p <=:=> ~p ~n", [SQLCommand, QueryResult]),
	case  QueryResult of
		[{selected, _, [{Aurius, _}]}, _] -> Aurius;
		{error, Reason} ->	io:format("Query error : ~p~n", [Reason])
	end.
