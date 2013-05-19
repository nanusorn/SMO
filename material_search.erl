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
-module (material_search).

-import (lists, [flatlength/1, nth/2]).

-export ([get_combination_option/3, find_all_support/3, get_set_support_list/5]).
-export ([filter_can_support/1, check_option_on_arena/4, verify_support_growth/3]).

get_combination_option (PlayerPid, CardOrder, CardID) ->
	case mnesia_odbc:get_combination_data (CardID) of
		{ok, CombineOption} ->
			% เก็บข้อมูลการ์ดทั้งสนามโดยที่ไม่รวมใบหลักไว้
			{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
			ArenaUpdate = filter_can_support (card_utility:remove (Arena, PlayerPid, CardOrder, CardID)),
			case check_arena_support_seal (ArenaUpdate, CombineOption) of
				[] -> {no_choise, not_enough_sub};
				CompleteCheck -> {have_choise, CompleteCheck}
			end;
		{error} ->
			{no_choise, no_combine_option}
	end.

% กรองการ์ดที่เป็น inactive และ มีมิสติกการ์ดติดอยู่ ออก
filter_can_support ([]) -> [];
filter_can_support ([{{CardOwner, CardOrder, CardID}, _} | Arena]) ->
	case combination:check_support (CardOwner, CardOrder, CardID) of
		support_ready -> [{CardOwner, CardOrder, CardID}] ++ filter_can_support (Arena);
		support_not_ready -> filter_can_support (Arena)
	end.

% complete_option = {option_number, sub_request_size, sub_seal_list}
check_arena_support_seal (_, []) -> [];
check_arena_support_seal (Arena, [Option | CombineOption]) ->
	% Support List - [{elem, 6}] หรือ [{elem, 6}, {elem, 6}]
	{OptionNum, SupportList} = mnesia_odbc:get_option_support_seal (Option),
%	SupportSize = flatlength (SupportList),
	SupportSize = flatlength(SupportList)*3,
	% ซีลรองจะมีรหัสเป็นสองตัวอักษร เพราะฉนั้น 2 หมายถึงท่า Double Combine และ 4 จะหมายถึงท่า Triple Combine
	case SupportSize of
%		2 ->	FilterArena = filter_double_combine (Arena);
%		4 ->	FilterArena = filter_triple_combine (Arena)
		3 ->	FilterArena = filter_double_combine (Arena);
		6 ->	FilterArena = filter_triple_combine (Arena)
	end,	
	ArenaCards = flatlength (FilterArena),
	case check_option_on_arena (FilterArena, SupportList, ArenaCards, 0) of
		{option_complete} ->
			SupportSize = flatlength(SupportList)*3,
			[{OptionNum, SupportSize, SupportList}] ++ check_arena_support_seal (Arena, CombineOption);
		{option_incompleted} ->
			check_arena_support_seal (Arena, CombineOption) 
	end.

% กรองการ์ดที่เป็นการ์ดที่อยู่ในท่ารวมร่างออกทั้งหมด เพราะไม่สามารถเป็นซีลรองให้การรวมแบบ Double Combine ได้ -
filter_double_combine ([]) -> [];
filter_double_combine ([{PlayerPid, CardOrder, CardID} | Arena]) ->
	case card_utility:get_card_combine_part (PlayerPid, CardOrder, CardID, support_seal) of
		[] ->	[{PlayerPid, CardOrder, CardID}] ++ filter_double_combine (Arena);
		_ -> filter_double_combine (Arena)
	end.

% กรองการ์ดที่เป็นการ์ดที่อยู่ในท่ารวมร่างแบบ Triple ออก เพราะไม่สามารถเป็นซีลรองให้การรวมแบบ Triple Combine ได้ -
filter_triple_combine ([]) -> [];
filter_triple_combine ([{PlayerPid, CardOrder, CardID} | Arena]) ->
	Support = card_utility:get_card_combine_part (PlayerPid, CardOrder, CardID, support_seal),
	SupportSize = flatlength (Support),
%	io:format("Support size ~p~n", [SupportSize]),
	case SupportSize of
% เอาการ์ดที่มีซีลรองรวมร่าง 2 ใบออก (ท่า Triple Combine)
%		2 -> filter_triple_combine (Arena);
%		_ -> [{PlayerPid, CardOrder, CardID}] ++ filter_triple_combine (Arena)

% เมื่อมีเวลาจะกลับมาแก้ไข การรวมร่างกับการ์ดที่รวมร่างอยู่
		0 ->	[{PlayerPid, CardOrder, CardID}] ++ filter_triple_combine (Arena);
		_ ->	filter_triple_combine (Arena)
	end.

% ตรวจสอบการ์ดในสนามว่ามี ความต้องการตรงกับการ์ดใบหลักหรือไม่ --
check_option_on_arena (_, _, ArenaCards, ArenaCards) -> {option_incompleted};
check_option_on_arena (_, [], _, _) -> {option_incompleted};
check_option_on_arena (Arena, SupportList, ArenaCards, StartCard) ->
	{L1, L2} = lists:split (StartCard, Arena),
	case check_support_request (L2 ++ L1, SupportList, []) of
		{all_option_found} ->
			{option_complete};
		{option_fail} ->
			check_option_on_arena (Arena, SupportList, ArenaCards, StartCard + 1)
	end.

check_support_request ([], _, _) -> {option_fail};
check_support_request (_, [], _) -> {all_option_found};
check_support_request (Arena, [Support | SupportList], Match) ->
	case check_arena (Arena, [Support], SupportList, Match) of
		{option_complete} ->
			{all_option_found};
		{found, Card} ->
			check_support_request (Arena, SupportList, Match ++ [Card]);
		{not_found} ->
			{option_fail}
	end.

check_arena ([], _, _, _) -> {not_found};
check_arena ([{CardOwner, CardOrder, CardID}| Arena], [{MainCode, MinorCode} | MoreCode], SupportList, Match) ->
	{Field, FieldID} = play_utility:get_card_infomation({CardOwner, CardOrder, CardID}, MainCode, MinorCode),
%	io:format ("Field ~p , FieldId ~p~n", [Field, FieldID]),
	case check_match (FieldID, Field) of
		{match} ->
			case MoreCode of
				[] ->	case check_diff_match (Match, {CardOwner, CardOrder, CardID}) of
						{same_card} ->
							check_arena (Arena, [{MainCode, MinorCode}] ++ MoreCode, SupportList, Match);
						{difference} ->
							{found, {CardOwner, CardOrder, CardID}}
					end;
				_ ->	case check_additional_code ({CardOwner, CardOrder, CardID}, MoreCode) of
						verify_code ->	{found, {CardOwner, CardOrder, CardID}};
						invalid_code ->
							check_arena (Arena, [{MainCode, MinorCode}] ++ MoreCode, SupportList, Match)
					end
			end;
		{not_match} ->
			case check_support_match (Match, {CardOwner, CardOrder, CardID}, MainCode, MinorCode) of
				support_not_valid ->
					check_arena (Arena, [{MainCode, MinorCode}] ++ MoreCode, SupportList, Match);
				support_match ->
					case check_main_card_condition ({CardOwner, CardOrder, CardID}, SupportList) of
						condition_complete -> {option_complete};
						condition_fail ->
							check_arena (Arena, [{MainCode, MinorCode}] ++ MoreCode, SupportList, Match)
					end
			end
	end.

%check_match (_, []) -> {not_match};
%check_match (FieldID, [FieldID|_]) -> {match};
%check_match (7, [FieldID | T]) ->
%	if FieldID >= 1, FieldID =< 7 ->
%		{match};
%	   true ->
%		check_match (FieldID, T)
%	end;
%check_match (FieldID, [_|T]) -> check_match (FieldID, T).

check_match (_, []) -> {not_match};
check_match (FieldID, [FieldID|_]) -> {match};
check_match (7, [FieldID | T]) ->
	if FieldID >= 1, FieldID =< 7 ->
		{match};
	   true ->
		check_match (FieldID, T)
	end;
check_match (FieldID, [_|T]) -> check_match (FieldID, T).

check_additional_code(_, []) -> verify_code;
check_additional_code({CardOwner, CardOrder, CardID}, [MainCode, MinorCode | MoreCode]) ->
	{Field, FieldID} = play_utility:get_card_infomation({CardOwner, CardOrder, CardID}, MainCode, MinorCode),
	case check_match (FieldID, Field) of
		{match} -> check_additional_code({CardOwner, CardOrder, CardID}, MoreCode);
		{not_match} -> invalid_code
	end.

% ตรวจสอบว่าการ์ดใบนี้เคยถูกเลือกไปแล้วหรือไม่ -
check_diff_match ([], _) -> {difference};
check_diff_match ([Card | _], Card) -> {same_card};
check_diff_match ([_ | T], Card) -> check_diff_match (T, Card).

% ตรวจสอบว่า มีการ์ดเงื่อนไขที่ผ่านมาหรือไม่ ถ้ามี จะไม่สามารถรวมร่างกับการ์ดที่เป็นซีลที่รวมร่างได้อีก
check_support_match (Match, {PlayerPid, CardOrder, CardID}, MainCode, MinorCode) ->
	case Match of
		[] -> check_support_seal (PlayerPid, CardOrder, CardID, MainCode, MinorCode);
		_ -> support_not_valid
	end.

% ตรวจสอบว่า ซีลเงื่อนไขมีการรวมร่างหรือไม่ ถ้าใช่ตรวจสอบว่า ซีลรองสามารถเป็นเงื่อนไขที่ถูกต้องได้หรือไม่ --
check_support_seal (PlayerPid, CardOrder, CardID, MainCode, MinorCode) ->
	case card_utility:get_card_combine_part (PlayerPid, CardOrder, CardID, support_seal) of	
		[{{SOwner, SOrder, SupportId}, _}] ->
			{Field, FieldID} = play_utility:get_card_infomation ({SOwner, SOrder, SupportId}, MainCode, MinorCode),
			case check_match (FieldID, Field) of
				{match} -> support_match;
				{not_match} -> support_not_valid
			end;
		_ -> support_not_valid
	end.

% ตรวจสอบว่า ซีลหลักของการ์ด เป็นเงื่อนไขที่ถูกต้องของการรวมร่าง หรือไม่ --
check_main_card_condition (_, []) -> condition_fail;
check_main_card_condition ({CardOwner, CardOrder, CardID}, [MainCode, MinorCode]) ->
	{Field, FieldID} = play_utility:get_card_infomation ({CardOwner, CardOrder, CardID}, MainCode, MinorCode),
	case check_match (FieldID, Field) of
		{match} -> condition_complete;
		{not_match} -> condition_fail
	end.

% OptionSelect -> [{elem, 6}], [{elem, 6}, {elem, 6}]
get_set_support_list (PlayerPid, CardOrder, CardID, OptionNumber, OptionSelect) ->
	{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	ArenaCheck = filter_can_support (card_utility:remove (Arena, PlayerPid, CardOrder, CardID)),
	case flatlength (OptionSelect) of
		% [{elem, 6}, {elem, 6}]
		2 ->	ArenaFilter = filter_triple_combine (ArenaCheck),
			verify_support_list (ArenaFilter, OptionNumber, OptionSelect);
		% [{elem, 6}]
		1 ->	{ok, CombineOption} = mnesia_odbc:get_combination_data (CardID),
			ArenaFilter = filter_double_combine (ArenaCheck),
			case check_high_priority_reject (CombineOption, OptionSelect) of
				[] -> ArenaFilter2 = ArenaFilter;
				ConditionFilterReject ->
					ArenaFilter2 = filter_reject_card (ArenaFilter, ConditionFilterReject)
			end,
			verify_support_list (ArenaFilter2, OptionNumber, OptionSelect)
	end.

% กรองการ์ดที่มี เงื่อนไขการรวมร่างที่สูงกว่าแล้วตรงกับเงื่อนไขของการ์ดใบหลัก
filter_reject_card ([], _) -> [];
filter_reject_card ([{CardOwner, CardOrder, CardID} | Arena], ConditionFilterReject) ->
	case filter_card({CardOwner, CardOrder, CardID}, ConditionFilterReject) of
		disallow -> filter_reject_card (Arena, ConditionFilterReject);
		allow -> [{CardOwner, CardOrder, CardID}] ++ filter_reject_card (Arena, ConditionFilterReject)
	end.
	
% ตรวจสอบการ์ดแต่ละใบว่าตรงกับเงื่อนไขที่จะตัดออกหรือไม่ --
filter_card(_, []) -> allow;
filter_card({CardOwner, CardOrder, CardID}, [[{MainCode, MinorCode}] | FilterCode]) ->
	{Field, FieldID} = play_utility:get_card_infomation({CardOwner, CardOrder, CardID}, MainCode, MinorCode),
	case check_match (FieldID, Field) of
		{match} -> disallow;
		{not_match} -> filter_card({CardOwner, CardOrder, CardID}, FilterCode)
	end.

% ตรวจสอบว่า เงื่อนไขที่เลือก ของการ์ดมีเงื่อนไขที่สูงกว่า หรือไม่ ถ้ามีให้นำมาตัดซีลรองรวมร่างออก
check_high_priority_reject ([], _) -> [];
check_high_priority_reject ([Option | CombineOption], OptionSelect) ->
	case mnesia_odbc:get_option_support_seal (Option) of
		{_, OptionSelect} -> check_high_priority_reject (CombineOption, OptionSelect);
		{_, SupportOption} ->
			case check_support_type (SupportOption, OptionSelect) of
				ignore_type -> check_high_priority_reject (CombineOption, OptionSelect);
				reject_type ->
					[SupportOption] ++ check_high_priority_reject (CombineOption, OptionSelect)
			end
	end.

check_support_type (SupportOption, [{MainSelect, _}]) ->
%	io:format("SupportOption ~p, MainSelect ~p~n", [SupportOption, MainSelect]),
	case SupportOption of
		[{MainCode, _}] -> compare_priority (MainCode, MainSelect);
		_ -> ignore_type
	end.

compare_priority (MainCode, MainSelect) ->
%	io:format("MainCode ~p, MainSelect ~p~n", [MainCode, MainSelect]),
	case MainSelect of
		name -> ignore_type;
		namging ->
			case MainCode of
				name -> reject_type;
				_ -> ignore_type
			end;
		type ->
			case MainCode of
				name -> reject_type;
				naming -> reject_type;
				_ -> ignore_type
			end;
		elem ->
			case MainCode of
				elem -> ignore_type;
				_ -> reject_type
			end;
		_ -> ignore_type
	end.

% ทำ combination ของการรวมร่าง
verify_support_list (Arena, OptionNumber, OptionSelect) ->
	AllSupport = find_all_support (Arena, OptionSelect, combine, []),
	{all_check, AllCheck} = check_possibility_combine (AllSupport, 1, [], []),
	{SetSize, CombineSet, AllCombineCheck} = get_set_reply_data (AllCheck, 0, [], []),
	CombineSize = flatlength(AllSupport),
%	io:format ("Combination set size ~p~n", [SetSize]),
	case SetSize of
		0 -> {no_support_set};
		% AllCombineCheck -> [{PlayerPid, CardOrder, CardID}, {...}]
		_ -> {ok, [CombineSize] ++ [SetSize] ++ CombineSet, [OptionNumber] ++ AllCombineCheck}
	end.

verify_support_growth (Arena, OptionNumber, OptionSelect) ->
	AllSupport = find_all_support (Arena, OptionSelect, growth, []),
%	io:format("All support ~p~n", [AllSupport]),
	{all_check, AllCheck} = check_possibility_combine (AllSupport, 1, [], []),
	{SetSize, CombineSet, AllCombineCheck} = get_set_reply_data (AllCheck, 0, [], []),
	CombineSize = flatlength(AllSupport),
	case SetSize of
		0 -> {no_support_set};
		% AllCombineCheck -> [{PlayerPid, CardOrder, CardID}, {...}]
		_ -> {ok, [CombineSize] ++ [SetSize] ++ CombineSet, [OptionNumber] ++ AllCombineCheck}
	end.

find_all_support (Arena, SupportRequest, AllSupport) ->
	find_all_support (Arena, SupportRequest, combine, AllSupport).

find_all_support (_, [], _, AllSupport) -> AllSupport;
find_all_support (Arena, [SupportReq | T], SupportType, AllSupport) ->
	{MaxSeal, SupportSeal} = find_support_from_arena (Arena, SupportType, SupportReq, 1, []),
	find_all_support (Arena, T, SupportType, AllSupport ++ [{MaxSeal, SupportSeal}]).

find_support_from_arena ([], _, _, SupportNumber, SupportSeal) -> {SupportNumber, SupportSeal};
%find_support_from_arena ([{PlayerPid, CardOrder, CardID} | Arena], SupportType, [MainCode, MinorCode], SupportNumber, SupportSeal) ->
find_support_from_arena ([{PlayerPid, CardOrder, CardID} | Arena], SupportType, {MainCode, MinorCode}, SupportNumber, SupportSeal) ->
	case SupportType of
		combine ->
			case combination:check_support(PlayerPid, CardOrder, CardID) of
				support_ready ->
					check_arena_support_seal (PlayerPid, CardOrder, CardID, Arena, SupportType, MainCode, MinorCode, SupportNumber, SupportSeal);
				support_not_ready ->
					find_support_from_arena (Arena, SupportType, {MainCode, MinorCode}, SupportNumber, SupportSeal)
			end;
		growth ->
			check_arena_support_seal (PlayerPid, CardOrder, CardID, Arena, SupportType, MainCode, MinorCode, SupportNumber, SupportSeal)
	end.

check_arena_support_seal (PlayerPid, CardOrder, CardID, Arena, SupportType, MainCode, MinorCode, SupportNumber, SupportSeal) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			{Field, FieldID} = play_utility:get_card_infomation({PlayerPid, CardOrder, CardID}, MainCode, MinorCode),
			case check_match (FieldID, Field) of
				{match} ->
					find_support_from_arena (Arena, SupportType, {MainCode, MinorCode}, SupportNumber + 1, SupportSeal ++ [{PlayerPid, CardOrder, CardID}]);
				{not_match} ->
					find_support_from_arena (Arena, SupportType, {MainCode, MinorCode}, SupportNumber, SupportSeal)
			end;
		is_not_seal ->
			find_support_from_arena (Arena, SupportType, [MainCode, MinorCode], SupportNumber, SupportSeal)
	end.

% ตรวจสอบท่ารวมร่างทั้งหมดที่เป็นไปได้ในการรวมร่างนั้นๆ
% AllSupport = [{1, CardList}, {2, CardList}, ... , {n, CardList}]
check_possibility_combine ([], _, CombineCheck, _) -> {add, [list_to_tuple(lists:sort(CombineCheck))]};
check_possibility_combine ([{Max, _} | _], Max, _, AllCombineCheck) -> {all_check, AllCombineCheck};
check_possibility_combine ([{Max, Cards} | T], Current, CombineCheck, AllCheck) ->
	{PlayerPid, CardOrder, CardID} = nth(Current, Cards),
	case check_duplicate_card ({PlayerPid, CardOrder, CardID}, CombineCheck) of
		{allow} ->
			ChekcUpdate = CombineCheck ++ [{PlayerPid, CardOrder, CardID}],
			case check_possibility_combine (T, 1, ChekcUpdate, AllCheck) of
				{add, CheckCombine} -> %% ครบทุก Material					
					AllCheckUpdate = AllCheck ++ CheckCombine,
					check_possibility_combine ([{Max, Cards}] ++ T, Current + 1, CombineCheck, lists:usort(AllCheckUpdate));
				{all_check, ACC} ->
					check_possibility_combine ([{Max, Cards}] ++ T, Current + 1, CombineCheck, ACC)
			end;
		{disallow} ->
			check_possibility_combine ([{Max, Cards}] ++ T, Current + 1, CombineCheck, AllCheck)
	end.

check_duplicate_card (_, []) -> {allow};
check_duplicate_card (Card, [Card|_]) -> {disallow};
check_duplicate_card (Card, [_|T]) -> check_duplicate_card (Card, T).

get_set_reply_data ([], Order, Set, Check) -> {Order, Set, Check};
get_set_reply_data ([SupportSet | AllSupportSet], Order, Set, Check) ->
	{SetReply, CheckReply} = get_combine_data (tuple_to_list (SupportSet), [], []),
	SetUpdate = Set ++ [Order] ++ SetReply,
	CheckUpdate = Check ++ [{Order, list_to_tuple (CheckReply)}],
	get_set_reply_data (AllSupportSet, Order + 1, SetUpdate, CheckUpdate).

get_combine_data ([], Set, Check) -> {Set, Check};
get_combine_data ([{PlayerPid, CardOrder, CardID} | T], Set, Check) ->
	SetUpdate = Set ++ [CardOrder, <<CardID:16>>],
	CheckUpdate = Check ++ [{PlayerPid, CardOrder, CardID}],
	get_combine_data (T, SetUpdate, CheckUpdate).