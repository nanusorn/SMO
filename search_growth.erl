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
-module (search_growth).

-include_lib ("stdlib/include/qlc.hrl").
-include ("play_record.hrl").

-import (mnesia_table, [do/1]).
-import (lists, [append/2, flatlength/1, split/2, nth/2]).

-compile (export_all).

get_growth_data (CardID) ->
	Query = qlc:q([X#growth_data.option_list ||
				X <- mnesia:table(growth_data), X#growth_data.card_id =:= CardID]),
	case do(Query) of
		[Result] -> {ok, Result};
		[] -> {error, no_growth_box}
	end.
	
get_card_data ([]) -> [];
get_card_data ([{CardData, _} | Cards]) -> [CardData] ++ get_card_data (Cards);
get_card_data ([{PlayerPid, CardOrder, CardID} | Cards]) -> [{PlayerPid, CardOrder, CardID}] ++ get_card_data (Cards).

get_growth_option (PlayerPid, CardOrder, CardID) -> 
	case get_growth_data (CardID) of
		{ok, GrowthOption} ->
			{ok, Arena} = mnesia_play:get_player_data (PlayerPid, arena_zone),
			ArenaUpdate = get_card_data (card_utility:remove (Arena, PlayerPid, CardOrder, CardID)),
%			case check_arena_growth_material (ArenaUpdate, 0, tuple_to_list(GrowthOption)) of
			case check_arena_growth_material (ArenaUpdate, GrowthOption) of
				[] -> 
					{can_not_growth};
				CompleteOption ->
					{can_growth, CompleteOption}
			end;
		{error, no_growth_box} -> {can_not_growth}
	end.
	
check_arena_growth_material (_, []) -> [];
check_arena_growth_material (Arena, [Option | GrowthOption]) ->
	{OptionNum, SupportList} = mnesia_odbc:get_option_support_growth (Option),
%	Supports = flatlength(SupportList)*3,
%	SupportSize = Supports + length(SupportList),
	ArenaCards = flatlength(Arena),
	case check_option_on_arena (Arena, SupportList, ArenaCards, 0) of
		{option_complete} ->
%			OptionComplete = [{OptionNum, SupportSize, SupportList}], เอา SupportSize ออก เพราะไม่ได้ใช้แล้ว
			OptionComplete = [{OptionNum, SupportList}],
			OptionComplete ++ check_arena_growth_material (Arena, GrowthOption);
		{option_incompleted} ->
			check_arena_growth_material (Arena, GrowthOption) 
	end.	
	
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
	case check_arena (Arena, Support, SupportList, Match) of
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
				_ ->	case check_additional_code({CardOwner, CardOrder, CardID}, MoreCode) of
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
					case check_main_card_condition({CardOwner, CardOrder, CardID}, SupportList) of
						condition_complete -> {option_complete};
						condition_fail ->
							check_arena (Arena, [{MainCode, MinorCode}] ++ MoreCode, SupportList, Match)
					end
			end
	end.

check_match (_, []) -> {not_match};
check_match (FieldID, [FieldID|_]) -> {match};
check_match (FieldID, FieldID) -> {match};
check_match ({more_than, L}, [FieldID|T]) ->
	if FieldID >= L ->
		{match};
	true ->
		check_match (FieldID, T)
	end;
check_match ({more_than, L}, FieldID) ->
	if FieldID >= L ->
		{match};
	true ->
		{not_match}
	end;

check_match ({less_than, L}, [FieldID|T]) ->
	if FieldID =< L ->
		{match};
	true ->
		check_match (FieldID, T)
	end;
check_match ({less_than, L}, FieldID) ->
	if FieldID =< L ->
		{match};
	true ->
		{not_match}
	end;

check_match (FieldID, [_|T]) -> check_match (FieldID, T).

check_additional_code(_, []) -> verify_code;
check_additional_code({CardOwner, CardOrder, CardID}, [{MainCode, MinorCode} | MoreCode]) ->
	{Field, FieldID} = play_utility:get_card_infomation({CardOwner, CardOrder, CardID}, MainCode, MinorCode),
	case check_match (FieldID, Field) of
		{match} -> check_additional_code({CardOwner, CardOrder, CardID}, MoreCode);
		{not_match} -> invalid_code
	end.

% ตรวจสอบว่าการ์ดใบนี้เคยถูกเลือกไปแล้วหรือไม่ -
check_diff_match ([], _) -> {difference};
check_diff_match ([Card | _], Card) -> {same_card};
check_diff_match ([_ | T], Card) -> check_diff_match (T, Card).

check_support_match (Match, {PlayerPid, CardOrder, CardID}, MainCode, MinorCode) ->
	case Match of
		[] -> check_support_seal (PlayerPid, CardOrder, CardID, MainCode, MinorCode);
		_ -> support_not_valid
	end.

check_support_seal (PlayerPid, CardOrder, CardID, MainCode, MinorCode) ->
	case card_utility:get_card_combine_part(PlayerPid, CardOrder, CardID, support_seal) of	
		[{{SOwner, SOrder, SupportId}, _}] ->
			{Field, FieldID} = play_utility:get_card_infomation({SOwner, SOrder, SupportId}, MainCode, MinorCode),
			case check_match (FieldID, Field) of
				{match} -> support_match;
				{not_match} -> support_not_valid
			end;
		_ -> support_not_valid
	end.

check_main_card_condition (_, []) -> condition_fail;
check_main_card_condition ({CardOwner, CardOrder, CardID}, [[{MainCode, MinorCode}]]) ->
	{Field, FieldID} = play_utility:get_card_infomation({CardOwner, CardOrder, CardID}, MainCode, MinorCode),
	case check_match (FieldID, Field) of
		{match} -> condition_complete;
		{not_match} -> condition_fail
	end.	
	
get_set_support_list (CardOwner, CardOrder, CardID, OptionNumber, Growth_Option) ->
	{ok, Arena} = mnesia_play:get_player_data (CardOwner, arena_zone),
	ArenaCheck = get_card_data (card_utility:remove (Arena, CardOwner, CardOrder, CardID)),
	verify_support_list (ArenaCheck, OptionNumber, Growth_Option).
		
verify_support_list (Arena, OptionNumber, OptionSelect) ->
	AllSupport = find_all_support (Arena, OptionSelect, []),
	{all_check, AllCheck} = check_possibility_growth (AllSupport, 1, [], []),
	{SetSize, GrowthSet, AllGrowthCheck} = get_set_reply_data (AllCheck, 0, [], []),
	GrowthSize = flatlength(AllSupport),
	case SetSize of
		0 -> {no_support_set};
		_ -> {ok, [GrowthSize] ++ [SetSize] ++ GrowthSet, [OptionNumber] ++ AllGrowthCheck}
	end.

find_all_support (_, [], AllSupport) -> AllSupport;
find_all_support (Arena, [SupportReq | T], AllSupport) ->
	{MaxSeal, SupportSeal} = find_support_from_arena (Arena, SupportReq, 1, []),
	find_all_support (Arena, T, AllSupport ++ [{MaxSeal, SupportSeal}]).

find_support_from_arena ([], _, SupportNumber, SupportSeal) -> {SupportNumber, SupportSeal};
find_support_from_arena ([{PlayerPid, CardOrder, CardID} | Arena], [{MainCode, MinorCode}|MoreCode], SupportNumber, SupportSeal) ->
	check_arena_support_seal (PlayerPid, CardOrder, CardID, Arena, [{MainCode, MinorCode}|MoreCode], SupportNumber, SupportSeal).

check_arena_support_seal (PlayerPid, CardOrder, CardID, Arena, [{MainCode, MinorCode}|MoreCode], SupportNumber, SupportSeal) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			case check_arena ([{PlayerPid, CardOrder, CardID}], [{MainCode, MinorCode} | MoreCode], [], []) of
				{option_complete} ->
					find_support_from_arena (Arena, [{MainCode, MinorCode} | MoreCode], SupportNumber + 1, SupportSeal ++ [{PlayerPid, CardOrder, CardID}]);
				{found, _Card} ->
					find_support_from_arena (Arena, [{MainCode, MinorCode} | MoreCode], SupportNumber + 1, SupportSeal ++ [{PlayerPid, CardOrder, CardID}]);
				{not_found} ->
					find_support_from_arena (Arena, [{MainCode, MinorCode} | MoreCode], SupportNumber, SupportSeal)
			end;
		is_not_seal ->
			find_support_from_arena (Arena, [{MainCode, MinorCode}], SupportNumber, SupportSeal)
	end.

check_possibility_growth ([], _, GrowthCheck, _) -> {add, [list_to_tuple(lists:sort(GrowthCheck))]};
check_possibility_growth ([{Max, _} | _], Max, _, AllGrowthCheck) -> {all_check, AllGrowthCheck};
check_possibility_growth ([{Max, Cards} | T], Current, GrowthCheck, AllCheck) ->
	{PlayerPid, CardOrder, CardID} = nth(Current, Cards),
	case check_duplicate_card ({PlayerPid, CardOrder, CardID}, GrowthCheck) of
		{allow} ->
			ChekcUpdate = GrowthCheck ++ [{PlayerPid, CardOrder, CardID}],
			case check_possibility_growth (T, 1, ChekcUpdate, AllCheck) of
				{add, CheckGrowth} -> %% ครบทุก Material					
					AllCheckUpdate = AllCheck ++ CheckGrowth,
					check_possibility_growth ([{Max, Cards}] ++ T, Current + 1, GrowthCheck, lists:usort(AllCheckUpdate));
				{all_check, ACC} ->
					check_possibility_growth ([{Max, Cards}] ++ T, Current + 1, GrowthCheck, ACC)
			end;
		{disallow} ->
			check_possibility_growth ([{Max, Cards}] ++ T, Current + 1, GrowthCheck, AllCheck)
	end.

check_duplicate_card (_, []) -> {allow};
check_duplicate_card (Card, [Card|_]) -> {disallow};
check_duplicate_card (Card, [_|T]) -> check_duplicate_card (Card, T).

get_set_reply_data ([], Order, Set, Check) -> {Order, Set, Check};
get_set_reply_data ([SupportSet | AllSupportSet], Order, Set, Check) ->
	{SetReply, CheckReply} = get_growth_data (tuple_to_list (SupportSet), [], []),
	SetUpdate = Set ++ [Order] ++ SetReply,
	CheckUpdate = Check ++ [{Order, list_to_tuple (CheckReply)}],
	get_set_reply_data (AllSupportSet, Order + 1, SetUpdate, CheckUpdate).

get_growth_data ([], Set, Check) -> {Set, Check};
get_growth_data ([{PlayerPid, CardOrder, CardID} | T], Set, Check) ->
	SetUpdate = Set ++ [CardOrder, <<CardID:16>>],
	CheckUpdate = Check ++ [{PlayerPid, CardOrder, CardID}],
	get_growth_data (T, SetUpdate, CheckUpdate).
	
	
	
	
	
	
	
	
	
