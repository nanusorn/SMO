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
-module(s_skill_check).
-import (mnesia_table, [do/1]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("record.hrl").
-include_lib("s_skill.hrl").
%-include_lib("s_added_skill.hrl").

%===============================================================================
is_interfere(CardID) ->
	Result1 = do(qlc:q( [ X#card_skill.can_interfere|| X <- mnesia:table(card_skill), X#card_skill.card_id =:= CardID])),
	Result = function_utility:del(Result1),
	case	[y] -- Result of
		[] -> 1;
		_ -> 0
	end.
%===============================================================================
card_skill_available(CardOwner, CardOrder, CardID, Interfere) ->
	%PlayerOppID = mnesia_play:get_opponent_pid(PlayerOwnID),
	SskillList1 = do(qlc:q( [ X#card_skill.skill_no|| X <- mnesia:table(card_skill), X#card_skill.card_id =:= CardID ])),
	SskillList =function_utility:qsort(SskillList1),
	DelDup = function_utility:del(SskillList),
	UserSkill = card_with_skill_no({CardOwner, CardOrder, CardID}, DelDup),
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	SupSkill =
	case function_utility:is_contain([{skill, use_supporter_skill}], CardFx) of
		[] -> [];
		_ ->
			Support = arena_zone:get_support_seals(CardOwner, CardOrder, CardID),
			get_supporter_skill(Support)
	end,
		
	Skill = 
	case Interfere of
		y ->	check_interfere_skill(UserSkill ++ SupSkill);
		_ -> UserSkill ++ SupSkill
	end,
	%SkillAvailable =  skill_available({CardZone, {PlayerOwnID, CardOrder, CardID}}, Skill, PlayerOppID),
	%AddedSkillAvai = all_added_skill({CardZone, {PlayerOwnID, CardOrder, CardID}}, PlayerOppID),
	%AllSkill = SkillAvailable, %++AddedSkillAvai,
	if
		%length(AllSkill) > 1 -> {need_to_select_one, AllSkill};
		length(Skill) >= 1 -> {need_to_select_one, Skill};
		%length(AllSkill) =:= 1, length(DelDup) > 1 -> {need_to_select_one, AllSkill};
		%length(AllSkill) =:= 1, length(DelDup) =:= 1 -> {do_not_need_to_select, AllSkill};
		true -> {error, no_skill}
	end.
%------------------------------------------------------------------
get_supporter_skill([]) -> [];
get_supporter_skill([{SupOwner, SupOrder, SupID}|Support]) ->
	SskillList1 = do(qlc:q( [X#card_skill.skill_no|| X <- mnesia:table(card_skill), X#card_skill.card_id =:= SupID ])),
	SskillList =function_utility:qsort(SskillList1),
	DelDup = function_utility:del(SskillList),
	card_with_skill_no({SupOwner, SupOrder, SupID}, DelDup) ++ get_supporter_skill(Support).
%------------------------------------------------------------------
card_with_skill_no(_, []) -> [];
card_with_skill_no({CardOwner, CardOrder, CardID}, [SkillNo|No]) ->
	[{CardOwner, CardOrder, CardID, SkillNo}] ++ card_with_skill_no({CardOwner, CardOrder, CardID}, No).
%------------------------------------------------------------------
check_interfere_skill([{CardOwner, CardOrder, CardID, SkillNo}|CardWithSkill]) ->
	CanInterfere = do(qlc:q( [ X#card_skill.can_interfere|| X <- mnesia:table(card_skill), X#card_skill.card_id =:= CardID, X#card_skill.skill_no =:= SkillNo ])),
	CanFere = function_utility:del(CanInterfere),
	case CanFere of
		[y] -> [{CardOwner, CardOrder, CardID, SkillNo}] ++ check_interfere_skill(CardWithSkill);
		_ -> check_interfere_skill(CardWithSkill)
	end;
check_interfere_skill([]) -> [].
%===============================================================================
skill_available({CardZone, {PlayerOwnID, CardOrder, CardID}}, {SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, [SskillNo|Tail]}, PlayerOppID) ->
	Result = s_skill:start_skill_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, {SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, SskillNo}, PlayerOppID),
	io:format('Result ~p~n', [Result]),
	% เป็๋น Id ของ skill ที่ ไม่รวม Id ที่เป็น จากนั้น หรือ จากนั้นสั่ง
	case Result of
		 {ok, _} -> [{SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, SskillNo}]++skill_available({CardZone, {PlayerOwnID, CardOrder, CardID}}, {SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, Tail}, PlayerOppID);
		_ -> skill_available({CardZone, {PlayerOwnID, CardOrder, CardID}}, {SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, Tail}, PlayerOppID)
	end;
skill_available(_, {_SkillOwnerPid, _SkillOwnerOrder, _SkillOwnerID, []}, _) -> [].
%===============================================================================
all_added_skill({Zone, {PlayerOwnID, CardOrder, CardID}}, PlayerOppID) ->
	AddedSkillList = game_info:card_added_skill({Zone, {PlayerOwnID, CardOrder, CardID}}),
	if
		AddedSkillList =/= [] -> 
			AddedSkillAvai =  added_skill_available({Zone, {PlayerOwnID, CardOrder, CardID}}, AddedSkillList, PlayerOppID);
		true -> AddedSkillAvai = []
	end,
	AddedSkillAvai.
%===============================================================================
added_skill_available({CardZone, {PlayerOwnID, CardOrder, CardID}}, [{GiverOwnId, GiverCardOrder, GiverCardID, AddedSkillNo}|Tail], PlayerOppID) ->
	Result = s_added_skill:start_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, {GiverCardID, AddedSkillNo}, PlayerOppID),
	%io:format('Result ~p~n', [Result]),
	if
		Result =:= [ok] -> [{GiverOwnId, GiverCardOrder, GiverCardID, AddedSkillNo}]++added_skill_available({CardZone, {PlayerOwnID, CardOrder, CardID}}, Tail, PlayerOppID);
		Result =/= [ok] -> added_skill_available({CardZone, {PlayerOwnID, CardOrder, CardID}}, Tail, PlayerOppID)
	end;
added_skill_available(_, [], _) -> [].	
%===============================================================================
resource_use(CardID, SkillIden) ->
	case is_number(SkillIden) of
		true ->
			Result1 = do(qlc:q( [X#card_skill.skill_mp|| X <- mnesia:table(card_skill), X#card_skill.card_id =:= CardID,  X#card_skill.skill_no =:= SkillIden]));
		false ->
			{GiverCardID, AddedSkillNo} = SkillIden,
			Result1 = do(qlc:q( [X#card_skill.skill_mp|| X <- mnesia:table(card_skill), X#card_skill.card_id =:= GiverCardID,  X#card_skill.skill_no =:= AddedSkillNo]))
	end,
	Result = function_utility:del(Result1),
	if 
		Result =:= [] -> {error, no_skill};
		Result =/= [] -> 
			case Result of
				[{Mp, Counter}] -> {ok, [Mp, Counter]};
				[{Counter}] -> {ok, [0, Counter]};
				[Mp] -> {ok, [Mp, 0]}	
			end
	end.
%===============================================================================
% get_select_target_amount(CardOwner, CardOrder, CardID, SkillNo) ->
	% SkillId = do(qlc:q( [X#card_skill.skill_id||X <- mnesia:table(card_skill), X#card_skill.skill_no, X#card_skill.card_id =:= CardID])),
	% check_select_amount_each_id(SkillId).
	% 
% check_select_amount_each_id([SkillId|IdList]) ->
	% CheckCount = do(qlc:q( [X#card_skill.other_match_count||X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	% case CheckCount of
		
get_select_target_amount(CardOwner, CardOrder, CardID, SkillNo) -> do_not_need.
	
check_then(SkillID) ->
	[Then] = do(qlc:q([X#card_skill.then_assign||X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillID])),
	case Then of
		null -> [];
		_ -> Then
	end.
		
