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
-module(s_added_skill).
-import (mnesia_table, [do/1]).
-compile(export_all).

-include_lib("s_skill.hrl").
-include_lib("stdlib/include/qlc.hrl").

%===============================================================================
%  {GiverCardID, AddedSkillNo} คือ ID ของการ์ดที่ให้ Skill และ AddedSkillNo คือ Skill Number ของการ์ดทีให้
% เพราะฉะนั้น ตอนที่ได้รับ Skill กัยการใด ผู้รับ ที่ care effect ด้วย
% และตอนที่ส่งกลับมาว่าจะใช้ skill ก็ต้องบอกว่าเป็น {GiverCardID, AddedSkillNo} แทน SkillNo
start_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, {GiverCardID, AddedSkillNo}, PlayerOppID) -> % check all possible conditions 
	%io:format('Start skill_effect_return ~n'),
	%io:format('CardID ~p~n', [CardID]),
	SkillList1 = do(qlc:q([X#card_skill.skill_id || X <- mnesia:table(card_skill), X#card_skill.card_id =:= GiverCardID, X#card_skill.skill_no =:= AddedSkillNo])),
	%io:format('SkillList ~p~n', [SkillList1]),
	if 
		SkillList1 =:= [] -> []; % if that CardID have no ability 
		SkillList1 =/= [] -> % if that CardID have ability no matter how many it have just start ability
			SkillList = sort:qsort(SkillList1),	
			s_skill_list_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, SkillList, PlayerOppID)
	end.
%===============================================================================
%For Loop		
s_skill_list_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, [SkillListHead|Tail], PlayerOppID)   -> 
	
			SkillGet = s_skill_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, SkillListHead, PlayerOppID),
			%io:format("Ability get is ~p~n", [AbilityGet]),
			SkillGet++s_skill_list_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, Tail, PlayerOppID);
s_skill_list_check(_, [], _) -> [].
%===============================================================================
s_skill_effect({CardZone, {PlayerOwnID, CardOrder, CardID }}, SkillId, PlayerOppID) ->
	[SskillNo] = do(qlc:q([X#card_skill.skill_no || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	SkillConditionChecked = start_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, SskillNo, PlayerOppID),
	%io:format ("SkillConditionChecked ~p~n", [SkillConditionChecked]),
	%io:format ("SkillId ~p~n", [SkillId]),
	if 
		SkillConditionChecked =/= [ok] -> []; % if in the present condition no card can use ability then exit function
		SkillConditionChecked =:= [ok] ->
			s_skill_effect:s_skill_all_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, SkillId, PlayerOppID)
			%io:format ("Start OwnerTargetCheck ~n"),
	end.
%===============================================================================
start_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, {GiverCardID, AddedSkillNo}, PlayerOppID) ->
	SskillList1 = do(qlc:q([X#card_skill.skill_id || X <- mnesia:table(card_skill), X#card_skill.card_id =:= GiverCardID, X#card_skill.skill_no =:= AddedSkillNo])),
	%io:format('SkillList ~p~n', [SkillList1]),
	if 
		SskillList1 =:= [] -> Codition = []; % if that CardID have no ability 
		SskillList1 =/= [] -> % if that CardID have ability no matter how many it have just start ability
			SskillList = sort:qsort(SskillList1),		
			Codition = condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, SskillList, PlayerOppID),
			%io:format ("Codition ~p~n", [Codition]),
			function_utility:del(sort:qsort([Codition]))
	end,
	Result = [{ok, ok, ok, ok}]--Codition,
	if
		Result =:= [] -> BB = [ok];
		Result =/= [] -> BB = Codition
	end,
	%io:format('BB ~p~n', [BB]),
	BB.
%===============================================================================
condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, [SskillId|Tail], PlayerOppID) ->
	SskillCheck = skill_condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, SskillId, PlayerOppID),
	%io:format("Ability get is ~p~n", [AbilityGet]),
	SskillCheck++condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, Tail, PlayerOppID);
condition_check(_, [], _) -> [].
%===============================================================================
skill_condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, SkillId, PlayerOppID) -> % compare require condition of ability with present condition(this function just return final comparition weather ok or mismatch)
	%io:format('skill_condition_check ~n'),	
	% first compare player require condition with player present condition
	[DoesItNeed]= do(qlc:q( [ X#card_skill.skill_condition_need_check || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])), 
	%io:format('DoesItNeed ~p~n', [DoesItNeed]),	
		if
				DoesItNeed =:= n -> [{ok, ok, ok, ok}];
				DoesItNeed =:= y ->
					[PlayerOwnCheck] = do(qlc:q( [ X#card_skill.playerown_must_check || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])), 
					%io:format('PlayerOwnCheck ~p~n', [PlayerOwnCheck]),
					if 
						PlayerOwnCheck =:= n -> PlayerOwn = ok; % don't need to check player condition then return ok
						PlayerOwnCheck =:= y -> PlayerOwn = playerown_condition_check(PlayerOwnID, SkillId)  % need to check player condition then check then check
					end,
					[PlayerCheck1] = do(qlc:q( [ X#card_skill.player_must_check || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])), 
					%io:format('PlayerCheck1 ~p~n', [PlayerCheck1]),
					if 
						PlayerOwn =/= ok -> PlayerCheck = n;
						PlayerOwn =:= ok -> PlayerCheck = PlayerCheck1
					end,
					if 
						PlayerCheck =:= n -> Player = ok; % don't need to check player condition then return ok
						PlayerCheck =:= y -> Player = player_condition_check(PlayerOppID, PlayerOwnID, SkillId)  % need to check player condition then check then check
					end,
					[SelfCheck1] = do(qlc:q( [ X#card_skill.owner_must_check|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])),
					%io:format('SelfCheck1 ~p~n', [SelfCheck1]),
					if 
						Player =/= ok -> SelfCheck = n;
						Player =:= ok -> SelfCheck = SelfCheck1
					end,
					if 
						SelfCheck =:= n -> Self = ok; % don't need to check self condition then return no to do below statement
						SelfCheck =:= y -> Self = self_condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, PlayerOppID, SkillId)
					end,
					[OtherCheck1] = do(qlc:q( [ X#card_skill.other_must_check|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])),
					%io:format('OtherCheck1 ~p~n', [OtherCheck1]),
					if 
						Self =/= ok -> OtherCheck = n;
						Self =:= ok -> OtherCheck = OtherCheck1
					end,
					if 
				   	OtherCheck =:= n -> Other = ok;
						OtherCheck =:= y -> Other = list_condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, SkillId, PlayerOppID)% compare other card require condition to other card present condition then return ok or mismatch					
					end,
					[{PlayerOwn, Player, Self, Other}]
			end.
%===============================================================================
%===============================================================================
playerown_condition_check(PlayerOwnID, SkillId) ->
	[PlayerOwnCondition] =  player_own_condition_require(SkillId),
	check_all_palyerown_require_attribute(PlayerOwnID, PlayerOwnCondition).
%===============================================================================
%playerown_turn_check, playerown_pharse_check, playerown_mp_check, playerown_action_check
player_own_condition_require(SkillId) ->
	do(qlc:q([X#card_skill.playerown_check|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])).
%===============================================================================
check_all_palyerown_require_attribute(PlayerOwnID, [ConditionHead|Tail]) ->
	Return = check_each_palyerown_require_attribute(PlayerOwnID, ConditionHead),
	if 
		Return =:= true -> check_all_palyerown_require_attribute(PlayerOwnID, Tail);
		Return =:= false -> player_own_mismatch
	end;
check_all_palyerown_require_attribute(_, []) -> ok.
%===============================================================================
check_each_palyerown_require_attribute(PlayerOwnID, {Attribute, Value}) ->
	case Attribute of
		turn -> attribute_check:player_turn_check(game_info:player_turn(PlayerOwnID), Value);
		pharse -> attribute_check:player_pharse_check(game_info:player_pharse(PlayerOwnID), Value);
		mp -> attribute_check:player_mp_check(game_info:player_mp(PlayerOwnID), Value);
		action -> attribute_check:player_action_check(game_info:player_action(PlayerOwnID), Value)
	end.
%===============================================================================
%===============================================================================
player_condition_check(PlayerOppID, PlayerOwnID, SkillId) -> 
	[PlayerCondition] =  player_condition_require(SkillId),
	[RequirePlayer] = do(qlc:q([ X#card_skill.player_side_check || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	check_all_palyer_require_attribute(PlayerOwnID, PlayerOppID, PlayerCondition, RequirePlayer).
%===============================================================================
player_condition_require(SkillId) ->
	do(qlc:q([X#card_skill.player_check|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])).
%===============================================================================
check_all_palyer_require_attribute(PlayerOwnID, PlayerOppID, [ConditionHead|Tail], RequirePlayer) ->
	Return = check_each_palyer_require_attribute(PlayerOwnID, PlayerOppID, ConditionHead, RequirePlayer),
	if 
		Return =:= true -> check_all_palyer_require_attribute(PlayerOwnID, PlayerOppID, Tail, RequirePlayer);
		Return =:= false -> player_other_mismatch
	end;
check_all_palyer_require_attribute(_, _, [], _) -> ok.
%===============================================================================
check_each_palyer_require_attribute(PlayerOwnID, PlayerOppID, {Attribute, Value}, RequirePlayer) ->
	case Attribute of
		turn -> attribute_check:player_turn_check(game_info:player_turn(PlayerOwnID), game_info:player_turn(PlayerOppID), Value, RequirePlayer);
		pharse -> attribute_check:player_pharse_check(game_info:player_pharse(PlayerOwnID), game_info:player_pharse(PlayerOppID), Value, RequirePlayer);
		mp -> attribute_check:player_mp_check(game_info:player_mp(PlayerOwnID), game_info:player_mp(PlayerOppID), Value, RequirePlayer);
		action -> attribute_check:player_action_check(game_info:player_action(PlayerOwnID), game_info:player_action(PlayerOppID), Value, RequirePlayer)
	end.
%===============================================================================
%===============================================================================
self_condition_check({Zone ,{PlayerOwnID, CardOrder, CardID}}, PlayerOppID, SkillId) ->
	[SelfConditionRequire] = self_card_condition_require(SkillId),
	check_all_self_require_attribute({Zone ,{PlayerOwnID, CardOrder, CardID}}, PlayerOppID, SelfConditionRequire, SkillId).
%===============================================================================
self_card_condition_require(SkillId) ->
	do(qlc:q([X#card_skill.owner_check|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])).
%===============================================================================
check_all_self_require_attribute(AbilOwnCardData, PlayerOppID, [ConditionHead|Tail], SkillId) ->
	Return = check_self:check_each_self_require_attribute(AbilOwnCardData, PlayerOppID, ConditionHead, SkillId),
	if 
		Return =:= true -> check_all_self_require_attribute(AbilOwnCardData, PlayerOppID, Tail, SkillId);
		Return =:= false -> self_mismatch
	end;
check_all_self_require_attribute(_, _, [], _) -> ok.
%===============================================================================
list_condition_check({CardZone, {PlayerOwnID, AbilOwnCardOrder, CardID }}, SkillId, PlayerOppID) -> %CardOrder, CardID, PlayerOwnID, PlayerOppID
	[{RequirePlayer, RequirePresentZone, RequireCardType}] = do(qlc:q([{X#card_skill.other_player_check,X#card_skill.other_present_check, X#card_skill.other_card_type_check} || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),       
	%io:format('RequirePlayer ~p~n',[RequirePlayer]),
	%io:format('RequirePresentZone ~p~n',[RequirePresentZone]),
	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	PlayerZoneList1 = skill_card_list:player_zone_list({RequirePlayer, RequirePresentZone, RequireCardType}, {PlayerOwnID, PlayerOppID}), % Return a list of cards of each player in require zone
	[SelfInclude] = do(qlc:q([X#card_skill.other_self_include_check || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	%io:format('SelfInclude ~p~n',[SelfInclude]),
	%io:format('PlayerZoneList1 ~p~n',[PlayerZoneList1]),
	if
		SelfInclude =:= n -> PlayerZoneList = PlayerZoneList1--[{CardZone, {PlayerOwnID, AbilOwnCardOrder, CardID}}];
		SelfInclude =/= n -> PlayerZoneList = PlayerZoneList1
	end,
	%io:format('PlayerZoneList ~p~n',[PlayerZoneList]),
	CheckedAll = check_all({CardZone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, SkillId, PlayerZoneList),
	%io:format('CheckedAll ~p~n',[CheckedAll]),
	[MatchCount] = do(qlc:q([X#card_skill.other_match_count || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	case is_number(MatchCount) of
		true ->
			if 
				  length(CheckedAll) >= MatchCount -> ok; % not yet include delete [] function
				  length(CheckedAll) < MatchCount -> other_card_count_mismatch
			end;
		false ->
			case MatchCount of
				more_than_owner ->
					OpponentCount = length(card_utility:get_all_card(PlayerOppID, seal_card, arena_zone)),
					OwnerCount = length(card_utility:get_all_card(PlayerOwnID, seal_card, arena_zone)),
					if 
						OpponentCount > OwnerCount -> ok;
						OpponentCount =< OwnerCount -> less_opponent_seal
					end;
				less_than_owner ->
					OpponentCount = length(card_utility:get_all_card(PlayerOppID, seal_card, arena_zone)),
					OwnerCount = length(card_utility:get_all_card(PlayerOwnID, seal_card, arena_zone)),
					if 
						OpponentCount < OwnerCount -> ok;
						OpponentCount >= OwnerCount -> more_opponent_seal
					end;
				{less_than, X} ->
					if
						length(CheckedAll) >= X -> not_less_than_X;
						length(CheckedAll) < X -> ok
					end;
				{equal_to, X} ->
					if
						length(CheckedAll) =/= X -> not_equal_to_X;
						length(CheckedAll) =:= X -> ok
					end
			end
	end.
%===============================================================================
check_all(_, _, []) -> [];
check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, SkillId, [PlayerZoneListHeadl|Tail]) ->
	%io:format('PlayerZoneListHeadl ~p~n',[PlayerZoneListHeadl]),
	[ConditionNeed] = do(qlc:q([X#card_skill.other_check|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	check_other:check_all_other_require_attribute({CardZone, {AbilOwnPlayerOwnID ,AbilOwnCardOrder, AbilOwnCardID}}, PlayerZoneListHeadl, ConditionNeed)++check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, SkillId, Tail).
%===============================================================================