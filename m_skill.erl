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
-module(m_skill).
-compile(export_all).
-import (mnesia_table, [do/1]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("record.hrl").
-include_lib("m_skill.hrl").

%===============================================================================
start_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, MskillNo, PlayerOppID) -> % check all possible conditions 
	%io:format('Start skill_effect_return ~n'),
	%io:format('CardID ~p~n', [CardID]),
	MskillList1 = do(qlc:q([X#mystic_skill.mskill_id || X <- mnesia:table(mystic_skill), X#mystic_skill.card_id =:= CardID, X#mystic_skill.mskill_no =:= MskillNo])),
	%io:format('MskillList ~p~n', [MskillList1]),
	if 
		MskillList1 =:= [] -> []; % if that CardID have no ability 
		MskillList1 =/= [] -> % if that CardID have ability no matter how many it have just start ability
			MskillList = function_utility:qsort(MskillList1),	
			m_skill_list_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, MskillList, PlayerOppID)
	end.
%===============================================================================
%For Loop		
m_skill_list_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, [MskillListHead|Tail], PlayerOppID)   -> 
	
			MskillGet = m_skill_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, MskillListHead, PlayerOppID),
			%io:format("Ability get is ~p~n", [AbilityGet]),
			MskillGet++m_skill_list_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, Tail, PlayerOppID);
m_skill_list_check(_, [], _) -> [].
%===============================================================================
m_skill_effect({CardZone, {PlayerOwnID, CardOrder, CardID }}, MskillId, PlayerOppID) ->
	[MskillNo] = do(qlc:q([X#mystic_skill.mskill_no || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	MskillConditionChecked = start_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, MskillNo, PlayerOppID),
	%io:format ("MskillConditionChecked ~p~n", [MskillConditionChecked]),
	%io:format ("MskillId ~p~n", [MskillId]),
	if 
		MskillConditionChecked =/= [ok] -> []; % if in the present condition no card can use ability then exit function
		MskillConditionChecked =:= [ok] ->
			m_skill_effect:m_skill_all_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, MskillId, PlayerOppID)
			%io:format ("Start OwnerTargetCheck ~n"),
	end.
%===============================================================================
start_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, MskillNo, PlayerOppID) -> % check all possible conditions 
	%io:format('Start skill_effect_return ~n'),
	%io:format('CardID ~p~n', [CardID]),
	MskillList1 = do(qlc:q([X#mystic_skill.mskill_id || X <- mnesia:table(mystic_skill), X#mystic_skill.card_id =:= CardID, X#mystic_skill.mskill_no =:= MskillNo])),
	%io:format('MskillList ~p~n', [MskillList1]),
	if 
		MskillList1 =:= [] -> Codition = []; % if that CardID have no ability 
		MskillList1 =/= [] -> % if that CardID have ability no matter how many it have just start ability
			MskillList = function_utility:qsort(MskillList1),		
			Codition = condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, MskillList, PlayerOppID),
			%io:format ("Codition ~p~n", [Codition]),
			function_utility:del(function_utility:qsort([Codition]))
	end,
	Result = [{ok, ok, ok, ok}]--Codition,
	if
		Result =:= [] -> BB = [ok];
		Result =/= [] -> BB = Codition
	end,
	%io:format('BB ~p~n', [BB]),
	BB.
%===============================================================================
%For Loop		
condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, [MskillId|Tail], PlayerOppID) ->
	MskillCheck = mskill_condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, MskillId, PlayerOppID),
	%io:format("Ability get is ~p~n", [AbilityGet]),
	MskillCheck++condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, Tail, PlayerOppID);
condition_check(_, [], _) -> [].
%===============================================================================
mskill_condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, MskillId, PlayerOppID) -> % compare require condition of ability with present condition(this function just return final comparition weather ok or mismatch)
	%io:format('skill_condition_check ~n'),	
	% first compare player require condition with player present condition
	[DoesItNeed]= do(qlc:q( [ X#mystic_skill.mskill_condition_need_check || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId ])), 
	%io:format('DoesItNeed ~p~n', [DoesItNeed]),	
		if
				DoesItNeed =:= n -> [{ok, ok, ok, ok}];
				DoesItNeed =:= y ->
					[PlayerOwnCheck] = do(qlc:q( [ X#mystic_skill.playerown_must_check || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId ])), 
					%io:format('PlayerOwnCheck ~p~n', [PlayerOwnCheck]),
					if 
						PlayerOwnCheck =:= n -> PlayerOwn = ok; % don't need to check player condition then return ok
						PlayerOwnCheck =:= y -> PlayerOwn = playerown_condition_check(PlayerOwnID, MskillId)  % need to check player condition then check then check
					end,
					[PlayerCheck1] = do(qlc:q( [ X#mystic_skill.player_must_check || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId ])), 
					%io:format('PlayerCheck1 ~p~n', [PlayerCheck1]),
					if 
						PlayerOwn =/= ok -> PlayerCheck = n;
						PlayerOwn =:= ok -> PlayerCheck = PlayerCheck1
					end,
					if 
						PlayerCheck =:= n -> Player = ok; % don't need to check player condition then return ok
						PlayerCheck =:= y -> Player = player_condition_check(PlayerOppID, PlayerOwnID, MskillId)  % need to check player condition then check then check
					end,
					[SelfCheck1] = do(qlc:q( [ X#mystic_skill.owner_must_check|| X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId ])),
					%io:format('SelfCheck1 ~p~n', [SelfCheck1]),
					if 
						Player =/= ok -> SelfCheck = n;
						Player =:= ok -> SelfCheck = SelfCheck1
					end,
					if 
						SelfCheck =:= n -> Self = ok; % don't need to check self condition then return no to do below statement
						SelfCheck =:= y -> Self = self_condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, PlayerOppID, MskillId)
					end,
					[OtherCheck1] = do(qlc:q( [ X#mystic_skill.other_must_check|| X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId ])),
					%io:format('OtherCheck1 ~p~n', [OtherCheck1]),
					if 
						Self =/= ok -> OtherCheck = n;
						Self =:= ok -> OtherCheck = OtherCheck1
					end,
					if 
				   	OtherCheck =:= n -> Other = ok;
						OtherCheck =:= y -> Other = list_condition_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, MskillId, PlayerOppID)% compare other card require condition to other card present condition then return ok or mismatch					
					end,
					[{PlayerOwn, Player, Self, Other}]
			end.
%===============================================================================
%===============================================================================
playerown_condition_check(PlayerOwnID, MskillId) ->
	[PlayerOwnCondition] =  player_own_condition_require(MskillId),
	check_all_palyerown_require_attribute(PlayerOwnID, PlayerOwnCondition).
%===============================================================================
%playerown_turn_check, playerown_pharse_check, playerown_mp_check, playerown_action_check
player_own_condition_require(MskillId) ->
	do(qlc:q([X#mystic_skill.playerown_check|| X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])).
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
player_condition_check(PlayerOppID, PlayerOwnID, MskillId) -> 
	[PlayerCondition] =  player_condition_require(MskillId),
	[RequirePlayer] = do(qlc:q([ X#mystic_skill.player_side_check || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	check_all_palyer_require_attribute(PlayerOwnID, PlayerOppID, PlayerCondition, RequirePlayer).
%===============================================================================
player_condition_require(MskillId) ->
	do(qlc:q([X#mystic_skill.player_check|| X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])).
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
self_condition_check({Zone ,{PlayerOwnID, CardOrder, CardID}}, PlayerOppID, MskillId) ->
	[SelfConditionRequire] = self_card_condition_require(MskillId),
	check_all_self_require_attribute({Zone ,{PlayerOwnID, CardOrder, CardID}}, PlayerOppID, SelfConditionRequire).
%===============================================================================
self_card_condition_require(MskillId) ->
	do(qlc:q([X#mystic_skill.owner_check|| X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])).
%===============================================================================
check_all_self_require_attribute(AbilOwnCardData, PlayerOppID, [ConditionHead|Tail]) ->
	Return = check_self:check_each_self_require_attribute(AbilOwnCardData, PlayerOppID, ConditionHead),
	if 
		Return =:= true -> check_all_self_require_attribute(AbilOwnCardData, PlayerOppID, Tail);
		Return =:= false -> self_mismatch
	end;
check_all_self_require_attribute(_, _, []) -> ok.
%===============================================================================
list_condition_check({CardZone, {PlayerOwnID, AbilOwnCardOrder, CardID }}, MskillId, PlayerOppID) -> %CardOrder, CardID, PlayerOwnID, PlayerOppID
	[{RequirePlayer, RequirePresentZone, RequireCardType}] = do(qlc:q([{X#mystic_skill.other_player_check,X#mystic_skill.other_present_check, X#mystic_skill.other_card_type_check} || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),       
	%io:format('RequirePlayer ~p~n',[RequirePlayer]),
	%io:format('RequirePresentZone ~p~n',[RequirePresentZone]),
	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	PlayerZoneList1 = skill_card_list:player_zone_list({RequirePlayer, RequirePresentZone, RequireCardType}, {PlayerOwnID, PlayerOppID}), % Return a list of cards of each player in require zone
	[SelfInclude] = do(qlc:q([X#mystic_skill.other_self_include_check || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	%io:format('SelfInclude ~p~n',[SelfInclude]),
	%io:format('PlayerZoneList1 ~p~n',[PlayerZoneList1]),
	if
		SelfInclude =:= n -> PlayerZoneList = PlayerZoneList1--[{CardZone, {PlayerOwnID, AbilOwnCardOrder, CardID}}];
		SelfInclude =/= n -> PlayerZoneList = PlayerZoneList1
	end,
	%io:format('PlayerZoneList ~p~n',[PlayerZoneList]),
	CheckedAll = check_all({CardZone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, MskillId, PlayerZoneList),
	%io:format('CheckedAll ~p~n',[CheckedAll]),
	[MatchCount] = do(qlc:q([X#mystic_skill.other_match_count || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
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
check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, MskillId, [PlayerZoneListHeadl|Tail]) ->
	%io:format('PlayerZoneListHeadl ~p~n',[PlayerZoneListHeadl]),
	[ConditionNeed] = do(qlc:q([X#mystic_skill.other_check|| X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	check_other:check_all_other_require_attribute({CardZone, {AbilOwnPlayerOwnID ,AbilOwnCardOrder, AbilOwnCardID}}, PlayerZoneListHeadl, ConditionNeed)++check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, MskillId, Tail).
%===============================================================================