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
-module(s_skill).
-compile(export_all).
-import (mnesia_table, [do/1]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("record.hrl").
-include_lib("s_skill.hrl").

start({CardZone, {OwnerPid, CardOrder, CardID}}, SkillIdentify, PlayerOppID) ->
	case is_number(SkillIdentify) of
		true -> start_effect({CardZone, {OwnerPid, CardOrder, CardID}}, SkillIdentify, PlayerOppID);
		false -> s_added_skill:start_effect({CardZone, {OwnerPid, CardOrder, CardID}}, SkillIdentify, PlayerOppID)
	end.
%===============================================================================
start_effect({CardZone, {OwnerPid, CardOrder, CardID}}, S_SkillNo, PlayerOppID) -> % check all possible conditions 
	SkillList1 = do(qlc:q([X#card_skill.skill_id || X <- mnesia:table(card_skill), X#card_skill.card_id =:= CardID, X#card_skill.skill_no =:= S_SkillNo])),
	if 
		SkillList1 =:= [] -> []; % if that CardID have no ability 
		SkillList1 =/= [] -> % if that CardID have ability no matter how many it have just start ability
			SkillList = function_utility:qsort(SkillList1),	
			s_skill_list_check({CardZone, {OwnerPid, CardOrder, CardID}}, SkillList, PlayerOppID)
	end.
%===============================================================================
%For Loop
s_skill_list_check(_, [], _) -> [];
s_skill_list_check({CardZone, {OwnerPid, CardOrder, CardID}}, [SkillListHead|Tail], PlayerOppID)   -> 
	[Result] = do(qlc:q([X#card_skill.then_assign|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillListHead])),
	case Result of
		null ->
			SkillGet = s_skill_effect:s_skill_all_effect({CardZone, {OwnerPid, CardOrder, CardID}}, SkillListHead, PlayerOppID),
			SkillGet++s_skill_list_check({CardZone, {OwnerPid, CardOrder, CardID}}, Tail, PlayerOppID);
		_ -> s_skill_effect:s_skill_all_effect({CardZone, {OwnerPid, CardOrder, CardID}}, SkillListHead, PlayerOppID)
	end.
%===============================================================================
start_id_check({CardZone, {OwnerPid, CardOrder, CardID}}, SskillList1, PlayerOppID) -> % check all possible conditions 
	SskillList = function_utility:qsort(SskillList1),		
	SkillId = condition_check({CardZone, {OwnerPid, CardOrder, CardID}}, SskillList, PlayerOppID, []),
	function_utility:del(function_utility:qsort(SkillId)),
	if
		SkillId =:= [] -> {error, no_skill_match};
		true -> {ok, SkillId}
	end.
%===============================================================================
start_check({CardZone, {OwnerPid, CardOrder, CardID}}, SskillNo, PlayerOppID) -> % check all possible conditions 
	SskillList1 = do(qlc:q([X#card_skill.skill_id || X <- mnesia:table(card_skill), X#card_skill.card_id =:= CardID, X#card_skill.skill_no =:= SskillNo])),
	SkillIdMatch =
	if 
		SskillList1 =:= [] -> []; % if that CardID have no ability 
		SskillList1 =/= [] -> % if that CardID have ability no matter how many it have just start ability
			SskillList = function_utility:qsort(SskillList1),		
			SkillId = condition_check({CardZone, {OwnerPid, CardOrder, CardID}}, SskillList, PlayerOppID, []),
			function_utility:del(function_utility:qsort(SkillId))
	end,
	if
		SkillIdMatch =:= [] -> {error, no_skill_match};
		true -> {ok, SkillIdMatch}
	end.
%===============================================================================
start_skill_check({CardZone, {OwnerPid, CardOrder, CardID}}, {SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, SskillNo}, PlayerOppID) -> % check all possible conditions 
	SskillList1 = do(qlc:q([X#card_skill.skill_id || X <- mnesia:table(card_skill), X#card_skill.card_id =:= SkillOwnerID, X#card_skill.skill_no =:= SskillNo])),
	SkillIdMatch =
	if 
		SskillList1 =:= [] -> []; % if that CardID have no ability 
		SskillList1 =/= [] -> % if that CardID have ability no matter how many it have just start ability
			SskillList = function_utility:qsort(SskillList1),		
			SkillId = condition_check({CardZone, {OwnerPid, CardOrder, CardID}}, SskillList, PlayerOppID, []),
			function_utility:del(function_utility:qsort(SkillId))
	end,
	if
		SkillIdMatch =:= [] -> {error, no_skill_match};
		true -> {ok, SkillIdMatch}
	end.
%===============================================================================
condition_check({CardZone, {OwnerPid, CardOrder, CardID}}, [SskillId|Tail], PlayerOppID, IDReturn) ->
	%io:format('{CardZone:~p, {OwnerPid:~p, CardOrder:~p, CardID:~p}}, SskillId:~p, PlayerOppID:~p~n', [CardZone, OwnerPid, CardOrder, CardID, SskillId, PlayerOppID]),
	
	% check skill condition ของ SkillId นั้น ถ้่า condition ถูกต้่อง จะ return ค่าเป็น [{ok, ok, ok, ok}]
	SskillCheck = skill_condition_check({CardZone, {OwnerPid, CardOrder, CardID}}, SskillId, PlayerOppID),
	case SskillCheck of
		[{ok, ok, ok, ok}] ->
				% check ว่า ต่อจาก SkillId นี้แล้ว ต้องทำ Skill ประเภท จากนั้นหรือเปล่า
			ThenAssignSkilId = do(qlc:q([X#card_skill.then_assign|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SskillId])),
			case ThenAssignSkilId of
				% ถ้าไม่ใช่ Skill จากนั้นให้ ทำ SkillId ต่อไปในขั้นตอนเดียวกันเลย
				[null] -> condition_check({CardZone, {OwnerPid, CardOrder, CardID}}, Tail, PlayerOppID, IDReturn ++ [SskillId]);
				% ถ้าเป็น Skill จากนั้น ต้องทำ SkillId นี้ให้เสร็จก่อนจึงจะกลับมาทำ 
				_ -> IDReturn ++ [SskillId]
			end;
		_Mismatch -> %smo_logger:fmsg("skill id ~p mismatch case ~p~n", [SskillId, _Mismatch]), []
			[ThenAssignSkilId] = do(qlc:q([X#card_skill.then_assign|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SskillId])),
			case ThenAssignSkilId of
				% ถ้าไม่ใช่ Skill จากนั้นให้ ทำ SkillId ต่อไปในขั้นตอนเดียวกันเลย
				null -> condition_check({CardZone, {OwnerPid, CardOrder, CardID}}, Tail, PlayerOppID, IDReturn);
				% ถ้าเป็น Skill จากนั้น ต้องทำ SkillId นี้ให้เสร็จก่อนจึงจะกลับมาทำ 
				[{then_do, ThenAssign}] -> condition_check({CardZone, {OwnerPid, CardOrder, CardID}}, Tail--ThenAssign, PlayerOppID, IDReturn)
			end
	end;
condition_check(_, [], _, IDReturn) -> IDReturn.
%===============================================================================
skill_condition_check({CardZone, {OwnerPid, CardOrder, CardID}}, SkillId, PlayerOppID) -> % compare require condition of ability with present condition(this function just return final comparition weather ok or mismatch)
	% first compare player require condition with player present condition
	%io:format('{CardZone:~p, {OwnerPid:~p, CardOrder:~p, CardID:~p}}, SkillId:~p, PlayerOppID:~p~n', [CardZone, OwnerPid, CardOrder, CardID, SkillId, PlayerOppID]),
	[DoesItNeed]= do(qlc:q( [ X#card_skill.skill_condition_need_check || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])), 
		if
				DoesItNeed =:= n -> [{ok, ok, ok, ok}];
				DoesItNeed =:= y ->
					[PlayerOwnCheck] = do(qlc:q( [ X#card_skill.playerown_must_check || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])), 
					%io:format('PlayerOwnCheck ~p~n', [PlayerOwnCheck]),
					if 
						PlayerOwnCheck =:= n -> PlayerOwn = ok; % don't need to check player condition then return ok
						PlayerOwnCheck =:= y -> PlayerOwn = playerown_condition_check(OwnerPid, SkillId)  % need to check player condition then check then check
					end,
					[PlayerCheck1] = do(qlc:q( [ X#card_skill.player_must_check || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])), 
					%io:format('PlayerCheck1 ~p~n', [PlayerCheck1]),
					if 
						PlayerOwn =/= ok -> PlayerCheck = n;
						PlayerOwn =:= ok -> PlayerCheck = PlayerCheck1
					end,
					if 
						PlayerCheck =:= n -> Player = ok; % don't need to check player condition then return ok
						PlayerCheck =:= y -> Player = player_condition_check({OwnerPid, CardOrder, CardID}, PlayerOppID, SkillId)  % need to check player condition then check then check
					end,
					[SelfCheck1] = do(qlc:q( [ X#card_skill.owner_must_check|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])),
					%io:format('SelfCheck1 ~p~n', [SelfCheck1]),
					if 
						Player =/= ok -> SelfCheck = n;
						Player =:= ok -> SelfCheck = SelfCheck1
					end,
					if 
						SelfCheck =:= n -> Self = ok; % don't need to check self condition then return no to do below statement
						SelfCheck =:= y -> Self = self_condition_check({CardZone, {OwnerPid, CardOrder, CardID}}, PlayerOppID, SkillId)
					end,
					[OtherCheck1] = do(qlc:q( [ X#card_skill.other_must_check|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])),
					%io:format('OtherCheck1 ~p~n', [OtherCheck1]),
					if 
						Self =/= ok -> OtherCheck = n;
						Self =:= ok -> OtherCheck = OtherCheck1
					end,
					if 
				   	OtherCheck =:= n -> Other = ok;
						OtherCheck =:= y -> Other = list_condition_check({CardZone, {OwnerPid, CardOrder, CardID}}, SkillId, PlayerOppID)% compare other card require condition to other card present condition then return ok or mismatch					
					end,
					[{PlayerOwn, Player, Self, Other}]
			end.
%===============================================================================
%===============================================================================
playerown_condition_check(OwnerPid, SkillId) ->
	[PlayerOwnCondition] =  player_own_condition_require(SkillId),
	check_all_palyerown_require_attribute(OwnerPid, PlayerOwnCondition).
%===============================================================================
%playerown_turn_check, playerown_pharse_check, playerown_mp_check, playerown_action_check
player_own_condition_require(SkillId) ->
	do(qlc:q([X#card_skill.playerown_check|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])).
%===============================================================================
check_all_palyerown_require_attribute(OwnerPid, [ConditionHead|Tail]) ->
	Return = check_each_palyerown_require_attribute(OwnerPid, ConditionHead),
	if 
		Return =:= true -> check_all_palyerown_require_attribute(OwnerPid, Tail);
		true -> Return
	end;
check_all_palyerown_require_attribute(_, []) -> ok.
%===============================================================================
check_each_palyerown_require_attribute(OwnerPid, {Attribute, Value}) ->
	case Attribute of
		turn -> attribute_check:player_turn_check(game_info:player_turn(OwnerPid), Value);
		pharse -> attribute_check:player_pharse_check(game_info:player_pharse(OwnerPid), Value);
		mp -> attribute_check:player_mp_check(game_info:player_mp(OwnerPid), Value);
		action -> attribute_check:player_action_check(game_info:player_action(OwnerPid), Value)
	end.
%===============================================================================
player_condition_check({OwnerPid, CardOrder,  CardID}, PlayerOppID, SkillId) -> 
	[{PlayerCondition, RequirePlayer}] =  do(qlc:q([{X#card_skill.player_check,
																											X#card_skill.player_side_check
																										  }|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	{PlayerPid, PlayerOppPid, ReqPlayer} = attribute_check:check_controller({OwnerPid, CardOrder, CardID}, PlayerOppID, RequirePlayer),
	check_all_palyer_require_attribute(PlayerPid, PlayerOppPid, PlayerCondition, ReqPlayer).
%===============================================================================
check_all_palyer_require_attribute(PlayerPid, PlayerOppPid, [ConditionHead|Tail], RequirePlayer) ->
	Return = check_each_palyer_require_attribute(PlayerPid, PlayerOppPid, ConditionHead, RequirePlayer),
	if 
		Return =:= true -> check_all_palyer_require_attribute(PlayerPid, PlayerOppPid, Tail, RequirePlayer);
		true -> Return
	end;
check_all_palyer_require_attribute(_, _, [], _) -> ok.
%===============================================================================
check_each_palyer_require_attribute(OwnerPid, PlayerOppID, {Attribute, Value}, RequirePlayer) ->
	case Attribute of
		turn -> attribute_check:player_turn_check(game_info:player_turn(OwnerPid), game_info:player_turn(PlayerOppID), Value, RequirePlayer);
		pharse -> attribute_check:player_pharse_check(game_info:player_pharse(OwnerPid), game_info:player_pharse(PlayerOppID), Value, RequirePlayer);
		mp -> attribute_check:player_mp_check(game_info:player_mp(OwnerPid), game_info:player_mp(PlayerOppID), Value, RequirePlayer);
		action -> attribute_check:player_action_check(game_info:player_action(OwnerPid), game_info:player_action(PlayerOppID), Value, RequirePlayer)
	end.
%===============================================================================
self_condition_check({Zone ,{OwnerPid, CardOrder, CardID}}, PlayerOppID, SkillId) ->
	[SelfConditionRequire] = do(qlc:q([X#card_skill.owner_check|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	check_all_self_require_attribute({Zone ,{OwnerPid, CardOrder, CardID}}, PlayerOppID, SelfConditionRequire, SkillId).
%===============================================================================
check_all_self_require_attribute(AbilOwnCardData, PlayerOppID, [ConditionHead|Tail], SkillId) ->
	Return = check_self:check_each_self_require_attribute(AbilOwnCardData, PlayerOppID, ConditionHead, SkillId, use_skill),
	if 
		Return =:= true -> check_all_self_require_attribute(AbilOwnCardData, PlayerOppID, Tail, SkillId);
		true -> Return
	end;
check_all_self_require_attribute(_, _, [], _) -> ok.
%===============================================================================
list_condition_check({CardZone, {OwnerPid, AbilOwnCardOrder, CardID}}, SkillId, PlayerOppID) ->
	[{RequirePlayer, ReqZone, ReqCardType}] = do(qlc:q([{X#card_skill.other_player_check,
																															X#card_skill.other_present_check, 
																															X#card_skill.other_card_type_check
																														} || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),       
	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	 {PlayerPid, PlayerOppPid, ReqPlayer} = attribute_check:check_controller({OwnerPid, AbilOwnCardOrder, CardID}, PlayerOppID, RequirePlayer),
	% Return a list of cards of each player in require zone
	PlayerZoneList1 = skill_card_list:player_zone_list({ReqPlayer, ReqZone, ReqCardType}, {PlayerPid, PlayerOppPid}), 
	[SelfInclude] = do(qlc:q([X#card_skill.other_self_include_check || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	if
		SelfInclude =:= n -> PlayerZoneList = PlayerZoneList1--[{CardZone, {OwnerPid, AbilOwnCardOrder, CardID}}];
		true -> PlayerZoneList = PlayerZoneList1
	end,
	[ConditionNeed] = do(qlc:q([X#card_skill.other_check|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	[MatchCount] = do(qlc:q([X#card_skill.other_match_count || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	check_all({CardZone, {OwnerPid, AbilOwnCardOrder, CardID}}, ConditionNeed, PlayerZoneList, MatchCount).
%============================================================
check_all(_, _, [], _) ->
	case stack_pool:get_last_stack(self(), card_match_skill) of
		{ok, _ } -> stack_pool:remove_stack_option(self(), card_match_skill), card_require_not_enough;
		_ -> card_require_not_enough
	end;
check_all({CardZone, {AbilOwnOwnerPid, AbilOwnCardOrder, AbilOwnCardID}}, ConditionNeed, [PlayerZoneListHead|Tail], MatchCount) ->
	CardMatch = check_other:check_all_other_require_attribute({CardZone, {AbilOwnOwnerPid ,AbilOwnCardOrder, AbilOwnCardID}}, PlayerZoneListHead, ConditionNeed),
	stack_pool:add_stack_option_field(self(), card_match_skill, CardMatch),
	{ok, AllCardMatch} = stack_pool:get_last_stack(self(), card_match_skill),
	case attribute_check:check_count(AllCardMatch, MatchCount) of
		true -> stack_pool:remove_stack_option(self(), card_match_skill), ok;
		_ -> check_all({CardZone, {AbilOwnOwnerPid, AbilOwnCardOrder, AbilOwnCardID}}, ConditionNeed, Tail, MatchCount) 
	end.
%===============================================================================
% start_check_then({CardZone, {OwnerPid, CardOrder, CardID}}, SkillId, PlayerOppID) ->
	% ThenDoSkilId = do(qlc:q([X#card_skill.then_do || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	% case ThenDoSkilId of
		% [{_, NextList}] ->
			% SskillList = function_utility:qsort([SkillId]++NextList),		
			% Condition = condition_check({CardZone, {OwnerPid, CardOrder, CardID}}, SskillList, PlayerOppID, []);
			% %function_utility:del(function_utility:qsort([Condition]))
		% _ -> SskillList = [SkillId],
			% Condition = condition_check({CardZone, {OwnerPid, CardOrder, CardID}}, SskillList, PlayerOppID)
	% end,
	% case Condition of
		% [] -> {error, Condition};
		% _  -> {ok, SskillList}
	% end.
%===============================================================================
