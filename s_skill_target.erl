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
-module(s_skill_target).
-import (mnesia_table, [do/1]).
-compile (export_all).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("s_skill.hrl").
%===============================================================================
playerown_skill_target_check(SkillId) -> 
	[Player1Effect ]= do(qlc:q( [ X#card_skill.playerown_have_effect|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])),
	if 
		Player1Effect =/= y -> [];
		Player1Effect =:= y -> 1
	end.
%===============================================================================
playeropp_skill_target_check({PlayerOwnID, CardOrder, CardID}, PlayerOppID, SkillId) -> 
	[Player2Effect] = do(qlc:q( [ X#card_skill.player_have_effect|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])),
	if 
		Player2Effect =/= y -> PlayerPid = [];
		true -> 
			[Player] = do(qlc:q([X#card_skill.player_which_got_effect|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
			{ControlPid, UnconPid, ReqPlayer} = attribute_check:check_controller({PlayerOwnID, CardOrder, CardID}, PlayerOppID, Player),
			case ReqPlayer of
				opponent ->	PlayerPid = UnconPid;
				owner -> PlayerPid = ControlPid
			end
	end,
	PlayerPid.
%===============================================================================
owner_skill_target_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, SkillId) ->
	[OwnerEffect] = do(qlc:q( [ X#card_skill.owner_have_effect|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])),
	if 
		OwnerEffect =/= y -> [];
		OwnerEffect =:= y -> 
			[Condition] = do(qlc:q( [ X#card_skill.owner_effect|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])),
			case Condition of 
				{do_not_need_select, _, _} ->	[{PlayerOwnID, CardOrder, CardID}];
				{Else, _, _} -> self_condition_target({CardZone, {PlayerOwnID, CardOrder, CardID}}, Else);
				_ -> []
			end
	end.
%===============================================================================
self_condition_target({CardZone, {PlayerOwnID, CardOrder, CardID}}, Else) ->
	case Else of
		player_select_exact_condition_curse ->
			game_info:card_curse({CardZone, {PlayerOwnID, CardOrder, CardID}})
	end.
%===============================================================================
other_skill_target_check({Zone, {PlayerOwnID, CardOrder, CardID}}, SkillId, PlayerOppID) ->	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	[OtherTargetHaveEffect] = do(qlc:q( [ X#card_skill.other_target_have_effect|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])),
	if 
		OtherTargetHaveEffect =/= y -> [];
		OtherTargetHaveEffect =:= y -> 
			%ListOfTarget = 
			list_target_check({Zone, {PlayerOwnID, CardOrder, CardID}}, SkillId, PlayerOppID)
			%[Condition] = do(qlc:q( [ X#card_skill.other_effect|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])),
			%case Condition of 
				%{do_not_need_select, _, _} ->	ListOfTarget;
				%{player_select_exact_target, _, _} ->	ListOfTarget;
				%{player_select_exact_condition_target, _, _} ->	ListOfTarget;
				%{{depend_on, _}, _, _} -> ListOfTarget;
				%{Else, _, _} -> other_condition_target(ListOfTarget, Else)
			%end
	end.
%===============================================================================
list_target_check({Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, SkillId, PlayerOppID) ->
	%io:format('list_target_check {~p, {~p, ~p, ~p, ~p, ~p}} ~n',[Zone, PlayerOwnID, AbilOwnCardOrder, CardID, SkillId, PlayerOppID]),
	[{RequirePlayer, ReqZone, ReqCardType}] = do(qlc:q([{X#card_skill.target_player_check, 
									 																						X#card_skill.target_present_check, 
																															X#card_skill.target_card_type_check
																														  } || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	{PlayerPid, PlayerOppPid, ReqPlayer} = attribute_check:check_controller({PlayerOwnID, AbilOwnCardOrder, CardID}, PlayerOppID, RequirePlayer),
	PlayerZoneList1 = skill_card_list:player_zone_list({ReqPlayer, ReqZone, ReqCardType}, {PlayerPid, PlayerOppPid}), % Return a list of cards of each player in require zone
	[SelfInclude] = do(qlc:q([X#card_skill.target_self_include_check || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	if
		SelfInclude =:= n -> PlayerZoneList = PlayerZoneList1--[{Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}];
		true -> PlayerZoneList = PlayerZoneList1
	end,
	CheckAllResult = check_all({Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, SkillId, PlayerZoneList),
	%io:format('CheckAllResult ~p~n',[CheckAllResult]),
	CheckAllResult.
%===============================================================================
%other_condition_target([{PlayerPid, CardOrder, CardID}|Tail], Condition) -> 
	%case Condition of 
		%player_select_exact_condition_curse -> 
			%CardZone = card_utility:check_card_zone(PlayerPid, CardOrder, CardID),
			%AllCurse = game_info:card_curse({CardZone, {PlayerPid, CardOrder, CardID}})++other_condition_target(Tail, Condition),
			%function_utility:del(function_utility:qsort(AllCurse))
	%end;
%other_condition_target([], _) -> [].
%===============================================================================
check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, SkillId, [PlayerZoneListHead|Tail]) ->
	[ConditionNeed] = do(qlc:q([X#card_skill.target_check || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	check_other:check_all_other_require_attribute({CardZone, {AbilOwnPlayerOwnID ,AbilOwnCardOrder, AbilOwnCardID}}, PlayerZoneListHead, ConditionNeed)++check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, SkillId, Tail);
check_all(_, _, []) -> [].
%===============================================================================	
or_skill_target_check({Zone,{PlayerOwnID, CardOrder, CardID}}, SkillId, PlayerOppID) -> 
	[ResultEffect]= do(qlc:q( [ X#card_skill.or_target_have_effect|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId ])),
	if 
		ResultEffect =/= y -> [];
		ResultEffect =:= y -> or_list_target_check(Zone, {PlayerOwnID, CardOrder, CardID}, SkillId, PlayerOppID)
	end.
%===============================================================================
or_list_target_check(Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}, SkillId, PlayerOppID) ->
	%io:format('list_target_check {~p, {~p, ~p, ~p, ~p, ~p}} ~n',[Zone, PlayerOwnID, AbilOwnCardOrder, CardID, SkillId, PlayerOppID]),
	[{RequirePlayer, RequirePresentZone, RequireCardType}] = do(qlc:q([{X#card_skill.or_target_player_check, X#card_skill.or_target_present_check, X#card_skill.or_target_card_type_check} || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),       
	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	PlayerZoneList1 = skill_card_list:player_zone_list({RequirePlayer, RequirePresentZone, RequireCardType}, {PlayerOwnID, PlayerOppID}), % Return a list of cards of each player in require zone
	[SelfInclude] = do(qlc:q([X#card_skill.or_target_card_type_check || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	if
		SelfInclude =:= n -> PlayerZoneList = PlayerZoneList1--[{Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}];
		SelfInclude =/= n -> PlayerZoneList = PlayerZoneList1
	end,
	CheckAllResult = or_check_all({Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, SkillId, PlayerZoneList),
	%io:format('CheckAllResult ~p~n',[CheckAllResult]),
	CheckAllResult.
%===============================================================================
or_check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, SkillId, [PlayerZoneListHead|Tail]) ->
	[ConditionNeed] = do(qlc:q([X#card_skill.or_check || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	check_other:check_all_other_require_attribute({CardZone, {AbilOwnPlayerOwnID ,AbilOwnCardOrder, AbilOwnCardID}}, PlayerZoneListHead, ConditionNeed)++or_check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, SkillId, Tail);
or_check_all(_, _, []) -> [].
%===============================================================================