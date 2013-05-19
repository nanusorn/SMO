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
-module(m_skill_target).
-import (mnesia_table, [do/1]).
-compile (export_all).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("m_skill.hrl").
%===============================================================================
playerown_skill_target_check(MskillId) -> 
	[Player1Effect ]= do(qlc:q( [ X#mystic_skill.playerown_have_effect|| X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId ])),
	if 
		Player1Effect =/= y -> [];
		Player1Effect =:= y -> 1
	end.
%===============================================================================
playeropp_skill_target_check(MskillId) -> 
	[Player2Effect] = do(qlc:q( [ X#mystic_skill.player_have_effect|| X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId ])),
	if 
		Player2Effect =/= y -> [];
		Player2Effect =:= y -> 1
	end.
%===============================================================================
owner_skill_target_check(MskillId) ->
	[OwnerEffect] = do(qlc:q( [ X#mystic_skill.owner_have_effect|| X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId ])),
	if 
		OwnerEffect =/= y -> [];
		OwnerEffect =:= y -> 1
	end.
%===============================================================================
other_skill_target_check({Zone, {PlayerOwnID, CardOrder, CardID}}, MskillId, PlayerOppID) ->	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	%io:format ("Start OtherTargetCheck ~n"),
	[OtherTargetHaveEffect] = do(qlc:q( [ X#mystic_skill.other_target_have_effect|| X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId ])),
	%io:format ("Start OtherTargetCheck ~n ~p~n",[OtherTargetHaveEffect]),
	if 
		OtherTargetHaveEffect =:= n -> [];
		OtherTargetHaveEffect =/= n -> 
			%[IsUntil] = do(qlc:q( [ X#mystic_skill.is_until|| X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId ])),
			%io:format('SIsUntil ~p~n',[IsUntil]),
			%if 
				%IsUntil =:= y -> [];
					  %io:format('Check by Requirement ~n'),
					  %requirement:card_requirement(MskillId);
				%IsUntil =/= y ->	
					list_target_check({Zone, {PlayerOwnID, CardOrder, CardID}}, MskillId, PlayerOppID)
			%end
	end.
%===============================================================================
list_target_check({Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, MskillId, PlayerOppID) ->
	%io:format('list_target_check {~p, {~p, ~p, ~p, ~p, ~p}} ~n',[Zone, PlayerOwnID, AbilOwnCardOrder, CardID, MskillId, PlayerOppID]),
	A = do(qlc:q([{X#mystic_skill.target_player_check, X#mystic_skill.target_present_check, X#mystic_skill.target_card_type_check} || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),       
	%io:format('all Require ~p~n', [A]),
	[{RequirePlayer, RequirePresentZone, RequireCardType}] = A,
	% retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	PlayerZoneList1 = skill_card_list:player_zone_list({RequirePlayer, RequirePresentZone, RequireCardType}, {PlayerOwnID, PlayerOppID}), % Return a list of cards of each player in require zone
	[SelfInclude] = do(qlc:q([X#mystic_skill.target_self_include_check || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	if
		SelfInclude =:= n -> PlayerZoneList = PlayerZoneList1--[{Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}];
		SelfInclude =/= n -> PlayerZoneList = PlayerZoneList1
	end,
	CheckAllResult = check_all({Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, MskillId, PlayerZoneList),
	%io:format('CheckAllResult ~p~n',[CheckAllResult]),
	CheckAllResult.
%===============================================================================
check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, MskillId, [PlayerZoneListHead|Tail]) ->
	[ConditionNeed] = do(qlc:q([X#mystic_skill.target_check || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	%io:format('ConditionNeed ~p~n',[ConditionNeed]),
	check_other:check_all_other_require_attribute({CardZone, {AbilOwnPlayerOwnID ,AbilOwnCardOrder, AbilOwnCardID}}, PlayerZoneListHead, ConditionNeed)++check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, MskillId, Tail);
check_all(_, _, []) -> [].
%===============================================================================
or_skill_target_check({Zone,{PlayerOwnID, CardOrder, CardID}}, MskillId, PlayerOppID) -> 
	[ResultEffect]= do(qlc:q( [ X#mystic_skill.or_target_have_effect|| X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId ])),
	if 
		ResultEffect =/= y -> [];
		ResultEffect =:= y -> or_list_target_check(Zone, {PlayerOwnID, CardOrder, CardID}, MskillId, PlayerOppID)
	end.
%===============================================================================
or_list_target_check(Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}, MskillId, PlayerOppID) ->
	%io:format('list_target_check {~p, {~p, ~p, ~p, ~p, ~p}} ~n',[Zone, PlayerOwnID, AbilOwnCardOrder, CardID, MskillId, PlayerOppID]),
	[{RequirePlayer, RequirePresentZone, RequireCardType}] = do(qlc:q([{X#mystic_skill.or_target_player_check, X#mystic_skill.or_target_present_check, X#mystic_skill.or_target_card_type_check} || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),       
			     % retreive whole card in every stage of the Game and check each card status wheater its matching to the ability requirement
	PlayerZoneList1 = skill_card_list:player_zone_list({RequirePlayer, RequirePresentZone, RequireCardType}, {PlayerOwnID, PlayerOppID}), % Return a list of cards of each player in require zone
	[SelfInclude] = do(qlc:q([X#mystic_skill.or_target_card_type_check || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	if
		SelfInclude =:= n -> PlayerZoneList = PlayerZoneList1--[{Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}];
		SelfInclude =/= n -> PlayerZoneList = PlayerZoneList1
	end,
	CheckAllResult = or_check_all({Zone, {PlayerOwnID, AbilOwnCardOrder, CardID}}, MskillId, PlayerZoneList),
	%io:format('CheckAllResult ~p~n',[CheckAllResult]),
	CheckAllResult.
%===============================================================================
or_check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, MskillId, [PlayerZoneListHead|Tail]) ->
	[ConditionNeed] = do(qlc:q([X#mystic_skill.or_check || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	%io:format('ConditionNeed ~p~n',[ConditionNeed]),
	check_other:check_all_other_require_attribute({CardZone, {AbilOwnPlayerOwnID ,AbilOwnCardOrder, AbilOwnCardID}}, PlayerZoneListHead, ConditionNeed)++or_check_all({CardZone, {AbilOwnPlayerOwnID, AbilOwnCardOrder, AbilOwnCardID}}, MskillId, Tail);
or_check_all(_, _, []) -> [].
%===============================================================================