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
-module(s_skill_effect).
-import (mnesia_table, [do/1]).
-export([s_skill_all_effect/3]).
%-compile (export_all).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("record.hrl").
-include_lib("s_skill.hrl").
%===============================================================================
playerown_effect(SkillId) -> 
	[{Duration, PlayerOwnEffect}] = do(qlc:q([{X#card_skill.playerown_duration_effect,
																									X#card_skill.playerown_effect
																								 }|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	{PlayerOwnEffect, Duration}.
%===============================================================================
player_effect(SkillId) ->
	[{Duration, PlayerEffect}] = do(qlc:q([{X#card_skill.player_duration_effect, 
																						 X#card_skill.player_effect
																					  }|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	{PlayerEffect, Duration}.
%===============================================================================
self_effect(SkillId) ->
	[{Duration, SelfEffect}] = do(qlc:q([{X#card_skill.owner_duration_effect,
																					X#card_skill.owner_effect
																				  }|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	{SelfEffect, Duration}.
%===============================================================================	
other_effect(SkillId) ->
	[{Duration, OtherEffect}] = do(qlc:q([{X#card_skill.other_duration_effect, 
																						X#card_skill.other_effect
																					  } || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	{OtherEffect , Duration}.
%===============================================================================
or_effect(SkillId) ->
	[{Duration, OrEffect}] = do(qlc:q([{X#card_skill.or_duration, 
																				 X#card_skill.or_result_effect 
																				}|| X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	{OrEffect , Duration}.
%===============================================================================
s_skill_all_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, SkillId, PlayerOppID) ->
	OwnerTargetChecked = s_skill_target:playerown_skill_target_check(SkillId),
	if
		OwnerTargetChecked =:= [] -> POwnEffect = [];
		OwnerTargetChecked =/= [] -> 
			{OwEffect, OwDuration} = playerown_effect(SkillId),
			POwnEffect = [{1, {PlayerOwnID, CardOrder, CardID, SkillId}, [PlayerOwnID], OwEffect, OwDuration}]
	end,
 	OppTargetChecked = s_skill_target:playeropp_skill_target_check({PlayerOwnID, CardOrder, CardID}, PlayerOppID, SkillId),
	if
	 	OppTargetChecked =:= [] -> POppEffect = [];
		true -> 
			{OpEffect, OpDuration} = player_effect(SkillId),
			POppEffect = [{2, {PlayerOwnID, CardOrder, CardID, SkillId}, [OppTargetChecked], OpEffect, OpDuration}]
	end,
 	SelfTargetChecked = s_skill_target:owner_skill_target_check({CardZone, {PlayerOwnID, CardOrder, CardID}}, SkillId),  % if in the present condition this card can use ability then check which card will take this effects
	if
	 	SelfTargetChecked =:= [] -> SelfEffect = [];
		SelfTargetChecked =/= [] -> 
			{SEffect, SDuration} = self_effect(SkillId),
			SelfEffect = [{3, {PlayerOwnID, CardOrder, CardID, SkillId}, SelfTargetChecked, SEffect, SDuration }]
	end,
 	OtherTargetChecked = s_skill_target:other_skill_target_check({CardZone,{PlayerOwnID, CardOrder, CardID}}, SkillId, PlayerOppID),
	if
	 	OtherTargetChecked =:= [] -> OtherEffect = [];
		OtherTargetChecked =/= [] -> 
		 	{OEffect, ODuration} = other_effect(SkillId) ,
			%{FxType, SAmount, Effect} = OEffect,
			%OtherEffect = [{4, {PlayerOwnID, CardOrder, CardID, SkillId}, OtherTargetChecked, {FxType, SAmount, check_charm(Effect)}, ODuration }]
			OtherEffect = [{4, {PlayerOwnID, CardOrder, CardID, SkillId}, OtherTargetChecked, OEffect, ODuration }]
	end,
	OrTargetChecked = s_skill_target:or_skill_target_check({CardZone,{PlayerOwnID, CardOrder, CardID}}, SkillId, PlayerOppID),
	if
	 	OrTargetChecked =:= [] -> OrEffect = [];
		OrTargetChecked =/= [] ->
		 	{OrEffect, OrDuration} = or_effect(SkillId) ,
			OrEffect = [{5, {PlayerOwnID, CardOrder, CardID, SkillId}, OrTargetChecked, OrEffect, OrDuration }]
	end,
	%[IfClues] = do(qlc:q([{X#card_skill.then_assign, X#card_skill.then_do} || X <- mnesia:table(card_skill), X#card_skill.skill_id =:= SkillId])),
	%io:format ("Effect of skill are [~p, ~p]~n ", [IfClues, POwnEffect++POppEffect++SelfEffect++OtherEffect++OrEffect]),
 	%[IfClues, POwnEffect++POppEffect++SelfEffect++OtherEffect++OrEffect].
	POwnEffect++POppEffect++SelfEffect++OtherEffect++OrEffect.
%===============================================================================	