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
-module(m_skill_effect).
-import (mnesia_table, [do/1]).
-compile (export_all).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("record.hrl").
-include_lib("m_skill.hrl").
%===============================================================================
playerown_effect(MskillId) -> 
	[Duration] = do(qlc:q([ X#mystic_skill.playerown_duration_effect || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	[PlayerOwnEffect] = do(qlc:q([ X#mystic_skill.playerown_effect || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	{PlayerOwnEffect, Duration}.
%===============================================================================
player_effect(MskillId) ->
	[Duration] = do(qlc:q([ X#mystic_skill.player_duration_effect || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	[PlayerEffect] = do(qlc:q([ X#mystic_skill.player_effect || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	{PlayerEffect, Duration}.
%===============================================================================
self_effect(MskillId) ->
	[Duration] = do(qlc:q([ X#mystic_skill.owner_duration_effect || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	[SelfEffect] = do(qlc:q([ X#mystic_skill.owner_effect || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	{SelfEffect, Duration}.
%===============================================================================	
other_effect(MskillId) ->
	[Duration] = do(qlc:q([ X#mystic_skill.other_duration_effect || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	[OtherEffect] = do(qlc:q([ X#mystic_skill.other_effect || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	{OtherEffect , Duration}.
%===============================================================================
or_effect(MskillId) ->
	[Duration] = do(qlc:q([ X#mystic_skill.or_duration || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	[OrEffect] = do(qlc:q([ X#mystic_skill.or_result_effect || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	%io:format('Duration ~p~n', [Duration]),
	%io:format('ResultEffect ~p~n', [OrEffect]),
	{OrEffect , Duration}.
%===============================================================================
s_skill_all_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, MskillId, PlayerOppID) ->
	OwnerTargetChecked = m_skill_target:playerown_skill_target_check(MskillId),
	if
		OwnerTargetChecked =:= [] -> POwnEffect = [{}];
		OwnerTargetChecked =/= [] -> 
			{OwEffect, OwDuration} = playerown_effect(MskillId),
			POwnEffect = [{{PlayerOwnID, CardOrder, CardID, MskillId}, OwEffect, OwDuration}]
	end,
 %io:format ("Start OppTargetCheck ~n"),
 	OppTargetChecked = m_skill_target:playeropp_skill_target_check(MskillId),
	if
	 	OppTargetChecked =:= [] -> POppEffect = [{}];
		OppTargetChecked =/= [] -> 
			{OpEffect, OpDuration} = player_effect(MskillId),
			POppEffect = [{{PlayerOwnID, CardOrder, CardID, MskillId}, OpEffect, OpDuration }]
	end,
 %io:format ("Start SelfTargetCheck ~n"),
 	SelfTargetChecked = m_skill_target:owner_skill_target_check(MskillId),  % if in the present condition this card can use ability then check which card will take this effects
	if
	 	SelfTargetChecked =:= [] -> SelfEffect = [{}];
		SelfTargetChecked =/= [] -> 
			{SEffect, SDuration} = self_effect(MskillId),
			SelfEffect = [{{PlayerOwnID, CardOrder, CardID, MskillId},[{PlayerOwnID, CardOrder, CardID}],SEffect, SDuration }]
	end,
 %io:format ("Start OtherTargetCheck ~n"),
 	OtherTargetChecked = m_skill_target:other_skill_target_check({CardZone,{PlayerOwnID, CardOrder, CardID}}, MskillId, PlayerOppID),
	if
	 	OtherTargetChecked =:= [] -> OtherEffect = [{}];
		OtherTargetChecked =/= [] -> 
		 	{OEffect, ODuration} = other_effect(MskillId) ,
			OtherEffect = [{{PlayerOwnID, CardOrder, CardID, MskillId}, OtherTargetChecked, OEffect, ODuration }]
	end,
	OrTargetChecked = m_skill_target:or_skill_target_check({CardZone,{PlayerOwnID, CardOrder, CardID}}, MskillId, PlayerOppID),
	if
	 	OrTargetChecked =:= [] -> OrEffect = [{}];
		OrTargetChecked =/= [] -> 
		 	{OrEffect, OrDuration} = or_effect(MskillId) ,
			OrEffect = [{{PlayerOwnID, CardOrder, CardID, MskillId}, OtherTargetChecked, OrEffect, OrDuration }]
	end,
	IfClues = do(qlc:q([{X#mystic_skill.then_assign, X#mystic_skill.then_do} || X <- mnesia:table(mystic_skill), X#mystic_skill.mskill_id =:= MskillId])),
	io:format ("Effect of skill are ~p~n ", [IfClues++POwnEffect++POppEffect++SelfEffect++OtherEffect++OrEffect]),
 	[list_to_tuple(IfClues++POwnEffect++POppEffect++SelfEffect++OtherEffect++OrEffect)].