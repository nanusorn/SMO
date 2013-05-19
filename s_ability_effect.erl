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
-module(s_ability_effect).
-import (mnesia_table, [do/1]).
%-compile (export_all).
-export([s_ability_all_effect/1, s_ability_effect/3]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("record.hrl").
%-include_lib("s_ability_new.hrl").
-include_lib("s_ability.hrl").
%===============================================================================
playerown_effect(AbilityId) -> 
	[Duration] = do(qlc:q([ X#card_ability.playerown_duration_effect || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	 case do(qlc:q([ X#card_ability.playerown_auto_effect || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])) of
		[y] ->
			{playerown_effect_start(AbilityId), Duration}
	end.
%===============================================================================
playerown_effect_start(AbilityId) -> 
	[PlayerOwnEffect] = do(qlc:q([ X#card_ability.playerown_effect || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	PlayerOwnEffect.
%===============================================================================
player_effect(AbilityId) ->
	[Duration] = do(qlc:q([ X#card_ability.player_duration_effect || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	 case do(qlc:q([ X#card_ability.player_auto_effect || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])) of
		[y] ->
			{player_effect_start(AbilityId), Duration}
	end.
%===============================================================================
player_effect_start(AbilityId) -> 
	[PlayerEffect] = do(qlc:q([ X#card_ability.player_effect || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	PlayerEffect.
%===============================================================================
self_effect(AbilityId) ->
	[Duration] = do(qlc:q([ X#card_ability.owner_duration_effect || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	case do(qlc:q([ X#card_ability.owner_auto_effect || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])) of
		[y] ->
			{self_effect_start(AbilityId), Duration};
		_ -> ok			
	end.
%===============================================================================
self_effect_start(AbilityId) ->
	[SelfEffect] = do(qlc:q([ X#card_ability.owner_effect || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	SelfEffect.
%===============================================================================	
other_effect(AbilityId) ->
	[{Duration, IsAuto}] = do(qlc:q([{X#card_ability.other_duration_effect,
																			  X#card_ability.other_auto_effect
																			 }|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	case  IsAuto of
		y->
			Effect = other_effect_start(AbilityId),
			{Effect , Duration};
		_ -> ok
	end.
%===============================================================================
other_effect_start(AbilityId) ->
	[OtherEffect] = do(qlc:q([ X#card_ability.other_effect || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
	OtherEffect.
%===============================================================================
s_ability_all_effect([]) -> [];
s_ability_all_effect([{CardData, AbilityId, PlayerOppID}|T]) -> s_ability_effect(CardData, AbilityId, PlayerOppID)++s_ability_all_effect(T).
%===============================================================================
s_ability_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID) ->
	[IsUntil] = do(qlc:q( [ X#card_ability.is_until|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
	%smo_logger:fmsg("is continuous ~p~n", [IsUntil]),
	%[EnableCondition] = do(qlc:q( [ X#card_ability.enable_condition|| X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId ])),
	% case IsUntil of
		% y -> POwnEffect = [], PlayerEffect = [], SelfEffect = [], OtherEffect = [];
		% _ -> 
			POwnEffect =
			case s_ability_target:playerown_target_check(AbilityId) of
				[] -> [];
				_ ->
					{OwEffect, OwDuration} = playerown_effect(AbilityId),
					[{player_own_effect, {{PlayerOwnID, CardOrder, CardID, AbilityId},  [PlayerOwnID], OwEffect, OwDuration}}]
			end,
			PlayerEffect =
			case s_ability_target:playeropp_target_check({PlayerOwnID, CardOrder, CardID}, PlayerOppID, AbilityId) of
				[] -> [];
				PlayerTarget -> 
					{OpEffect, OpDuration} = player_effect(AbilityId),
					[{any_player_effect, {{PlayerOwnID, CardOrder, CardID, AbilityId}, [PlayerTarget], OpEffect, OpDuration }}]
			end,
			SelfEffect =
			case s_ability_target:owner_target_check(AbilityId) of % if in the present condition this card can use ability then check which card will take this effects
				[] -> [];
				_ -> 
					{SEffect, SDuration} = self_effect(AbilityId),
					[{self_card_effect, {{PlayerOwnID, CardOrder, CardID, AbilityId},[{PlayerOwnID, CardOrder, CardID}],SEffect, SDuration }}]
			end,
			OtherEffect = 
			case s_ability_target:other_target_check({CardZone,{PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID) of
				[] -> 
					case IsUntil of
						y -> 
							case other_effect(AbilityId) of
								{OEffect, ODuration} ->
									[{other_cards_effect, {{PlayerOwnID, CardOrder, CardID, AbilityId}, [], OEffect, ODuration }}];
								_ -> []
							end;
						_ -> []
					end;
				OtherTargetChecked ->
					{OEffect, ODuration} = other_effect(AbilityId),
					[{other_cards_effect, {{PlayerOwnID, CardOrder, CardID, AbilityId}, OtherTargetChecked, OEffect, ODuration }}]
			end,
	%end,
		%OrTargetChecked = s_ability_target:or_target_check({CardZone,{PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID),
		%if
			%OrTargetChecked =:= [] -> OrEffect = [{}];
			%OrTargetChecked =/= [] -> 
				%{REffect, RDuration} = s_ability_effect:other_effect(AbilityId),
				%OrEffect = [{{PlayerOwnID, CardOrder, CardID, AbilityId}, OtherTargetChecked, REffect, RDuration }]
		%end,
		%IfClues = do(qlc:q([{X#card_ability.if_clues, X#card_ability.and_effect} || X <- mnesia:table(card_ability), X#card_ability.ability_id =:= AbilityId])),
		%io:format ("Effect of ability are ~p~n", [IsUntil++EnableCondition++POwnEffect++PlayerEffect++SelfEffect++OtherEffect ]),
		%io:format ("Effect of ability are ~p~n", [IfClues++IsUntil++EnableCondition++POwnEffect++POppEffect++SelfEffect++OtherEffect ]),
	[{IsUntil, [], POwnEffect++PlayerEffect++SelfEffect++OtherEffect}].
