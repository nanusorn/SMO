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
-module (combat).

-import (lists, [append/2]).

-export ([
						calculate_battle/6, 
						get_cards_destroy/0, 
						calculate_battle_all_target/3, 
						remove_end_of_fighting_effect/0, 
						check_remove_effect_end_of_fighting/3]).

check_attacker_power_to_compare(PlayerPid, CardOrder, CardID) ->
	EffectInterest = [
											% {combat, {power, power}}, 
											% {combat, {power, at}}, 
											% {combat, {power, df}}, 
											% {combat, {at, power}}, 
											% {combat, {df, power}}, 
											% {combat, {df, df}},
											% {combat, {at, at}}, 
											{combat, {when_attack, {power, at}}}, 
											{combat, {when_attack, {power, df}}},
											{combat, {when_attack, {at, power}}},
											{combat, {when_attack, {df, power}}},
											{combat, {when_attack, {df, df}}},
											{combat, {when_attack, {at, at}}},
											{combat, {when_fight_attack, {power, at}}}, 
											{combat, {when_fight_attack, {power, df}}},
											{combat, {when_fight_attack, {at, power}}},
											{combat, {when_fight_attack, {df, power}}},
											{combat, {when_fight_attack, {df, df}}},
											{combat, {when_fight_attack, {at, at}}}
										],
	CardFx = card_utility:get_all_card_effect(PlayerPid, CardOrder, CardID),
	smo_logger:fmsg("attacker effect ~p~n", [CardFx]),
	FxCardGot = function_utility:is_contain(EffectInterest, CardFx),
	attack_effect_transform(FxCardGot).
	
check_target_power_to_compare(PlayerPid, CardOrder, CardID) ->
	EffectInterest = [
											% {combat, {power, power}}, 
											% {combat, {power, at}}, 
											% {combat, {power, df}}, 
											% {combat, {at, power}}, 
											% {combat, {df, power}},
											% {combat, {df, df}},
											% {combat, {at, at}},
											{combat, {when_target, {power, at}}}, 
											{combat, {when_target, {power, df}}},
											{combat, {when_target, {at, power}}},
											{combat, {when_target, {df, power}}},
											{combat, {when_target, {df, df}}},
											{combat, {when_target, {at, at}}},
											{combat, {when_fight_target, {power, at}}}, 
											{combat, {when_fight_target, {power, df}}},
											{combat, {when_fight_target, {at, power}}},
											{combat, {when_fight_target, {df, power}}},
											{combat, {when_fight_target, {df, df}}},
											{combat, {when_fight_target, {at, at}}}
										],
	CardFx = card_utility:get_all_card_effect(PlayerPid, CardOrder, CardID),
	smo_logger:fmsg("target effect ~p~n", [CardFx]),
	FxCardGot = function_utility:is_contain(EffectInterest, CardFx),
	attack_effect_transform(FxCardGot).
	
check_attacker_all_power_to_compare(PlayerPid, CardOrder, CardID) ->
	% ถ้าเป็น Attacker สนใจ {Interest, _}
	EffectInterest = [
											% {combat, {power, power}}, 
											% {combat, {power, at}}, 
											% {combat, {power, df}}, 
											% {combat, {at, power}}, 
											% {combat, {df, power}},
											% {combat, {df, df}},
											% {combat, {at, at}},
											{combat, {when_attack, {power, at}}}, 
											{combat, {when_attack, {power, df}}},
											{combat, {when_attack, {at, power}}},
											{combat, {when_attack, {df, power}}},
											{combat, {when_attack, {df, df}}},
											{combat, {when_attack, {at, at}}}
										],
	CardFx = card_utility:get_all_card_effect(PlayerPid, CardOrder, CardID),
	FxCardGot = function_utility:is_contain(EffectInterest, CardFx),
	attack_effect_transform(FxCardGot).
	
check_target_all_power_to_compare(PlayerPid, CardOrder, CardID) ->
	% ถ้าเป็น Target สนใจ {_, Interest}
	EffectInterest = [
											% {combat, {power, power}}, 
											% {combat, {power, at}}, 
											% {combat, {power, df}}, 
											% {combat, {at, power}}, 
											% {combat, {df, power}},
											% {combat, {df, df}},
											% {combat, {at, at}},
											{combat, {when_target, {power, at}}}, 
											{combat, {when_target, {power, df}}},
											{combat, {when_target, {at, power}}},
											{combat, {when_target, {df, power}}},
											{combat, {when_target, {df, df}}},
											{combat, {when_target, {at, at}}}
										],
	CardFx = card_utility:get_all_card_effect(PlayerPid, CardOrder, CardID),
	FxCardGot = function_utility:is_contain(EffectInterest, CardFx),
	attack_effect_transform(FxCardGot).
	
attack_effect_transform([]) -> [];
attack_effect_transform([{combat,  {when_target, Compare}}|FxCardGot]) -> [Compare] ++ attack_effect_transform(FxCardGot);
attack_effect_transform([{combat,  {when_attack, Compare}}|FxCardGot]) -> [Compare] ++ attack_effect_transform(FxCardGot);
attack_effect_transform([{combat,  {when_fight_attack, Compare}}|FxCardGot]) -> [Compare] ++ attack_effect_transform(FxCardGot);
attack_effect_transform([{combat,  {when_fight_target, Compare}}|FxCardGot]) -> [Compare] ++ attack_effect_transform(FxCardGot);
attack_effect_transform([{combat, Compare}|FxCardGot]) -> [Compare] ++ attack_effect_transform(FxCardGot).


set_destroyed_status(DestroyList) ->
	lists:foreach( fun({PlayerPid, CardOrder, CardID}) ->
									CardZone = card_utility:check_card_zone(PlayerPid, CardOrder, CardID),
									card_utility:add_card_status(PlayerPid, CardOrder, CardID, attack_destroyed, CardZone)
									%card_utility:add_card_status(PlayerPid, CardOrder, CardID, destroyed, support_cards)
								end, DestroyList).

calculate_battle(PlayerPidAS, ASCardOrder, ASCardID, PlayerPidTS, TSCardOrder, TSCardID) ->
	calculate_battle(PlayerPidAS, ASCardOrder, ASCardID, PlayerPidTS, TSCardOrder, TSCardID, normal_attack).
calculate_battle(PlayerPidAS, ASCardOrder, ASCardID, PlayerPidTS, TSCardOrder, TSCardID, AttackType) ->
	%{ok, Line} = card_utility:get_card_option_field(PlayerPidTS, TSCardOrder, TSCardID, line, arena_zone),
	{ok, Line} = card_utility:get_card_option_field(PlayerPidTS, TSCardOrder, TSCardID, line),
	{ok, ASCombatData} = arena_zone:get_combat_data(PlayerPidAS, ASCardOrder, ASCardID),
	{ok, TSCombatData} = arena_zone:get_combat_data(PlayerPidTS, TSCardOrder, TSCardID),

	%SupportAttacker = arena_zone:get_support_seals(PlayerPidAS, ASCardOrder, ASCardID),
%	MysticAttacker = arena_zone:get_mystic_pasted (PlayerPidAS, ASCardOrder, ASCardID),
	AttackerList = [{PlayerPidAS, ASCardOrder, ASCardID}],% ++ SupportAttacker,% ++ MysticAttacker,

	%SupportTarget = arena_zone:get_support_seals (PlayerPidTS, TSCardOrder, TSCardID),
%	MysticTarget = arena_zone:get_mystic_pasted (PlayerPidTS, TSCardOrder, TSCardID),
	TargetList = [{PlayerPidTS, TSCardOrder, TSCardID}],% ++ SupportTarget,% ++ MysticTarget,
	
	AttackCanDestroy =
	case AttackType of
		normal_attack -> check_attacker_can_destroy(AttackerList);
		_ -> []
	end,
	
	case AttackType of
		normal_attack ->
			AttackerCompare = check_attacker_power_to_compare(PlayerPidAS, ASCardOrder, ASCardID),
			TargetCompare = check_target_power_to_compare(PlayerPidTS, TSCardOrder, TSCardID);
		_ ->
			AttackerCompare = check_attacker_all_power_to_compare(PlayerPidAS, ASCardOrder, ASCardID),
			TargetCompare = check_target_all_power_to_compare(PlayerPidTS, TSCardOrder, TSCardID)
	end,
	
	case check_combat (ASCombatData, TSCombatData, Line, AttackerCompare, TargetCompare) of
		{ok, destroyed, destroyed} ->
			case AttackType of
				normal_attack -> set_destroyed_status(AttackCanDestroy ++ TargetList);
				all_attack -> set_destroyed_status(TargetList)
			end;
		{ok, alive, destroyed} -> set_destroyed_status(TargetList);
		{ok, destroyed, alive} ->
			case AttackType of
				normal_attack -> set_destroyed_status(AttackCanDestroy);
				all_attack -> no_card_destroy
			end;
		{ok, alive, alive} -> no_card_destroy
	end.

check_attacker_can_destroy([]) -> [];
check_attacker_can_destroy([{AttackOwner, AttackOrder, AttackID}|Attacker]) ->
	CardFx =card_utility:get_all_card_effect(AttackOwner, AttackOrder, AttackID, arena_zone),
	case function_utility:is_contain([{protect_attack, [counter_attack]}], CardFx) of
		[] -> [{AttackOwner, AttackOrder, AttackID}] ++check_attacker_can_destroy(Attacker);
		_ -> []
	end.
	
check_combat ({AAtk, ADef, ASpd}, {TAtk, TDef, TSpd}, Line, AttackerCompare, TargetCompare) ->
	case Line of
		0 -> check_atk_to_df (AAtk, ADef, ASpd, TAtk, TDef, TSpd, AttackerCompare, TargetCompare);
		1 -> check_atk_to_atk (AAtk, ADef, ASpd, TAtk, TDef, TSpd, AttackerCompare, TargetCompare)
	end.

check_atk_to_df (AAtk, ADef, ASpd, TAtk, TDef, TSpd, AttackerCompare, TargetCompare) ->
	{AttackerPower, TargetPower} = get_power_compare (AAtk, AAtk, ADef, ASpd, TDef, TAtk, TDef, TSpd, AttackerCompare, TargetCompare),
	SpAttacker = set_power_border (ASpd),
	SpTarget = set_power_border (TSpd),
	smo_logger:fmsg("----- Attacker At is ~p Sp is ~p And Attacked Df is ~p Sp is ~p ~n", [AttackerPower, SpAttacker, TargetPower, SpTarget]),
	if 
		AttackerPower > TargetPower -> {ok, alive, destroyed};
		AttackerPower =:= TargetPower, SpAttacker > SpTarget -> {ok, alive, destroyed};
		true -> {ok, alive, alive}
	end.

check_atk_to_atk (AAtk, ADef, ASpd, TAtk, TDef, TSpd, AttackerCompare, TargetCompare) ->
	{AttackerPower, TargetPower} = get_power_compare (AAtk, AAtk, ADef, ASpd, TAtk, TAtk, TDef, TSpd, AttackerCompare, TargetCompare),
	SpAttacker = set_power_border (ASpd),
	SpTarget = set_power_border (TSpd),
	smo_logger:fmsg("----- Attacker Power is ~p Sp is ~p And Attacked Power is ~p Sp is ~p ~n", [AttackerPower, SpAttacker, TargetPower, SpTarget]),
	if 
		AttackerPower > TargetPower -> {ok, alive, destroyed};
		AttackerPower =:= TargetPower ->
			if
				SpAttacker > SpTarget -> {ok, alive, destroyed};
				SpAttacker =:= SpTarget -> {ok, destroyed, destroyed};
				SpAttacker < SpTarget -> {ok, destroyed, alive};
				true -> {ok, destroyed, alive}
			end;
		true -> {ok, destroyed, alive}
	end.

get_power_compare (NAP, AAtk, ADef, _, NTP, TAtk, TDef, _, AttackerCompare, TargetCompare) ->
	{AttckerPower, DefenderPower} =
	case lists:reverse(AttackerCompare) of
		[] ->	 {NAP, NTP};
		[{power, power} | _] -> {NAP, NTP};
		[{power, at} | _] -> {NAP, TAtk}	;
		[{power, df}|_] -> {NAP, TDef};
		[{at, power}|_] -> {AAtk, NTP};
		[{df, power}|_] -> {ADef, NTP};
		[{df, df}|_] -> {ADef, TDef};
		[{at, at}|_] -> {AAtk, TAtk}
	end,
	TargetRes =
	case lists:reverse(TargetCompare) of
		[] ->	 {AttckerPower, DefenderPower};
		[{power, power} | _] -> {NAP, NTP};%{AttckerPower, DefenderPower};
		[{power, at} | _] -> {NAP, TAtk}	;%{AttckerPower, TAtk}	;
		[{power, df}|_] -> {NAP, TDef};%{AttckerPower, TDef};
		[{at, power}|_] -> {AAtk, NTP}; %{AAtk, DefenderPower};
		[{df, power}|_] -> {ADef, NTP}; %{ADef, DefenderPower};
		[{df, df}|_] -> {ADef, TDef};
		[{df, at}|_] -> {AAtk, TAtk}
	end,
	TargetRes.

set_power_border (Value) ->
	if
		Value > 5 -> 5;
		Value < 0 -> 0;
		true -> Value
	end.

remove_end_of_fighting_effect() ->
	Seals = card_utility:get_all_card(seal_card, arena_zone),
	remove_end_of_fighting_effect(Seals).
	
remove_end_of_fighting_effect([]) -> remove_all_effect;
remove_end_of_fighting_effect([{{CardOwner, CardOrder, CardID}, _} | T]) ->
	check_remove_effect_end_of_fighting(CardOwner, CardOrder, CardID),
	%check_remove_effect_end_of_fighting (CardOwner, CardOrder, CardID, skill_effect),
	remove_end_of_fighting_effect(T).

check_remove_effect_end_of_fighting(CardOwner, CardOrder, CardID) ->
	{ok, Fx} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect, arena_zone),
	FxUpdate = check_remove_effect(CardOwner, CardOrder, CardID, Fx),
	card_utility:update_card_option_field(CardOwner, CardOrder, CardID, receive_effect, FxUpdate, arena_zone),
	case stack_pool:get_last_stack(self(), end_fight_remove) of
		{ok, FxRemove} ->
			effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, FxRemove, remove);
		_ ->	no_effect_remove
	end.

check_remove_effect(_, _, _, []) -> [];
check_remove_effect(PlayerPid, CardOrder, CardID, [{GiveFxCard, Effect, Duration} | T]) ->
	case Duration of
		end_of_fighting ->
			stack_pool:add_stack_option_field(self(), end_fight_remove, [{GiveFxCard, Effect}]),
			check_remove_effect(PlayerPid, CardOrder, CardID, T);
		_ ->	[{GiveFxCard, Effect, Duration}] ++ check_remove_effect(PlayerPid, CardOrder, CardID, T)
	end.

% สำหรับการโจมตีออล
calculate_battle_all_target (PlayerPid, CardOrder, CardID) ->
	{ok, SealsTarget} = stack_pool:get_last_stack(self(), attack_target_selected),
	check_attack_target(PlayerPid, CardOrder, CardID, SealsTarget).

check_attack_target(PlayerPid, CardOrder, CardID, AllTarget) ->
	lists:foreach(
								fun({TargetPid, TargetOrder, TargetID}) ->
									calculate_battle(PlayerPid, CardOrder, CardID, TargetPid, TargetOrder, TargetID, all_attack)
								end, AllTarget).

get_cards_destroy() ->
	SealList = card_utility:get_all_card(seal_card, arena_zone),
	Support = card_utility:get_all_card(seal_card, support_cards),
	get_cards_destroy(arena_zone, SealList) ++ get_cards_destroy(support_cards, Support).

get_cards_destroy (_, []) -> [];
get_cards_destroy (Zone, [{{PlayerPid, CardOrder, CardID}, _} | T]) ->
	case card_utility:check_card_status(PlayerPid, CardOrder, CardID, attack_destroyed, Zone) of
		{ok, have_status} -> [{PlayerPid, CardOrder, CardID}] ++ get_cards_destroy(Zone, T);
		{ok, have_no_status} -> get_cards_destroy(Zone, T)
	end.