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

-import (lists, [flatlength/1, split/2]).

-compile (export_all).

check_card_effect (PlayerPid, CardOrder, CardID, EffectCheck) ->
	io:format("-- Check effect ~p~n", [EffectCheck]),
	CardFx = card_utility:get_all_card_effect (PlayerPid, CardOrder, CardID),
	case EffectCheck of
		to_df -> check_effect (CardFx, {attack, to_df});
		from_df -> check_effect (CardFx, {attack, from_df});
		switch_power -> check_effect (CardFx, {power, switch});
		skill_no_combine -> check_effect (CardFx, {skill, no_combine});
		any_curse -> check_any_curse (CardFx, 0);
		stone_curse -> check_effect (CardFx, {curse, stone_curse});
		freeze_curse -> check_effect (CardFx, {curse, freeze_curse});
		charm_curse -> check_effect (CardFx, {curse, charm_curse});
		poison_curse -> check_effect (CardFx, {curse, poison_curse});
		death_curse -> check_effect (CardFx, {curse, death_curse});
		last_dance_curse -> check_effect (CardFx, {curse, last_dance_curse});
		dimension_curse -> check_effect (CardFx, {curse, dimension_curse});
		inactive -> check_effect (CardFx, {action, be_inactive});
		cancel_all_mystic -> check_effect (CardFx, {cancel_mystic, [all]});
		cancel_opponent_mystic -> check_effect (CardFx, {cancel_mystic, [all_opponent]});
		cancel_all_curse -> check_effect (CardFx, {cancel_curse, [all]});
		compare_power_vs_df -> check_effect (CardFx, {action, at_vs_df});
		compare_power_vs_at -> check_effect (CardFx, {action, at_vs_at});
		compare_df_vs_power -> check_effect (CardFx, {action, df_vs_at});
		protect_attack_all -> check_effect (CardFx, {protect_attack, [attack_all]});
		attack_any_action -> check_effect (CardFx, {atk, without_combine});
		additional_select_attack -> check_effect (CardFx, {atk, additional_select_attack});
		_ -> io:format("!!! --- Other effect check ~p~n", [EffectCheck]),
			{ok, no_effect}
	end.

% check_effect_action ([], _) -> [];
% check_effect_action ([{_, Effect, _} | T], Fx) ->
	% io:format("Effect ~p - Fx ~p~n", [Effect, Fx]),
	% case get_effect_action (Effect, Fx) of
		% {ok, have_effect, FxAction} -> [FxAction] ++ check_effect_action (T, Fx);
		% {ok, no_effect} -> check_effect_action (T, Fx)
	% end.

% get_effect_action ([], _) -> {ok, no_effect};
% get_effect_action ([{Fx, FxAction} | _], Fx) -> {ok, have_effect, FxAction};
% get_effect_action ([_ | T], Fx) -> get_effect_action (T, Fx).

check_any_curse (_, 7) -> {ok, no_effect};
check_any_curse (CardEffect, Number) ->
	case Number of
		0 -> Curse = stone_curse;
		1 -> Curse = freeze_curse;
		2 -> Curse = charm_curse;
		3 -> Curse = poison_curse;
		4 -> Curse = death_curse;
		5 -> Curse = last_dance_curse;
		6 -> Curse = dimension_curse
	end,
	case check_effect (CardEffect, {curse, Curse}) of
		{ok, have_effect} -> {ok, have_effect};
		{ok, no_effect} -> check_any_curse (CardEffect, Number + 1)
	end.

check_effect ([], _) -> {ok, no_effect};
check_effect ([{_, Effect, _} | T], EffectCheck) ->
%	io:format("Effect ~p~n", [Effect]),
	case check_have_effect (Effect, EffectCheck) of
		{ok, have_effect} -> {ok, have_effect};
		{ok, no_effect} -> check_effect (T, EffectCheck)
	end.

check_have_effect ([], _) -> {ok, no_effect};
check_have_effect ([EffectCheck | _], EffectCheck) -> {ok, have_effect};
check_have_effect ([{curse, {Curse, _}} | _], {curse, Curse}) -> {ok, have_effect};
check_have_effect ([_ | T], EffectCheck) -> check_have_effect (T, EffectCheck).

% get_ability_field (PlayerPid, CardOrder, CardID, Field, Zone) ->
	% {ok, Ability} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, give_effect, Zone),
	% {ConditionAbility, Effect, TargetType, AbilityID, Duration} = Ability,
	% case Field of
		% ability_condition -> {ok, ConditionAbility};
		% ability_effect -> {ok, Effect};
		% target_type -> {ok, TargetType};
		% ability_id -> {ok, AbilityID};
		% ability_duration -> {ok, Duration};
		% _ -> io:format("Get ability field out of range ~p~n", [Field])
	% end.

% add_ability (ConditionAbility, PlayerOwnEffect, OpponentEffect, SelfEffect, OtherEffect) ->
	% ability_to_player (ConditionAbility, PlayerOwnEffect),
	% ability_to_opponent (ConditionAbility, OpponentEffect),
	% ability_to_self (ConditionAbility, SelfEffect),
	% ability_to_other (ConditionAbility, OtherEffect).

% ability_to_player (_, _) -> ok.

% ability_to_opponent (_, {}) -> ok;
% ability_to_opponent (Cond, EFx) ->
	% set_ability (Cond, opponent, EFx).

% ability_to_self (_, {}) -> ok;
% ability_to_self (Cond, EFx) ->
% %	io:format("Self Fx ~p~n", [EFx]),
	% set_ability (Cond, self, EFx).

% ability_to_other (_, {}) -> ok;
% ability_to_other (Cond, EFx) ->
% %	io:format("Other Fx ~p~n", [EFx]),
	% set_ability (Cond, other, EFx).

% set_ability (ConditionAbility, TargetType, {{PlayerPid, CardOrder, CardID, AbilityID}, Effect, Duration}) ->
	% set_ability (ConditionAbility, TargetType, {{PlayerPid, CardOrder, CardID, AbilityID}, [], Effect, Duration});
% set_ability (ConditionAbility, TargetType, {{PlayerPid, CardOrder, CardID, AbilityID}, _, Effect, Duration}) ->
	% Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	% {ok, OldAbility} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, give_effect, Zone),
	% AbilityUpdate = check_add_ability (OldAbility, {ConditionAbility, Effect, TargetType, AbilityID, Duration}),
	% card_utility:update_card_option_field (PlayerPid, CardOrder, CardID, give_effect, AbilityUpdate, Zone),
% %	io:format ("Add ability ~p to ~p~n", [AbilityUpdate, {PlayerPid, CardOrder, CardID}]),
	% case ConditionAbility of
		% [] ->	check_zone_condition (PlayerPid, CardOrder, CardID, TargetType, AbilityID, Effect, Duration);
		% _ ->	
			% case check_ability_condition ({PlayerPid, CardOrder, CardID}, ConditionAbility, AbilityID) of
				% {ok, ability_activate} ->
					% check_zone_condition (PlayerPid, CardOrder, CardID, TargetType, AbilityID, Effect, Duration);
				% {ok, ability_inactive} ->
					% activate_ability_effect ({PlayerPid, CardOrder, CardID}, TargetType, AbilityID, Effect, remove, Duration)
			% end
	% end.

% check_zone_condition (PlayerPid, CardOrder, CardID, TargetType, AbilityID, Effect, Duration) ->
% %	io:format("check zone card condition ~n"),
	% case card_utility:check_card_zone (PlayerPid, CardOrder, CardID) of
		% {error, zone_card_error} ->
			% activate_ability_effect ({PlayerPid, CardOrder, CardID}, TargetType, AbilityID, Effect, remove, Duration);
		% _ ->
% %			io:format("Card zone ~p~n", [Zone]),
			% activate_ability_effect ({PlayerPid, CardOrder, CardID}, TargetType, AbilityID, Effect, add, Duration)
	% end.

% check_add_ability ([], Ability) -> [Ability];
% check_add_ability ([{_, _, _, AbilityID, _} | T], {ConditionAbility, Effect, TargetType, AbilityID, Duration}) ->
	% [{ConditionAbility, Effect, TargetType, AbilityID, Duration}] ++ T;
% check_add_ability ([H | T], Ability) -> [H] ++ check_add_ability (T, Ability).

% get_only_card_data ([]) -> [];
% get_only_card_data ([{CardData, _} | T]) -> [CardData | get_only_card_data (T)].

% check_all_ability_affect () ->
	% Seals = card_utility:get_all_card(seal_card, arena_zone),
	% check_have_ability_affect (Seals).

% check_have_ability_affect ([]) -> 0;
% check_have_ability_affect ([{{CardOwner, CardOrder, CardID}, _} | T]) ->	
	% CardZone = card_utility:check_card_zone (CardOwner, CardOrder, CardID),
	% {ok, Abilities} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, give_effect, CardZone),
% %	io:format ("Abilities ~p~n", [Abilities]),
	% case flatlength(Abilities) of
		% 0 ->	check_have_ability_affect (T);
		% _ ->
			% stack_pool:add_stack_option_field (self(), card_have_ability_check, [{CardOwner, CardOrder, CardID}]),
			% 1 + check_have_ability_affect (T)
	% end.

% check_ability_have_condition ([{[_], _, _, _, _} | _]) ->
	% have_condition;
% check_ability_have_condition ([{[], Fx, _, _, _} | _]) ->
	% check_have_activate (Fx).

% check_have_activate ([]) -> no_condition;
% check_have_activate ([{at, '+B'} | _]) -> have_condition;
% check_have_activate ([{at, '-D'} | _]) -> have_condition;
% check_have_activate ([_ | T]) -> check_have_activate (T).

% check_card_affect () ->
	% case stack_pool:get_last_stack (self(), card_affect) of
		% {ok, []} ->	interfere_step:return_play();
		% {ok, [{CardOwner, CardOrder, CardID} | Cards]} ->
			% %A = [{CardOwner, CardOrder, CardID} | Cards],
			% %smo_logger:fmsg("card affect ~w", [A]),
			% stack_pool:set_stack_option (self(), card_affect, Cards),
			% check_ability_affect (CardOwner, CardOrder, CardID)
	% end.

% check_ability_affect (CardOwner, CardOrder, CardID) ->
	% case card_utility:get_card_option_field (CardOwner, CardOrder, CardID, give_effect) of
		% {ok, []} ->
			% interfere_step:return_play (check_play_step);
		% {ok, Abilities} ->
			% is_ability_affect (CardOwner, CardOrder, CardID, Abilities)
	% end.

% is_ability_affect (_, _, _, []) ->
	% CardsDestroy = stack_pool:get_last_stack (self(), cards_destroy),
% %	io:format ("Card destroy effect ~p~n", [CardsDestroy]),
	% case CardsDestroy of
		% {ok, []} ->
			% interfere_step:return_play (check_play_step);
		% {error, _} ->
			% interfere_step:return_play (check_play_step);
		% {ok, Cards} ->
			% {ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
			% stack_pool:remove_stack_option (self(), cards_destroy),
			% destroy:check_card_destroyed (PlayerPid, Cards)
	% end;
% is_ability_affect (CardOwner, CardOrder, CardID, [{ConditionAbility, Effect, TargetType, AbilityID, Duration} | Abilitys]) ->
	% set_ability (ConditionAbility, TargetType, {{CardOwner, CardOrder, CardID, AbilityID}, [], Effect, Duration}),
	% is_ability_affect (CardOwner, CardOrder, CardID, Abilitys).

% check_ability_condition (_, [], _) -> {ok, ability_activate};
% check_ability_condition ({PlayerPid, CardOrder, CardID}, [Condition | T], AbilityID) ->
% %	io:format ("Check ability condition ~p~n", [Condition]),
	% case Condition of
		% s_at_line ->
			% {ok, CardOption} = arena_zone:get_card_option (PlayerPid, CardOrder, CardID),
			% case seal_card:is_on_line (CardOption, 1) of
				% on_line -> check_ability_condition ({PlayerPid, CardOrder, CardID}, [{curse, {n, dimension_curse}}] ++ T, AbilityID);
				% off_line -> {ok, ability_inactive}
			% end;
		% s_df_line ->
			% {ok, CardOption} = arena_zone:get_card_option (PlayerPid, CardOrder, CardID),
			% case seal_card:is_on_line (CardOption, 0) of
				% on_line -> check_ability_condition ({PlayerPid, CardOrder, CardID},  [{curse, {n, dimension_curse}}] ++ T, AbilityID);
				% off_line -> {ok, ability_inactive}
			% end;
		% s_combine ->
			% {ok, Combine} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, combine, arena_zone),
			% case flatlength(Combine) of
				% 0 -> {ok, ability_inactive};
				% _ -> check_ability_condition ({PlayerPid, CardOrder, CardID}, T, AbilityID)
			% end;
		% s_growth ->
			% {ok, Combine} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, combine, arena_zone),
			% case flatlength(Combine) of
				% 0 -> {ok, ability_inactive};
				% _ -> check_ability_condition ({PlayerPid, CardOrder, CardID}, T, AbilityID)
			% end;
		% fire_in_shrine ->
			% {ok, ShrineCard} = mnesia_play:get_player_data (PlayerPid, shrine_cards),
			% case check_card_condition (ShrineCard, {elem, 4}) of
				% {ok, match} -> check_ability_condition ({PlayerPid, CardOrder, CardID}, T, AbilityID);
				% {ok, dont_match} -> {ok, ability_inactive}
			% end;
		% knight_in_shrine ->
			% {ok, ShrineCard} = mnesia_play:get_player_data (PlayerPid, shrine_cards),
			% case check_card_condition (ShrineCard, {type, "Knight"}) of
				% {ok, match} -> check_ability_condition ({PlayerPid, CardOrder, CardID}, T, AbilityID);
				% {ok, dont_match} -> {ok, ability_inactive}
			% end;
		% seal_curse_in_arena ->
			% SealCards = card_utility:get_all_card (seal_card, arena_zone),
			% case count_effect (SealCards, any_curse) of
				% 0 -> {ok, ability_inactive};
				% _ -> check_ability_condition ({PlayerPid, CardOrder, CardID}, T, AbilityID)
			% end;
		% unicorn_support ->
			% SupportList = arena_zone:get_support_seals (PlayerPid, CardOrder, CardID),
			% case check_card_condition (SupportList, "Bà¸?") of
				% {ok, match} -> check_ability_condition ({PlayerPid, CardOrder, CardID}, T, AbilityID);
				% {ok, dont_match} -> {ok, ability_inactive}
			% end;
		% owner_arena_more_seal ->
			% OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
			% MySeals = card_utility:get_all_card (PlayerPid, seal_card, arena_zone),
			% OppSeals = card_utility:get_all_card (OpponentPid, seal_card, arena_zone),
			% MySize = flatlength (MySeals),
			% OpSize = flatlength (OppSeals),
			% if 
				% MySize > OpSize -> check_ability_condition ({PlayerPid, CardOrder, CardID}, T, AbilityID);
				% true -> {ok, ability_inactive}
			% end;
		% opponent_arena_more_seal ->
			% OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
			% MySeals = card_utility:get_all_card (PlayerPid, seal_card, arena_zone),
			% OppSeals = card_utility:get_all_card (OpponentPid, seal_card, arena_zone),
			% MySize = flatlength (MySeals),
			% OpSize = flatlength (OppSeals),
			% if 
				% OpSize > MySize -> check_ability_condition ({PlayerPid, CardOrder, CardID}, T, AbilityID);
				% true -> {ok, ability_inactive}
			% end;
		% seal_freeze_curse_in_arena ->
			% SealCards = card_utility:get_all_card (seal_card, arena_zone),
			% case count_effect (SealCards, freeze_curse) of
				% 0 -> {ok, ability_inactive};
				% _ -> check_ability_condition ({PlayerPid, CardOrder, CardID}, T, AbilityID)
			% end;
		% dragon_support ->
			% case attribute_check:combine_check({arena_zone, {PlayerPid, CardOrder, CardID}}, {type, "Dragon"}) of
				% true -> check_ability_condition({PlayerPid, CardOrder, CardID}, T, AbilityID);
				% _Error -> {ok, ability_inactive}
			% end;
		 % {curse, RequireCurse} ->
		 	% Curse = game_info:card_curse({arena_zone, {PlayerPid, CardOrder, CardID}}),
		 	% case 	attribute_check:curse_check(Curse, RequireCurse) of
				% true ->  check_ability_condition({PlayerPid, CardOrder, CardID}, T, AbilityID);
				% _ -> {ok, ability_inactive}
			% end;
		% _ ->	case is_atom (Condition) of
				% true -> TextList = atom_to_list (Condition);
				% false ->
					% io:format("---- Condition is not atom ~p -----~n", [Condition]),
					% TextList = []
			% end,
			% case TextList of
				% [111, 110, 95, 97, 114, 101, 110, 97, 95, Major, Minor] -> % "on_arena_XX"
					% SealCards = card_utility:get_all_card (seal_card, arena_zone),
					% case check_card_condition (SealCards, [Major, Minor]) of
						% {ok, match} -> check_ability_condition ({PlayerPid, CardOrder, CardID}, T, AbilityID);
						% {ok, dont_match} -> {ok, ability_inactive}
					% end;
				% [111, 110, 95, 111, 119, 110, 95, 97, 114, 101, 110, 97, 95, Major, Minor] -> % "on_own_arena_XX"
					% SealCards = card_utility:get_all_card (PlayerPid, seal_card, arena_zone),
					% case check_card_condition (SealCards, [Major, Minor]) of
						% {ok, match} -> check_ability_condition ({PlayerPid, CardOrder, CardID}, T, AbilityID);
						% {ok, dont_match} -> {ok, ability_inactive}
					% end;
				% % "xX_S_own_arena" - à¸–à¹?à¸²à¸¡à¸µà¸?à¸§à¸²à¸¡à¸ªà¸²à¸¡à¸²à¸£à¸–à¸?à¸µà¹?à¸¡à¸²à¸?à¸?à¸§à¹?à¸² 1 à¹?à¸? à¹?à¸?à¸ªà¸?à¸²à¸¡à¸?à¹?à¸²à¸¢à¹€à¸£à¸² à¹?à¸«à¹?à¸—à¸³à¸?à¸²à¸?à¹€à¸?à¸µà¸¢à¸?à¹?à¸?à¹€à¸”à¸µà¸¢à¸§
				% [120, X, 95, 83, 95, 111, 119, 110, 95, 97, 114, 101, 110, 97] ->
					% Want = X - 48,
					% Count = ability_activate:check_card_same (CardID, PlayerPid, player_arena, count_all),
					% if
						% Count >= Want -> 
							% case check_ability_duplicate_effect (PlayerPid, CardOrder, CardID, AbilityID) of
								% no_duplicate -> check_ability_condition ({PlayerPid, CardOrder, CardID}, T, AbilityID);
								% duplicate -> {ok, ability_inactive}
							% end;
						% true -> {ok, ability_inactive}
					% end;
				% _ ->	io:format("---- Other condition is ~p -----~n", [Condition]),
					% {ok, ability_inactive}
			% end
	% end.

% check_ability_duplicate_effect (PlayerPid, CardOrder, CardID, AbilityID) ->
	% Seals = card_utility:get_all_card (PlayerPid, seal_card, arena_zone),
	% SealCheck = card_utility:remove (Seals, PlayerPid, CardOrder, CardID),
	% check_ability_duplicate (SealCheck, AbilityID).

% check_ability_duplicate ([], _) -> no_duplicate;
% check_ability_duplicate ([{{PlayerPid, CardOrder, CardID}, _} | T], AbilityID) ->
	% {ok, Abilities} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, give_effect, arena_zone),
	% {ok, RFx} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, receive_effect, arena_zone),
	% case flatlength (Abilities) of
		% 0 ->	check_ability_duplicate (T, AbilityID);
		% _ ->	case check_has_duplicate (Abilities, AbilityID) of
				% no_duplicate -> check_ability_duplicate (T, AbilityID);
				% duplicate -> 
					% case check_duplicate_effect (RFx, AbilityID) of
						% {ok, no_duplicate_effect} -> check_ability_duplicate (T, AbilityID);
						% {ok, duplicate_effect} -> duplicate
					% end
			% end
	% end.

% check_has_duplicate ([], _) -> no_duplicate;
% check_has_duplicate ([{_, _, _, AbilityID, _} | _], AbilityID) -> duplicate;
% check_has_duplicate ([_ | Abilitys], AbilityID) -> check_has_duplicate (Abilitys, AbilityID).

% check_duplicate_effect (RFx, AbilityID) ->
	% AbilityNumber = query_ability:no_of_ability(AbilityID),
	% case AbilityNumber of
		% 1 -> AbilityCheck = {ability_id, ability_1};
		% 2 -> AbilityCheck = {ability_id, ability_2};
		% 3 -> AbilityCheck = {ability_id, ability_3};
		% 4 -> AbilityCheck = {ability_id, ability_4};
		% 5 -> AbilityCheck = {ability_id, ability_5};
		% _ -> AbilityCheck = {ability_id, ability_other}
	% end,
	% has_duplicate_effect (RFx, AbilityCheck).

% has_duplicate_effect ([], _) -> {ok, no_duplicate_effect};
% has_duplicate_effect ([{_, Effect, _} | T], EffectCheck) ->
	% case received_ability (Effect, EffectCheck) of
		% {effect_id, not_found} -> has_duplicate_effect (T, EffectCheck);
		% {effect_id, found} -> {ok, duplicate_effect}
	% end.

count_effect ([], _) -> 0;
count_effect ([{{PlayerPid, CardOrder, CardID}, _} | T], FxType) ->
	case check_card_effect (PlayerPid, CardOrder, CardID, FxType) of
		{ok, no_effect} -> count_effect (T, FxType);
		{ok, have_effect} -> 1 + count_effect (T, FxType)
	end.	
	
% activate_ability_effect ({CardOwner, CardOrder, CardID}, TargetType, AbilityID, Fx, CheckType, Duration) ->
	% OpponentPid = mnesia_play:get_opponent_pid (CardOwner),
	% case TargetType of
		% player_own_effect -> Targets = [{CardOwner, CardOrder, CardID}];
		% any_player_effect -> Targets = [{CardOwner, CardOrder, CardID}];
		% self_card_effect -> Targets = [{CardOwner, CardOrder, CardID}];
		% other_cards_effect -> 
			% Targets = 
			% case CheckType of
				% add ->
					% s_ability_target:list_target_check(arena_zone, {CardOwner, CardOrder, CardID}, AbilityID, OpponentPid);
				% remove ->
					% s_ability_target:remove_ability_target(arena_zone, {CardOwner, CardOrder, CardID}, AbilityID, OpponentPid)
			% end
	% end,
	% %smo_logger:fmsg("--------------~w-------------- Target may receive effect ~w", [{CardOwner, CardOrder, CardID}, Targets]),
	% %smo_logger:fmsg(" ++++++++++~w+++++++++++++Target had receive effect ~w", [{CardOwner, CardOrder, CardID}, card_utility:get_card_option_field (CardOwner, CardOrder, CardID, target_obtain_ability)]),
	% case CheckType of
		% add ->
			% case card_utility:get_card_option_field (CardOwner, CardOrder, CardID, target_obtain_ability) of
				% {ok, OldObtain} ->
					% case OldObtain -- Targets of
						% [] -> no_remove_old_obtain_ability;
						% RemoveTargetAbility ->
							% lists:foreach(fun({TPid, TOrder, TID}) -> 
															% remove_old_obtain_effect({CardOwner, CardOrder, CardID}, {TPid, TOrder, TID}, AbilityID, Fx)
													  % end, RemoveTargetAbility)
					% end;
				% {error, _} ->
					% no_obtain_target
			% end,
			% card_utility:update_card_option_field (CardOwner, CardOrder, CardID, target_obtain_ability, Targets),
			% check_effect_to_target ({CardOwner, CardOrder, CardID}, Targets, Fx, CheckType, AbilityID, Duration);
		% remove ->
			% card_utility:update_card_option_field (CardOwner, CardOrder, CardID, target_obtain_ability, []),
			% check_effect_to_target ({CardOwner, CardOrder, CardID}, Targets, Fx, CheckType, AbilityID, Duration)
	% end.

% remove_old_obtain_effect(GFxCardData, {TPid, TOrder, TID}, AbilityID, Fx) ->
	% case card_utility:check_card_zone (TPid, TOrder, TID) of
		% {error, _} -> card_no_zone;
		% CardZone ->
			% {ok, RFx} = card_utility:get_card_option_field (TPid, TOrder, TID, receive_effect, CardZone),
			% AbilityNumber = query_ability:no_of_ability(AbilityID),
			% case AbilityNumber of
				% 1 -> AbilityCheck = {ability_id, ability_1};
				% 2 -> AbilityCheck = {ability_id, ability_2};
				% 3 -> AbilityCheck = {ability_id, ability_3};
				% 4 -> AbilityCheck = {ability_id, ability_4};
				% 5 -> AbilityCheck = {ability_id, ability_5};
				% _ -> AbilityCheck = {ability_id, ability_other}
			% end,
			% RFxUpdate = remove_effect_target (RFx, GFxCardData, AbilityCheck),
			% card_utility:update_card_option_field(TPid, TOrder, TID, receive_effect, RFxUpdate, CardZone),
			% effect_activate:send_update_activate_effect(TPid, TOrder, TID, Fx, remove)
	% end.

% check_effect_to_target (_, [], _, _, _, _) -> ok;
% check_effect_to_target (GFxCardData, [{TPid, TOrder, TID} | T], Effect, CheckType, AbilityID, Duration) ->
	% AbilityNumber = query_ability:no_of_ability(AbilityID),
	% case card_utility:get_card_option_field (TPid, TOrder, TID, receive_effect, arena_zone) of
		% {ok, RFx} ->
			% case AbilityNumber of
				% 1 -> AbilityCheck = {ability_id, ability_1};
				% 2 -> AbilityCheck = {ability_id, ability_2};
				% 3 -> AbilityCheck = {ability_id, ability_3};
				% 4 -> AbilityCheck = {ability_id, ability_4};
				% 5 -> AbilityCheck = {ability_id, ability_5};
				% _ -> AbilityCheck = {ability_id, ability_other}
			% end,
			% case CheckType of
				% add ->
					% RFxUpdate = add_effect_target ({TPid, TOrder, TID}, RFx, {GFxCardData, Effect, Duration}, GFxCardData, AbilityCheck);
				% remove ->
					% RFxUpdate = remove_effect_target (RFx, GFxCardData, AbilityCheck)
			% end,
			% card_utility:update_card_option_field (TPid, TOrder, TID, receive_effect, RFxUpdate, arena_zone),
			% effect_activate:send_update_activate_effect(TPid, TOrder, TID, [], add),
			% check_update_activate_effect (TPid, TOrder, TID, Effect, RFxUpdate, RFx, CheckType);
		% {error, _} -> other_zone
	% end,
	% check_effect_to_target (GFxCardData, T, Effect, CheckType, AbilityID, Duration).

% check_update_activate_effect (TPid, TOrder, TID, Effect, RFxUpdate, RFx, CheckType) ->
	% UpdateSize = flatlength (RFxUpdate),
	% OlderSize = flatlength (RFx),
	% case CheckType of
		% add ->
			% if
				% UpdateSize > OlderSize ->
					% effect_activate:send_update_activate_effect (TPid, TOrder, TID, Effect, add);
				% true ->
					% effect_not_add
			% end;
		% remove ->
			% if
				% OlderSize > UpdateSize ->
					% effect_activate:send_update_activate_effect (TPid, TOrder, TID, Effect, remove);
				% true ->
					% effect_not_remove
			% end
	% end,
	% case stack_pool:get_last_stack(self(), call_by) of
		% {ok, effect_activate_1} -> mod_ability_effect:effect_activate_1();
		% _ -> 
			% case stack_pool:get_last_stack(self(), play) of
				% {ok, play_ability_affect} -> ok;
				% _ -> interfere_step:return_play(check_play_step)
			% end
	% end.

% add_effect_target (Target, [], AddEffect, _, EffectCheck) ->
	% {GFxCardData, Effect, Duration} = AddEffect,
	% activate_effect (Target, Effect),
	% [{GFxCardData, Effect ++ [EffectCheck], Duration}];
% add_effect_target (Target, [{GFx, Effect, Duration} | T], AddEffect, GFx, EffectCheck) ->
	% case received_ability (Effect, EffectCheck) of
		% {effect_id, found} -> [{GFx, Effect, Duration}] ++ T;
		% {effect_id, not_found} -> [{GFx, Effect, Duration}] ++ add_effect_target (Target, T, AddEffect, GFx, EffectCheck)
	% end;
% add_effect_target (Target, [H | T], AddEffect, GFx, EffectCheck) -> [H] ++ add_effect_target (Target, T, AddEffect, GFx, EffectCheck).

% activate_effect ({PlayerPid, CardOrder, CardID}, []) ->
	% ability_affect:remove_all_action (PlayerPid, CardOrder, CardID, arena_zone);
% activate_effect ({PlayerPid, CardOrder, CardID}, [{cancel_mystic, [all]} | T]) ->
	% mystic_card:destroy (PlayerPid, CardOrder, CardID, all_mystic),
	% activate_effect ({PlayerPid, CardOrder, CardID}, T);
% activate_effect ({PlayerPid, CardOrder, CardID}, [{cancel_mystic, [all_opponent]} | T]) ->
	% mystic_card:destroy (PlayerPid, CardOrder, CardID, all_opponent),
	% activate_effect ({PlayerPid, CardOrder, CardID}, T);
% activate_effect ({PlayerPid, CardOrder, CardID}, [{curse, Curse} | T]) ->
	% effect_activate:send_update_activate_effect (PlayerPid, CardOrder, CardID, [{curse, Curse}], add),
	% activate_effect ({PlayerPid, CardOrder, CardID}, T); 
% activate_effect (Target, [_ | T]) -> activate_effect (Target, T).

received_ability ([], _) -> {effect_id, not_found};
received_ability ([EffectCheck | _], EffectCheck) -> {effect_id, found};
received_ability ([_ | T], EffectCheck) -> received_ability (T, EffectCheck).

remove_effect_target ([], _, _) -> [];
remove_effect_target ([{GFx, Effect, Duration} | T], GFx, EffectCheck) ->
	case received_ability (Effect, EffectCheck) of
		{effect_id, found} -> T;
		{effect_id, not_found} ->  [{GFx, Effect, Duration}] ++ remove_effect_target (T, GFx, EffectCheck)
	end;
remove_effect_target ([H | T], GFx, EffectCheck) -> [H] ++ remove_effect_target (T, GFx, EffectCheck).

% check_card_condition ([], _) -> {ok, dont_match};
% check_card_condition ([CardData | T], [MajorSub, MinorSub]) ->
	% {Field, FieldID} = get_card_infomation (CardData, MajorSub, MinorSub),
	% case check_match (FieldID, Field) of
		% {found} -> {ok, match};
		% {not_found} -> check_card_condition (T, [MajorSub, MinorSub])
	% end.	

get_card_infomation({CardData, _}, MajorSub, MinorSub) ->
	get_card_infomation(CardData, MajorSub, MinorSub);
get_card_infomation({CardOwner, CardOrder, CardID}, MajorSub, MinorSub) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			OptionPart = play_utility:check_option_part(MajorSub),
			{ok, Field} = seal_card:get_seal_base_power({CardOwner, CardOrder, CardID}, OptionPart),
			FieldID = MinorSub,
%			case OptionPart of
%				card_name -> FieldID = material_search:get_parse_name ([MajorSub, MinorSub]);
%				_ -> <<FieldID:16>> = <<MajorSub, MinorSub>>
%			end,
			{Field, FieldID};
		is_not_seal -> {[], 0}
	end.

% check_match (_, []) -> {not_found};
% check_match (FieldID, [FieldID|_]) -> {found};
% check_match (FieldID, [_|T]) -> check_match (FieldID, T).

% reduce_duration ([]) -> reduce_all_effect;
% reduce_duration ([{{PlayerPid, CardOrder, CardID}, _} | T]) ->
	% {ok, RFx} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, receive_effect, arena_zone),
	% RFxUpdate = check_reduce_effect (RFx),
	% card_utility:update_card_option_field (PlayerPid, CardOrder, CardID, receive_effect, RFxUpdate, arena_zone),
	% remove_effect (T).

% check_reduce_effect ([]) -> [];
% check_reduce_effect ([{GiveFxCard, Effect, Duration} | T]) ->
	% DurationUpdate = Duration - 1,
	% [{GiveFxCard, Effect, DurationUpdate} | check_reduce_effect (T)].
	
% remove_effect ([]) -> remove_all_effect;
% remove_effect ([{{PlayerPid, CardOrder, CardID}, _} | T]) ->
	% {ok, RFx} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, receive_effect, arena_zone),
	% RFxUpdate = check_remove_effect (RFx),
	% card_utility:update_card_option_field (PlayerPid, CardOrder, CardID, receive_effect, RFxUpdate, arena_zone),
	% remove_effect (T).

% check_remove_effect ([]) -> [];
% check_remove_effect ([{GiveFxCard, Effect, Duration} | T]) ->
	% case Duration of
		% 0 -> check_remove_effect (T);
		% _ -> [{GiveFxCard, Effect, Duration} | check_remove_effect (T)]
	% end.

% =========== Ability Hand Zone =================
% add_hand_ability (ConditionAbility, _PlayerOwnEffect, _OpponentEffect, SelfEffect, OtherEffect) ->
% %	hand_effect_player (ConditionAbility, PlayerOwnEffect),
% %	hand_effect_opponent (ConditionAbility, OpponentEffect),
	% hand_effect_self (ConditionAbility, SelfEffect),
	% hand_effect_other (ConditionAbility, OtherEffect).
% 
% % hand_effect_player (ConditionAbility, PlayerOwnEffect) -> ok.
% % hand_effect_opponent (ConditionAbility, OpponentEffect) -> ok.
% 
% hand_effect_self (_, {}) -> ok;
% hand_effect_self (Cond, EFx) -> set_hand_ability (Cond, self_card_effect, EFx).
% 
% hand_effect_other (_, {}) -> ok;
% hand_effect_other (Cond, EFx) -> set_hand_ability (Cond, other_card_effect, EFx).
% 
% add_card_ability ([], Ability) -> [Ability];
% add_card_ability ([{_, _, AbilityCheck} | T], {ConditionAbility, Effect, AbilityCheck}) -> [{ConditionAbility, Effect, AbilityCheck}] ++ T;
% add_card_ability ([H | T], Ability) -> [H] ++ add_card_ability (T, Ability).
% 
% set_hand_ability (ConditionAbility, TargetType, {{PlayerPid, CardOrder, CardID, AbilityNumber}, _, Effect, Duration}) ->
	% case AbilityNumber of
		% 1 -> AbilityCheck = ability_1;
		% 2 -> AbilityCheck = ability_2;
		% 3 -> AbilityCheck = ability_3;
		% 4 -> AbilityCheck = ability_4;
		% 5 -> AbilityCheck = ability_5;
		% _ -> AbilityCheck = ability_other
	% end,
	% {ok, CardAbility} = hand_zone:get_option_field (PlayerPid, CardOrder, CardID, give_effect),
	% Ability = add_card_ability (CardAbility, {ConditionAbility, Effect, self_card_effect, AbilityCheck, Duration}),
	% 
	% card_utility:update_card_option_field (PlayerPid, CardOrder, CardID, give_effect, Ability, hand_cards),
% 
	% OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	% case TargetType of
		% self_card_effect -> Targets = [{PlayerPid, CardOrder, CardID}];
		% other_card_effect -> Targets = s_ability_target:list_target_check(hand_zone, {PlayerPid, CardOrder, CardID}, AbilityNumber, OpponentPid)
	% end,
	% hand_effect_target ({PlayerPid, CardOrder, CardID}, Targets, Effect, add, AbilityCheck).
% 
% hand_effect_target (_, [], _, _, _) -> interfere_step:return_play(check_play_step);
% hand_effect_target (GFxCardData, [{TPid, TOrder, TID} | T], Effect, CheckType, AbilityCheck) ->
	% case hand_zone:get_option_field (TPid, TOrder, TID, receive_effect) of
		% {ok, RFx} ->
			% case CheckType of
				% add -> RFxUpdate = add_hand_effect_target (RFx, {GFxCardData, Effect, 99}, GFxCardData, {ability_id, AbilityCheck});
				% remove -> RFxUpdate = remove_effect_target (RFx, GFxCardData, {ability_id, AbilityCheck})
			% end,
% %			io:format("Update hand effect target ~p~n", [RFxUpdate]),
			% card_utility:update_card_option_field (TPid, TOrder, TID, receive_effect, RFxUpdate, hand_cards);
		% {error, _} -> other_zone
	% end,
	% hand_effect_target (GFxCardData, T, Effect, CheckType, AbilityCheck).
% 
% add_hand_effect_target ([], AddEffect, _, EffectCheck) ->
	% {GFxCardData, Effect, Duration} = AddEffect,
	% [{GFxCardData, Effect ++ [EffectCheck], Duration}];
% add_hand_effect_target ([{GFx, Effect, Duration} | T], AddEffect, GFx, EffectCheck) ->
	% case received_ability (Effect, EffectCheck) of
		% {effect_id, found} -> [{GFx, Effect, Duration}] ++ T;
		% {effect_id, not_found} -> [{GFx, Effect, Duration}] ++ add_hand_effect_target (T, AddEffect, GFx, EffectCheck)
	% end;
% add_hand_effect_target ([H | T], AddEffect, GFx, EffectCheck) -> [H] ++ add_hand_effect_target (T, AddEffect, GFx, EffectCheck).