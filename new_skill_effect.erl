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
-module(new_skill_effect).
-export([
						activate_skill_effect/4,
						check_continuous_effect/0,
						activate_skill_to_card/3,
						activate_skill_to_player/3
					]).

activate_skill_effect(CardOwner, CardOrder, CardID, {WhomEffect, SkillTarget, [{{GPid, GOrder, GID}, Fx, Duration}], SkillID}) ->
	Then = s_skill_check:check_then(SkillID),
	stack_pool:add_stack_option_field(self(), then, Then),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	%RemainTarget = skill_utility:check_target_cancel_skill(PlayerPid, CardOrder, CardID, MTarget),
	case SkillTarget of
		[] -> interfere_step:return_play(check_play_step);
		_ ->
			StackOption = [
											{card_give_effect, {GPid, GOrder, GID, SkillID}}, 
											{effect_affect, Fx}, 
											{duration, Duration}, 
											{skill_target, SkillTarget}, 
											%{skill_id, SkillID}, 
											{card_player, PlayerPid}],
			stack_pool:push_stack(self(), CardOwner, CardOrder, CardID, StackOption),
			case WhomEffect of
				1 -> activate_skill_to_player(CardOwner, CardOrder, CardID);
				2 -> activate_skill_to_player(CardOwner, CardOrder, CardID);
				3 -> activate_skill_to_card(CardOwner, CardOrder, CardID);
				4 -> activate_skill_to_card(CardOwner, CardOrder, CardID)
			end
	end.
	
check_continuous_effect() ->
	case stack_pool:get_last_stack(self(), play) of
		{ok, activate_skill_to_card} ->	stack_pool:set_stack_option(self(), play, skill_card_check_continuous_effect);
		{ok, activate_skill_to_player} -> stack_pool:set_stack_option(self(), play, skill_player_check_continuous_effect)
	end,
	continuous_ability:check_continuous_target().
	
activate_skill_to_player(CardOwner, CardOrder, CardID) ->
	case stack_pool:get_last_stack(self(), effect_affect) of
		{ok, []} -> 
			stack_pool:pop_stack_out(self()),
			interfere_step:return_play(check_play_step);
		{ok, [Effect|Remain]} -> 
			stack_pool:set_stack_option(self(), play, activate_skill_to_player),
			stack_pool:set_stack_option(self(), this_activate_effect, Effect),
			stack_pool:set_stack_option(self(), effect_affect, Remain),
			activate_effect_to_player(CardOwner, CardOrder, CardID, Effect)
	end.
	
activate_skill_to_card(CardOwner, CardOrder, CardID) ->
	case stack_pool:get_last_stack(self(), effect_affect) of
		{ok, []} -> 
			stack_pool:pop_stack_out(self()),
			interfere_step:return_play(check_play_step);
		{ok, [Effect|Remain]} -> 
			stack_pool:set_stack_option(self(), play, activate_skill_to_card),
			stack_pool:set_stack_option(self(), this_activate_effect, Effect),
			stack_pool:set_stack_option(self(), effect_affect, Remain),
			activate_effect_to_card(CardOwner, CardOrder, CardID, Effect)
	end.
	
activate_effect_to_player(CardOwner, CardOrder, CardID, {EffectType, Effect}) ->
	case EffectType of
		player_action ->
			{ok, SkillTarget} = stack_pool:get_last_stack(self(), skill_target),
			case Effect of
				{show_opponent_card, CardType} ->
					[PlayerTarget] = SkillTarget,
					OpponentPid = mnesia_play:get_opponent_pid(PlayerTarget),
					reveal_controller:activate_player_reveal_hand(PlayerTarget, OpponentPid, CardType);
				{random_select, target_player, [hand_cards], Num, move_to_deck} ->
					[PlayerTarget] = SkillTarget,
					{ok, HandCard} = mnesia_play:get_player_data(PlayerTarget, hand_cards),
					Random = function_utility:random_select(Num, HandCard),
					Target = function_utility:exclude_option(Random),
					move_to_library:move_to_library(PlayerTarget, Target)
			end;
		draw_card ->
			{ok,  [Target]} = stack_pool:get_last_stack(self(), skill_target),
			case Effect of
				{target_erase, mc} -> 
					Draw = check_effect_todo({target, mc}),
					erase(target),
					 draw_card:draw(Target, whatever, Draw, 0);
				DrawAmount ->
					 draw_card:draw(Target, whatever, DrawAmount, 0)
			end;
		draw_mystic ->
					{ok,  [Target]} = stack_pool:get_last_stack(self(), skill_target),
					DrawCount =
					case Effect of
						{InitAmount, as_discard_amount} -> 
							case get({discard, Target}) of
								undefined -> 1;
								Number -> erase({discard, Target}),
									{InitAmount, Number}
 							end;
						DrawAmount -> DrawAmount
					end,
					draw_card:draw(Target, mystic_deck, DrawCount, 0);
		discard ->
			case Effect of
				all_mystic_put_amount ->
					{ok,  [Target]} = stack_pool:get_last_stack(self(), skill_target),
					{ok, HandCard} = mnesia_play:get_player_data (Target, hand_cards),
					HandMystic = function_utility:card_match_condition(HandCard, [{card_type, mystic}]),
					put({discard, Target}, length(HandMystic)),
					shrine_zone:card_to_shrine(Target, HandMystic);
				all_mystic ->
					{ok,  [Target]} = stack_pool:get_last_stack(self(), skill_target),
					{ok, HandCard} = mnesia_play:get_player_data(Target, hand_cards),
					HandMystic = function_utility:card_match_condition(HandCard, [{card_type, mystic}]),
					shrine_zone:card_to_shrine(Target, HandMystic)
			end;
		reveal_card_on_hand ->
			OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
			reveal_controller:activate_player_reveal_hand(CardOwner, OpponentPid, all_card);
		mp ->
			Value = check_effect_todo(Effect),
			{ok,  [Target]} = stack_pool:get_last_stack(self(), skill_target),
			OpposePid = mnesia_play:get_opponent_pid(Target),
			set_effect(Target, mp_rest, Value),
			{ok, ReceiverMp} = mnesia_play:get_player_data(Target, mp_rest),
			{ok, OpposeMp} = mnesia_play:get_player_data(OpposePid, mp_rest),
			gen_server:cast(Target, {send, [16#88, 16#76, 0, ReceiverMp, OpposeMp]}),
			gen_server:cast(OpposePid, {send, [16#88, 16#76, 0, OpposeMp, ReceiverMp]}),
			interfere_step:return_play(check_play_step);
		card_on_hand ->
			{ok,  [Target]} = stack_pool:get_last_stack(self(), skill_target),
			{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
			{_, _, _, MAbilityID} = CardGive,
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, [Target], Duration}]),
			{ok, PlayerFx} = mnesia_play:get_player_data (Target, player_effect),
			mnesia_play:set_player_data(Target, player_effect, PlayerFx ++ [{CardGive, [{card_hand_max, Effect}], Duration}]),
			PlayerHandFx = continuous_ability:get_player_effect(Target, card_hand_max),
			HandSize = 7,
			OpposePid = mnesia_play:get_opponent_pid(Target),
			gen_server:cast(OpposePid, {send, [16#88, 16#76, 1, 0, HandSize + PlayerHandFx]}),
			interfere_step:return_play(check_play_step);
		mp_max ->
			{ok,  [Target]} = stack_pool:get_last_stack(self(), skill_target),
			{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
			{_, _, _, MAbilityID} = CardGive,
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, [Target], Duration}]),
			{ok, PlayerFx} = mnesia_play:get_player_data (Target, player_effect),
			mnesia_play:set_player_data(Target, player_effect, PlayerFx++ [{CardGive, [{mp_max, Effect}], Duration}]),
			interfere_step:return_play(check_play_step);
		curse ->
			{ok,  [Target]} = stack_pool:get_last_stack(self(), skill_target),
			%SkillTarget = skill_card_list:only_on_zone(arena_zone, Target),
			TargetSealList = card_utility:get_all_card(Target, seal_card, arena_zone),
			{ok, {GPid, GOrder, GId, SkillID}} = stack_pool:get_last_stack(self(), card_give_effect),
			GZone = card_utility:check_card_zone(GPid, GOrder, GId),
			RemainTarget = function_utility:card_match_condition({GZone, {GPid, GOrder, GId}},  function_utility:at_zone(arena_zone, TargetSealList), [{protect_curse, {n, Effect}}, {curse, {n, Effect}}, {protect_skill, n}, {protect_skill, {n, [skill_all]}}]),
			SkillTarget = skill_utility:check_target_cancel_skill(GPid, GOrder, GId, RemainTarget),
			case SkillTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					{ok, Duration} = stack_pool:get_last_stack(self(), duration),
					FinalEffect =
					case Effect of
						charm_curse -> {charm_curse, CardOwner};
						{Curse, all} -> Curse;
						_ -> Effect
					end,
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{SkillID, SkillTarget, Duration}]),
					curse_activation:curse_activation_assign(PlayerPid, SkillTarget, [{{GPid, GOrder, GId, SkillID}, [{curse, FinalEffect}], Duration}], receive_effect)
			end;
		_ -> interfere_step:return_play(check_play_step)
	end.	
	
set_effect(Receiver, PlayerField, Value) ->
	{ok, PlayerValue} = mnesia_play:get_player_data(Receiver, PlayerField),
	if
		PlayerValue + Value < 0 ->
			%io:format('set player data: 0 ~n'),
			mnesia_play:set_player_data(Receiver, PlayerField, 0);
		true ->
			%io:format('set player data: ~p~n', [PlayerValue + Value]),
			mnesia_play:set_player_data(Receiver, PlayerField, PlayerValue + Value)
	end.
	
check_effect_todo(Effect) ->
	case is_integer(Effect) of
		true -> Effect;
		false -> 
			case Effect of
				{target, Collect} ->
					[{TPid, TOrder, TId}] = get(target),
					TZone = card_utility:check_card_zone(TPid, TOrder, TId),
					case Collect of
								mc -> game_info:card_mpcast({TZone, {TPid, TOrder, TId}})
					end;
				{Operator, Value} ->
					Power =
					case Value of
						{target, Collect} ->
							[{TPid, TOrder, TId}] = get(target),
							TZone = card_utility:check_card_zone(TPid, TOrder, TId),
							case Collect of
								mc -> game_info:card_mpcast({TZone, {TPid, TOrder, TId}})
							end;
						{get_erase, Collect} -> 
							case get(Collect) of
								undefined -> 0;
								Get -> erase(Collect), Get
							end
					end,
					case Operator of
						'+' -> Power;
						'-' -> -Power
					end;
				_ -> 0
			end
	end.
%% move_attack_target, to_s
%% cancel_ability
%% counter, loose_all_and_put_all_loose_counter
%% counter, add_all_other_loose_counter
%%protect_attack, [counter_attack]
%% skill, can_use_even_target_sp_mismatch

% show_opponent_card

% elem, {select, 1}
% action, reveal

	
activate_effect_to_card(CardOwner, CardOrder, CardID, {EffectType, Effect}) ->
	%smo_logger:fmsg("skill {~p, ~p, ~p} is on ~p ~n ", [CardOwner, CardOrder, CardID, card_utility:check_card_zone(CardOwner, CardOrder, CardID)]),
	case EffectType of
		curse ->
			{ok,  Target} = stack_pool:get_last_stack(self(), skill_target),
			{ok, {GPid, GOrder, GId, SkillID}} = stack_pool:get_last_stack(self(), card_give_effect),
			SkillTarget_ = skill_card_list:only_on_zone(arena_zone, Target),
			SkillTarget = skill_utility:check_target_cancel_skill(GPid, GOrder, GId, SkillTarget_),
			case SkillTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					{ok, Duration} = stack_pool:get_last_stack(self(), duration),
					FinalEffect =
					case Effect of
						charm_curse -> {charm_curse, CardOwner};
						{Curse, all} -> Curse;
						_ -> Effect
					end,
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{SkillID, SkillTarget, Duration}]),
					curse_activation:curse_activation_assign(PlayerPid, SkillTarget, [{{GPid, GOrder, GId, SkillID}, [{curse, FinalEffect}], Duration}], receive_effect)
			end;
		{curse_timer, Curse} ->
			{ok,  Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			case SkillTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->	curse_activation:curse_duration_decrese(SkillTarget, {curse, Curse}, Effect)
			end;
		player_action ->
			{ok,  SkillTarget} = stack_pool:get_last_stack(self(), skill_target),
			case Effect of
				sacrifice ->
					{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
					%OnAreList = skill_card_list:only_on_zone(arena_zone, SkillTarget),
					destroy:check_card_destroyed(PlayerPid, SkillTarget, sacrifice);
				{sacrifice_collect, ToCollect} ->
					case ToCollect of
						target -> put(target, SkillTarget);
						{target, Power} ->
							[{TPid, TOrder, TId}] = SkillTarget,
							TZone = card_utility:check_card_zone(TPid, TOrder, TId),
							case Power of
								mc -> 
									Mc = game_info:card_mpcast({TZone, {TPid, TOrder, TId}}), put(mc, Mc)
							end
					end,
					{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
					OnAreList = skill_card_list:only_on_zone(arena_zone, SkillTarget),
					destroy:check_card_destroyed(PlayerPid, OnAreList, sacrifice);
				discard ->
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					discard:player_discard(PlayerPid, SkillTarget);
				{discard_collect, ToCollect} ->
					case ToCollect of
						target -> put(target, SkillTarget);
						{target, Power} ->
							[{TPid, TOrder, TId}] = SkillTarget,
							TZone = card_utility:check_card_zone(TPid, TOrder, TId),
							case Power of
								mc -> 
									Mc = game_info:card_mpcast({TZone, {TPid, TOrder, TId}}), put(mc, Mc)
							end
					end,
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					discard:player_discard(PlayerPid, SkillTarget);
				%สั่งแยกการรวมร่าง
				assign_break_combine ->
					[{CardOwner, CardOrder, CardID} | _] = SkillTarget,
					{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
					card_utility:add_card_status(CardOwner, CardOrder, CardID, break_combine_fx, arena_zone),
					break_combine:assign_break_combine(PlayerPid, CardOwner, CardOrder, CardID);
				to_at_line -> line_change:move_to_line(SkillTarget, 1);
				move_to_arena -> move_to_arena:move_to_arena(SkillTarget);
				can_move_to_shirne -> 
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					shrine_zone:decide_to_shrine(PlayerPid, SkillTarget);
					%shrine_zone:card_to_shrine(PlayerPid, SkillTarget);
				player_reveal ->
					reveal_controller:activate_consider_card(SkillTarget);
				consider ->
					%{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					reveal_controller:activate_consider_card(SkillTarget);
				move_to_hand ->
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					move_to_hand:move_card_to_hand(PlayerPid, SkillTarget);
				{recheck_condition, {TrueCondition, {TrueFxType, TrueEffect}}, {{_FalseCondition, {FalseFxType, FalseEffect}}}} ->
					case function_utility:card_match_condition(SkillTarget, TrueCondition) of
						[] -> activate_effect_to_card(CardOwner, CardOrder, CardID, {TrueFxType, FalseEffect});
						_ -> activate_effect_to_card(CardOwner, CardOrder, CardID, {FalseFxType, TrueEffect})
					end
				% จั่วการ์ด
				% {draw_card, DrawAmount} ->
					% [Target|_] = SkillTarget,
					% draw_card:draw(Target, whatever, DrawAmount, 0)
			end;
		action ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			case SkillTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->	
					case Effect of
						destroy -> 
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
							destroy:check_card_destroyed(PlayerPid, SkillTarget, destroy_effect);
						change_line ->
							OnArena = skill_card_list:only_on_zone(arena_zone, SkillTarget),
							line_change:change_line(OnArena);
						paste_to_other_target ->
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
							other_active_effect:paste_mystic_to_other_target(PlayerPid, SkillTarget);
						move_to_arena ->
							move_to_arena:move_to_arena(SkillTarget);
						move_to_at_line ->
							stack_pool:set_stack_option(self(), move_effect, {move_to_line, 1}),
							move_to_arena:move_to_arena(SkillTarget);
						move_to_hand ->
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
							move_to_hand:move_card_to_hand(PlayerPid, SkillTarget);
						move_to_library ->
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
							move_to_library:move_to_library(PlayerPid, SkillTarget);
						to_remove_zone -> 
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
							remove_zone:move_to_remove_zone(PlayerPid, SkillTarget);
						attach_to_s ->
							[{TPid, TOrder, TCardID}] = SkillTarget,
							stack_pool:set_stack_option(self(), assigned_target, [{CardOwner, CardOrder, CardID}]),
							move_to_arena:move_mystic_card_to_arena(TPid, TOrder, TCardID);
						{discard, mp_cast} ->
							[{CardOwner, CardOrder, CardID}|_] = SkillTarget,
							Mc = game_info:card_mpcast({hand_cards, {CardOwner, CardOrder, CardID}}),
							put(mc, Mc),
							{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
							discard:player_discard(PlayerPid, SkillTarget);
						break_combine ->
							[{TPid, TOrder, TID} | _] = SkillTarget,
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),	
							%card_utility:add_card_status(TPid, TOrder, TID, break_combine_fx, arena_zone),
							force_break:force_break_combine(PlayerPid, TPid, TOrder, TID);
						assign_s_combine ->
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player), 
							[{CardOwner, CardOrder, CardID}] = SkillTarget,
							combination:declare_combination(PlayerPid, CardOwner, CardOrder, CardID);
						combine ->
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
							[{CardOwner, CardOrder, CardID}] = SkillTarget,
							combination:check_seal_condition(PlayerPid, CardOwner, CardOrder, CardID)	;
						{recheck_condition, {TrueCondition, {TrueFxType, TrueEffect}}, {{_FalseCondition, {FalseFxType, FalseEffect}}}} ->
							case function_utility:card_match_condition(SkillTarget, TrueCondition) of
								[] -> activate_effect_to_card(CardOwner, CardOrder, CardID, {TrueFxType, FalseEffect});
								_ -> activate_effect_to_card(CardOwner, CardOrder, CardID, {FalseFxType, TrueEffect})
							end
				end
		end;
		assign_atk ->
			{ok,  SkillTarget} = stack_pool:get_last_stack(self(), skill_target),
			case Effect of
				to_seal_being_combine ->
					[{TCardOwner, TCardOrder, TCardID}] = SkillTarget,
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					UnconPid = mnesia_play:get_opponent_pid(PlayerPid),
					{ok, TargetList} = mnesia_play:get_player_data(UnconPid, arena_zone),
					SealCombine = function_utility:card_match_condition(TargetList,  [{action, being_combine}]),
					stack_pool:set_stack_option(self(), assigned_target, SealCombine),
					%put(skill_atk_combine, {ok}),
					%card_utility:add_card_status (CardOwner, CardOrder, CardID, inactive_attack_allow, arena_zone),
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					card_utility:set_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect, [{CardGive, [{attack, to_df}], end_of_fighting}], arena_zone),
					stack_pool:set_stack_option(self(), attack_case, skill_effect),
					assign_atk_controller:check_attack_condition(PlayerPid, TCardOwner, TCardOrder, TCardID);
				to_seal_using_skill ->
					[{TCardOwner, TCardOrder, TCardID}] = SkillTarget,
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					UnconPid = mnesia_play:get_opponent_pid(PlayerPid),
					{ok, TargetList} = mnesia_play:get_player_data(UnconPid, arena_zone),
					SealUseSkill = function_utility:card_match_condition(TargetList,  [{action, using_skill}]),
					stack_pool:set_stack_option(self(), assigned_target, SealUseSkill),
					%card_utility:add_card_status (CardOwner, CardOrder, CardID, inactive_attack_allow, arena_zone),
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					card_utility:set_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect, [{CardGive, [{attack, to_df}], end_of_fighting}], arena_zone),
					stack_pool:set_stack_option(self(), attack_case, skill_effect),
					assign_atk_controller:check_attack_condition(PlayerPid, TCardOwner, TCardOrder, TCardID);
				assign_s_attack_to_df ->
					[{TCardOwner, TCardOrder, TCardID}] = SkillTarget,
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					%card_utility:add_card_status (CardOwner, CardOrder, CardID, inactive_attack_allow, arena_zone),
					card_utility:set_card_option_field(TCardOwner, TCardOrder, TCardID, receive_effect, [{CardGive, [{attack, only_to_df}], end_of_fighting}], arena_zone),
					stack_pool:set_stack_option(self(), attack_case, skill_effect),
					assign_atk_controller:check_attack_condition(PlayerPid, TCardOwner, TCardOrder, TCardID);
				Effect ->
					[{TCardOwner, TCardOrder, TCardID}] = SkillTarget,
					TargetCardZone = card_utility:check_card_zone(TCardOwner, TCardOrder, TCardID),
					stack_pool:set_stack_option(self(), attack_case, skill_effect),
					special_atk(TargetCardZone, {TCardOwner, TCardOrder, TCardID}, Effect)
			end;
		heal_curse ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			case SkillTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ -> 
					CurseHeal =
					case Effect of
						[charm_curse] -> game_info:special_curse(charm_curse, SkillTarget);
						[last_dance_curse] -> game_info:special_curse(last_dance_curse, SkillTarget);
						_ -> Effect
					end,
					curse_activation:heal_target_curse(SkillTarget, CurseHeal)
			end;
		heal_selected_curse ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			case SkillTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					CurseHeal = 
					case Effect of
						[charm_curse] -> game_info:special_curse(charm_curse, SkillTarget);
						[last_dance_curse] -> game_info:special_curse(last_dance_curse, SkillTarget);
						_ -> Effect
					end,
					curse_activation:heal_target_curse(Target, CurseHeal)
			end;
		atk ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step);
		move ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step);
		comabat ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step);
		skill -> 
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step);
		ability -> 
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step);
		loss_cancel_curse -> 
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step);
		protect_curse ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step);
		change_line ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step);
		cancel_curse ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			curse_activation:heal_target_curse(SkillTarget, Effect);
		cancel_mystic ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			DestroyMystic =
			case Effect of
				[all] -> 
					mystic_card:destroy(SkillTarget, all_mystic);
				[all_opponent] -> mystic_card:destroy(SkillTarget, all_opponent);
				[opponent_relic] -> mystic_card:destroy(SkillTarget, opponent_relic)
			end,
			continuous_ability:remove_mystic_remain_effect(SkillTarget, DestroyMystic, Effect),
			case DestroyMystic of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					{ok, PlayerPid} = mnesia_play:get_game_data (self(), player_turn),
					shrine_zone:card_to_shrine(PlayerPid, DestroyMystic)
			end;
		attack ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step);
		loss_ability ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step);
		loss_skill ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step);
		cancel_ability ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			continuous_ability:remove_ability_remain_effect(SkillTarget, Effect),
			interfere_step:return_play(check_play_step);
		counter ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			case Effect of
				loose_all_and_put_all_loose_counter -> 
					AllCounter = function_utility:cal_loose_all_card_counter(SkillTarget, 0),
					put(all_card_counter, AllCounter),
					interfere_step:return_play(check_play_step);
				add_all_other_loose_counter ->
					Counter = get(all_card_counter),
					erase(all_card_counter),
					activate_effect_to_card(CardOwner, CardOrder, CardID, {counter, Counter});
				_ ->
					activate_effect_to_target(SkillTarget, [{EffectType, Effect}]),
					interfere_step:return_play(check_play_step)
			end;
		move_attack_target ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			case Effect of
				to_s ->
					lists:foreach(fun({TPid, TOrder, TId}) ->
						card_utility:add_card_status(TPid, TOrder, TId, {attack_to, [{CardOwner, CardOrder, CardID}]}) end, SkillTarget),
					interfere_step:return_play(check_play_step);
				_ -> ""
			end;
		move_skill_target ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			case Effect of
				{PreviousTarg, to_s} ->
					lists:foreach(fun({TPid, TOrder, TId}) ->
						card_utility:add_card_status(TPid, TOrder, TId, {skill_from, PreviousTarg, skill_to, [{CardOwner, CardOrder, CardID}]}) end, SkillTarget),
					interfere_step:return_play(check_play_step);
				_ -> ""
			end;
		swap_power ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			lists:foreach(fun({TPid, TOrder, TID}) ->
				seal_card:swap_card_power(TPid, TOrder, TID, Effect),
				effect_activate:send_update_activate_effect(TPid, TOrder, TID, [], update) end, SkillTarget),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step);
		elem ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			lists:foreach(fun({TPid, TOrder, TID}) ->
				seal_card:change_card_element(TPid, TOrder, TID, Effect),
				effect_activate:send_update_activate_effect(TPid, TOrder, TID, [], update) end, SkillTarget),
			add_effect_to_all_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step);
		_ ->
			{ok, Target} = stack_pool:get_last_stack(self(), skill_target),
			SkillTarget = skill_utility:check_target_cancel_skill(CardOwner, CardOrder, CardID, Target),
			activate_effect_to_target(SkillTarget, [{EffectType, Effect}]),
			interfere_step:return_play(check_play_step)
	end.

special_atk(TargetCardZone, {CardOwner, CardOrder, CardID}, Effect) ->
	case TargetCardZone of
		arena_zone ->
			{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
			case Effect of
				once_with_dragon_supporter_at ->
					SupportSeal = arena_zone:get_support_seals(CardOwner, CardOrder, CardID),
					CardAt =
					case function_utility:card_match_condition(SupportSeal, [{type, "Dragon"}]) of
						[{SupPlayPid, SupCardOrder, SubCardID}|_] -> game_info:card_at({support_cards, {SupPlayPid, SupCardOrder, SubCardID}});
						_ -> 0
					end,
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, receive_effect, [{CardGive, [{at, {equal, CardAt}}], 0}]);
				{once_with_at, AtValue} ->
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, receive_effect, [{CardGive, [{at, {equal, AtValue}}], 0}], arena_zone);
				{once_with_at, AtValue, SpecialTarget} ->
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, receive_effect, [{CardGive, [{at, SpecialTarget}], end_of_fighting}], arena_zone),
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, receive_effect, [{CardGive, [{at, {equal, AtValue}}], end_of_fighting}], arena_zone);
				AtkTime ->
					{ok, Duration} = stack_pool:get_last_stack(self(), duration),
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, receive_effect, [{CardGive, [{atk, AtkTime}], Duration}])
			end,
			%card_utility:add_card_status(CardOwner, CardOrder, CardID, inactive_attack_allow, arena_zone),
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
			assign_atk_controller:check_attack_condition(PlayerPid, CardOwner, CardOrder, CardID);
		_ ->
			interfere_step:return_play()
	end.
	
% add_effect_to_all_target(Target, CardGive, Effect, Duration) ->
	% lists:foreach(fun({TPid, TCor, TCid}) ->
									% set_option({TPid, TCor, TCid}, CardGive, Effect, Duration) 
								% end, Target),
	% interfere_step:return_play(check_play_step).
	
% set_option({TPid, TOrder, TID}, CardGive, Effect, Duration) ->
 % TargetCardZone = card_utility:check_card_zone(TPid, TOrder, TID),
 % card_utility:set_card_option_field(TPid, TOrder, TID, receive_effect, [{CardGive, Effect, Duration}], TargetCardZone).

add_effect_to_all_target(SkillTarget, Fx) ->
	{ok, {GPid, GOrder, GId, SkillID}} = stack_pool:get_last_stack(self(), card_give_effect),
	{ok, Duration} = stack_pool:get_last_stack(self(), duration),
	card_utility:set_card_option_field(GPid, GOrder, GId, give_effect, [{SkillID, SkillTarget, Duration}]),
	lists:foreach(
		fun({PlayerPid, CardOrder, CardID}) ->
			case mnesia_odbc:is_seal_card (CardID) of
				is_seal -> card_utility:set_card_option_field(PlayerPid, CardOrder, CardID, receive_effect, [{{GPid, GOrder, GId, SkillID}, Fx, Duration}]);
				is_not_seal ->
					{ok, SFx} = card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, receive_effect),
					card_utility:update_card_option_field(PlayerPid, CardOrder, CardID, receive_effect, SFx ++ [{{GPid, GOrder, GId, SkillID}, Fx, Duration}])
			end
		end, SkillTarget).
	
activate_effect_to_target(SkillTarget, [CheckFx]) ->
	{ok, {GPid, GOrder, GID, SkillID}} = stack_pool:get_last_stack(self(), card_give_effect),
	{ok, Duration} = stack_pool:get_last_stack(self(), duration),
	card_utility:set_card_option_field(GPid, GOrder, GID, give_effect, [{SkillID, SkillTarget, Duration}]),
	lists:foreach(
		fun({TPid, TCor, TCid} ) -> 
			Fx = effect_value:check_value(GPid, GOrder, GID, CheckFx, {TPid, TCor, TCid}),
			TargetCardZone = card_utility:check_card_zone(TPid, TCor, TCid),
			card_utility:set_card_option_field(TPid, TCor, TCid, receive_effect, [{{GPid, GOrder, GID, SkillID}, Fx, Duration}], TargetCardZone),
			effect_activate:send_update_activate_effect(TPid, TCor, TCid, Fx, add)
		end, SkillTarget).

% skill_give_effect_to(CardOwner, CardOrder, CardID) ->
	% case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, give_effect) of
		% {ok, GFx} -> extract_target_give_effect(GFx);
		% _ -> []
	% end.
	
% extract_target_give_effect([]) -> [];
% extract_target_give_effect([{_MAbilityID, Target, Duration}|Tail]) ->
	% case Duration of
		% depend_on_s  -> Target ++ extract_target_give_effect(Tail);
		% _ -> extract_target_give_effect(Tail)
	% end;
% extract_target_give_effect([_|Tail]) -> extract_target_give_effect(Tail).	
