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
-module (mystic_effect).

-import (lists, [flatlength/1]).

-compile (export_all).


set_activate_mystic_to_target(CardOwner, CardOrder, CardID, {TargetType, MAbilityID, {SelectType, _SelectAmount, MTarget, Effect, Duration}}) ->
	[{IsContinuous, _EffectGotType}] = new_mystic_check:is_continuous(TargetType, MAbilityID),
	ThenID = new_mystic_check:check_then_id(MAbilityID),
	case ThenID of
		[null] -> stack_pool:set_stack_option(self(), then_mystic_ability_id, n);
		ID -> stack_pool:set_stack_option(self(), then_mystic_ability_id, ID)
	end,
	%io:format("Mystic {~p, ~p, ~p} is ~p Continuous", [CardOwner, CardOrder, CardID, IsContinuous]),
	case IsContinuous of
		null ->
			set_stack_do_effect({CardOwner, CardOrder, CardID, MAbilityID}, TargetType, MTarget, Effect, Duration);
		% กันพลาด
		n ->
			set_stack_do_effect({CardOwner, CardOrder, CardID, MAbilityID}, TargetType, MTarget, Effect, Duration);
		_ -> 
			%io:format("Mystic {~p, ~p, ~p} is ~p SelectType", [CardOwner, CardOrder, CardID, SelectType]),
			case SelectType of
				%{do_not_need_select, 0} ->
				do_not_need_select ->
					continuous_ability:set_ability_active({CardOwner, CardOrder, CardID, MAbilityID}),
					interfere_step:return_play(check_play_step);
				_ ->
					continuous_ability:set_ability_active({CardOwner, CardOrder, CardID, MAbilityID}, [{TargetType, MTarget}]),
					%interfere_step:return_play(check_play_step)
					set_stack_do_effect({CardOwner, CardOrder, CardID, MAbilityID}, TargetType, MTarget, Effect, Duration)
			end
	end.
	
set_stack_do_effect({CardOwner, CardOrder, CardID, MAbilityID}, TargetType, MTarget, Effect, Duration) ->
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	%MysticTarget = check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MTarget),
	case MTarget of
		[] -> interfere_step:return_play(check_play_step);
		_ ->
			StackOption = [
											{card_give_effect, {CardOwner, CardOrder, CardID, MAbilityID}}, 
											%{effect_got_immediately, EffectGotType}, 
											{effect_affect, Effect}, 
											{duration, Duration}, 
											{mystic_target, MTarget}, 
											%{mystic_ability_id, MAbilityID}, 
											{card_player, PlayerPid}],
			stack_pool:push_stack(self(), CardOwner, CardOrder, CardID, StackOption),
			case TargetType of
				owner -> activate_mystic_to_player(CardOwner, CardOrder, CardID);
				player -> activate_mystic_to_player(CardOwner, CardOrder, CardID);
				this -> activate_mystic_to_card(CardOwner, CardOrder, CardID);
				target -> activate_mystic_to_card(CardOwner, CardOrder, CardID);
				beyond -> activate_mystic_to_card(CardOwner, CardOrder, CardID)
			end
	end.
	
check_target_cancel_mystic(_PlayerPid, _CardOrder, _CardID, []) -> [];	
check_target_cancel_mystic(PlayerPid, CardOrder, CardID, [{TPid, TOrder, TID}|MysticTarget]) ->
	Interest = [{cancel_mystic, [all]}, {cancel_mystic, [all_opponent]}, {cancel_mystic, [opponent_relic]}],
	TargetFx = card_utility:get_all_card_effect(TPid, TOrder, TID),
	%smo_logger:fmsg("Target Effect are ~p~n", [TargetFx]),
	FxContain = function_utility:is_contain(Interest, TargetFx),
	%smo_logger:fmsg("Effect card contain are ~p~n", [FxContain]),
	case FxContain of
		[] -> [{ TPid, TOrder, TID}] ++ check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget);
		_ ->
			case new_mystic_check:check_cancel_priority(FxContain, []) of
				[1|_] -> check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget);
				[2|_] ->
					case TPid of
						PlayerPid -> [{TPid, TOrder, TID}] ++ check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget);
						_ ->	check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget)
					end;
				[3|_] ->
					case TPid of
						PlayerPid -> 
							[{TPid, TOrder, TID}] ++ check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget);
						_ -> 
							Get = mnesia_odbc:get_mystic_data(CardID, card_type),
							io:format("++++++++++++++++++mystic data get ~p~n", [Get]),
							case Get of
								{ok, 2} -> check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget);
								_ -> [{TPid, TOrder, TID}] ++ check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget)
							end
					end
			end
					
		% [{cancel_mystic, [all]}] -> check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget);
		% [{cancel_mystic, [all_opponent]}] ->
			% case TPid of
				% PlayerPid -> [{TPid, TOrder, TID}] ++ check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget);
				% _ ->	check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget)
			% end;
		% [{cancel_mystic, [opponent_relic]}] ->
			% case TPid of
				% PlayerPid -> 
					% [{TPid, TOrder, TID}] ++ check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget);
				% _ -> 
					% Get = mnesia_odbc:get_mystic_data(CardID, card_type),
					% io:format("++++++++++++++++++mystic data get ~p~n", [Get]),
					% case Get of
						% {ok, 2} -> check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget);
						% _ -> [{TPid, TOrder, TID}] ++ check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget)
					% end
			% end;			
		% _A -> %smo_logger:fmsg("Card can not cancel mystic case ~p~n", [_A]),
			% case check_cancel_priority([{cancel_mystic, [all]}, {cancel_mystic, [all_opponent]}, {cancel_mystic, [opponent_relic]}], []) of
			% [{ TPid, TOrder, TID}] ++ check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget)
	end;
check_target_cancel_mystic(PlayerPid, CardOrder, CardID, [Pid|MysticTarget]) -> [Pid] ++ check_target_cancel_mystic(PlayerPid, CardOrder, CardID, MysticTarget).
% ------------------------------------------------------------
check_then_abiltity_id(CardOwner, CardOrder, CardID) ->
	case stack_pool:get_last_stack(self(), then_mystic_ability_id) of
		{ok, n} -> casting_card:activate_mystic_to_target(CardOwner, CardOrder, CardID);
		{ok, ID} -> casting_card:do_then_ability_id(CardOwner, CardOrder, CardID, ID)
	end.

check_continuous_effect() ->
	case stack_pool:get_last_stack(self(), play) of
		{ok, activate_mystic_to_card} ->	stack_pool:set_stack_option(self(), play, mystic_card_check_continuous_effect);
		{ok, activate_mystic_to_player} -> stack_pool:set_stack_option(self(), play, mystic_player_check_continuous_effect)
	end,
	continuous_ability:check_continuous_target().
	
activate_mystic_to_player(CardOwner, CardOrder, CardID) ->
	stack_pool:set_stack_option(self(), play, activate_mystic_to_player),
	case stack_pool:get_last_stack(self(), effect_affect) of
		{ok, []} -> 
			stack_pool:pop_stack_out(self()),
			interfere_step:return_play(check_play_step);
		{ok, [Effect|Remain]} -> 
			stack_pool:set_stack_option(self(), this_activate_effect, Effect),
			stack_pool:set_stack_option(self(), effect_affect, Remain),
			activate_effect_to_player(CardOwner, CardOrder, CardID, Effect)
	end.
	
activate_mystic_to_card(CardOwner, CardOrder, CardID) ->
	stack_pool:set_stack_option(self(), play, activate_mystic_to_card),
	case stack_pool:get_last_stack(self(), effect_affect) of
		{ok, []} -> 
			stack_pool:pop_stack_out(self()),
			interfere_step:return_play(check_play_step);
		{ok, [Effect|Remain]} -> 
			stack_pool:set_stack_option(self(), this_activate_effect, Effect),
			stack_pool:set_stack_option(self(), effect_affect, Remain),
			activate_effect_to_card(CardOwner, CardOrder, CardID, Effect)
	end.
	
activate_effect_to_player(CardOwner, CardOrder, CardID, {EffectType, Effect}) ->
	case EffectType of
		draw_mystic ->
					{ok,  [Target]} = stack_pool:get_last_stack(self(), mystic_target),
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
					{ok,  [Target]} = stack_pool:get_last_stack(self(), mystic_target),
					{ok, HandCard} = mnesia_play:get_player_data (Target, hand_cards),
					HandMystic = function_utility:card_match_condition(HandCard, [{card_type, mystic}]),
					put({discard, Target}, length(HandMystic)),
					shrine_zone:card_to_shrine(Target, HandMystic);
				all_mystic ->
					{ok,  [Target]} = stack_pool:get_last_stack(self(), mystic_target),
					{ok, HandCard} = mnesia_play:get_player_data (Target, hand_cards),
					HandMystic = function_utility:card_match_condition(HandCard, [{card_type, mystic}]),
					shrine_zone:card_to_shrine(Target, HandMystic)
			end;
		reveal_card_on_hand ->
			OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
			reveal_controller:activate_player_reveal_hand(CardOwner, OpponentPid, all_card);
		mp ->
			Value = check_effect_todo(Effect),
			{ok,  [Target]} = stack_pool:get_last_stack(self(), mystic_target),
			OpposePid = mnesia_play:get_opponent_pid(Target),
			set_effect(Target, mp_rest, Value),
			{ok, ReceiverMp} = mnesia_play:get_player_data(Target, mp_rest),
			{ok, OpposeMp} = mnesia_play:get_player_data(OpposePid, mp_rest),
			gen_server:cast(Target, {send, [16#88, 16#76, 0, ReceiverMp, OpposeMp]}),
			gen_server:cast(OpposePid, {send, [16#88, 16#76, 0, OpposeMp, ReceiverMp]}),
			interfere_step:return_play(check_play_step);
		card_on_hand ->
			{ok,  [Target]} = stack_pool:get_last_stack(self(), mystic_target),
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
			{ok,  [Target]} = stack_pool:get_last_stack(self(), mystic_target),
			{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
			{_, _, _, MAbilityID} = CardGive,
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, [Target], Duration}]),
			{ok, PlayerFx} = mnesia_play:get_player_data (Target, player_effect),
			mnesia_play:set_player_data(Target, player_effect, PlayerFx++ [{CardGive, [{mp_max, Effect}], Duration}]),
			interfere_step:return_play(check_play_step);
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
	
check_effect_todo(EffectToDo) ->
	case is_integer(EffectToDo) of
		true -> EffectToDo;
		false -> 0
	end.
	
activate_effect_to_card(CardOwner, CardOrder, CardID, {EffectType, Effect}) ->
	%smo_logger:fmsg("mystic {~p, ~p, ~p} is on ~p ~n ", [CardOwner, CardOrder, CardID, card_utility:check_card_zone(CardOwner, CardOrder, CardID)]),
	case EffectType of
		curse ->
			{ok,  Target} = stack_pool:get_last_stack(self(), mystic_target),
			{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
			MysticTarget_ = skill_card_list:only_on_zone(arena_zone, Target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, MysticTarget_),
			FinalEffect =
			case Effect of
				charm_curse -> {charm_curse, CardOwner};
				_ -> Effect
			end,
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			{_, _, _, MAbilityID} = CardGive,
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, MysticTarget, Duration}]),
					curse_activation:curse_activation_assign(PlayerPid, MysticTarget, [{CardGive, [{curse, FinalEffect}], Duration}], receive_effect)
			end;
		{curse_timer, Curse} ->
			{ok,  Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->	curse_activation:curse_duration_decrese(MysticTarget, {curse, Curse}, Effect)
			end;
		player_action ->
			case Effect of
				change_line ->
					{ok, MysticTarget} = stack_pool:get_last_stack(self(), mystic_target),
					line_change:change_line(MysticTarget);
				sacrifice ->
						{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
						{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
						MysticTarget = skill_card_list:only_on_zone(arena_zone, Target),
						destroy:check_card_destroyed(PlayerPid, MysticTarget, sacrifice);
				move_to_hand ->
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					{ok, MysticTarget} = stack_pool:get_last_stack(self(), mystic_target),
					move_to_hand:move_card_to_hand(PlayerPid, MysticTarget);
				move_to_remove_zone -> 
					{ok, MysticTarget} = stack_pool:get_last_stack(self(), mystic_target),
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					remove_zone:move_to_remove_zone(PlayerPid, MysticTarget)
			end;
		{remove_counter, Amount} ->
			{ok, [{CardOwner, CardOrder, CardID}]} = stack_pool:get_last_stack(self(), target),
			CardZone = card_utility:check_card_zone (CardOwner, CardOrder, CardID),
			{ok, ReceiveFx} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect, CardZone),
			case Amount of
				all -> 
					CounterRemove = game_info:card_counter({CardZone, {CardOwner, CardOrder, CardID}});
				_ -> 
					CounterRemove = Amount
			end,
			case delete_counter:check_effect(CounterRemove, [], ReceiveFx) of
				{0, RemainFx} ->
					card_utility:update_card_option_field(CardOwner, CardOrder, CardID, receive_effect, RemainFx, CardZone);
				{CouRem, RemainFx} -> 
					card_utility:update_card_option_field(CardOwner, CardOrder, CardID, receive_effect, RemainFx, CardZone),
					{ok, SkillFx} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, skill_effect, CardZone),
					{_, RemainSFx} = delete_counter:check_effect(CouRem, [], SkillFx),
					card_utility:update_card_option_field(CardOwner, CardOrder, CardID, skill_effect, RemainSFx, CardZone)
			end,
			effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update);
		action ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->	
					case Effect of
					move_to_remove_zone -> 
						{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
						remove_zone:move_to_remove_zone(PlayerPid, MysticTarget);
					move_to_arena ->
						move_to_arena:move_to_arena(MysticTarget);
					move_to_hand ->
						{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
						move_to_hand:move_card_to_hand(PlayerPid, MysticTarget);
					discard ->
						{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
						shrine_zone:card_to_shrine(PlayerPid, MysticTarget);
					destroy -> 
						{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
						destroy:check_card_destroyed(PlayerPid, MysticTarget, destroy_effect);
					break_combine -> 
						[{TPid, TOrder, TID} | _] = MysticTarget,
						{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),	
						card_utility:add_card_status(TPid, TOrder, TID, break_combine_fx, arena_zone),
						force_break:force_break_combine(PlayerPid, TPid, TOrder, TID);
						%break_combine:assign_break_combine(PlayerPid, TPid, TOrder, TID);
					sacrifice ->
						{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
						{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
						MysticTarget = skill_card_list:only_on_zone(arena_zone, Target),
						destroy:check_card_destroyed(PlayerPid, MysticTarget, sacrifice);
					change_line ->
						line_change:change_line(MysticTarget);
					double_combine_when_not_combine ->
						[{TPid, TOrder, TID}] = MysticTarget,
						{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
						{GOwner, GOrder, GID, MAbilityID} = CardGive,
						{ok, Duration} = stack_pool:get_last_stack(self(), duration),
						card_utility:set_card_option_field(TPid, TOrder, TID, receive_effect, [{CardGive, [{special, [{CardOwner, CardOrder, CardID}]}], Duration}], arena_zone),
						card_utility:set_card_option_field(GOwner, GOrder, GID, give_effect, [{MAbilityID, [{TPid, TOrder, TID}], Duration}]),
						check_double_combine_effect(CardOwner, CardOrder, CardID, TPid, TOrder, TID);
					be_inactive ->
						{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
						{ok, Duration} = stack_pool:get_last_stack(self(), duration),
						{GPid, GOrder, GID, MAbilityID} = CardGive,
						card_utility:set_card_option_field(GPid, GOrder, GID, give_effect, [{MAbilityID, MysticTarget, Duration}]),
						add_effect_to_all_target(MysticTarget, CardGive, [{action, inactive}], Duration);
					be_active ->
						lists:foreach(
													fun({TPid, TOrder, TID}) ->
														arena_zone:set_seal_active(TPid, TOrder, TID)
													end, MysticTarget),
						interfere_step:return_play(check_play_step)					
				end
			end;
		heal_curse ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ -> 
					CurseHeal =
					case Effect of
						[charm_curse] -> game_info:special_curse(charm_curse, MysticTarget);
						[last_dance_curse] -> game_info:special_curse(last_dance_curse, MysticTarget);
						_ -> Effect
					end,
					curse_activation:heal_target_curse(MysticTarget, CurseHeal)
			end;
		atk ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					{ok, Duration} = stack_pool:get_last_stack(self(), duration),
					{_, _, _, MAbilityID} = CardGive,
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, MysticTarget, Duration}]),
					add_effect_to_all_target(MysticTarget, CardGive, [{EffectType, Effect}], Duration)
			end;
		protect_attack ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			io:format("mystic effect protect_attack, ~p~n", [Target]),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					{ok, Duration} = stack_pool:get_last_stack(self(), duration),
					{_, _, _, MAbilityID} = CardGive,
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, MysticTarget, Duration}]),
					add_effect_to_all_target(MysticTarget, CardGive, [{EffectType, Effect}], Duration)
			end;
		protect_skill ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					{ok, Duration} = stack_pool:get_last_stack(self(), duration),
					{_, _, _, MAbilityID} = CardGive,
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, MysticTarget, Duration}]),
					add_effect_to_all_target(MysticTarget, CardGive, [{EffectType, Effect}], Duration)
			end;
		skill ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					{ok, Duration} = stack_pool:get_last_stack(self(), duration),
					{_, _, _, MAbilityID} = CardGive,
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, MysticTarget, Duration}]),
					add_effect_to_all_target(MysticTarget, CardGive, [{EffectType, Effect}], Duration)
			end;
		ability ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					{ok, Duration} = stack_pool:get_last_stack(self(), duration),
					{_, _, _, MAbilityID} = CardGive,
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, MysticTarget, Duration}]),
					add_effect_to_all_target(MysticTarget, CardGive, [{EffectType, Effect}], Duration)
			end;
		when_attack ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					{ok, Duration} = stack_pool:get_last_stack(self(), duration),
					{_, _, _, MAbilityID} = CardGive,
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, MysticTarget, Duration}]),
					add_effect_to_all_target(MysticTarget, CardGive, [{EffectType, Effect}], Duration)
			end;
		change_line ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					{ok, Duration} = stack_pool:get_last_stack(self(), duration),
					{_, _, _, MAbilityID} = CardGive,
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, MysticTarget, Duration}]),
					add_effect_to_all_target(MysticTarget, CardGive, [{EffectType, Effect}], Duration)
			end;
		move ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					{ok, Duration} = stack_pool:get_last_stack(self(), duration),
					{_, _, _, MAbilityID} = CardGive,
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, MysticTarget, Duration}]),
					add_effect_to_all_target(MysticTarget, CardGive, [{EffectType, Effect}], Duration)
			end;
		check_condition ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					case Effect of
						{Condition, SubFx} ->
							CardMatch = function_utility:card_match_condition(MysticTarget, Condition),
							case CardMatch of
								[] -> activate_effect_to_card(CardOwner, CardOrder, CardID, {mismatch_condtion, effect});%interfere_step:return_play(check_play_step);
								_ -> activate_effect_to_card(CardOwner, CardOrder, CardID, SubFx)
							end
					end
			end;
		swap_power ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			{_, _, _, MAbilityID} = CardGive,
			lists:foreach(fun({TPid, TOrder, TID}) ->
				seal_card:swap_card_power(TPid, TOrder, TID, Effect),
				set_option({TPid, TOrder, TID}, CardGive, [{EffectType, Effect}], Duration),
				effect_activate:send_update_activate_effect(TPid, TOrder, TID, [], update) end, MysticTarget),
			card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, MysticTarget, Duration}]),
			interfere_step:return_play(check_play_step);
			%add_effect_to_all_target(MysticTarget, CardGive, [{EffectType, Effect}], Duration);
		elem ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			lists:foreach(fun({TPid, TOrder, TID}) ->
				seal_card:change_card_element(TPid, TOrder, TID, Effect),
				effect_activate:send_update_activate_effect(TPid, TOrder, TID, [], update) end, MysticTarget),
			{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
			{_, _, _, MAbilityID} = CardGive,
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			add_effect_to_all_target(MysticTarget, CardGive, [{EffectType, Effect}], Duration),
			card_utility:set_card_option_field(CardOwner, CardOrder, CardID, give_effect, [{MAbilityID, MysticTarget, Duration}]),
			interfere_step:return_play(check_play_step);
		_ ->
			{ok, Target} = stack_pool:get_last_stack(self(), mystic_target),
			MysticTarget = check_target_cancel_mystic(CardOwner, CardOrder, CardID, Target),
			case MysticTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
					{ok, Duration} = stack_pool:get_last_stack(self(), duration),
					{GPid, GOrder, GID, MAbilityID} = CardGive,
					card_utility:set_card_option_field(GPid, GOrder, GID, give_effect, [{MAbilityID, MysticTarget, Duration}]),
					activate_effect_to_target(MysticTarget, [{CardGive, {EffectType, Effect}, Duration}])
			end
	end.
	
add_effect_to_all_target(Target, CardGive, Effect, Duration) ->
	lists:foreach(fun({TPid, TCor, TCid}) ->
									set_option({TPid, TCor, TCid}, CardGive, Effect, Duration) 
								end, Target),
	interfere_step:return_play(check_play_step).
	
set_option({TPid, TOrder, TID}, CardGive, Effect, Duration) ->
 TargetCardZone = card_utility:check_card_zone(TPid, TOrder, TID),
 card_utility:set_card_option_field(TPid, TOrder, TID, receive_effect, [{CardGive, Effect, Duration}], TargetCardZone).

activate_effect_to_target(Target, [{CardGive, Fx, Duration}]) ->
	lists:foreach(
									fun({TPid, TCor, TCid}) -> 
										TargetCardZone = card_utility:check_card_zone(TPid, TCor, TCid),
										{GOwner, GOrder, GID, _} = CardGive,
										ResFX = effect_value:check_value(GOwner, GOrder, GID, Fx, {TPid, TCor, TCid}),
										card_utility:set_card_option_field(TPid, TCor, TCid, receive_effect, [{CardGive, ResFX, Duration}], TargetCardZone),
										effect_activate:send_update_activate_effect(TPid, TCor, TCid, [], update)
									end, Target),
	interfere_step:return_play(check_play_step).

mystic_give_effect_to(CardOwner, CardOrder, CardID) ->
	case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, give_effect) of
		{ok, GFx} -> extract_target_give_effect(GFx);
		_ -> []
	end.
	
extract_target_give_effect([]) -> [];
extract_target_give_effect([{_MAbilityID, Target, Duration}|Tail]) ->
	case Duration of
		depend_on_s  -> Target ++ extract_target_give_effect(Tail);
		_ -> extract_target_give_effect(Tail)
	end;
extract_target_give_effect([_|Tail]) -> extract_target_give_effect(Tail).	
% ------------- Mystic Fx Control ----------------------
% keep_mystic_ability (PlayerPid, CardOrder, CardID) ->
	% io:format("mystic_effect:keep_mystic_ability~n"),
	% {ok, AbilityNumber} = stack_pool:get_last_stack (self(), mystic_ability_number),
	% OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	% Result = m_ability:start_effect ({arena_zone, {PlayerPid, CardOrder, CardID}}, AbilityNumber, OpponentPid),
	% MysticAbility = mystic_ability (Result),
	% stack_pool:set_stack_option (self(), mystic_ability, MysticAbility).
% 
% mystic_ability ([]) -> [];
% mystic_ability ([{{ThenAssign, Then}, PlayerFx, OppFx, SelfFx, OtherFx, _} | FxList]) ->
	% get_tuple_fx_data (ThenAssign, Then, [PlayerFx, OppFx, SelfFx, OtherFx], 0) ++ mystic_ability (FxList).
% 
% get_tuple_fx_data (_, _, [], _) -> [];
% get_tuple_fx_data (ThenAssign, Then, [{{CardOwner, CardOrder, CardID, AbilityID}, {FxTargetType, FxTargetNumber, Fx}, DurationFx} | FxTuple], TupleNumber) ->
	% case TupleNumber of
		% 0 ->	PlayerTarget = player,
			% case FxTargetType of
				% player_select_exact_player ->
					% FxTarget = [player, opponent];
				% _ ->	FxTarget = [player]
			% end;
		% 1 ->	PlayerTarget = opponent,
			% case FxTargetType of
				% player_select_exact_player ->
					% FxTarget = [player, opponent];
				% _ ->	FxTarget = [opponent]
			% end
	% end,
	% card_utility:set_card_option_field (CardOwner, CardOrder, CardID, give_effect, [{[], Fx, PlayerTarget, AbilityID, DurationFx}]),
	% TupleFx = [{ThenAssign, Then, AbilityID, FxTargetType, FxTargetNumber, FxTarget, {CardOwner, CardOrder, CardID}, Fx, DurationFx}],
	% TupleFx ++ get_tuple_fx_data (ThenAssign, Then, FxTuple, TupleNumber + 1);
% get_tuple_fx_data (ThenAssign, Then, [{{PlayerPid, CardOrder, CardID, AbilityID}, TARGET, {FxTargetType, FxTargetNumber, Fx}, DurationFx} | FxTuple], TupleNumber) ->
	% TupleFx = [{ThenAssign, Then, AbilityID, FxTargetType, FxTargetNumber, TARGET, {PlayerPid, CardOrder, CardID}, Fx, DurationFx}],
	% TupleFx ++ get_tuple_fx_data (ThenAssign, Then, FxTuple, TupleNumber + 1);
% get_tuple_fx_data (ThenAssign, Then, [{} | FxTuple], TupleNumber) ->
	% get_tuple_fx_data (ThenAssign, Then, FxTuple, TupleNumber + 1).

% check_ability_select_target (PlayerPid) -> 
	% {ok, MysticAbility} = stack_pool:get_last_stack (self(), mystic_ability),
	% check_ability_select_target (PlayerPid, MysticAbility).

% check_ability_select_target (_, []) ->
	% stack_pool:set_stack_option (self(), play, play_mystic_target_done),
	% interfere_step:return_play (check_play_step);
% check_ability_select_target (PlayerPid, [FxTuple | MysticAbility]) ->
	% stack_pool:set_stack_option (self(), mystic_ability, MysticAbility),
	% case check_fx_target_type (FxTuple) of
		% {select_target_exact_card, Target, TNumber} ->
			% stack_pool:set_stack_option (self(), wait_player_select_target, FxTuple),
			% stack_pool:set_stack_option (self(), target_type, ecard),
			% stack_pool:set_stack_option (self(), mystic_target, Target),
			% {ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack(self()),
			% % Pasted = 
			% % case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, paste_to, arena_zone) of
				% % {ok, [{SPid, SOrder, Sid}]} -> [{SPid, SOrder, Sid}];
				% % _ -> []
			% % end,
			% TargetReply = get_reply_target (Target, PlayerPid),
			% % 0 คือลักษณะการเลือกเป้าหมายแบบเจาะจงจำนวน
			% % 1 คือลักษณะการเลือกเป้าหมายแบบที่สามารถเลือกได้ตั้งแต่ 1 ถึงจำนวนที่กำหนด
			% gen_server:cast(self(), {activate_select_mystic_target, PlayerPid, 0, TNumber, flatlength(Target), TargetReply});
		% {select_target_exact_player, TNumber} ->
			% stack_pool:set_stack_option (self(), wait_player_select_target, FxTuple),
			% stack_pool:set_stack_option (self(), target_type, eplayer),
			% gen_server:cast(self(), {activate_select_mystic_target, PlayerPid, 1, TNumber, 0, []});
		% random_target_exact_card ->
			% stack_pool:add_stack_option_field (self(), target_selected, [FxTuple]),
			% stack_pool:set_stack_option (self(), random_target, random),
			% check_ability_select_target (MysticAbility);
		% select_target_none ->
			% stack_pool:add_stack_option_field (self(), target_selected, [FxTuple]),
			% check_ability_select_target (MysticAbility)
	% end.

% get_reply_target ([], _) -> [];
% get_reply_target ([{PlayerPid, CardOrder, CardID} | T], PlayerPid) ->
	% OptionalData = check_card_zone (PlayerPid, CardOrder, CardID),
	% [1, CardOrder, <<CardID:16>>] ++ OptionalData ++ get_reply_target (T, PlayerPid);
% get_reply_target ([{OwnerPid, CardOrder, CardID} | T], PlayerPid) ->
	% OptionalData = check_card_zone (OwnerPid, CardOrder, CardID),
	% [0, CardOrder, <<CardID:16>>] ++ OptionalData ++ get_reply_target (T, PlayerPid).

% check_card_zone (PlayerPid, CardOrder, CardID) ->
	% CardZone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	% case CardZone of
		% arena_zone -> ZoneInt  = 0;
		% shrine_cards -> ZoneInt  = 1;
		% seal_deck -> ZoneInt  = 2;
		% mystic_deck -> ZoneInt  = 3;
		% hand_cards -> ZoneInt  = 4;
		% remove_zone -> ZoneInt  = 5;
		% Other -> io:format ("Card on ~p out of range~n", [Other]),
				% ZoneInt = 99
	% end,
	% case mnesia_odbc:is_seal_card (CardID) of
		% is_seal ->
			% {ok, Line} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, line, CardZone);
		% is_not_seal ->
			% Line = 0
	% end,
	% [ZoneInt, Line].

% {then_assign, then, ability_id, target_type, target_number, target, fx_flag, card_give_fx, fx, duration}
% check_fx_target_type ({_, _, _, player_select_exact_target, TNumber, Target, _, _, _}) ->
	% {select_target_exact_card, Target, TNumber};
% check_fx_target_type ({_, _, _, player_select_exact_player, TNumber, _, _, _, _}) ->
	% {select_target_exact_player, TNumber};
% check_fx_target_type ({_, _, _, random_select_exact_target, _, _, _, _, _}) ->
	% random_target_exact_card;
% check_fx_target_type ({_, _, _, do_not_need_select, _, _, _, _, _}) ->
	% select_target_none.

% update_mystic_target (TargetSelected) ->
	% case stack_pool:get_last_stack (self(), wait_player_select_target) of
		% {ok, FxTuple} ->
			% set_update_target(FxTuple, TargetSelected),
			% stack_pool:remove_stack_option (self(), wait_player_select_target);
		% _ ->	io:format ("Stack option wait_player_select_target not found~n")
	% end.

% set_update_target ({TAssign, Then, AbilityId, TargetType, TargetNumber, _, GFx, Fx, Duration}, TargetSelected) ->
	% case is_pid(TargetSelected) of
		% true ->
			% case GFx of
				% {TargetSelected, _, _} ->
					% Target = [player];
				% _ ->	Target = [opponent]
			% end;
		% false ->
			% Target = TargetSelected
	% end,
	% Ability = [{TAssign, Then, AbilityId, TargetType, TargetNumber, Target, GFx, Fx, Duration}],
	% stack_pool:add_stack_option_field (self(), target_selected, Ability).

% verify_mystic_target (PlayerPid, CardOrder, CardID) ->
	% {ok, AbilityNumber} = stack_pool:get_last_stack (self(), mystic_ability_number),
	% OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	% Result = m_ability:start_effect ({arena_zone, {PlayerPid, CardOrder, CardID}}, AbilityNumber, OpponentPid),
	% MysticResult = mystic_ability(Result), 
% %	io:format ("Verify MResult ~p~n", [MysticResult]),
	% case MysticResult of
		% [] ->	stack_pool:set_stack_option (self(), can_pasted, non_pasted),
			% casting_card:update_casting_mystic_success (PlayerPid, CardOrder, CardID);
		% _ ->	verity_mystic_target_condition (PlayerPid, CardOrder, CardID, MysticResult)
	% end.

% verity_mystic_target_condition (PlayerPid, CardOrder, CardID, MysticResult) ->
	% case stack_pool:get_last_stack (self(), target_selected) of
		% {ok, MysticAbility} ->
			% case check_match_target (MysticAbility, MysticResult) of
				% [] ->	stack_pool:remove_stack_option (self(), tuple_mismatch),
					% interfere_step:return_play (check_play_step);
				% TupleMismatch ->
					% stack_pool:set_stack_option (self(), mystic_ability, TupleMismatch),
					% case stack_pool:get_last_stack (self(), play) of
						% {ok, play_casting_card_10} ->
							% remove_stack_target_selected_mismatch (TupleMismatch),
							% keep_mystic_ability (PlayerPid, CardOrder, CardID),
							% casting_card:activate_select_mystic_target (PlayerPid, CardOrder, CardID);
						% {ok, play_move_card_to_arena_7} ->
							% move_to_arena:move_card_to_arena_5 (PlayerPid, CardOrder, CardID)
					% end
			% end;
		% _ ->	io:format ("Get mystic target selected error!!!!~n")
	% end.

% remove_stack_target_selected_mismatch (TupleMismatch) ->
	% case stack_pool:get_last_stack (self(), target_selected) of
		% {ok, TargetSelected} ->
			% check_remove_target_selected (TargetSelected, TupleMismatch);
		% _ ->	remove_completed
	% end.
% 
% check_remove_target_selected (TargetSelected, []) ->
	% stack_pool:set_stack_option (self(), target_selected, TargetSelected);
% check_remove_target_selected (TargetSelected, [AbilityMismatch | TupleMismatch]) ->
	% TSUpdate = check_remove_mismatch_target_selected (TargetSelected, AbilityMismatch),
	% check_remove_target_selected (TSUpdate, TupleMismatch).
% 
% check_remove_mismatch_target_selected ([], _) -> [];
% check_remove_mismatch_target_selected ([Ability | TargetSelected], AbilityMisMatch) ->
	% {_, _, Aid, _, _, _, _, _, _} = Ability,
	% case AbilityMisMatch of
		% {_, _, Aid, _, _, _, _, _, _} ->
			% check_remove_mismatch_target_selected (TargetSelected, AbilityMisMatch);
		% _ ->	[Ability] ++ check_remove_mismatch_target_selected (TargetSelected, AbilityMisMatch)
	% end.
% 
% check_match_target ([], _) -> [];	
% check_match_target ([{TA, T, Aid, TT, TN, MTarget, GFx, Fx, Du} | MAbility], MResult) ->
	% case get_mystic_result_target (Aid, MResult, MTarget) of
		% match ->
			% check_match_target (MAbility, MResult);
		% mismatch ->
			% [{TA, T, Aid, TT, TN, MTarget, GFx, Fx, Du}] ++ check_match_target (MAbility, MResult)
	% end.
% 
% get_mystic_result_target (Aid, [], MTarget) ->
	% io:format ("Aid ~p not found target ~p match~n", [Aid, MTarget]),
	% mismatch;
% get_mystic_result_target (Aid, [{_, _, Aid, _, _, RTarget, _, _, _} | MResult], MTarget) ->
	% smo_logger:fmsg("Mystic target Selected is~p ~nTarget Which System Check are ~p~n", [MTarget, RTarget]),
	% case MTarget -- RTarget of
		% [] -> match;
		% _ ->	get_mystic_result_target (Aid, MResult, MTarget)
	% end;
% get_mystic_result_target (Aid, [_ | MResult], MTarget) -> get_mystic_result_target (Aid, MResult, MTarget).
% 
% get_mystic_target () ->
	% case stack_pool:get_last_stack (self(), target_selected) of
		% {ok, MAbility} ->
			% MTarget = get_target (MAbility),
			% case lists:flatlength (MTarget) of
				% 0 -> io:format ("ERROR!!! No have target~n");
				% 1 -> MTarget;
				% _ -> io:format ("ERROR!!! have target more than 1~n")
			% end;
		% _ ->	io:format ("Get mystic target selected error!!!!~n")
	% end.
% 
% get_target ([]) -> [];
% get_target ([{_, _, _, _, _, MTarget, _, _, _} | MAbility]) ->
	% MTarget ++ get_target (MAbility).

	%stack_pool:set_stack_option (self(), effect_to_player, MTarget),
	%stack_pool:set_stack_option (self(), target_type, TargetType),
	%FxUpdate = check_curse_effect(Fx),
	%stack_pool:set_stack_option(self(), card_fx, [{GFx, FxUpdate, Du}]),
	% case TargetType of
		% owner ->
			% io:format ("mystic to player ~n"),
			% stack_pool:set_stack_option (self(), target_player, player),
			% mystic_card:activate_effect_to_player(PlayerPid, CardOrder, CardID, activate_only);
		% player ->
			% io:format ("mystic to player ~n"),
			% stack_pool:set_stack_option (self(), target_player, opponent),
			% mystic_card:activate_effect_to_player(PlayerPid, CardOrder, CardID, activate_only);
		% Target ->
			% io:format ("mystic to target ~p~n", [Target]),
			% mystic_card:activate_mystic_effect(PlayerPid, CardOrder, CardID, MTarget)
	%end.

% check_curse_effect ([]) -> [];
% check_curse_effect ([{curse, charm_curse} | Fx]) ->
	% {ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	% [{curse, {charm_curse, PlayerPid}}] ++ check_curse_effect (Fx);
% check_curse_effect ([F | Fx]) -> [F] ++ check_curse_effect (Fx).

% update_ability_continues () ->
	% {ok, MAbility} = stack_pool:get_last_stack (self(), target_selected),
	% %io:format ("Then ability id list ~p~n", [MAbility]),
	% TAidL = update_ability_continues (MAbility),
% %	io:format ("Then ability id list ~p~n", [TAidL]),
	% case TAidL of
		% [] ->	seperate_done;
		% ThenAbilityIdList ->
			% case update_then_ability (ThenAbilityIdList, MAbility) of
				% [] -> ok;
				% ThenAbilityList ->
					% stack_pool:set_stack_option (self(), then_ability, ThenAbilityList),
% %					io:format ("Diff ~p ~p~n", [MAbility, ThenAbilityList]),
					% AbilityActivate = MAbility -- ThenAbilityList,
					% stack_pool:set_stack_option (self(), target_selected, AbilityActivate),
					% update_ability_continues ()
			% end
	% end.

% update_ability_continues ([]) -> [];
% update_ability_continues ([{null, null, _, _, _, _, _, _, _} | MAbility]) ->
	% update_ability_continues (MAbility);
% update_ability_continues ([{null, TAid, _, _, _, _, _, _, _} | MAbility]) ->
	% [TAid] ++ update_ability_continues (MAbility).

% update_then_ability ([], _) -> [];
% update_then_ability ([TAid | ThenAbilityIdList], MAbility) ->
	% get_then_ability (TAid, MAbility) ++ update_then_ability (ThenAbilityIdList, MAbility).
% 
% get_then_ability (_, []) -> [];
% get_then_ability (TAid, [{A, B, TAid, C, D, E, F, G, H} | MAbility]) ->
	% [{A, B, TAid, C, D, E, F, G, H}] ++ get_then_ability (TAid, MAbility);
% get_then_ability (TAid, [_ | MAbility]) -> get_then_ability (TAid, MAbility).
% 
% check_ability_continues () ->
	% ThenAbility = stack_pool:get_last_stack (self(), then_ability),
% %	io:format ("Then ability ~p~n", [ThenAbility]),
	% case ThenAbility of
		% {ok, []} ->
			% stack_pool:set_stack_option (self(), play, casting_mystic_card_11_1),
			% interfere_step:return_play (check_play_step);
		% {ok, ThenAbilityList} ->
			% stack_pool:set_stack_option (self(), play, casting_mystic_card_11),
			% stack_pool:set_stack_option (self(), target_selected, ThenAbilityList),
			% stack_pool:remove_stack_option (self(), then_ability),
			% interfere_step:return_play (check_play_step);
		% _ ->	stack_pool:set_stack_option (self(), play, casting_mystic_card_11_1),
			% interfere_step:return_play (check_play_step)
	% end.
% 
player_select_mystic_effect(PlayerPid, Data) ->
	case stack_pool:get_last_stack(self(), mystic_effect) of
		{ok, {magical_world, PlayerPid, CardOrder, CardID, TPid, TOrder, Tid}} ->
			[_, CombineOption] = Data,
			set_card_combination_effect(PlayerPid, CardOrder, CardID, CombineOption, TPid, TOrder, Tid);
		O -> io:format ("Mystic fx ~p not control~n", [O])
	end.
% -------------- Magical World - ถ้า Seal ที่ [S] ติดไม่ได้รวมร่าง Seal นั้นนับว่าอยู่ในสภาพ Double Combination ------------------------
check_double_combine_effect(PlayerPid, CardOrder, CardID, TPid, TOrder, Tid) ->
	% ตรวจสอบว่าการ์ดมีท่ารวมร่างแบบ Double กี่ท่า
	stack_pool:set_stack_option(self(), mystic_effect, {magical_world, PlayerPid, CardOrder, CardID, TPid, TOrder, Tid}),
	{OptionSize, OptionList} = mnesia_odbc:size_combine_type (Tid, double_combine),
%	io:format ("Double combine size ~p - Option list ~p~n", [OptionSize, OptionList]),
	case OptionSize of
		0 -> interfere_step:return_play (check_play_step);
		1 -> set_card_combination_effect(PlayerPid, CardOrder, CardID, OptionList, TPid, TOrder, Tid);
		_ -> gen_server:cast(self(), {activate_select_mystic_option, PlayerPid, 16#01, [TPid, TOrder, Tid]})
	end.

set_card_combination_effect(PlayerPid, CardOrder, CardID, CombineOption, TPid, TOrder, Tid) ->
	case card_utility:get_card_option_field(TPid, TOrder, Tid, combine, arena_zone) of
		{ok, []} ->
			card_utility:add_card_status(TPid, TOrder, Tid, combine_with_effect, arena_zone),
			{ok, PowerChange} =  mnesia_odbc:get_power_change_data(Tid, CombineOption),
			ArenaCardOption = arena_zone:get_card_option(TPid, TOrder, Tid),
		%	io:format ("Arena card option ~p~n", [ArenaCardOption]),
			case ArenaCardOption of
				{ok, TCardOption} ->
					ArenaCard = arena_zone:get_card(PlayerPid, CardOrder, CardID),
		%			io:format ("Arena card ~p~n", [ArenaCard]),
					case ArenaCard of
						{ok, Card} ->
							% ทำให้ การ์ดใบรองรวมร่างเป็น มิสติกใบนี้ และมีค่าพลังเปลี่ยนไปตามที่ควรจะเป็น
							case get(support_target) of
								undefined -> 	put(support_target, [{{TPid, TOrder, Tid}, CombineOption}]);
								MysticSupport -> put(support_target, MysticSupport ++ [{{TPid, TOrder, Tid}, CombineOption}])
							end,
							OptionChange = [{combine_support, Card}, {combine_option, CombineOption}, {combine_power, PowerChange}],
							update_option_effect (TPid, TOrder, Tid, TCardOption, OptionChange),
							% 16#01 คือ mystic fx id ของการ์ด Magical World
							gen_server:cast(self(), {update_select_mystic_fx, TPid, 16#01, [TOrder, <<Tid:16>>, CombineOption]}),
							effect_activate:send_update_activate_effect(TPid, TOrder, Tid, [], update);
						{error, _} ->
							interfere_step:return_play(check_play_step)
					end;
				{error, _} ->
					interfere_step:return_play(check_play_step)
			end;
		_ ->
			% card_utility:add_card_status(TPid, TOrder, Tid, combine_with_effect, arena_zone),
			% case get(support_target) of
				% undefined -> 	put(support_target, [{{TPid, TOrder, Tid}, CombineOption}]);
				% MysticSupport -> put(support_target, MysticSupport ++ [{{TPid, TOrder, Tid}, CombineOption}])
				% %_ -> interfere_step:return_play(check_play_step)
			% end,
			% gen_server:cast(self(), {update_select_mystic_fx, TPid, 16#01, [TOrder, <<Tid:16>>, CombineOption]})

			card_utility:add_card_status(TPid, TOrder, Tid, combine_with_effect, arena_zone),
			{ok, CardUpdate} = arena_zone:get_card(PlayerPid, CardOrder, CardID),
			case get(support_target) of
				undefined -> 	put(support_target, [{{TPid, TOrder, Tid}, CombineOption}]);
				MysticSupport -> put(support_target, MysticSupport ++ [{{TPid, TOrder, Tid}, CombineOption}])
				%_ -> interfere_step:return_play(check_play_step)
			end,
			{ok, CombineState} = card_utility:get_card_option_field (TPid, TOrder, Tid, combine, arena_zone),
			%UpdateOption = [Card, {option_number, CombineOption}, {power_change, PowerChange}],
			CombineUpdate = seal_card:add_support_cards (CombineState, CardUpdate),
			card_utility:update_card_option_field (TPid, TOrder, Tid, combine, CombineUpdate),
			gen_server:cast(self(), {update_select_mystic_fx, TPid, 16#01, [TOrder, <<Tid:16>>, CombineOption]})
	end.
			

update_option_effect (PlayerPid, CardOrder, CardID, OptionUpdate, []) -> 
	arena_zone:update_card (PlayerPid, CardOrder, CardID, OptionUpdate);
update_option_effect (PlayerPid, CardOrder, CardID, TCardOption, [{Field, Data} | OptionChange]) ->
	case Field of
		combine_option ->
			OptionUpdate = seal_card:set_combine_option (TCardOption, Data);
		combine_power ->
			OptionUpdate = seal_card:change_combine_power (TCardOption, Data);
		combine_support ->
			OptionUpdate = seal_card:add_support_seal (TCardOption, Data)
	end,
%	io:format("OptionUpdate ~p~n", [OptionUpdate]),
	update_option_effect (PlayerPid, CardOrder, CardID, OptionUpdate, OptionChange).% ------------------ Heal Curse -------------------
heal_target_curse ([], _) ->
	case stack_pool:get_last_stack (self(), destroyed) of
		{ok, []} ->
			interfere_step:return_play (check_play_step);
		{error, _} ->
			interfere_step:return_play(check_play_step);
		{ok, [{CardOwner, CardOrder, CardID} | Cards]} ->
			destroy:check_card_destroyed (CardOwner, [{CardOwner, CardOrder, CardID}] ++ Cards, heal_destroy)
	end;
heal_target_curse([{PlayerPid, CardOrder, CardID} | Targets], Curse) ->
	case lists:flatlength(Curse) of
		1 ->
			Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
			heal_curse(PlayerPid, CardOrder, CardID, Curse, Zone),
			heal_target_curse(Targets, Curse);
		_ ->
			[_Curse|Tail] = Curse,
			Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
			heal_curse (PlayerPid, CardOrder, CardID, [_Curse], Zone),
			heal_target_curse(Targets, Tail)
	end.
heal_curse(CardOwner, CardOrder, CardID, [], Zone) -> 
	{ok, RFX} = card_utility:get_card_option_field (CardOwner, CardOrder, CardID, receive_effect, Zone),
	%{ok, SFx} = card_utility:get_card_option_field (CardOwner, CardOrder, CardID, skill_effect, Zone),
	URFx = remove_heal_effect(RFX),
	%USFx = remove_heal_effect (SFx),
	card_utility:update_card_option_field(CardOwner, CardOrder, CardID, receive_effect, URFx, Zone);
	%card_utility:update_card_option_field (CardOwner, CardOrder, CardID, skill_effect, USFx, Zone);
heal_curse(CardOwner, CardOrder, CardID, [HCurse | Curse], Zone) ->
	smo_logger:fmsg("heal curse ~p~n", [HCurse]),
	{ok, RFX} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect, Zone),
	%{ok, SFx} = card_utility:get_card_option_field (CardOwner, CardOrder, CardID, skill_effect, Zone),
	URFx = 
	case HCurse of
		all ->	
			% case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, line, arena_zone) of
				% {ok, 255} -> card_utility:set_card_option_field(CardOwner, CardOrder, CardID, line, 0, arena_zone);
				% {ok, 256} -> card_utility:set_card_option_field(CardOwner, CardOrder, CardID, line, 1, arena_zone)
			% end,							
			heal_all_curse(RFX);
			%USFx = heal_all_curse (SFx);
		_ ->
			% case HCurse of
				% dimension_curse ->
					% case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, line, arena_zone) of
						% {ok, 255} -> card_utility:set_card_option_field(CardOwner, CardOrder, CardID, line, 0, arena_zone);
						% {ok, 256} -> card_utility:set_card_option_field(CardOwner, CardOrder, CardID, line, 1, arena_zone)
					% end;
				% _ -> ""
			% end,					
			remove_curse_effect(RFX, HCurse)
			%USFx = remove_curse_effect (SFx, HCurse)
	end,
	card_utility:update_card_option_field(CardOwner, CardOrder, CardID, receive_effect, URFx, Zone),
	%card_utility:update_card_option_field (CardOwner, CardOrder, CardID, skill_effect, USFx, Zone),
	case stack_pool:get_last_stack(self(), remove_effect_data) of
		{ok, []} -> stack_pool:remove_stack_option (self(), remove_effect_data);
		{ok, Fx} ->
			effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, Fx, remove),
			stack_pool:remove_stack_option(self(), remove_effect_data);
		{error, _} -> not_process
	end,
	heal_curse(CardOwner, CardOrder, CardID, Curse, Zone).

heal_all_curse([]) -> [];
heal_all_curse([{GFx, Fx, Duration} | CardFx]) ->
	case remove_all_prefix_effect(GFx, Fx, curse) of
		[] -> heal_all_curse(CardFx);
		UFx -> [{GFx, UFx, Duration}] ++ heal_all_curse(CardFx)
	end.

remove_all_prefix_effect(_, [], _) -> [];
remove_all_prefix_effect({CardOwner, CardOrder, CardID, AbilitId}, [{Prefix, Value} | Fx], Prefix) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			stack_pool:add_stack_option_field(self(), remove_effect_data, [{Prefix, Value}]);
		is_not_seal ->
			ReceiveFx = mystic_effect:mystic_give_effect_to(CardOwner, CardOrder, CardID),
			lists:foreach(fun({SPid, SOrder, Sid}) -> end_of_subturn:remove_mystic_and_effect(CardOwner, CardOrder, CardID, SPid, SOrder, Sid) end, ReceiveFx),
			stack_pool:add_stack_option_field(self(), destroyed, [{CardOwner, CardOrder, CardID}])
	end,
	remove_all_prefix_effect({CardOwner, CardOrder, CardID, AbilitId}, Fx, Prefix);
remove_all_prefix_effect(GFx, [Any | Fx], Prefix) ->
	[Any] ++ remove_all_prefix_effect(GFx, Fx, Prefix).

remove_curse_effect ([], _) -> [];
remove_curse_effect ([{GFx, Fx, Duration} | CardFx], HCurse) ->
	case remove_effect (GFx, Fx, {curse, HCurse}) of
		[] -> remove_curse_effect (CardFx, HCurse);
		UFx -> [{GFx, UFx, Duration}] ++ remove_curse_effect (CardFx, HCurse)
	end.

remove_effect (_, [], _) -> [];
remove_effect ({CardOwner, CardOrder, CardID, AbilityID}, [CurseFx | Fx], CurseFx) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			stack_pool:add_stack_option_field (self(), remove_effect_data, [CurseFx]);
		is_not_seal ->
			{ok, [{SPid, SOrder, Sid}]} = card_utility:get_card_option_field (CardOwner, CardOrder, CardID, paste_to, arena_zone),
			stack_pool:add_stack_option_field (self(), destroyed, [{CardOwner, CardOrder, CardID}]),
			end_of_subturn:remove_mystic_and_effect (CardOwner, CardOrder, CardID, SPid, SOrder, Sid)
	end,
	remove_effect ({CardOwner, CardOrder, CardID, AbilityID}, Fx, CurseFx);
remove_effect (GFx, [Any | Fx], CurseFx) ->
	[Any] ++ remove_effect (GFx, Fx, CurseFx).

remove_heal_effect ([]) -> [];
remove_heal_effect ([{GFx, Fx, Duration} | CardFx]) ->
	case remove_effect (Fx) of
		[] ->	remove_heal_effect (CardFx);
		UFx -> [{GFx, UFx, Duration}] ++ remove_heal_effect (CardFx)
	end.

remove_effect ([]) -> [];
remove_effect ([{heal_curse, [all]} | Fx]) ->
	remove_effect (Fx);
remove_effect ([Any | Fx]) ->
	[Any] ++ remove_effect (Fx).

heal_target_curse ([], _, ability) ->
	case stack_pool:get_last_stack (self(), destroyed) of
		{ok, []} ->	[];
		{error, _} ->	[];
		{ok, [{CardOwner, CardOrder, CardID} | Cards]} ->
			destroy:check_card_destroyed (CardOwner, [{CardOwner, CardOrder, CardID}] ++ Cards, heal_destroy)
	end;
heal_target_curse([{PlayerPid, CardOrder, CardID} | Targets], Curse, ability) ->
	case lists:flatlength(Curse) of
		1 ->
			Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
			heal_curse(PlayerPid, CardOrder, CardID, Curse, Zone),
			heal_target_curse(Targets, Curse, ability);
		_ ->
			[_Curse|Tail] = Curse,
			Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
			heal_curse (PlayerPid, CardOrder, CardID, [_Curse], Zone),
			heal_target_curse(Targets, Tail, ability)
	end.
