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
-module(new_assign_atk).
-compile(export_all).
-export([
						seal_status/5,
						request_assign_attack/4,
						check_attack_type/3,
						ability_select_when_attacker/0,
						ability_select_when_attack_target/0,
						ability_select_when_attacker_fight/0,
						ability_select_when_target_fight/0,
						attacking_interfere_step/0,
						check_last_assign/3,
						verify_ability_when_attacker/0,
						activate_ability_when_attacker/0,
						verify_ability_when_target/0,
						activate_ability_when_target/0,
						collect_activate_ability/3,
						verify_ability_assign_attack/0,
						activate_ability_assign_attack/0,
						
						check_zone_attack/3,
						zone_selected/2,
						
						activate_select_target/0,
						assign_attack_target/3,
						update_attack_target_card/3,
						recheck_attacker_condition_again/3,
						pre_ability_active/0,
						pre_fighting_ability_active/3,
						verify_ability_when_attacker_fight/0,
						activate_ability_when_attacker_fight/0,
						verify_ability_when_target_fight/0,
						activate_ability_when_target_fight/0,
						start_combat/3,
						ability_select_attack_success/0,
						ability_select_object_success/0,
						ability_select_end_of_fighting/0,
						verify_ability_when_attack_success/0,
						activate_ability_when_attack_success/0,
						verify_ability_when_object_success/0,
						activate_ability_when_object_success/0,
						verify_ability_when_end_of_fighting/0,
						activate_ability_when_end_of_fighting/0,
						looser_seal_destroy/3,
						delete_each_attack_round_data/3,
						check_additional_attack_effect/4,
						end_of_each_round_attack/3,
						check_attack_round/4,
						player_select_additional_attack/1,
						collect_ability_before_activate/0
					]).
					
rejected_attack(CardOwner, CardOrder, CardID, AtomicReason) ->
	Reason = 
	case AtomicReason of
		no_target_attack -> 0;
		seal_not_ready -> 1;
		not_enough_mp -> 2;
		already_atk_to_hand -> 3;
		no_hand_card -> 4;
		disallow_assign_attack -> 5;
		hand_target_mismatch -> 6;
		cancel_attack -> 7;
		controller_not_allow -> 8;
		_Other -> smo_logger:fmsg("+++++++++++reject attack case ~p~n", [_Other]), 	255
	end,
	remove_all_concern_data(),
	erase(attack_rounds),
	if
		Reason >= 0 -> 
			gen_server:cast(self(), {cancel_attack, Reason});
		true -> io:format ("Atomic reason not match ~p~n", [Reason])
	end,
	case stack_pool:get_last_stack(self(), selected_attack_option) of
		{ok, {_, CombineCurrent}} -> card_utility:update_card_option_field(CardOwner, CardOrder, CardID, combine, CombineCurrent);
		_ -> ""
	end,
	effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update),
	stack_pool:pop_stack_out(self()),
	case stack_pool:get_last_stack(self(), play) of
		 {ok, _} -> interfere_step:return_play(check_play_step);
		 _ -> gen_server:cast(self(), {act_next_command})
	end.

% ส่วนการตรวจสอบ สถานะของ Seal ผู้โจมตี
seal_status(PlayerPid, CardOwner, CardOrder, CardID, Step) ->
	% ตรวจสอบผู้ควบคุมการ์ดใบนั้นๆ ว่าผู้ควบคุมเป็นคนเดียวกับเจ้าของการ์ดหรือไม่ -
	OppPid = mnesia_play:get_opponent_pid(CardOwner),
	{ControllerPid, _UncontPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OppPid, controller),
	% ตรวจสอบผู้ควบคุมการ์ดใบนั้นๆ ว่าผู้ควบคุมเป็นคนเดียวกับเจ้าของการ์ดหรือไม่ -
	case ControllerPid of
		PlayerPid -> assign_attack(PlayerPid, CardOwner, CardOrder, CardID, Step);
		_ -> {cancel_attack, controller_not_allow}
	end.
				% CardOwner ->
					% assign_attack(PlayerPid, CardOwner, CardOrder, CardID);
				% _ ->	{cancel_attack, controller_not_allow}
			% end;
		% controller_not_allow ->
			% case PlayerPid of
				% CardOwner ->
					% {cancel_attack, controller_not_allow};
				% _ ->	assign_attack(PlayerPid, CardOwner, CardOrder, CardID)
			% end
	%end.

assign_attack(PlayerPid, CardOwner, CardOrder, CardID, Step) ->
	% ตรวจสอบว่าผู้สั่งการสามารถสั่งการให้การ์ดโจมตีได้หรือไม่ -
	case mnesia_play:check_player_status(PlayerPid, disallow_first_attack) of
		{ok, have_no_status} ->
			case continuous_ability:check_player_got_effect(PlayerPid, player_effect, [{player_assign_atk, {disallow, all}}]) of
				have_effect -> {cancel_attack, disallow_assign_attack};
				_ ->
					% การ์ดที่สั่งโจมตีอยุ่ในสนามหรือไม่ -
				case card_utility:check_card_zone(CardOwner, CardOrder, CardID) of
					arena_zone ->
						assign_seal_attack(PlayerPid, CardOwner, CardOrder, CardID, Step);
					_ ->	io:format ("Attack seal not on arena zone~n")
				end
			end;
		{ok, have_status} -> {cancel_attack, disallow_assign_attack}
	end.

assign_seal_attack(PlayerPid, CardOwner, CardOrder, CardID, Step) ->
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	Check =
	case Step of
		pre_interfere -> check_double_disallow(CardFx);
		post_interfere -> curse:check_curse_status(CardFx, attacker)
	end,
	case Check of
		{ok, allow} ->
			check_seal_effect(PlayerPid, CardOwner, CardOrder, CardID);
		{ok, disallow} ->
			{cancel_attack, seal_not_ready}
	end.

check_double_disallow(CardFx) ->
	case	curse:check_curse_status(CardFx, assign_atk) of
		{ok, allow} -> curse:check_curse_status(CardFx, attacker);
		Disallow -> Disallow
	end.
	
check_seal_effect(PlayerPid, CardOwner, CardOrder, CardID) ->
	AllFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	AllInterest = [{attacker, disallow}],
	case function_utility:is_contain(AllInterest, AllFx) of
		[] -> check_active_seal_status(PlayerPid, CardOwner, CardOrder, CardID);
		[{attacker, disallow}|_] -> {cancel_attack, seal_not_ready};
		_Other -> check_active_seal_status(PlayerPid, CardOwner, CardOrder, CardID)
	end.
	
check_inactive_attack(_, _, _, []) -> {disallow};
check_inactive_attack(CardOwner, CardOrder, CardID, [inactive_attack_allow | _]) ->
	case ability_effect:check_card_effect(CardOwner, CardOrder, CardID, inactive) of
		{ok, have_effect} -> {disallow};
		{ok, no_effect} -> {allow}
	end;
check_inactive_attack(CardOwner, CardOrder, CardID, [_ | T]) -> check_inactive_attack(CardOwner, CardOrder, CardID, T).

check_active_seal_status(PlayerPid, CardOwner, CardOrder, CardID) ->
	case card_utility:check_card_active(CardOwner, CardOrder, CardID) of
		active_seal ->
			check_line_condition(PlayerPid, CardOwner, CardOrder, CardID);
		inactive_seal ->
			%% ถ้ามีความสามารถที่ทำให้โจมตีได้แม้เป็น inactive
			{ok, CardStatus} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, card_status, arena_zone),
			case check_inactive_attack(CardOwner, CardOrder, CardID, CardStatus) of
				{allow} ->
					check_line_condition(PlayerPid, CardOwner, CardOrder, CardID);
				{disallow} -> check_inactive_attack_effect(PlayerPid, CardOwner, CardOrder, CardID)
			end
	end.
	
check_inactive_attack_effect(PlayerPid, CardOwner, CardOrder, CardID) ->
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	FxContain = function_utility:is_contain([{attack, again_even_inactive}], CardFx),
	case FxContain of
		[] -> {cancel_attack, seal_not_ready};
		[{attack, again_even_inactive}|_] ->	 
			stack_pool:set_stack_option(self(), inactive_attack_effect, [{attack, again_even_inactive}]),
			check_line_condition(PlayerPid, CardOwner, CardOrder, CardID);
		_ -> {cancel_attack, seal_not_ready}
	end.	

check_line_condition(PlayerPid, CardOwner, CardOrder, CardID) ->
	case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, line, arena_zone) of
		{ok, 1} -> check_attack_potential(PlayerPid, CardOwner, CardOrder, CardID);
		{ok, 0} ->
			case ability_effect:check_card_effect(CardOwner, CardOrder, CardID, from_df) of
				{ok, have_effect} -> check_target_attack(PlayerPid, CardOwner, CardOrder, CardID);
				{ok, no_effect} -> {cancel_attack, seal_not_ready}
			end
	end.

check_attack_potential(PlayerPid, CardOwner, CardOrder, CardID) ->
	AbilityFx = ability_effect:check_card_effect(CardOwner, CardOrder, CardID, attack_any_action),
	case AbilityFx of
		{ok, have_effect} -> % ต้องเช็คว่า แต่ละท่าที่เลือกได้ มีท่าม่เป็นโจมตี All หรือไม่
			% ถ้ามี ให้โจมตีได้ (ไปก่อน)
			% ถ้าไม่มี ให้ check เป้าแบบปกติ
			check_target_attack(PlayerPid, CardOwner, CardOrder, CardID);
		_ ->
			case seal_card:get_seal_base_power(CardOwner, CardOrder, CardID, attack_time) of
			%case arena_zone:get_attack_type(CardOwner, CardOrder, CardID) of
				{ok, 1} ->	{attack};
				_ -> check_target_attack(PlayerPid, CardOwner, CardOrder, CardID)
			end
	end.
	
%can_attack_all(CardID) ->

check_target_attack(PlayerPid, CardOwner, CardOrder, CardID) ->
	case target_attack:check_assign_attack_target(PlayerPid, CardOwner, CardOrder, CardID) of
		{no_target, can_not_attack_to_hand} -> 
			AbilityFx = ability_effect:check_card_effect(CardOwner, CardOrder, CardID, attack_any_action),
			case AbilityFx of
				{ok, have_effect} -> {attack};
				_ -> {cancel_attack, no_target_attack}
			end;
		{no_target, already_atk_to_hand} -> {cancel_attack, already_atk_to_hand};
		_CanAttack -> smo_logger:fmsg("can attack case ~p~n", [_CanAttack]), {attack}
	end.
	
request_assign_attack(PlayerPid, CardOwner, CardOrder, CardID) ->	
	check_select_attack_effect(PlayerPid, CardOwner, CardOrder, CardID).
	
check_select_attack_effect(PlayerPid, CardOwner, CardOrder, CardID) ->
	AbilityFx = ability_effect:check_card_effect(CardOwner, CardOrder, CardID, attack_any_action),
	case AbilityFx of
		{ok, have_effect} ->
			put(card_attack, {CardOwner, CardOrder, CardID}),
			gen_server:cast(self(), {activate_select_attack_action, PlayerPid, CardOwner, CardOrder, CardID});
		{ok, no_effect} ->
			have_to_check_mp_cost(PlayerPid, CardOwner, CardOrder, CardID)
	end.

player_select_attack_action(PlayerPid, CombineOrder) ->
	%
	% การใช้ put หรือ get ในกรณี้นี้ ถ้ามีการโจมตี interfere ของ การโจมตี จะเกิดปัญหา รอการแก้ไข
	%
	%io:format("player select attack order ~p ~n", [CombineOrder]),
	{CardOwner, CardOrder, CardID} = get(card_attack), 
	erase(card_attack),
	%stack_pool:set_stack_option(self(), selected_attack_option, CombineOrder),
	%stack_pool:set_stack_option(self(), current_combine_data, CombineCurrent),
	% ดูว่าท่าโจมตีที่เลือก เป็นท่่าโจมตีของการรวมร่างท่าไหน และเป็๋นท่าอะไร
	case seal_card:get_combine_power_type(CardOwner, CardOrder, CardID, CombineOrder, attack_time) of
		% กรณีเป็น โจมตี All
		{ok, 1} ->	
			% ดูว่า ณ. ปัจจุบัน Seal นั้นมีการ รวมร่างเป็นอย่างไร 
			{ok, CombineCurrent} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, combine),
			% ท่าที่เลือก มีค่าพลังอย่างไร
			case CombineOrder of
				0 ->	PowerChange = seal_card:get_standard_power(CardOwner, CardOrder, CardID);
				_ ->	{ok, PowerChange} = seal_card:get_combine_power(CardOwner, CardOrder, CardID, CombineOrder)
			end,
			arena_zone:set_main_option(CardOwner, CardOrder, CardID, power_change, PowerChange),
			% บรรทัด ต่อไปนี้อาจทำให้เกิดปัญหา
			arena_zone:set_main_option(CardOwner, CardOrder, CardID, combine_option, [CombineOrder]),
			
			effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update),
			% เก็บท่ารวมร่างปัจจุบันเอาไว้
			put(current_combine_data, CombineCurrent),
			% เก็บท่ารวมร่างที่เลือกเอาไว้
			put(selected_attack_option, CombineOrder),
			% เก็บท่าโจมตี
			put(attack_type, attack_all),
			have_to_check_mp_cost(PlayerPid, CardOwner, CardOrder, CardID);
		_ ->
			case target_attack:check_assign_attack_target(PlayerPid, CardOwner, CardOrder, CardID) of
				{no_target, can_not_attack_to_hand} -> gen_server:cast(self(), {cancel_attack, 0});
				{no_target, already_atk_to_hand} -> gen_server:cast(self(), {cancel_attack, 0});
				_CanAttack ->
					put(selected_attack_option, CombineOrder),
					{ok, CombineCurrent} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, combine), 
					case CombineOrder of
						0 ->	PowerChange = seal_card:get_standard_power(CardOwner, CardOrder, CardID);
						_ ->	{ok, PowerChange} =seal_card:get_combine_power(CardOwner, CardOrder, CardID, CombineOrder)
					end,
					arena_zone:set_main_option(CardOwner, CardOrder, CardID, power_change, PowerChange),
					% บรรทัด ต่อไปนี้อาจทำให้เกิดปัญหา
					arena_zone:set_main_option(CardOwner, CardOrder, CardID, combine_option, [CombineOrder]),
					
					effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update),
					put(current_combine_data, CombineCurrent),
					put(attack_type, normal_attack),
					have_to_check_mp_cost(PlayerPid, CardOwner, CardOrder, CardID)
			end
	end.
	
have_to_check_mp_cost(PlayerPid, CardOwner, CardOrder, CardID) ->
	AttackCase =
	case stack_pool:get_last_stack(self(), attack_case) of
		{ok, AtkCase} -> AtkCase;
		_ -> player_assign
	end,
	case AttackCase of
		player_assign -> check_mp_cost(PlayerPid, CardOwner, CardOrder, CardID, AttackCase);
		_ -> 
			{ok, PlayerMp} = mnesia_play:get_player_data(PlayerPid, mp_rest),
			set_attacker(PlayerPid, CardOwner, CardOrder, CardID, AttackCase, PlayerMp)
	end.

check_mp_cost(PlayerPid, CardOwner, CardOrder, CardID, AttackCase) ->
	case can_pay_cost(PlayerPid, CardOwner, CardOrder, CardID) of
		{pay_cost, MpRest} -> set_attacker(PlayerPid, CardOwner, CardOrder, CardID, AttackCase, MpRest);
		{not_enough_mp} -> 
			case get(current_combine_data) of
				undefined -> "";
				CombineCurrent -> card_utility:update_card_option_field(CardOwner, CardOrder, CardID, combine, CombineCurrent)
			end,
			gen_server:cast(self(), {cancel_attack, 2})
	end.

set_attacker(PlayerPid, CardOwner, CardOrder, CardID, AttackCase, PlayerMp) ->
	AssignedTarget = 
	case stack_pool:get_last_stack(self(), assigned_target) of
		{ok, Target} -> Target;
		_ -> []
	end,
	stack_pool:push_stack(self(), CardOwner, CardOrder, CardID, [{play, new_player_select_attack_action}, {card_player, PlayerPid}, {attack_case, AttackCase}]),
	card_utility:add_card_status(CardOwner, CardOrder, CardID, attacker, arena_zone),
	mnesia_play:set_player_data(PlayerPid, player_status, assign_attack),
	case {get(selected_attack_option), get(current_combine_data)} of
		{undefined, undefined} -> "";
		{CombineOrder, CombineCurrent} -> 
			erase(selected_attack_option),
			erase(current_combine_data),
			stack_pool:set_stack_option(self(), selected_attack_option, {CombineOrder, CombineCurrent})
	end,
	% case get(current_combine_data) of
		% undefined -> "";
		% CombineCurrent -> 
			% erase(current_combine_data),
			% stack_pool:set_stack_option(self(), current_combine_data, CombineCurrent)
	% end,
	case AssignedTarget of
		[] -> "";
		_ -> 
			stack_pool:set_stack_option(self(), assign_attack_target, AssignedTarget),
			stack_pool:set_stack_option(self(), assigned_target, AssignedTarget)
	end,
	gen_server:cast(self(), {activate_attacker, PlayerPid, PlayerMp, CardOwner, [CardOrder, <<CardID:16>>]}).
	% หลังจากนี้ Client ส่ง 0818 กลับมาแล้ว Server ต้องเช็คว่า player_select_attack_action -> check_attack_type()
	
check_attack_type(CardOwner, CardOrder, CardID) ->
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	%io:format("get(attack_type) ~p~n", [get(attack_type)]),
	case get(attack_type) of
		undefined ->
			ATT =  
			case card_utility:check_card_status(CardOwner, CardOrder, CardID, be_combine, arena_zone) of
				{ok, have_no_status} ->
					case card_utility:check_card_status(CardOwner, CardOrder, CardID, combine_with_effect, arena_zone) of
						{ok, have_no_status} -> {ok, 0};
						{ok, have_status} -> seal_card:get_seal_base_power(CardOwner, CardOrder, CardID, attack_time)%arena_zone:get_attack_type (CardOwner, CardOrder, CardID)
					end;
				{ok, have_status} -> seal_card:get_seal_base_power(CardOwner, CardOrder, CardID, attack_time)%arena_zone:get_attack_type(CardOwner, CardOrder, CardID)
			end,
			AttackType = 
			case ATT of
				{ok, 1} -> attack_all;
				{ok, _} -> normal_attack;
				_ -> normal_attack
			end,
			%io:format("attack type of ~p~n", [AttackType]),
			case AttackType of
				normal_attack -> 	
					check_target_zone(PlayerPid, CardOwner, CardOrder, CardID);
				attack_all -> 
					stack_pool:set_stack_option(self(), attack_type, attack_all),
					attack_all:check_target_attack(PlayerPid, CardOwner, CardOrder, CardID)
			end;
		normal_attack -> 
			erase(attack_type),
			check_target_zone(PlayerPid, CardOwner, CardOrder, CardID);
		attack_all -> 
			erase(attack_type),
			stack_pool:set_stack_option(self(), attack_type, attack_all),
			%attack_all:check_target_attack(PlayerPid, CardOwner, CardOrder, CardID),
			case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, combine, arena_zone) of
				{ok, []} ->
					case stack_pool:get_last_stack(self(), selected_attack_option) of
						{ok, {CombineOrder, _}} -> 
							case CombineOrder of
								0 ->	PowerChange = seal_card:get_standard_power(CardOwner, CardOrder, CardID),
									stack_pool:set_stack_option(self(), selected_attack_option, {CombineOrder, []}),
									arena_zone:set_main_option(CardOwner, CardOrder, CardID, power_change, PowerChange);
								_ ->		
									{ok, PowerChange} =  seal_card:get_combine_power(CardOwner, CardOrder, CardID, CombineOrder),
									stack_pool:set_stack_option(self(), selected_attack_option, {CombineOrder, []}),
									arena_zone:set_main_option(CardOwner, CardOrder, CardID, power_change, PowerChange)
							end;
						_ -> ""
					end;
				{ok, _} ->
					case stack_pool:get_last_stack(self(), selected_attack_option) of
						{ok, {CombineOrder, CombineCurrent}} -> 
							case CombineOrder of
								0 ->	
									PowerChange = seal_card:get_standard_power(CardOwner, CardOrder, CardID),
									stack_pool:set_stack_option(self(), selected_attack_option, {CombineOrder, CombineCurrent}),
									arena_zone:set_main_option(CardOwner, CardOrder, CardID, power_change, PowerChange);
								_ ->	
									{ok, PowerChange} =  seal_card:get_combine_power(CardOwner, CardOrder, CardID, CombineOrder),
									stack_pool:set_stack_option(self(), selected_attack_option, {CombineOrder, CombineCurrent}),
									arena_zone:set_main_option(CardOwner, CardOrder, CardID, power_change, PowerChange)
							end;
				_ -> ""
					end
			end,
			attack_all:check_target_attack(PlayerPid, CardOwner, CardOrder, CardID)
	end.
			
check_target_zone(PlayerPid, CardOwner, CardOrder, CardID) ->
	TargetZone = target_attack:check_assign_attack_target(PlayerPid, CardOwner, CardOrder, CardID),
	case TargetZone of
		{have_target, AttackTarget} -> 
			stack_pool:set_stack_option(self(), attack_type, normal_attack),
			stack_pool:set_stack_option(self(), all_can_be_target, AttackTarget),
			card_utility:add_card_status(CardOwner, CardOrder, CardID, assign_attack, arena_zone),
			ability_active_when_assign_attack();
		{no_target, attack_to_hand} ->
			stack_pool:set_stack_option(self(), attack_type, to_hand_attack),
			card_utility:add_card_status(CardOwner, CardOrder, CardID, assign_attack, arena_zone),
			ability_active_when_assign_attack();
			%hand_atk:hand_attack_condition(PlayerPid, CardOwner, CardOrder, CardID);
		{check_target_zone, AttackTarget} ->
			stack_pool:set_stack_option(self(), check_zone_attack, AttackTarget),
			stack_pool:set_stack_option(self(), all_can_be_target, AttackTarget),
			card_utility:add_card_status(CardOwner, CardOrder, CardID, assign_attack, arena_zone),
			ability_active_when_assign_attack();
		{no_target, can_not_attack_to_hand} -> 
			arena_zone:set_seal_inactive(CardOwner, CardOrder, CardID),
			rejected_attack(CardOwner, CardOrder, CardID, no_target_attack)
	end.
	
ability_active_when_assign_attack() ->
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	stack_pool:set_stack_option(self(), play, new_assign_attack_ability_activate),
	mod_ability_activate:check_any_ability_activate(assign_attack, PlayerPid).
	
verify_ability_assign_attack() ->
	stack_pool:set_stack_option(self(), play, new_verify_assign_attack_ability_condition),
	mod_ability_activate:verify_ability_condition(assign_attack).

activate_ability_assign_attack() ->
	stack_pool:set_stack_option(self(), play, new_activate_assign_attack_ability_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(assign_attack, PlayerPid).
	
check_zone_attack(CardOwner, CardOrder, CardID) ->
	case stack_pool:get_last_stack(self(), attack_type) of
		{ok, to_hand_attack} ->
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
			hand_atk:hand_attack_condition(PlayerPid, CardOwner, CardOrder, CardID);
		_ ->
			case stack_pool:get_last_stack(self(), check_zone_attack) of
				  {ok, _AttackTarget} -> select_target_zone();
				  _ -> activate_select_target()
			end
	end.
	
select_target_zone() ->
	smo_logger:msg("select attack zone"),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	gen_server:cast(self(), {player_select_zone_attack, PlayerPid}).
	
zone_selected(PlayerPid, Zone) ->
	stack_pool:set_stack_option(self(), zone_selected, Zone),
	case Zone of
		[0] -> activate_select_target();
		_Data -> 
			{ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack(self()),
			stack_pool:set_stack_option(self(), attack_type, to_hand_attack),
			hand_atk:hand_attack_condition(PlayerPid, CardOwner, CardOrder, CardID)
	end.
	
activate_select_target() ->
	case stack_pool:get_last_stack(self(), assign_attack_target) of
		{ok, [{CardOwner, CardOrder, CardID}]} ->
			update_attack_target_card(CardOwner, CardOrder, CardID);
		_ ->	send_activate_select_target()
	end.
	
send_activate_select_target() ->
	{ok, AttackTarget} = stack_pool:get_last_stack(self(), all_can_be_target),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	AttackTargetSize = lists:flatlength(AttackTarget),
	ActivateAttackTarget = get_reply_list(PlayerPid, AttackTarget),
	gen_server:cast(self(), {activate_select_atk_target, PlayerPid, [AttackTargetSize] ++ ActivateAttackTarget}).
	
get_reply_list(_, []) -> [];
get_reply_list(PlayerPid, [{PlayerPid, CardOrder, CardID} | AttackTarget]) ->
	[1, CardOrder, <<CardID:16>>] ++ get_reply_list(PlayerPid, AttackTarget);
get_reply_list(PlayerPid, [{_, CardOrder, CardID} | AttackTarget]) ->
	[0, CardOrder, <<CardID:16>>] ++ get_reply_list(PlayerPid, AttackTarget).

% Client ส่ง การ์ดที่เลือกกลับมาให้
assign_attack_target(TCardOwner, TCardOrder, TCardID) ->
	{ok, AttackTarget} = stack_pool:get_last_stack(self(), all_can_be_target),
	case [{TCardOwner, TCardOrder, TCardID}] -- AttackTarget of
		[] ->	update_attack_target_card(TCardOwner, TCardOrder, TCardID);
		_ ->	send_activate_select_target() % เป็นกรณีที่ไม่น่าจะเกิดได้
	end.
	
update_attack_target_card(TCardOwner, TCardOrder, TCardID) ->
	stack_pool:set_stack_option(self(), attack_target_selected, [{TCardOwner, TCardOrder, TCardID}]),
	card_utility:add_card_status(TCardOwner, TCardOrder, TCardID, attacked, arena_zone),
	card_utility:add_card_status(TCardOwner, TCardOrder, TCardID, assign_attack_success, arena_zone),
	gen_server:cast(self(), {update_attack_target, TCardOwner, TCardOrder, TCardID}).
	
% เมื่อส่ง {update_attack_target,  ไป Client จะส่ง 0819 กลับมาให้เราทำ ability_select_when_attacker()
ability_select_when_attacker() ->
	{ok, PlayerPid} = mnesia_play:get_game_data(self(), player_turn),
	stack_pool:set_stack_option(self(), play, new_attacker_ability_activate),
	mod_ability_activate:check_any_ability_activate(seal_attacker, PlayerPid).

ability_select_when_attack_target() ->
	stack_pool:set_stack_option(self(), play, new_attacked_ability_activate),
	{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	mod_ability_activate:check_any_ability_activate(seal_attacked, PlayerPid).

ability_select_when_attacker_fight() ->
	stack_pool:set_stack_option(self(), play, new_attacker_fight_ability_activate),
	{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	mod_ability_activate:check_any_ability_activate(attacker_fight, PlayerPid).
	
ability_select_when_target_fight() ->
	stack_pool:set_stack_option (self(), play, new_attacked_fight_ability_activate),
	{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	mod_ability_activate:check_any_ability_activate(attacked_fight, PlayerPid).
	
attacking_interfere_step() ->
	case stack_pool:get_last_stack(self(), option) of
		{ok, Option} ->
			case is_atk_type(Option) of % เช็คว่าตีขึ้นมือ หรือ ตีธรรมดา เพื่อส่ง message ได้ถูกต้อง
				{already} ->
					stack_pool:set_stack_option(self(), play, hand_atk_normal),
					interfere_step:into_sub_interfere ();
				_ ->
					stack_pool:set_stack_option(self(), play, normal_atk),
					interfere_step:into_sub_interfere ()
			end;
		_ ->
			stack_pool:set_stack_option(self(), play, normal_atk),
			interfere_step:into_sub_interfere ()
		end.
		
is_atk_type([{attack_type, to_hand_attack}|_]) -> {already};
is_atk_type([_|T]) -> is_atk_type (T);
is_atk_type([]) -> {no_hand_atk}.
		
check_last_assign(CardOwner, CardOrder, CardID) ->
	%เช็คว่าหลัง interfere แล้วยังมีซีลรองรวมร่าง หรือ magical world แปะ อยู่หรือเปล่า
	case card_utility:get_card_option_field(CardOwner, CardOrder, CardID, combine, arena_zone) of
		{ok, []} ->
			case stack_pool:get_last_stack(self(), selected_attack_option) of
				{ok, {CombineOrder, _}} -> 
					case CombineOrder of
						0 ->	PowerChange = seal_card:get_standard_power(CardOwner, CardOrder, CardID),
							stack_pool:set_stack_option(self(), selected_attack_option, {CombineOrder, []}),
							arena_zone:set_main_option(CardOwner, CardOrder, CardID, power_change, PowerChange);
						_ ->	
							{ok, PowerChange} =  seal_card:get_combine_power(CardOwner, CardOrder, CardID, CombineOrder),
							stack_pool:set_stack_option(self(), selected_attack_option, {CombineOrder, []}),
							arena_zone:set_main_option(CardOwner, CardOrder, CardID, power_change, PowerChange)
					end;
				_ -> ""
			end;
		{ok, _} ->
			case stack_pool:get_last_stack(self(), selected_attack_option) of
				{ok, {CombineOrder, CombineCurrent}} -> 
					case CombineOrder of
						0 ->	
							PowerChange = seal_card:get_standard_power(CardOwner, CardOrder, CardID),
							stack_pool:set_stack_option(self(), selected_attack_option, {CombineOrder, CombineCurrent}),
							arena_zone:set_main_option(CardOwner, CardOrder, CardID, power_change, PowerChange);
						_ ->	
							{ok, PowerChange} =  seal_card:get_combine_power(CardOwner, CardOrder, CardID, CombineOrder),
							stack_pool:set_stack_option(self(), selected_attack_option, {CombineOrder, CombineCurrent}),
							arena_zone:set_main_option(CardOwner, CardOrder, CardID, power_change, PowerChange)
					end;
				_ -> ""
			end
	end,
	case get(b) of
		{play_in} ->
			erase(b),
			case card_utility:get_card_option_field (CardOwner, CardOrder, CardID, line, arena_zone) of
				{ok, 1} ->
					%{attack};
					recheck_attacker_condition(CardOwner, CardOrder, CardID);
				{ok, 0} ->
					case ability_effect:check_card_effect(CardOwner, CardOrder, CardID, from_df) of
							{ok, have_effect} ->
								%{attack};
								recheck_attacker_condition(CardOwner, CardOrder, CardID);
							{ok, no_effect} ->
								%{cancel_attack, seal_not_ready}
								case stack_pool:get_last_stack(self(), selected_attack_option) of
									{ok, {_, CombineCurrent_}} ->
										card_utility:update_card_option_field(CardOwner, CardOrder, CardID, combine, CombineCurrent_),
										effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update);
									_ -> ""
								end,
							remove_attacker_status(CardOwner, CardOrder, CardID),
							arena_zone:set_seal_inactive(CardOwner, CardOrder, CardID),
							rejected_attack(CardOwner, CardOrder, CardID, seal_not_ready)						
					end
			end;
		_ ->
			erase(b),
			recheck_attacker_condition(CardOwner, CardOrder, CardID)
	end.

recheck_attacker_condition(CardOwner, CardOrder, CardID) ->
	stack_pool:set_stack_option(self(), play, recheck_attacker_condition),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	case seal_status(PlayerPid, CardOwner, CardOrder, CardID, post_interfere) of
		{attack} -> recheck_attack_type(PlayerPid, CardOwner, CardOrder, CardID);
		{cancel_attack, Reason} -> 
			case stack_pool:get_last_stack(self(), selected_attack_option) of
				{ok, {_, CombineCurrent}} ->
					card_utility:update_card_option_field(CardOwner, CardOrder, CardID, combine, CombineCurrent),
					effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update);
				_ -> ""
			end,	
			arena_zone:set_seal_inactive(CardOwner, CardOrder, CardID),
			remove_attacker_status(CardOwner, CardOrder, CardID),
			combat:check_remove_effect_end_of_fighting(CardOwner, CardOrder, CardID),
			rejected_attack(CardOwner, CardOrder, CardID, Reason)
	end.

recheck_attack_type(PlayerPid, CardOwner, CardOrder, CardID) ->
	AttackType = target_attack:check_attack_target(PlayerPid, CardOwner, CardOrder, CardID),
	recheck_target_condition(PlayerPid, CardOwner, CardOrder, CardID, AttackType).

recheck_target_condition(PlayerPid, CardOwner, CardOrder, CardID, AttackType) ->
	case AttackType of
		{no_target, attack_to_hand}->
			case stack_pool:get_last_stack(self(), attack_type) of
				% กรณีที่สั่ง โจมตีขึ้นมือ มาแต่แรก ให้ทำงานตามปกติต่อไป
				{ok, to_hand_attack} ->
					collect_ability_before_activate(),
					interfere_step:return_play(check_play_step);
				_ -> % กรณีที่ไม่ได้สั่งโจมตีขึ้นมือมาแต่แรก แต่เมื่อเช็คเป้าใหม่แล้ว ต้องโจมตีขึ้นมือ ให้วนกลับไปทำ
					remove_attacked_status(),
					%ลบ Ability ที่มีการเลือกเป้าไปแล้วทั้งหมด 
					stack_pool:remove_stack_option(self(), {effect_to_target, seal_attacked}),
					stack_pool:remove_stack_option(self(), {effect_to_target, attacked_fight}),
					stack_pool:set_stack_option(self(), assign_attack, to_hand_attack),
					hand_atk:hand_attack_condition(PlayerPid, CardOwner, CardOrder, CardID)
			end;
		{no_target, can_not_attack_to_hand} ->
			remove_attacker_status(CardOwner, CardOrder, CardID),
			arena_zone:set_seal_inactive(CardOwner, CardOrder, CardID),
			rejected_attack(CardOwner, CardOrder, CardID, no_target_attack);
		{have_target, AttackTarget} ->
			case check_change_target_effect(CardOwner, CardOrder, CardID) of
				not_change -> do_nothing;
				{ok, Target} ->
					swap_target(Target),
					card_utility:remove_card_status(CardOwner, CardOrder, CardID, {attack_to, Target}, arena_zone)
			end,
			case  stack_pool:get_last_stack(self(), attack_target_selected) of
				{ok, AssignedTarget} -> % กรณี ที่ สั่งโจมตี Seal ในสนามมาแต่แรก
					case AssignedTarget -- AttackTarget of
						% ตรวจสอบว่า การ์ดที่สั่งโจมตีไปแต่แรก ยังเป็นเป้าหมายได้หริือไม่ 
						[] -> % เป็นเป้าได้ ให้ทำงานต่อไปตามปกติ
							collect_ability_before_activate(),
							interfere_step:return_play(check_play_step);
						_ -> % เป็นเป้าหมายไม่ได้แล้ว ให้ไปเลือกเป้าใหม่ แล้วการที่ตกเป็นเป้ามาก่อนสูญเสียสภาพการ์ดที่ถูกโจมตี
							case stack_pool:get_last_stack(self(), assigned_target) of
								{ok, _} -> 
									%arena_zone:set_seal_inactive(CardOwner, CardOrder, CardID),
									rejected_attack(CardOwner, CardOrder, CardID, no_target_attack);
								_ -> 
									remove_attacked_status(AssignedTarget),
									%ลบ Ability ที่มีการเลือกเป้าไปแล้วทั้งหมด
									stack_pool:set_stack_option(self(), all_can_be_target, AttackTarget),
									stack_pool:remove_stack_option(self(), {effect_to_target, seal_attacked}),
									stack_pool:remove_stack_option(self(), {effect_to_target, attacked_fight}),
									send_activate_select_target()
							end
					end;
				_ -> % กรณีที่ ไม่ไดสั่ง โจมตี Seal ในสนามมาแต่แรก คือ ตอนแรก สั่งโจมตีขึ้นมือ 
					% ในกรณีนี้ ต้องเลือกเป้าหมายใหม่เป็น Seal ที่อยู่ในสนาม
					stack_pool:set_stack_option(self(), all_can_be_target, AttackTarget),
					send_activate_select_target()
			end;
		{check_target_zone, AttackTarget} ->
			case stack_pool:get_last_stack(self(), check_zone_attack) of
				% มีการเลือก Zone เพื่อโจมตีมาแต่แรก
				{ok, _} ->
					collect_ability_before_activate(),
					interfere_step:return_play(check_play_step);
				_ ->
					stack_pool:set_stack_option(self(), all_can_be_target, AttackTarget),
					select_target_zone()
			end
	end.
	
check_change_target_effect(CardOwner, CardOrder, CardID) ->
	{ok, Status} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, card_status, arena_zone),
	check_card_status(Status).
	
check_card_status([]) -> not_change;
check_card_status([{attack_to, Target}|_]) -> {ok, Target};
check_card_status([_|Status]) -> check_card_status(Status).

swap_target(Target) ->
	remove_previous_target_status(),
	stack_pool:set_stack_option(self(), attack_target_selected, Target),
	lists:foreach(fun({TOwner, TOrder, TID}) ->
		case card_utility:check_card_zone(TOwner, TOrder, TID) of
			arena_zone -> card_utility:add_card_status(TOwner, TOrder, TID, attacked, arena_zone);
			_ -> do_nothing
		end
	end, Target).
	
remove_previous_target_status() ->
	case stack_pool:get_last_stack(self(), attack_target_selected) of
		{ok, Selected} ->
			lists:foreach(fun({PTOwner, PTOrder, PTID}) ->
			case card_utility:check_card_zone(PTOwner, PTOrder, PTID) of
				arena_zone -> card_utility:remove_card_status(PTOwner, PTOrder, PTID, attacked, arena_zone);
				_ -> do_nothing
			end end, Selected);
		_ -> do_nothing
	end.
	
collect_ability_before_activate() ->
	case get(this_attack_ability) of
		undefined ->
			Subturn = get(subturn),
			case get({card_give_fx, Subturn}) of
				undefined -> put(initial_ability, []), [];
				AllFx -> put(initial_ability, AllFx), AllFx
			end;
		_B4This ->
			get(initial_ability)
	end.

pre_ability_active() ->
	case stack_pool:get_last_stack(self(), attack_target_selected) of
		{ok, [{_, _, _}]} -> verify_ability_when_attacker();
		_ -> 
			stack_pool:set_stack_option(self(), play, new_verify_ability_when_hand_attacker),
			interfere_step:return_play(check_play_step)
	end.
	
% ตรวจสอบ Play ก่อนหน้าว่าเป็น hand_atk_normal หรือ normal_atk ก็แล้ววแต่ ให้กลับมาทำ collect_ability_before_activate()
verify_ability_when_attacker() ->
	stack_pool:set_stack_option(self(), play, new_verify_seal_attacker_condition),
	mod_ability_activate:verify_ability_condition(seal_attacker).

activate_ability_when_attacker() ->
	stack_pool:set_stack_option(self(), play, new_activate_seal_attacker_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(seal_attacker, PlayerPid).
	
activate_attacker_suspend_effect(CardOwner, CardOrder, CardID) ->
	AttackFx = check_when_attack_effect(CardOwner, CardOrder, CardID),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	stack_pool:set_stack_option(self(), play, activate_suspend_attacker_effect),
	activate_when_attack_effect(PlayerPid, AttackFx).

verify_ability_when_target() ->
	stack_pool:set_stack_option(self(), play, new_verify_seal_attacked_condition),
	mod_ability_activate:verify_ability_condition(seal_attacked).

activate_ability_when_target() ->
	stack_pool:set_stack_option(self(), play, new_activate_seal_attacked_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	mod_ability_effect:check_any_ability_activate(seal_attacked, PlayerPid).

activate_target_suspend_effect() ->
	{ok, AttackTarget} = stack_pool:get_last_stack(self(), attack_target_selected),
	TargetFx = check_when_target_effect(AttackTarget),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	stack_pool:set_stack_option(self(), play, activate_suspend_target_effect),
	activate_when_attack_effect(PlayerPid, TargetFx).

% เมื่อมีการทำงานของ Ability แล้ว ให้กลับมาเก็บว่ามี Ability ใดทำงานไปบ้่างเพื่อจะได้ไม่ให้ทำงานซ้ำอีก
collect_activate_ability(CardOwner, CardOrder, CardID) ->
	Subturn = get(subturn),
	ThisSubTAbility =
			case get({card_give_fx, Subturn}) of
				undefined -> [];
				_AllFx -> _AllFx
			end,
	InitialAbility = collect_ability_before_activate(),
	put(this_attack_ability, ThisSubTAbility -- InitialAbility),
	recheck_attacker_condition_again(CardOwner, CardOrder, CardID).

recheck_attacker_condition_again(CardOwner, CardOrder, CardID) ->
	stack_pool:set_stack_option(self(), play, recheck_attacker_condition_again),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	case check_seal_status(PlayerPid, CardOwner, CardOrder, CardID) of
		{attack} -> 
			case stack_pool:get_last_stack(self(), inactive_attack_effect) of
				{ok, [{attack, again_even_inactive}]} -> 
					function_utility:remove_effect([{attack, again_even_inactive}], {arena_zone, CardOwner, CardOrder, CardID});
				_ -> ""
			end,
			recheck_attack_type_again(PlayerPid, CardOwner, CardOrder, CardID);
		{cancel_attack, Reason} -> 
			case stack_pool:get_last_stack(self(), selected_attack_option) of
				{ok, {_, CombineCurrent}} ->
					card_utility:update_card_option_field(CardOwner, CardOrder, CardID, combine, CombineCurrent),
					effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update);
				_ -> ""
			end,
			remove_attacker_status(CardOwner, CardOrder, CardID),
			combat:check_remove_effect_end_of_fighting(CardOwner, CardOrder, CardID),
			arena_zone:set_seal_inactive(CardOwner, CardOrder, CardID),
			rejected_attack(CardOwner, CardOrder, CardID, Reason)
	end.
%เช็คสถานะหลัง interfere
check_seal_status(PlayerPid, CardOwner, CardOrder, CardID) ->
	% ตรวจสอบผู้ควบคุมการ์ดใบนั้นๆ ว่าผู้ควบคุมเป็นคนเดียวกับเจ้าของการ์ดหรือไม่ -
	OppPid = mnesia_play:get_opponent_pid(CardOwner),
	{ControllerPid, _UncontPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OppPid, controller),
	% ตรวจสอบผู้ควบคุมการ์ดใบนั้นๆ ว่าผู้ควบคุมเป็นคนเดียวกับเจ้าของการ์ดหรือไม่ -
	case ControllerPid of
		PlayerPid -> check_assign_attack(PlayerPid, CardOwner, CardOrder, CardID);
		_ -> {cancel_attack, controller_not_allow}
	end.

check_assign_attack(PlayerPid, CardOwner, CardOrder, CardID) ->
	% ตรวจสอบว่าผู้สั่งการสามารถสั่งการให้การ์ดโจมตีได้หรือไม่ -
	case mnesia_play:check_player_status(PlayerPid, disallow_first_attack) of
		{ok, have_no_status} ->
			case continuous_ability:check_player_got_effect(PlayerPid, player_effect, [{player_assign_atk, {disallow, all}}]) of
				have_effect -> {cancel_attack, disallow_assign_attack};
				_ ->
					% การ์ดที่สั่งโจมตีอยุ่ในสนามหรือไม่ -
				case card_utility:check_card_zone(CardOwner, CardOrder, CardID) of
					arena_zone ->
						check_active_seal_status(PlayerPid, CardOwner, CardOrder, CardID);
					_ ->	io:format ("Attack seal not on arena zone~n")
				end
			end;
		{ok, have_status} -> {cancel_attack, disallow_assign_attack}
	end.

recheck_attack_type_again(PlayerPid, CardOwner, CardOrder, CardID) ->
	AttackType = target_attack:check_attack_target(PlayerPid, CardOwner, CardOrder, CardID),
	recheck_target_condition_again(PlayerPid, CardOwner, CardOrder, CardID, AttackType).

recheck_target_condition_again(PlayerPid, CardOwner, CardOrder, CardID, AttackType) ->
	case AttackType of
		{no_target, attack_to_hand}->
			case stack_pool:get_last_stack(self(), attack_type) of
				% กรณีที่สั่ง โจมตีขึ้นมือ มาแต่แรก ให้ทำงานตามปกติต่อไป
				{ok, to_hand_attack} ->
					stack_pool:remove_stack_option(self(), attack_target_selected),
					interfere_step:return_play(check_play_step);
				_ -> % กรณีที่ไม่ได้สั่งโจมตีขึ้นมือมาแต่แรก แต่เมื่อเช็คเป้าใหม่แล้ว ต้องโจมตีขึ้นมือ ให้วนกลับไปทำ
					remove_attacked_status(),
					stack_pool:set_stack_option(self(), select_type_of, reselect),
					%ลบ Ability ที่มีการเลือกเป้าไปแล้วทั้งหมด 
					stack_pool:remove_stack_option(self(), {effect_to_target, seal_attacked}),
					stack_pool:remove_stack_option(self(), {effect_to_target, attacked_fight}),
					stack_pool:set_stack_option(self(), assign_attack, to_hand),
					hand_atk:hand_attack_condition(PlayerPid, CardOwner, CardOrder, CardID)
			end;
		{no_target, can_not_attack_to_hand} ->
			remove_attacker_status(CardOwner, CardOrder, CardID),
			arena_zone:set_seal_inactive(CardOwner, CardOrder, CardID),
			rejected_attack(CardOwner, CardOrder, CardID, no_target_attack);
		{have_target, AttackTarget} ->
			% ตรวจสอบว่า Seal ที่โจมตี ได้รับ effect ให้เปลี่ยนเป้าหมายหรือไม่
			case check_change_target_effect(CardOwner, CardOrder, CardID) of
				% ไม่มีการเปลี่ยนเป้า
				not_change -> do_nothing;
				% มีการเปลี่ยนเป้า
				{ok, Target} ->
					swap_target(Target),
					card_utility:remove_card_status(CardOwner, CardOrder, CardID, {attack_to, Target}, arena_zone)
			end,
			case stack_pool:get_last_stack(self(), attack_target_selected) of
				{ok, AssignedTarget} -> % กรณี ที่ สั่งโจมตี Seal ในสนามมาแต่แรก
					case AssignedTarget -- AttackTarget of
						% ตรวจสอบว่า การที่สั่งโจมตีไปแต่แรก ยังเป็นเป้าหมายได้หริือไม่ 
						[] -> % เป็นเป้าได้ ให้ทำงานต่อไปตามปกติ
							interfere_step:return_play(check_play_step);
						_ -> % เป็นเป้าหมายไม่ได้แล้ว ให้ไปเลือกเป้าใหม่ แล้วการที่ตกเป็นเป้ามาก่อนสูญเสียสภาพการ์ดที่ถูกโจมตี
							remove_attacked_status(AssignedTarget),
							%ลบ Ability ที่มีการเลือกเป้าไปแล้วทั้งหมด
							stack_pool:set_stack_option(self(), select_type_of, reselect),
							stack_pool:set_stack_option(self(), all_can_be_target, AttackTarget),
							stack_pool:remove_stack_option(self(), {effect_to_target, seal_attacked}),
							stack_pool:remove_stack_option(self(), {effect_to_target, attacked_fight}),
							send_activate_select_target()
					end;
				_ -> % กรณีที่ ไม่ไดสั่ง โจมตี Seal ในสนามมาแต่แรก คือ ตอนแรก สั่งโจมตีขึ้นมือ 
					% ในกรณีนี้ ต้องเลือกเป้าหมายใหม่เป็น Seal ที่อยู่ในสนาม
					stack_pool:set_stack_option(self(), select_type_of, reselect),
					stack_pool:set_stack_option(self(), all_can_be_target, AttackTarget),
					send_activate_select_target()
			end;
		{check_target_zone, AttackTarget} ->
			case stack_pool:get_last_stack(self(), check_zone_attack) of
				% มีการเลือก Zone เพื่อโจมตีมาแต่แรก
				{ok, _} ->
					interfere_step:return_play(check_play_step);
				_ ->
					stack_pool:set_stack_option(self(), all_can_be_target, AttackTarget),
					select_target_zone()
			end
	end.

pre_fighting_ability_active(CardOwner, CardOrder, CardID) ->
	card_utility:add_card_status(CardOwner, CardOrder, CardID, attack_success, arena_zone),
	case stack_pool:get_last_stack(self(), attack_target_selected) of
		{ok, [{TPlayerPid, TCardOrder, TCardID}]} -> 
			card_utility:add_card_status(TPlayerPid, TCardOrder, TCardID, been_attack_success),
			verify_ability_when_attacker_fight();
		_ -> 
			stack_pool:set_stack_option(self(), play, new_activate_hand_target),
			interfere_step:return_play(check_play_step)
	end.
	
% interfere_step.erl [seal_attacked_effect -> verify_ability_when_attacker_fight()]
verify_ability_when_attacker_fight() ->
	stack_pool:set_stack_option(self(), play, new_verify_attacker_fight_condition),
	mod_ability_activate:verify_ability_condition(attacker_fight).
	
activate_ability_when_attacker_fight() ->
	stack_pool:set_stack_option(self(), play, new_activate_attaker_fight_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(attacker_fight, PlayerPid).

verify_ability_when_target_fight() ->
	stack_pool:set_stack_option(self(), play, new_verify_attacked_fight_condition),
	mod_ability_activate:verify_ability_condition(attacked_fight).

activate_ability_when_target_fight() ->
	stack_pool:set_stack_option(self(), play, new_activate_attaked_fight_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(attacked_fight, PlayerPid).

%interfere_step.erl [activate_attaked_fight_effect -> start_combat(CardOwner, CardOrder, CardID) 
start_combat(CardOwner, CardOrder, CardID) ->
	{ok, [{TargetPid, TargetOrder, TargetID}]} = stack_pool:get_last_stack(self(), attack_target_selected),
	case card_utility:get_card_option_field (CardOwner, CardOrder, CardID, line, arena_zone) of
		{error,not_on_zone} ->
			rejected_attack(CardOwner, CardOrder, CardID, disallow_assign_attack);
		_ ->
			case stack_pool:get_last_stack(self(), selected_attack_option) of
				{ok, {_, CombineCurrent}} ->
					%stack_pool:remove_stack_option(self(), selected_attack_option),
					%{ok, CombineCurrent} = stack_pool:get_last_stack(self(), current_combine_data),
					% case stack_pool:get_last_stack(self(), current_combine_data) of
						% {ok, Data} -> CombineCurrent = Data;
						% _ -> CombineCurrent = []
					% end,
					combat:calculate_battle(CardOwner, CardOrder, CardID, TargetPid, TargetOrder, TargetID),	
					card_utility:update_card_option_field(CardOwner, CardOrder, CardID, combine, CombineCurrent),
					combat:remove_end_of_fighting_effect(),
					effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update),
					ability_select_attack_success();
				_ ->
					combat:calculate_battle(CardOwner, CardOrder, CardID, TargetPid, TargetOrder, TargetID),
					combat:remove_end_of_fighting_effect(),
					ability_select_attack_success()
			end
	end.
		

ability_select_attack_success() ->
	stack_pool:set_stack_option(self(), play, new_activate_attack_success),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_activate:check_any_ability_activate(attack_success, PlayerPid).

ability_select_object_success() ->
	stack_pool:set_stack_option(self(), play, new_activate_been_attack_success),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_activate:check_any_ability_activate(been_attack_success, PlayerPid).

ability_select_end_of_fighting() ->
	stack_pool:set_stack_option(self(), play, new_activate_end_of_fighting),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_activate:check_any_ability_activate(end_of_fighting, PlayerPid).

verify_ability_when_attack_success() ->
	stack_pool:set_stack_option(self(), play, new_verify_attack_success_ability),
	mod_ability_activate:verify_ability_condition(attack_success).

activate_ability_when_attack_success() ->
	stack_pool:set_stack_option(self(), play, new_activate_attack_success_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	mod_ability_effect:check_any_ability_activate(attack_success, PlayerPid).

verify_ability_when_object_success() ->
	stack_pool:set_stack_option(self(), play, new_verify_been_attack_success_ability),
	mod_ability_activate:verify_ability_condition(been_attack_success).

activate_ability_when_object_success() ->
	stack_pool:set_stack_option(self(), play, new_activate_been_attack_success_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	mod_ability_effect:check_any_ability_activate(been_attack_success, PlayerPid).

verify_ability_when_end_of_fighting() ->
	stack_pool:set_stack_option(self(), play, new_verify_end_of_fighting_ability),
	mod_ability_activate:verify_ability_condition(end_of_fighting).

% interfere_step.erl [verify_end_of_fighting_ability -> activate_ability_when_end_of_fighting()]	
activate_ability_when_end_of_fighting() ->
	stack_pool:set_stack_option(self(), play, new_activate_end_of_fighting_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(end_of_fighting, PlayerPid).

% interfere_step.erl [activate_end_of_fighting_effect -> looser_seal_destroy()]
looser_seal_destroy(CardOwner, CardOrder, CardID) ->
	stack_pool:set_stack_option(self(), play, new_normal_atk_destroyed),
	DestroyList = combat:get_cards_destroy(),
	lists:foreach(fun({DestroyOwner, DestroyOrder, DestroyID}) -> card_utility:remove_card_status(DestroyOwner, DestroyOrder, DestroyID, attack_destroyed) end, DestroyList),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	case lists:flatlength(DestroyList) of
		0 ->	
			{ok, PlayerList} = mnesia_play:get_game_data(self(), player_list),
			lists:foreach(fun({Pid, _}) ->
											gen_server:cast(self(), {update_change_zone, same_zone, Pid, [16#88, 16#1e], [0, 0, 0]})
								end, PlayerList);
		_ ->
			CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
			% Check ว่ามีความสามารถโจมตีใบรองได้หรือไม่
			case function_utility:is_contain([{attack, to_supporter}], CardFx) of
				% ถ้าไม่มี ให้ Seal ทีแพ้ตก Shrine ปกติ
				[] -> destroy:check_card_destroyed(PlayerPid, DestroyList, normal_attack_destroy);
				% ถ้ามี
				_ ->
					case check_support_to_destroy(DestroyList, CardOwner, CardOrder, CardID) of
						[] -> destroy:check_card_destroyed(PlayerPid, DestroyList, normal_attack_destroy);
						destroy_list_destroy -> destroy:check_card_destroyed(PlayerPid, DestroyList, normal_attack_destoy);
						Support -> destroy_support_seal:support_to_destroy(PlayerPid, Support)
					end
			end
	end.
	
check_support_to_destroy([], _, _, _) -> [];
% Check ว่า เป็นใบที่โจมตี ตีที่จะถูก Destroy
check_support_to_destroy([{CardOwner, CardOrder, CardID}|_DestroyList], CardOwner, CardOrder, CardID) -> [];
% ---
check_support_to_destroy([{TOwner, TOrder, TID}|DestroyList], CardOwner, CardOrder, CardID) ->
	TZone = card_utility:check_card_zone(TOwner, TOrder, TID),
	case TZone of
		support_cards -> [{TOwner, TOrder, TID}] ++ check_support_to_destroy(DestroyList, CardOwner, CardOrder, CardID);
		 _ -> destroy_list_destroy%check_support_to_destroy(DestroyList, CardOwner, CardOrder, CardID)
	end.
	
delete_each_attack_round_data(CardOwner, CardOrder, CardID) ->
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, attacker),
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, attack_success),
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, assign_attack_success),
	case stack_pool:get_last_stack(self(), attack_target_selected) of
		{ok, Target} -> 
			lists:foreach(
										fun({TPlayerPid, TCardOrder, TCardID}) ->
											card_utility:remove_card_status(TPlayerPid, TCardOrder, TCardID, attacked),
											card_utility:remove_card_status(TPlayerPid, TCardOrder, TCardID, been_attack_success)
										end, Target);
		_ -> ""
	end,
	end_of_each_round_attack(CardOwner, CardOrder, CardID).

end_of_each_round_attack(CardOwner, CardOrder, CardID) ->
	remove_all_concern_data(),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	check_attack_round(PlayerPid, CardOwner, CardOrder, CardID).
	
% Seal จะโจมตีหลายครั้งได้ก็ต่อเมื่อ
% 1 - อยู่ในท่ารวมร่างที่ทำให้สามารถทำได้ 
	% 1.1 - ท่ารวมร่างนั้นอาจเกิดจากผลของ Mystic, Skill, Ability ได้
% 2 - ได้รับผลของ Effect ใดๆ เช่น Skill, Ability, Mystic
check_attack_round(PlayerPid, CardOwner, CardOrder, CardID) ->
	AtkOption = stack_pool:get_last_stack(self(), selected_attack_option),
	stack_pool:pop_stack_out(self()),
	case card_utility:check_card_zone(CardOwner, CardOrder, CardID) of
		arena_zone ->
			ATT =
			case AtkOption of
				{ok, {CombineOrder, CurrentCombin}} -> 
					A = {CombineOrder, CurrentCombin},
					case mnesia_odbc:get_combination_attack_type(CardID, CombineOrder) of
						0 -> 1;
						Round -> Round
					end;
				_ -> 
					A = xxx,
					case card_utility:check_card_status(CardOwner, CardOrder, CardID, be_combine, arena_zone) of
					{ok, have_no_status} ->
						case card_utility:check_card_status(CardOwner, CardOrder, CardID, combine_with_effect, arena_zone) of
							{ok, have_no_status} -> 0;
							{ok, have_status} ->
								case seal_card:get_seal_base_power(CardOwner, CardOrder, CardID, attack_time) of
									{ok, Result} -> Result;
									_ -> 0
								end
						end;
					{ok, have_status} ->
						case seal_card:get_seal_base_power(CardOwner, CardOrder, CardID, attack_time) of
							{ok, Result} -> Result;
							_ -> 0
						end
					end
			end,
			SealAtkRound = 
			case ATT > 1 of
				true -> ATT;
				_ -> 1
			end,
			AtkRound =
			case get(attack_rounds) of
				 undefined -> 1;
				 _Round -> _Round
			end,
			AttackRound = SealAtkRound - AtkRound,
			case AttackRound of
				0 -> 
					stack_pool:remove_stack_option(self(), selected_attack_option),
					check_additional_attack_effect(PlayerPid, CardOwner, CardOrder, CardID);
				_ -> 
					{ok, PlayerMp} = mnesia_play:get_player_data(PlayerPid, mp_rest),
					set_attacker(PlayerPid, CardOwner, CardOrder, CardID, card_effect, PlayerMp),
					case A of
						xxx -> 
							put(attack_rounds, AtkRound + 1);
						{_CombineOrder, _CurrentCombin} ->
							stack_pool:set_stack_option(self(), selected_attack_option, {_CombineOrder, _CurrentCombin}),
							put(attack_rounds, AtkRound + 1)
					end
			end;
		_ -> 
			erase(attack_rounds),
			next_command_to_client()
	end.
	
check_additional_attack_effect(PlayerPid, CardOwner, CardOrder, CardID) ->
	case check_addition_attack(CardOwner, CardOrder, CardID) of
		 had_additional -> 
		 	erase(attack_rounds),
			put(attacker_data, {PlayerPid, CardOwner, CardOrder, CardID}),
		 	gen_server:cast(self(), {activate_select_additional_attack, PlayerPid});
		 _ -> complete_all_attack(PlayerPid, CardOwner, CardOrder, CardID)
	end.
	
player_select_additional_attack(AdditionalSelect) ->
	{PlayerPid, CardOwner, CardOrder, CardID} = get(attacker_data),
	erase(attacker_data),
	case AdditionalSelect of
		[0] -> complete_all_attack(PlayerPid, CardOwner, CardOrder, CardID);
		[1] ->
			{ok, PlayerMp} = mnesia_play:get_player_data(PlayerPid, mp_rest),
			set_attacker(PlayerPid, CardOwner, CardOrder, CardID, card_effect, PlayerMp);
		_ ->	gen_server:cast(self(), {activate_select_additional_attack, PlayerPid})
	end.
	
complete_all_attack(PlayerPid, CardOwner, CardOrder, CardID) ->
	erase(attack_rounds),
	mnesia_play:remove_player_status(PlayerPid, assign_attack),
	CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	%card_utility:remove_card_status(CardOwner, CardOrder, CardID, inactive_attack_allow, CardZone),
	card_utility:add_card_status(CardOwner, CardOrder, CardID, attack_successful, CardZone),
	arena_zone:set_seal_inactive(CardOwner, CardOrder, CardID),
	case stack_pool:get_last_stack(self(), play) of
	 {ok, PlayStep} -> interfere_step:return_play(PlayStep);
	 _ -> 
	 	next_command_to_client()
	end.
% ---------------------------- Function Utility ------------------------------------------------------------------------
can_pay_cost(PlayerPid, CardOwner, CardOrder, CardID) ->
	{ok, PlayerMp} = mnesia_play:get_player_data(PlayerPid, mp_rest),
	%case card_utility:check_card_status(CardOwner, CardOrder, CardID, inactive_attack_allow, arena_zone) of
	%case get(skill_atk_combine) of
		%{ok, have_status} ->
	%	{ok} ->
	%		erase(skill_atk_combine),
	%		check_mp_cost (PlayerPid, PlayerMp, 0);
		%{ok, have_no_status} ->
	%	_ ->
	%		erase(skill_atk_combine),
			{ok, MpAtk} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, mp_atk),
			check_mp_cost(PlayerPid, PlayerMp, MpAtk).
	%end.

check_mp_cost(PlayerPid, PlayerMp, MpAtk) ->
	if
		MpAtk < 0 ->
			mnesia_play:set_mp_rest(PlayerPid, PlayerMp),
			{pay_cost, PlayerMp};
		PlayerMp < MpAtk ->
			{not_enough_mp};
		true ->
			mnesia_play:set_mp_rest(PlayerPid, PlayerMp - MpAtk),
			{pay_cost, PlayerMp - MpAtk}
	end.
	
remove_attacker_status(CardOwner, CardOrder, CardID) ->
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, attacker),
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, assign_attack_success),
	case stack_pool:get_last_stack(self(), attack_target_selected) of
		{ok, SealsTarget} -> 	remove_attacked_status(SealsTarget);
		_ -> ""
	end.
	
remove_attacked_status() ->
	case stack_pool:get_last_stack(self(), attack_target_selected) of
		{ok, Target} -> 
			lists:foreach(fun({TargetPid, TargetOrder, TargetID}) ->
				card_utility:remove_card_status(TargetPid, TargetOrder, TargetID, attacked)	end, Target);
		_ -> ""
	end.
	
remove_attacked_status(SealsTarget) ->
	lists:foreach(
								fun({TargetPid, TargetOrder, TargetID}) ->
									card_utility:remove_card_status(TargetPid, TargetOrder, TargetID, attacked)
								end, SealsTarget).
	
remove_all_concern_data() -> 
	erase(this_attack_ability), 
	erase(initial_ability).
	
next_command_to_client() ->
	stack_pool:pop_stack_out (self()),
	PlayReturn = stack_pool:get_last_stack (self(), play),
	case PlayReturn of
		{ok, StackPlay} -> interfere_step:return_play(StackPlay);
		{error, _} -> gen_server:cast(self(), {act_next_command})
	end.

check_addition_attack(CardOwner, CardOrder, CardID) ->
	{ok, ReceiveFx} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect),
	case check_addition_receive({CardOwner, CardOrder, CardID}, [], ReceiveFx, receive_effect) of
		had_additional -> had_additional;
		_ -> 
			{ok, SkillFx} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, skill_effect),
			check_addition_receive({CardOwner, CardOrder, CardID}, [], SkillFx, skill_effect)
	end.

check_addition_receive(_, _, [], _) -> no_additional_attack;
check_addition_receive({CardOwner, CardOrder, CardID}, Checked, [{GiveFx, Fx, Duration}|ReceiveFx], ReceiveType) ->
	case check_receive(Fx, Fx) of
		{received, FxRemain} -> 
			case FxRemain of
				[] -> card_utility:update_card_option_field(CardOwner, CardOrder, CardID, ReceiveType, Checked ++ ReceiveFx);
				_ -> card_utility:update_card_option_field(CardOwner, CardOrder, CardID, ReceiveType,  Checked ++ [{GiveFx, FxRemain, Duration}|ReceiveFx])
			end,
			had_additional;
		_ -> check_addition_receive({CardOwner, CardOrder, CardID}, Checked ++ [{GiveFx, Fx, Duration}], ReceiveFx, ReceiveType)
	end.
	
check_receive([], _) -> had_not_receive;
check_receive([{atk, additional_select_attack}|_], InitFx) -> {received, InitFx -- [{atk, additional_select_attack}]};
check_receive([_|Fx], InitFx) -> check_receive(Fx, InitFx).

check_when_attack_effect(CardOwner, CardOrder, CardID) ->
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	Require = when_attack,
	is_contain({CardOwner, CardOrder, CardID}, Require, CardFx).
	
check_when_target_effect([]) -> [];	
check_when_target_effect([{CardOwner, CardOrder, CardID}|Target]) ->
	check_when_target(CardOwner, CardOrder, CardID) ++ check_when_target_effect(Target).
	
check_when_target(CardOwner, CardOrder, CardID) ->
	CardFx = CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	Require = when_attack_target,
	is_contain({CardOwner, CardOrder, CardID}, Require, CardFx).

is_contain(_, _, []) -> [];
is_contain({CardOwner, CardOrder, CardID}, FxRequire, [{Gfx, CardFx, Duration}|Tail]) ->
	[{Gfx, check_card_effect(FxRequire, CardFx), Duration, [{CardOwner, CardOrder, CardID}]}] ++ is_contain({CardOwner, CardOrder, CardID}, FxRequire, Tail).
	
check_card_effect(_, []) -> [];
check_card_effect(FxType, [{FxType, SubFx}|CardFx]) -> [SubFx]	++ check_card_effect(FxType, CardFx);
check_card_effect(FxType, [_|CardFx]) -> check_card_effect(FxType, CardFx).

activate_when_attack_effect(PlayerPid, AttackFx) -> 
	StackOption = [{activate_effect, AttackFx}],
	stack_pool:push_stack(self(), PlayerPid, 0, 0, StackOption),
	activate_effect().
	
activate_effect() -> 
	case stack_pool:get_last_stack(self(), activate_effect) of
		{ok, []} -> 
			stack_pool:pop_stack_out(self()),
			interfere_step:return_play(check_play_step);
		{ok, [{GFx, Fx, Duration, RFx}|Remain]} ->
			stack_pool:set_stack_option(self(), activate_effect, Remain),
			stack_pool:set_stack_option(self(), card_had_give_effect, GFx),
			stack_pool:set_stack_option(self(), effect_activate, Fx),
			stack_pool:set_stack_option(self(), card_receive_effect, RFx),
			stack_pool:set_stack_option(self(), duration, Duration),
			effect_activate()
		end.
		
effect_activate() ->
	case stack_pool:get_last_stack(self(), effect_activate) of
		{ok, []} -> activate_effect();
		{ok, [{EffectType, Effect}|FxRemain]} ->
			stack_pool:set_stack_option(self(), effect_activate, FxRemain),
			stack_pool:set_stack_option(self(), play, activate_no_trigger_effect),
			activate({EffectType, Effect})
	end.
	
activate({EffectType, Effect}) ->
	case EffectType of
		_ ->
			{ok, GFx} = stack_pool:get_last_stack(self(), card_had_give_effect),
			{ok, RFx} = stack_pool:get_last_stack(self(), card_receive_effect),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			activate_effect_to_target(RFx, [{GFx, {EffectType, Effect}, Duration}]),
			interfere_step:return_play(check_play_step)
	end.
	
activate_effect_to_target(Target, [{CardGive, Fx, Duration}]) ->
	lists:foreach(fun({TPid, TCor, TCid}) ->
									TZone = card_utility:check_card_zone(TPid, TCor, TCid),
									{GOwner, GOrder, GID, _} = CardGive,
									ResFX = effect_value:check_value(GOwner, GOrder, GID, Fx, {TPid, TCor, TCid}),
									card_utility:set_card_option_field(TPid, TCor, TCid, receive_effect, [{CardGive, ResFX, Duration}], TZone),
									effect_activate:send_update_activate_effect(TPid, TCor, TCid, [], update)
								end, Target).
