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
-module(new_seal_skill).
-export([
						assign_use_skill/4,
						player_select_skill/3,
						check_assign_use_skill_ability/0,
						verify_assign_use_skill_ability/0,
						activate_assign_use_skill_ability/0,
						check_use_skill_ability/0,
						check_ability_need_activate/3,
						verify_use_skill_ability/0,
						check_seal_skill/3,
						set_use_skill/1,
						set_use_skill/0,
						set_seal_use_skill_loop/2,
						player_select_target_skill/2,
						seal_use_skill_into_interfere/0, 
						activate_use_skill_effect/0,
						verify_card_controller/3,
						verify_use_skill_success_ability/3,
						activate_use_skill_success_effect/0,
						activate_skill/3,
						check_then/3,
						end_of_use_skill2/3
					]).
					
reject_assign_use_skill(PlayerPid, Reason) ->
	io:format("Reject use skill reason ~p~n", [Reason]),
	case Reason of
		no_skill_to_use -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 0});
		card_inactive -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 8});
		cannot_use_skill_interfere -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 10});
		card_no_any_zone -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 11});
		controller_not_allow -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 12});
		disallow_use_first_subturn -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 13});
		curse_condition -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 14});
		suspend_other_assign -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 16}); 
		_ -> io:format("Reject use skill reason out of range ~p~n", [Reason])
	end.
	
reject_use_skill(PlayerPid, Reason) ->
	io:format("Reject use skill reason ~p~n", [Reason]),
	stack_pool:pop_stack_out(self()),
	case Reason of
		no_skill_to_use -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 0});
		no_skill_target -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 1});
		not_enough_mp -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 3});
		card_inactive -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 8});
		cancel_use_skill -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 9});
		card_no_any_zone -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 11});
		controller_not_allow -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 12});
		curse_condition -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 14});
		other_case -> gen_server:cast(self(), {reject_use_skill, PlayerPid, 99})
	end.
	
assign_use_skill(PlayerPid, CardOwner, CardOrder, CardID) ->
	io:format("player use skill ~p~n", [PlayerPid]),
	OppPid = mnesia_play:get_opponent_pid(CardOwner),
	{ControllerPid, UncontPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OppPid, controller),
	io:format("controller ~p uncontroller ~p~n", [ControllerPid, UncontPid]),
	% ตรวจสอบผู้ควบคุมการ์ดใบนั้นๆ ว่าผู้ควบคุมเป็นคนเดียวกับเจ้าของการ์ดหรือไม่ -
	case PlayerPid of
		ControllerPid -> check_first_subturn_condition(PlayerPid, CardOwner, CardOrder, CardID);
		UncontPid -> reject_assign_use_skill(PlayerPid, controller_not_allow)
	end.
	
check_first_subturn_condition(PlayerPid, CardOwner, CardOrder, CardID) ->
	% ตรวจสอบว่าผู้สั่งการสามารถสั่งการให้การ์ดใช้สกิลได้หรือไม่ -
	case mnesia_play:check_player_status(PlayerPid, disallow_first_attack) of
		{ok, have_no_status} ->
			check_suspend_assign(PlayerPid, CardOwner, CardOrder, CardID);
		{ok, have_status} ->
			reject_assign_use_skill(PlayerPid, disallow_use_first_subturn)
	end.

% ตรวจสอบว่า Card ที่ใช้ Skill นั้นมีการสั่งการอื่นค้างอยู่หรือไม่
check_suspend_assign(PlayerPid, CardOwner, CardOrder, CardID) ->
	{ok, AllStatus} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, card_status),
	ForbidStatus = [changing_line, attacker, being_combine, breaking_combine, using_skill],	
	CheckForbidAction = AllStatus -- ForbidStatus,
	case length(CheckForbidAction) =/= length(AllStatus) of
		true -> reject_assign_use_skill(PlayerPid, suspend_other_assign);
		_ -> check_assign_use_skill_condition(PlayerPid, CardOwner, CardOrder, CardID)
	end.
% Skill Interfere 
check_assign_use_skill_condition(PlayerPid, CardOwner, CardOrder, CardID) ->
	StackCheck = stack_pool:get_last_stack(self(), play),
	case StackCheck of
		{ok, _} -> % ใช้งานในช่วงที่จำเป็นต้องมี Interfere
			SkillInterfere = s_skill_check:is_interfere(CardID),
			case SkillInterfere of
				1 ->	check_curse_condition(PlayerPid, CardOwner, CardOrder, CardID);
				_ ->	reject_assign_use_skill(PlayerPid, cannot_use_skill_interfere)
			end;
		_ ->	check_curse_condition(PlayerPid, CardOwner, CardOrder, CardID)
	end.
% ตรวจสอบว่า Seal ที่ใช้ Skill นั้นติด Curse ที่ ทำให้ไม่สามารถใช้ Skill ได้หริือไม่
check_curse_condition(PlayerPid, CardOwner, CardOrder, CardID) ->
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	case curse:check_curse_status(CardFx, use_skill) of
		{ok, allow} -> check_card_loss_skill(PlayerPid, CardOwner, CardOrder, CardID);
		{ok, disallow} -> reject_assign_use_skill(PlayerPid, curse_condition)
	end.
% ตรวจสอบว่า Card ที่ใช้ Skill นั้นได้รับ effect ทำให้ไม่สามารถใช้ Skill ได้หริือไม่
check_card_loss_skill(PlayerPid, CardOwner, CardOrder, CardID) ->
	AllFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	AllInterest = [{skill, {loss, [all]}}, {skill, {cannot_use, [all]}}],
	case function_utility:is_contain(AllInterest, AllFx) of
		[] -> check_seal_condition(PlayerPid, CardOwner, CardOrder, CardID);
		[{skill, {loss, [all]}}|_] -> reject_assign_use_skill(PlayerPid, no_skill_to_use);
		[{skill, {cannot_use, [all]}}|_] -> reject_assign_use_skill(PlayerPid, no_skill_to_use);
		_Other -> check_seal_condition(PlayerPid, CardOwner, CardOrder, CardID)
	end.
% ตรวจสอบว่า Card ที่ใช้ Skill ถ้าเป็น Seal ตรวจสอบว่าเป็น Inactive หรือไม่ถ้าเป็น Mystic ตรวจสอบว่าใช้ Skill ไปหรือยัง
check_seal_condition(PlayerPid, CardOwner, CardOrder, CardID) ->
	case card_utility:check_card_zone(CardOwner, CardOrder, CardID) of
		{error, _} -> reject_assign_use_skill(PlayerPid, card_no_any_zone);
		CardZone ->
			case mnesia_odbc:is_seal_card(CardID) of
				is_seal -> 
					case card_utility:check_card_active(CardOwner, CardOrder, CardID) of
						active_seal -> set_card_use_skill(PlayerPid, CardOwner, CardOrder, CardID, CardZone, []);
						% เป็น Inactive Seal ตรวจสอบต่อว่าได้รับความสามารถ ให้ ใช้ Skill ได้ แม่เป็น Inactive หรือเปล่า
						inactive_seal -> 
							AllFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
							AllInterest = [{skill, again_even_inactive}],
							case function_utility:is_contain(AllInterest, AllFx) of
								[] -> reject_assign_use_skill(PlayerPid, card_inactive);
								[{skill, again_even_inactive}|_] ->
									function_utility:remove_effect([{skill, again_even_inactive}], {CardZone, CardOwner, CardOrder, CardID}),
									set_card_use_skill(PlayerPid, CardOwner, CardOrder, CardID, CardZone, again_even_inactive)
							end
					end;
				is_not_seal -> 
					case card_utility:check_card_status(CardOwner, CardOrder, CardID, had_use_skill, CardZone) of
						{ok, have_status} -> reject_assign_use_skill(PlayerPid, card_inactive);
						_ ->	
							set_card_use_skill(PlayerPid, CardOwner, CardOrder, CardID, CardZone, [])
					end
			end		
	end.
	
set_card_use_skill(PlayerPid, CardOwner, CardOrder, CardID, CardZone, SpecialUse) ->
	Interfere = 
	case stack_pool:get_last_stack(self(), play) of
		{ok, _} -> y;
		_ -> n
	end,
	% ตรวจสอบท่าที่ต้องเลือก 
	case s_skill_check:card_skill_available(CardOwner, CardOrder, CardID, Interfere) of
		{need_to_select_one, SkillList} ->
			stack_pool:push_stack(self(), CardOwner, CardOrder, CardID, [{card_player, PlayerPid}, {previous_zone, CardZone}, {special_skill_use, SpecialUse}, {using_skill, [{CardOwner, CardOrder, CardID}]}]),
			stack_pool:set_stack_option(self(), skill_number_list, SkillList),
			gen_server:cast(self(), {activate_select_skill, PlayerPid, CardOwner, SkillList});
			%gen_server:cast(self(), {update_card_use_skill, CardOwner, CardOrder, CardID});
		{error, no_skill} -> reject_assign_use_skill(PlayerPid, no_skill_to_use)
	end.

player_select_skill(PlayerPid, CardOwner, Data) ->
	io:format("Select Skill data ~p~n", [Data]),
	{ok, SkillList} = stack_pool:get_last_stack(self(), skill_number_list),
	case Data -- SkillList of
		[] -> %check_skill_cost(PlayerPid, Data);
			check_need_select_target_amount(PlayerPid, Data);
		_ -> gen_server:cast(self(), {activate_select_skill, PlayerPid, CardOwner, SkillList})
	end.

% ตรวจสอบว่า มีการเลือกจำนวนของเป้าหมายก่อนหรือไม่
check_need_select_target_amount(PlayerPid, SelectData) ->
	{ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack(self()),
	case s_skill_check:get_select_target_amount(CardOwner, CardOrder, CardID, SelectData) of
		{need_select, _SkillId, Player, {Init, Max}} -> 
			Selector =
			case Player of
				owner -> CardOwner;
				opponent -> mnesia_play:get_opponent_pid(CardOwner);
				controller -> 
					OppPid = mnesia_play:get_opponent_pid(CardOwner),
					{ControllerPid, _, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OppPid, controller),
					ControllerPid;
				uncontrol -> 
					OppPid = mnesia_play:get_opponent_pid(CardOwner),
					{_, Uncontrol, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OppPid, controller),
					Uncontrol
			end,
			skill_utility:select_target_amount(Selector, {Init, Max});
		do_not_need -> is_skilll_available(PlayerPid, CardOwner, CardOrder, CardID, SelectData)
	end.
		
is_skilll_available(PlayerPid, CardOwner, CardOrder, CardID, [{SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, No}]) ->
	OppPid = mnesia_play:get_opponent_pid(CardOwner),
	{ok, CardZone} = stack_pool:get_last_stack(self(), previous_zone),
	SkillAvai = s_skill_check:skill_available({CardZone, {CardOwner, CardOrder, CardID}}, {SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, [No]}, OppPid),
	case SkillAvai of
		[{SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, No}] -> check_skill_cost(PlayerPid, CardOwner, CardOrder, CardID, [{SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, No}]);
		_ -> reject_use_skill(PlayerPid, no_skill_to_use)
	end.

% 605.mod 1. ตรวจสอบ Cost ถ้าไม่เพียงพอให้นำ Seal ที่ประกาศใช้ Skill ออกจากการประกาศใช้ Skill
check_skill_cost(PlayerPid, CardOwner, CardOrder, CardID, [{SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, SkillNo}]) ->
	%{ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack(self()),
	% Check ว่าต้องจ่าย cost หรือไม่
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	case function_utility:is_contain([{skill, use_without_cost}], CardFx) of
		[] ->
			{ok, [MpUse, CounterUse]} = s_skill_check:resource_use(SkillOwnerID, SkillNo),
			 %io:format('Mp Use: ~p, Counter Use: ~p ~n', [MpUse, CounterUse]),
			{ok, PlayerMp} = mnesia_play:get_player_data(PlayerPid, mp_rest),
			CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
			Counter = game_info:card_counter({CardZone, {CardOwner, CardOrder, CardID}}),
			%io:format('Player Mp: ~p, Card Charge Counter: ~p ~n', [PlayerMp, Counter]),
			case mnesia_odbc:is_seal_card(CardID) of
				is_seal -> 
					case CardZone of
						arena_zone ->
							{ok, MS} = arena_zone:get_card_power(CardOwner, CardOrder, CardID, mp_skill);
						hand_cards ->
							case hand_zone:check_ability(CardOwner, CardOrder, CardID, ms) of
								{ok, MS} -> MS;
								_ -> MS = 0
							end;
						_ -> MS = 0
					end;
				is_not_seal -> MS = 0
			end,
			NetMs = 
			case MpUse+MS < 0 of
				true -> 0;
				_ -> MpUse+MS
			end,
			if
				PlayerMp >= NetMs, Counter >= CounterUse ->
					{ok, ReceiveFx} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect, CardZone),
					{0, RemainFx} = delete_counter:check_effect(CounterUse, [], ReceiveFx),
					card_utility:update_card_option_field(CardOwner, CardOrder, CardID, receive_effect, RemainFx, CardZone),
					effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update),
					card_utility:add_card_status(CardOwner, CardOrder, CardID, using_skill),
					card_utility:add_card_status(CardOwner, CardOrder, CardID, had_use_skill, CardZone),
					set_skill_user_mp(PlayerPid, CardOwner, CardOrder, CardID, PlayerMp - NetMs, {SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, SkillNo});
				true -> reject_use_skill(PlayerPid, not_enough_mp)
			end;
		_ -> 
			card_utility:add_card_status(CardOwner, CardOrder, CardID, using_skill),
			stack_pool:set_stack_option(self(), using_skill_number, [{SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, SkillNo}]),
			{ok, PlayerMp} = mnesia_play:get_player_data(PlayerPid, mp_rest),
			gen_server:cast(self(), {update_card_use_skill, PlayerPid, CardOwner, CardOrder, CardID, PlayerMp, {SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, SkillNo}})
			%gen_server:cast(self(), {update_player_select_skill, PlayerPid, SkillNo})
	end.

% 605.mod 2 ประกาศ Seal ที่ต้องการใช้ Skill จะเรียกว่าเป็นการสั่ง Seal ใช้ Skill ใน Phase นี้ --
% 605.mod 3 ประกาศท่าที่จะใช้ Skill ของ Seal หรือ Mystic Card
set_skill_user_mp(PlayerPid, CardOwner, CardOrder, CardID, MpRest, {SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, SkillNo}) ->
	stack_pool:set_stack_option(self(), using_skill_number, [{SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, SkillNo}]),
	mnesia_play:set_mp_rest(PlayerPid, MpRest),
	%OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
	%stack_pool:set_stack_option(self(), play, update_player_mp),
	%gen_server:cast(self(), {update_player_mp, PlayerPid, MpRest}),
	%ส่งไปบอกฝั่งตรงข้ามว่า seal xxx ของเราใช้ skill number นี้ และรอรับ message กลับมา
	%gen_server:cast(self(), {update_card_use_skill, CardOwner, CardOrder, CardID}).
	gen_server:cast(self(), {update_card_use_skill, PlayerPid, CardOwner, CardOrder, CardID, MpRest, {SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, SkillNo}}).
			% เอาสองอันนี้มารวมกัน
			%gen_server:cast(self(), {update_card_use_skill, CardOwner, CardOrder, CardID}) 
			%gen_server:cast(self(), {update_player_select_skill, PlayerPid, SkillNo}).

% 605.mod 4 Ability เมื่อผู้เล่น สั่งการ เมื่อผู้เล่นสั่งใช้ Skill ถ้ามีการเลือก การทำงาน ให้ทำ
check_assign_use_skill_ability() ->
	stack_pool:set_stack_option(self(), play, check_assign_use_skill_ability),
	{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	mod_ability_activate:check_any_ability_activate(assign_use_skill, PlayerPid).
% 605.mod 5 ตรวจสอบเงื่อนไขการเกิด Ability เมื่อผู้เล่น สั่งการ เมื่อผู้เล่นสั่งใช้ Skill
verify_assign_use_skill_ability() ->
	stack_pool:set_stack_option(self(), play, verify_assign_use_skill_ability),
	mod_ability_activate:verify_ability_condition(assign_use_skill).
% 605.mod 6 Ability เมื่อผู้เล่น สั่งการ เมื่อผู้เล่นสั่งใช้ Skill ทำงาน
activate_assign_use_skill_ability() ->
	stack_pool:set_stack_option(self(), play, activate_assign_use_skill_ability),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(assign_use_skill, PlayerPid).

check_seal_skill(CardOwner, CardOrder, CardID) ->
	OppPid = mnesia_play:get_opponent_pid(CardOwner),
	{ok, [{SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, SkillNo}]} = stack_pool:get_last_stack(self(), using_skill_number),
	{ok, CardZone} = stack_pool:get_last_stack(self(), previous_zone),
	{ok, SkillIdList} = s_skill:start_skill_check({CardZone, {CardOwner, CardOrder, CardID}}, {SkillOwnerPid, SkillOwnerOrder, SkillOwnerID, SkillNo}, OppPid),
	stack_pool:set_stack_option(self(), skill_id_list, SkillIdList),
	CheckSkillReturn = s_skill:s_skill_list_check({CardZone, {CardOwner, CardOrder, CardID}}, SkillIdList, OppPid),
	io:format("Skill tuple retrurn  is ~p~n", [CheckSkillReturn]),
	stack_pool:set_stack_option(self(), check_skill_return, CheckSkillReturn),
	set_use_skill().

then_do_skill(CardOwner, CardOrder, CardID, SkillId) ->
	CardZone  = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	io:format("then do ~p, ~p ~n", [SkillId, CardZone]),
	OppPid = mnesia_play:get_opponent_pid(CardOwner),
	CheckSkillReturn = s_skill:s_skill_list_check({CardZone, {CardOwner, CardOrder, CardID}}, SkillId, OppPid),
	stack_pool:set_stack_option(self(), then_case, then_do),
	stack_pool:set_stack_option(self(), check_skill_return, CheckSkillReturn),
	set_use_skill().
	
then_assign_skill(CardOwner, CardOrder, CardID, SkillId) ->
	CardZone  = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	OppPid = mnesia_play:get_opponent_pid(CardOwner),
	CheckSkillReturn = s_skill:s_skill_list_check({CardZone, {CardOwner, CardOrder, CardID}}, SkillId, OppPid),
	stack_pool:set_stack_option(self(), then_case, then_assign),
	stack_pool:set_stack_option(self(), check_skill_return, CheckSkillReturn),
	set_use_skill().
	
% นำแต่ละ Tuple ของ Skill ที่ต้องกระทำมาดำเนินการ
set_use_skill() ->
	{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	case stack_pool:get_last_stack(self(), check_skill_return) of
		{ok, []} ->
			stack_pool:set_stack_option(self(), play, finish_set_card_use_skill),
			gen_server:cast(self(), {finish_set_seal_use_skill, PlayerPid});
		{ok, [CheckSkillReturn|Check]} ->
			stack_pool:set_stack_option(self(), check_skill_return, Check),
			pre_set_seal_use_skill_loop(PlayerPid, CheckSkillReturn)
	end.
	
set_use_skill(PlayerPid) ->
	case stack_pool:get_last_stack(self(), check_skill_return) of
		{ok, []} ->
			stack_pool:set_stack_option(self(), play, finish_set_card_use_skill),
			gen_server:cast(self(), {finish_set_seal_use_skill, PlayerPid});
		{ok, [CheckSkillReturn|Check]} ->
			stack_pool:set_stack_option(self(), check_skill_return, Check),
			{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
			pre_set_seal_use_skill_loop(PlayerPid, CheckSkillReturn)
	end.
	
% 605.mod 7. Skill ของ Seal ที่ประกาศ ถ้ามีการเลือกการทำงานของ Skill ให้เลือกใน Phase นี้ และจะไม่มีการเปลี่ยนแปลง นอกจากจะมี Effect ให้สามารถทำได้ --
pre_set_seal_use_skill_loop(PlayerPid, {WhomEffect, {GPid, GOrder, GID, SkillID}, Target, {EffectTargetType, TargetNumber, Fx}, Duration}) ->
	FxToTarget = [{{GPid, GOrder, GID}, Fx, Duration}],	 
	Set1Interest = [{heal_curse, {select, 1}}],
	Set2Interest = [{at, {{select_seal_controller_arena, []}, defend}}, {curse, {freeze_curse, all}}],
	Set3Interest = [{check_action, move_to_arena}, {elem, {select, 1}}, {move_skill_target, {{select_seal_controller_arena, [{naming, "Unicorn"}, {action, skill_target}], 1}, to_s}}],
	AllInterest = Set1Interest++Set2Interest++Set3Interest,
	LeftOver = Fx--AllInterest,
	Interest = Fx--LeftOver,
	case Interest of
		%จำเป็นต้องเลือก Condition ก่อน
		%---------------------- ต้องย้่ายไปไว้ที่ select_condition 605.3--------
		[{heal_curse, {select, X}}] ->
			AllCurse = function_utility:all_curse_from_target(Target),
			ThisSetSeal = [{WhomEffect, Target, EffectTargetType, TargetNumber, FxToTarget, SkillID}],
			stack_pool:set_stack_option(self(), this_set_seal_use_skill, ThisSetSeal),
			skill_utility:select_curse_to_fx(AllCurse, X);
		% [{elem, {select, X}}] ->
			% ThisSetSeal = [{EffectTargetType, WhomEffect, TargetNumber, TargetList, FxToTarget, SkillID}],
			% %stack_pool:set_stack_option(self(), tail_fx_of_this_skilll_id, Tail),
			% stack_pool:set_stack_option(self(), this_set_seal_use_skill, ThisSetSeal),
			% skill_utility:select_element_to_fx([1, 2, 3, 4, 5, 6] , X);
			% 
		[{_, {{select_seal_controller_arena, ConditionNeed}, _}}] ->
			ThisSetSeal = [{WhomEffect, Target, EffectTargetType, TargetNumber, FxToTarget, SkillID}],
			stack_pool:set_stack_option(self(), this_set_seal_use_skill, ThisSetSeal),
			{ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack(self()),
			{ok, CardZone} = stack_pool:get_last_stack(self(), previous_zone),
			{ControllerPid, UncontrolPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, mnesia_play:get_opponent_pid(CardOwner), controller),
			Target1 = skill_card_list:player_zone_list({owner, [arena_zone], seal}, {ControllerPid, UncontrolPid}),
			Target2 = attribute_check:check_all({CardZone, {CardOwner, CardOrder, CardID}}, ConditionNeed, Target1),
			TarZoLi = skill_card_list:with_zone_line(Target2),
			TargetReply = skill_utility:target_to_client(TarZoLi, PlayerPid, 0),
			DialogCode = dialog_text:text_on_panel(FxToTarget),
			stack_pool:set_stack_option(self(), interest_effect, Interest),
			stack_pool:set_stack_option(self(), pre_set_seal_use_skill_loop, reveal_power),
			gen_server:cast(self(), {activate_select_skill_target, PlayerPid, 0, DialogCode, 0, 1, TargetReply});
		[{curse, {_, all}}] ->
			DialogCode = dialog_text:text_on_panel(FxToTarget),
			ThisSetSeal = [{WhomEffect, Target, EffectTargetType, TargetNumber, FxToTarget, SkillID}],
			stack_pool:set_stack_option(self(), this_set_seal_use_skill, ThisSetSeal),
			stack_pool:set_stack_option(self(), effect_interest, Interest),				
			stack_pool:set_stack_option(self(), pre_set_seal_use_skill_loop, select_player),
			gen_server:cast(self(), {activate_select_skill_target, PlayerPid, 1, DialogCode, 0, 1, []});
		[{elem, {select, X}}] ->
			ThisSetSeal = [{WhomEffect, Target, EffectTargetType, TargetNumber, FxToTarget, SkillID}],
			stack_pool:set_stack_option(self(), this_set_seal_use_skill, ThisSetSeal),
			skill_utility:select_element_to_fx(PlayerPid, 0, [1, 2, 3, 4, 5, 6, 7] , X);
		[{move_skill_target, {{select_seal_controller_arena, ConditionNeed, Amt}, to_s}}] ->
			ThisSetSeal = [{WhomEffect, Target, EffectTargetType, TargetNumber, FxToTarget, SkillID}],
			stack_pool:set_stack_option(self(), this_set_seal_use_skill, ThisSetSeal),
			{ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack(self()),
			{ok, CardZone} = stack_pool:get_last_stack(self(), previous_zone),
			{ControllerPid, UncontrolPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, mnesia_play:get_opponent_pid(CardOwner), controller),
			Target1 = skill_card_list:player_zone_list({owner, [arena_zone], seal}, {ControllerPid, UncontrolPid}),
			Target2 = attribute_check:check_all({CardZone, {CardOwner, CardOrder, CardID}}, ConditionNeed, Target1),
			TarZoLi = skill_card_list:with_zone_line(Target2),
			TargetReply = skill_utility:target_to_client(TarZoLi, PlayerPid, 0),
			DialogCode = dialog_text:text_on_panel(FxToTarget),
			stack_pool:set_stack_option(self(), interest_effect, Interest),
			stack_pool:set_stack_option(self(), pre_set_seal_use_skill_loop, swap_skill_target),
			gen_server:cast(self(), {activate_select_skill_target, PlayerPid, 0, DialogCode, 0, Amt, TargetReply});
		%---------------------------------------------------------------------------
		_ ->
			set_seal_use_skill_loop(PlayerPid, {WhomEffect, Target, EffectTargetType, TargetNumber, FxToTarget, SkillID})
	end.
		
%608. mod 8 เลือกเป้าหมาย Skill
set_seal_use_skill_loop(PlayerPid, {WhomEffect, Target, EffectTargetType, TargetNumber, FxToTarget, SkillID}) ->
	io:format("-------------------Effect Target Type ~p~n ----------------", [EffectTargetType]),
	case EffectTargetType of
		player_select_exact_target ->
			{RealFxToTarget, RealTarget} = skill_utility:real_fx_to_target(FxToTarget, Target),
			TarZoLi = skill_card_list:with_zone_line(RealTarget),
			TargetReply = skill_utility:target_to_client(TarZoLi, PlayerPid, 0),
			DialogCode = dialog_text:text_on_panel(FxToTarget),
			stack_pool:set_stack_option(self(), this_set_seal_use_skill, [{WhomEffect, RealTarget, EffectTargetType, TargetNumber, RealFxToTarget, SkillID}]),
			gen_server:cast(self(), {activate_select_skill_target, PlayerPid, 0, DialogCode, 0, TargetNumber, TargetReply});
		opponent_select_exact_target ->
			{RealFxToTarget, RealTarget} = skill_utility:real_fx_to_target(FxToTarget, Target),
			OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
			TarZoLi = skill_card_list:with_zone_line(RealTarget),
			TargetReply = skill_utility:target_to_client(TarZoLi, OpponentPid, 0),
			DialogCode = dialog_text:text_on_panel(FxToTarget),
			stack_pool:set_stack_option(self(), this_set_seal_use_skill, [{WhomEffect, RealTarget, EffectTargetType, TargetNumber, RealFxToTarget, SkillID}]),
			gen_server:cast(self(), {activate_select_skill_target, OpponentPid, 0, DialogCode, 0, TargetNumber, TargetReply});
		player_select_range_target ->
			{RealFxToTarget, RealTarget} = skill_utility:real_fx_to_target(FxToTarget, Target),
			TarZoLi = skill_card_list:with_zone_line(RealTarget),
			TargetReply = skill_utility:target_to_client(TarZoLi, PlayerPid, 0),
			DialogCode = dialog_text:text_on_panel(FxToTarget),
			stack_pool:set_stack_option(self(), this_set_seal_use_skill, [{WhomEffect, RealTarget, EffectTargetType, TargetNumber, RealFxToTarget, SkillID}]),
			gen_server:cast(self(), {activate_select_skill_target, PlayerPid, 0, DialogCode, 1, TargetNumber, TargetReply});
		% must_select_range_target ->
			% stack_pool:set_stack_option (self(), whom_effect, WhomEffect),
			% {RealFxToTarget, RealTarget} = skill_utility:real_fx_to_target(FxToTarget, TargetList),
			% stack_pool:set_stack_option (self(), fx_to_target, RealFxToTarget),
			% stack_pool:set_stack_option (self(), skill_id, SkillID),
			% stack_pool:set_stack_option (self(), skill_tail, Tail),
			% stack_pool:set_stack_option (self(), check_skill_return, Tail),
			% TarZoLi = skill_card_list:with_zone_line(RealTarget),
			% TargetReply = skill_utility:target_to_client(TarZoLi, PlayerPid, 0),
			% DialogCode = dialog_text:text_on_panel(FxToTarget),
			% gen_server:cast(self(), {activate_select_skill_target, PlayerPid, 1, DialogCode, 0, TargetNumber, TargetReply});
		
		player_select_exact_player ->
			{RealFxToTarget, RealTarget} = skill_utility:real_fx_to_target(FxToTarget, Target),
			DialogCode = dialog_text:text_on_panel(FxToTarget),
			stack_pool:set_stack_option(self(), this_set_seal_use_skill, [{WhomEffect, RealTarget, EffectTargetType, TargetNumber, RealFxToTarget, SkillID}]),
			gen_server:cast(self(), {activate_select_skill_target, PlayerPid, 1, DialogCode, 0, TargetNumber, []});
		do_not_need_select ->
			{RealFxToTarget, RealTarget} = skill_utility:real_fx_to_target(FxToTarget, Target),
			function_utility:add_card_status(skill_target, Target),
			stack_pool:add_stack_option_field(self(), skill_target, [{{SkillID, WhomEffect}, Target}]),
			stack_pool:add_stack_option_field(self(), skill_all_effect_result, [{WhomEffect, RealTarget, EffectTargetType, TargetNumber, RealFxToTarget, SkillID}]),
			set_use_skill(PlayerPid);	
		{depend_on, OtherSkillId} ->
			{ok, AllSkillTarget} = stack_pool:get_last_stack(self(), skill_target),
			DependOnTarget = skill_utility:depend_on_target({OtherSkillId, WhomEffect}, AllSkillTarget),
			{RealFxToTarget, RealTarget} = skill_utility:real_fx_to_target(FxToTarget, DependOnTarget),
			function_utility:add_card_status(skill_target, RealTarget),
			stack_pool:add_stack_option_field(self(), skill_target, [{{SkillID, WhomEffect}, RealTarget}]),
			stack_pool:add_stack_option_field(self(), skill_all_effect_result, [{WhomEffect, RealTarget, EffectTargetType, TargetNumber, RealFxToTarget, SkillID}]),
			set_use_skill(PlayerPid);
		random_select_exact_target ->
			RandomTarget = function_utility:random_select(TargetNumber, Target),
			{RealFxToTarget, RealTarget} = skill_utility:real_fx_to_target(FxToTarget, RandomTarget),
			stack_pool:add_stack_option_field(self(), skill_target, [{{SkillID, WhomEffect}, RealTarget}]),
			function_utility:add_card_status(skill_target, RealTarget),
			stack_pool:add_stack_option_field(self(), skill_all_effect_result, [{WhomEffect, RealTarget, EffectTargetType, TargetNumber, RealFxToTarget, SkillID}]),
			set_use_skill(PlayerPid);
		player_can_select_own_exact_target ->
			stack_pool:set_stack_option(self(), this_set_seal_use_skill, [{WhomEffect, Target, EffectTargetType, TargetNumber, FxToTarget, SkillID}]),
			gen_server:cast(self(), {activate_player_select_skill_decision, PlayerPid});
		opponent_can_select_own_exact_target ->
			OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
			stack_pool:set_stack_option(self(), this_set_seal_use_skill, [{WhomEffect, Target, EffectTargetType, TargetNumber, FxToTarget, SkillID}]),
			gen_server:cast(self(), {activate_player_select_skill_decision, OpponentPid});
		player_select_1_deck_and_system_select_top_deck ->
			{RealFxToTarget, RealTarget} = skill_utility:real_fx_to_target(FxToTarget, Target),
			%TarZoLi = skill_card_list:with_zone_line(RealTarget),
			%DialogCode = dialog_text:text_on_panel(FxToTarget),
			stack_pool:set_stack_option(self(), this_set_seal_use_skill, [{WhomEffect, RealTarget, EffectTargetType, TargetNumber, RealFxToTarget, SkillID}]),
			gen_server:cast(self(), {activate_select_deck, PlayerPid});
		%player_cancel_this_effect -> set_use_skill(PlayerPid);
		R -> io:format("Check seal skill result ~p~n", [R]),
			{ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack(self()),
			card_utility:remove_card_status(CardOwner, CardOrder, CardID, using_skill),
			reject_use_skill(PlayerPid, other_case)
	end.
	
player_select_target_skill(PlayerPid, TargetList) ->
	% อาจ check ด้วยว่า TargetList เป็นการ์ด ที่อยู่ใน _Target หรือไม่
	{ok, [{WhomEffect, _Target, EffectTargetType, TargetNumber, FxToTarget, SkillID}]} = stack_pool:get_last_stack(self(), this_set_seal_use_skill),
	stack_pool:add_stack_option_field(self(), skill_target, [{{SkillID, WhomEffect}, TargetList}]),
	stack_pool:set_stack_option(self(), target_list, TargetList),
	TextCode = dialog_text:text_on_panel(FxToTarget),
	%ส่งไปบอกฝั่งตรงข้่ามว่า เราเลือก seal ตาม TargetList นี้จากผลของการใช้ skill = TextCode
	stack_pool:set_stack_option(self(), text_code, TextCode),
	stack_pool:add_stack_option_field(self(), skill_all_effect_result, [{WhomEffect, TargetList, EffectTargetType, TargetNumber, FxToTarget, SkillID}]),
	function_utility:add_card_status(skill_target, TargetList),
	stack_pool:set_stack_option(self(), play, player_select_target_skill),
	{ok, {CardOwner, _, _, _}} = stack_pool:get_last_stack(self()),
	case function_utility:check_receive_pid(TargetList) of
		[] -> 
			TarZoLi = skill_card_list:with_zone_line(TargetList),
			gen_server:cast(self(), {update_player_select_target_skill, PlayerPid, CardOwner, TarZoLi, TextCode, all_is_card});
		_ ->
			%เพื่อส่งไปบอกฝั่งตรงข้ามผู้ใช้สกิลว่า ผู้ใช้สกิล เลือกเป้าหมายใบไหน
			gen_server:cast(self(), {update_player_select_target_skill, PlayerPid, CardOwner, TargetList, TextCode, all_is_pid})
	end.
		
check_ability_need_activate(CardOwner, CardOrder, CardID) ->
	case stack_pool:get_last_stack(self(), then_case) of
		{ok, then_do} -> verify_card_controller(CardOwner, CardOrder, CardID);
		_ -> check_use_skill_ability()
	end.

% 608 mod 9 Ability เมื่อ การ์ด ใช้ Skill ถ้ามีการเลือกเป้าหมาย หรือเลือกการทำงานให้ทำ
check_use_skill_ability() ->
	stack_pool:set_stack_option(self(), play, check_use_skill_ability),
	{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	mod_ability_activate:check_any_ability_activate(assign_use_skill, PlayerPid).
	
% 608 mod 10. Interfere Step
seal_use_skill_into_interfere() ->
	stack_pool:set_stack_option(self(), play, play_card_use_skill_into_interfere),
	interfere_step:into_sub_interfere().
	
% 605.9. 
	%ตรวจสอบว่า Ability ถูกต้องตามเงื่อนไขในการทำงานหรือไม่ ถ้าเงื่อนไขไม่ครบ Ability นั้นจะไม่ทำงาน หาก Ability ไม่สามารถส่งผลกับเป้าหมายที่กำหนด
	% หรือ เป้าหมายที่กำหนดไม่อยู่ในสภาพตกเป็นเป้าหมายได้ ให้กลับไปเลือกเป้าหมายของ Ability ใน phase 605.6 ใหม่โดยไม่สามารถเลือกเป้าหมายของ Skill
	% ใน Phase 605.7 ใหม่ได้ หากไม่มีเป้าหมายใหม่ที่ Ability สามารถส่งผล หรือกำหนดได้ Ability นั้นจะไม่ทำงาน ถ้าเงื่อนไขในการเกิด Ability
	% ยังคงถูกต้องและมีเป้าหมายที่ถูกต้อง Ability เมื่อ Seal ใช้ Skill จะทำงาน ใน Phase นี้ --
% skill_check_activate_ability(CardOwner, CardOrder, CardID) ->
	% case stack_pool:get_last_stack(self(), use_skill) of
		% {ok, _} -> verify_card_controller(CardOwner, CardOrder, CardID);
		% _ -> verify_use_skill_ability()
	% end.

%608.mod 11 
	%ตรวจสอบว่า  Ability  ถูกต้องตามเงื่อนไขในการทำงานหืรอไม่  ถ้าเงื่อนไขไม่ครบ Ability  นั้นจะไม่
	%ทำงาน หาก Ability ไม่สามารถส่งผลกับเป้าหมายที่กำหนด หรือ เป้าหมายทกำหนดไม่อยู่ในสภาพตก
	%เป็นเป้าหมายได้  ให้กลับไปเลือกเป้าหมายของ Ability ใน   phase 608.15 ใหม่โดยไม่สามารถเลือก
	%เป้าหมายของ Skill  ใน Phase 608.16  ใหม่ได้ หากไม่มีเป้าหมายใหมที่ Ability สามารถส่งผล หรือ 
	%กาหนดได้   Ability  นั้นจะไม่ทำงาน   ถ้าเงื่อนไขในการเกิด  Ability  ยังคงถูกต้องและมีเป้าหมายที่
	%ถูกต้อง Ability เมื่อ Seal ใช้ Skill จะทำงาน ใน Phase นี้ 	
verify_use_skill_ability() ->
	stack_pool:set_stack_option(self(), play, verify_use_new_skill_ability),
	mod_ability_activate:verify_ability_condition(use_skill).

activate_use_skill_effect() ->
	stack_pool:set_stack_option(self(), play, activate_use_new_skill_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(use_skill, PlayerPid).

% 605.mod 12
	%ตรวจสอบว่าการใช้ Skill นั้นถูกต้องตามเงื่อนไขในการใช้ Skill หรือไม่ ถ้าเงื่อนไขไม่ครบให้ Seal ที่กำลังใช้ Skill สูญเสียสภาพ Seal ที่กำลังใช้ Skill
	% และนำ Seal ออกจากการประกาศใช้ Skill หากเป็น Seal จะกลายเป็น Inactive Seal หาก Skill ไม่สามารถส่งผลกับเป้าหมายที่กำหนด หรือ เป้าหมายที่ กำหนด
	% ไม่อยู่ในสภาพตกเป็นเป้าหมายได้ ให้กลับเข้าสู่ phase 605.7 ใหม่ และ Ability เมื่อใช้ Skill ที่ทำงานใน Phase 605.9 จะไม่ทำงานซ้ำอีก หากไม่มีเป้าหมายใหม่ --
	% ที่ Skill สามารถส่งผล หรือ กำหนดได้ ให้ Seal ที่ กำลังใช้ Skill สูญเสียสภาพ Seal ที่กำลังใช้ Skill และนำ Seal ออกจากการประกาศใช้ Skill
	% หากเป็น Seal จะกลายเป็น Inactive Seal
verify_card_controller(CardOwner, CardOrder, CardID) ->
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	OppPid = mnesia_play:get_opponent_pid(CardOwner),
	{ControllerPid, UncontPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OppPid, controller),
	% ตรวจสอบผู้ควบคุมการ์ดใบนั้นๆ ว่าผู้ควบคุมเป็นคนเดียวกับเจ้าของการ์ดหรือไม่ -
	case PlayerPid of
		ControllerPid -> verify_curse_condition(PlayerPid, CardOwner, CardOrder, CardID);
		UncontPid -> 
			set_card_used_skill(CardOwner, CardOrder, CardID),
			reject_use_skill(PlayerPid, controller_not_allow)
	end.
	
verify_curse_condition(PlayerPid, CardOwner, CardOrder, CardID) ->
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	case curse:check_curse_status(CardFx, use_skill) of
		{ok, allow} -> verify_card_inactive(PlayerPid, CardOwner, CardOrder, CardID);
		{ok, disallow} -> 
			set_card_used_skill(CardOwner, CardOrder, CardID),
			reject_use_skill(PlayerPid, curse_condition)
	end.
	
verify_card_inactive(PlayerPid, CardOwner, CardOrder, CardID) ->
	case card_utility:check_card_zone(CardOwner, CardOrder, CardID) of
		{error, _} -> reject_use_skill(PlayerPid, card_no_any_zone);
		CardZone ->
			case mnesia_odbc:is_seal_card(CardID) of
				is_seal -> 
					case card_utility:check_card_active(CardOwner, CardOrder, CardID) of
						active_seal -> verify_skill_condition(PlayerPid, {CardZone, CardOwner, CardOrder, CardID});
						% เป็น Inactive Seal ตรวจสอบต่อว่าได้ใช้ความสามารถ ให้ ใช้ Skill ได้ แม่เป็น Inactive หรือเปล่า
						inactive_seal -> 
							case stack_pool:get_last_stack(self(), special_skill_use) of
								{ok, again_even_inactive} -> verify_skill_condition(PlayerPid, {CardZone, CardOwner, CardOrder, CardID});
								{ok, _} -> 
									%card_utility:remove_card_status(CardOwner, CardOrder, CardID, using_skill, CardZone),
									set_card_used_skill(CardOwner, CardOrder, CardID, CardZone),
									reject_use_skill(PlayerPid, card_inactive);
								_ -> 
									%card_utility:remove_card_status(CardOwner, CardOrder, CardID, using_skill, CardZone),
									set_card_used_skill(CardOwner, CardOrder, CardID, CardZone),
									reject_use_skill(PlayerPid, card_inactive)
							end
					end;
				is_not_seal -> verify_skill_condition(PlayerPid, {CardZone, CardOwner, CardOrder, CardID})
			end		
	end.
	
verify_skill_condition(PlayerPid, {CardZone, CardOwner, CardOrder, CardID}) ->
	OppPid =mnesia_play:get_opponent_pid(CardOwner),
	{ok, SkillIdList} = stack_pool:get_last_stack(self(), skill_id_list),
	stack_pool:set_stack_option(self(), check_case, post_skill_interfere),
	case s_skill:start_id_check({CardZone, {CardOwner, CardOrder, CardID}}, SkillIdList, OppPid) of
		{ok, _} -> verify_card_loss_skill(PlayerPid, CardOwner, CardOrder, CardID);
		_ ->
			%card_utility:remove_card_status(CardOwner, CardOrder, CardID, using_skill, CardZone),
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
			set_card_used_skill(CardOwner, CardOrder, CardID, CardZone),
			reject_use_skill(PlayerPid, cancel_use_skill)
	end.
	
verify_card_loss_skill(PlayerPid, CardOwner, CardOrder, CardID) ->
	AllFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	AllInterest = [{skill, {loss, [all]}}, {skill, {cannot_use, [all]}}],
	case function_utility:is_contain(AllInterest, AllFx) of
		[] -> verify_skill_target(CardOwner, CardOrder, CardID);
		[{skill, {loss, [all]}}|_] -> 
			set_card_used_skill(CardOwner, CardOrder, CardID),
			reject_use_skill(PlayerPid, no_skill_to_use);
		[{skill, {cannot_use, [all]}}|_] -> 
			set_card_used_skill(CardOwner, CardOrder, CardID),
			reject_use_skill(PlayerPid, no_skill_to_use)
	end.
	
verify_skill_target(CardOwner, CardOrder, CardID) ->
	case stack_pool:get_last_stack(self(), skill_all_effect_result) of
		{ok, []} ->
			stack_pool:set_stack_option(self(), play, send_seal_skill_animate),
			gen_server:cast(self(), {activate_seal_skill_animation, CardOwner, CardOrder, CardID});
			%activate_skill(CardOwner, CardOrder, CardID);
		{ok, [FxResult|AllFxResult]} -> 
			stack_pool:set_stack_option(self(), skill_all_effect_result, AllFxResult),
			verify_each_target({CardOwner, CardOrder, CardID}, FxResult);
		_ -> pre_end_of_use_skill(CardOwner, CardOrder, CardID)
	end.
	
check_swap_target({CardOwner, CardOrder, CardID}, Target, Case) ->
	case Case of
		3 ->
			% Check ว่า มีการย้ายเป้าหมาย จาก Effect ใดๆหรือไม่และ เป้าหมายเดิมก่อนที่จะย้ายไปนั้น มี เป้าหมายเดิมอยู่หรือไม่
			case check_swap_target_effect({CardOwner, CardOrder, CardID}, Target) of
				% มี Effect ที่ส่งผลให้ย้ายเป้าของ skill และ เป้าหมายก่อนหน้านั้นคือ S ในกรณีนี้จะย้ายเป้าไปได้ แต่ไม่เกิดผลคือ ทั้ง S และ เป้าหมายใหม่จะไม่ได้รับผลของ Effect
				{swap, _SwapTarg} -> [];
				_ -> 	Target
			end;
		4 ->
			case check_swap_target_effect({CardOwner, CardOrder, CardID}, Target) of
				% มี Effect ที่ส่งผลให้ย้ายเป้าของ skill และ เป้าหมายก่อนหน้านั้นคือ S ในกรณีนี้จะย้ายเป้าไปได้ แต่ไม่เกิดผลคือ ทั้ง S และ เป้าหมายใหม่จะไม่ได้รับผลของ Effect
				{swap, SwapTarg} -> SwapTarg;
				_ -> 	Target
			end;
		_ -> Target
	end.
	
check_swap_target_effect({CardOwner, CardOrder, CardID}, Target) ->
	{ok, Status} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, card_status),
	check_card_status(Status, Target).
	
check_card_status([], _) -> wont_swap;
check_card_status([{skill_from, PreTarget, skill_to, NewTarget}|_], Target) -> compare_target(Target, PreTarget, NewTarget);
check_card_status([_|Status], Target) -> check_card_status(Status, Target).

compare_target(Target, PreTarget, NewTarget) ->
	CheckTar = PreTarget -- Target,
	case length(CheckTar) < length(PreTarget) of
		true -> 
			lists:foreach(fun({PTPid, PTOrder, PTId}) ->
						card_utility:remove_card_status(PTPid, PTOrder, PTId, skill_target) end, PreTarget),
			lists:foreach(fun({TPid, TOrder, TId}) ->
						card_utility:add_card_status(TPid, TOrder, TId, skill_target) end, NewTarget),
			{swap, NewTarget};
		_ -> wont_swap
	end.
		

verify_each_target({CardOwner, CardOrder, CardID}, {WhomEffect, InitTarget, EffectTargetType, _TargetNumber, FxToTarget, SkillID}) ->
	OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
	CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	SkillTarget = 
	case WhomEffect of
		1 ->
			% case s_skill:playerown_condition_check(CardOwner, SkillID) of
				% ok -> SkillTarget = [CardOwner];
				% _ -> SkillTarget = []
			% end;
			InitTarget;
		2 ->
			%SkillTarget =  [s_skill_target:playeropp_skill_target_check({CardOwner, CardOrder, CardID}, OpponentPid, SkillID)];
			InitTarget;
		3 ->
			case s_skill:self_condition_check({CardZone ,{CardOwner, CardOrder, CardID}}, OpponentPid, SkillID) of
				ok -> [{CardOwner, CardOrder, CardID}];
				_ -> []
			end;
		4 ->
			InitSkillTarget = s_skill_target:list_target_check({CardZone, {CardOwner, CardOrder, CardID}}, SkillID, OpponentPid),
			skill_utility:real_skill_target(InitSkillTarget, FxToTarget)
	end,
	Target = check_swap_target({CardOwner, CardOrder, CardID}, InitTarget, WhomEffect),
	%io:format("Skill Init Target :~p and Skill Recent Target :~p", [SkillTarget, Target]),
	case skill_utility:verify_skill_target(SkillTarget, Target) of
		target_verify ->	
			stack_pool:add_stack_option_field(self(), skill_all_effect, [{WhomEffect, Target, FxToTarget, SkillID}]),
			verify_skill_target(CardOwner, CardOrder, CardID);
		{target_fail, []} ->
			% case mnesia_odbc:is_seal_card(CardID) of
				% is_seal -> card_utility:set_card_option_field(CardOwner, CardOrder, CardID, active, inactive, CardZone);
				% _ -> do_nothing
			% end,
			%card_utility:remove_card_status(CardOwner, CardOrder, CardID, using_skill, CardZone),
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
			no_longer_be_target(Target),
			set_card_used_skill(CardOwner, CardOrder, CardID, CardZone),
			reject_use_skill(PlayerPid, no_skill_target);
					%interfere_step:return_play(check_play_step);
		{target_fail, SkillTarget} ->	
			case EffectTargetType of
				do_not_need_select ->
					InterestFx = [{curse, {freeze_curse, all}}],
					case function_utility:interest_contain(InterestFx, FxToTarget) of
						[] -> stack_pool:add_stack_option_field(self(), skill_all_effect, [{WhomEffect, SkillTarget, FxToTarget, SkillID}]);
						[{curse, {freeze_curse, all}}] -> stack_pool:add_stack_option_field(self(), skill_all_effect, [{WhomEffect, Target, FxToTarget, SkillID}])
					end,
					verify_skill_target(CardOwner, CardOrder, CardID);
				_ ->
					%card_utility:remove_card_status(CardOwner, CardOrder, CardID, using_skill, CardZone),
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					no_longer_be_target(Target),
					set_card_used_skill(CardOwner, CardOrder, CardID, CardZone),
					reject_use_skill(PlayerPid, no_skill_target)
			end
	end.
	
no_longer_be_target(SkillTarget) ->
	lists:foreach(fun(Target) -> 
		case Target of
			{TPid, TOrder, TId} -> card_utility:remove_card_status(TPid, TOrder, TId, skill_target);
			_ -> do_nothing 
		end end, SkillTarget).
	
% 607 mod 13 หากเงื่อนไข Skill ถูกต้อง ให้ทำงาน 
activate_skill(CardOwner, CardOrder, CardID) ->
	no_longer_be_target(),
	%stack_pool:set_stack_option (self(), play, check_after_case_conditon),
	case stack_pool:get_last_stack(self(), skill_all_effect) of
		{ok, []} -> check_then(CardOwner, CardOrder, CardID);
		{ok, [SkillFx|SkillAllFx]} ->
			stack_pool:set_stack_option(self(), skill_latest_affect, SkillFx),
			stack_pool:set_stack_option(self(), skill_all_effect, SkillAllFx),
			stack_pool:set_stack_option(self(), play, activate_skill_effect),
			new_skill_effect:activate_skill_effect(CardOwner, CardOrder, CardID, SkillFx)
	end.
	
no_longer_be_target() ->
	case stack_pool:get_last_stack(self(), skill_latest_affect) of
		{ok, {_, SkillTarget, _, _}} -> no_longer_be_target(SkillTarget);
		_ -> do_nothing
	end.
	% หลังจาก Effect ก้่อนหน้่านี้ เกิดผล เรียบร้อยแล้ว ก่อนจะทำงานกับ Effect ถัดไป
	% card_utility:remove_card_status(TOwner, TOrder, TID, skill_target),
	
check_then(CardOwner, CardOrder, CardID) ->
	case stack_pool:get_last_stack(self(), then) of
		{ok, []} -> pre_end_of_use_skill(CardOwner, CardOrder, CardID);
		{ok, [{then_do, ThenDo}]} -> 
			stack_pool:remove_stack_option(self(), then),
			stack_pool:set_stack_option(self(), skill_id_list, ThenDo),
			then_do_skill(CardOwner, CardOrder, CardID, ThenDo);
		{ok, [{then_assign, ThenAssign}]} -> 
			stack_pool:remove_stack_option(self(), then),
			stack_pool:set_stack_option(self(), skill_id_list, ThenAssign),
			then_assign_skill(CardOwner, CardOrder, CardID, ThenAssign);
		_ -> pre_end_of_use_skill(CardOwner, CardOrder, CardID)
	end.
	
pre_end_of_use_skill(CardOwner, CardOrder, CardID) ->
	CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal -> 
			case CardZone of
				arena_zone ->
					GetPreZone = stack_pool:get_last_stack(self(), previous_zone),
					case GetPreZone of
						{ok, hand_cards} ->
							card_utility:remove_card_status(CardOwner, CardOrder, CardID, using_skill_success, arena_zone),
							end_of_use_skill(CardOwner, CardOrder, CardID);
						{ok, arena_zone} ->
							card_utility:remove_card_status(CardOwner, CardOrder, CardID, using_skill_success, arena_zone),
							arena_zone:set_seal_inactive(CardOwner, CardOrder, CardID),
							end_of_use_skill(CardOwner, CardOrder, CardID)
					end;
				_ ->	end_of_use_skill(CardOwner, CardOrder, CardID)
			end;
		_ -> pre_end_of_use_skill(CardOwner, CardOrder, CardID, CardZone)
	end.
	%pre_end_of_use_skill(CardOwner, CardOrder, CardID, CardZone).
	
pre_end_of_use_skill(CardOwner, CardOrder, CardID, CardZone) ->
	{ok, Status} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, card_status, CardZone),
	AllCardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	case attribute_check:use_skill_duration_contain(AllCardFx) of
		no_use_skill_duration -> 
			case used_skill(Status) of
				{ok, 0} -> card_utility:add_card_status(CardOwner, CardOrder, CardID, {used_skill, 1}, CardZone);
				{ok, UsedCount} ->
					card_utility:remove_card_status(CardOwner, CardOrder, CardID, {used_skill, UsedCount}, CardZone),
					card_utility:add_card_status(CardOwner, CardOrder, CardID, {used_skill, UsedCount+1}, CardZone)
			end,
			end_of_use_skill(CardOwner, CardOrder, CardID);
		{_, Fx, {use_skill, CountRequire}} ->
			{ok, UsedCount} = used_skill(Status),
			if 
				UsedCount =:= CountRequire-1 -> 
					stack_pool:set_stack_option(self(), play, go_to_end_of_use_skill),
					case Fx of
						[{move, sacrifice}] -> 
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
							destroy:check_card_destroyed(PlayerPid, [{CardOwner, CardOrder, CardID}, use_at_skill_destroy]);
						_ -> end_of_use_skill(CardOwner, CardOrder, CardID)
					end;
				UsedCount =:= 0 -> 
					card_utility:add_card_status(CardOwner, CardOrder, CardID, {used_skill, 1}, CardZone),
					end_of_use_skill(CardOwner, CardOrder, CardID);
				true ->
					card_utility:remove_card_status(CardOwner, CardOrder, CardID, {used_skill, UsedCount}, CardZone),
					card_utility:add_card_status(CardOwner, CardOrder, CardID, {used_skill, UsedCount+1}, CardZone),
					end_of_use_skill(CardOwner, CardOrder, CardID)
			end
	end.
	
used_skill([{used_skill, UsedCount}|_]) -> {ok, UsedCount};
used_skill([_|Tail]) -> used_skill(Tail);
used_skill([]) -> {ok, 0}.
	
% 605.12. 
end_of_use_skill(CardOwner, CardOrder, CardID) ->
	check_use_skill_success_ability(CardOwner, CardOrder, CardID).
	%Ability ที่จะทำงานเมื่อ Seal ใช้ Skill สำเร็จ ถ้ามีการเลือกการทำงานของ Ability หรือเลือกเป้าหมายให้เลือกใน Phase นี้ --
check_use_skill_success_ability(CardOwner, CardOrder, CardID) ->
	CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	card_utility:add_card_status(CardOwner, CardOrder, CardID, using_skill_success, CardZone),
	card_utility:add_card_status(CardOwner, CardOrder, CardID, use_skill_success, CardZone),
	stack_pool:set_stack_option(self(), play, check_skill_success_ability),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_activate:check_any_ability_activate(use_skill_success, PlayerPid).

% 605.13. 
	%ตรวจสอบว่า Ability นั้นถูกต้องตามเงื่อนไขในการทำงานหรือไม่ ถ้าเงื่อนไขไม่ครบ Ability นั้นจะไม่ทำงาน หาก Ability ไม่สามารถส่งผลกับเป้าหมายที่กำหนด
	% หรือ เป้าหมายที่กำหนดไม่อยู่ในสภาพตกเป็นเป้าหมายได้ ให้กลับไปเลือกเป้าหมายของ Abilityใน phase 605.12. ใหม่ หากไม่มีเป้าหมายใหม่ที่ Ability สามารถส่งผล
	% หรือ กำหนดได้ Ability นั้นจะไม่ทำงาน ถ้าเงื่อนไขในการเกิด Ability ยังคงถูกต้องและมีเป้าหมายที่ถูกต้อง Ability เมื่อ Seal ใช้ Skill สำเร็จ จะทำงาน ใน Phase นี้ --
verify_use_skill_success_ability(CardOwner, CardOrder, CardID) ->
	stack_pool:set_stack_option(self(), play, verify_skill_success_ability),
	mod_ability_activate:verify_ability_condition(use_skill_success).
	
activate_use_skill_success_effect() ->
	stack_pool:set_stack_option(self(), play, activate_skill_success_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(use_skill_success, PlayerPid).
	
end_of_use_skill2(CardOwner, CardOrder, CardID) ->	
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, use_skill_success),
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, using_skill),
	stack_pool:pop_stack_out(self()),
	case stack_pool:get_last_stack(self(), play) of
		{ok, StackPlay} ->
			interfere_step:return_play(StackPlay);
		{error, _} -> 
			gen_server:cast(self(), {act_next_command})
	end.
	
set_card_used_skill(CardOwner, CardOrder, CardID) ->
	CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	set_card_used_skill(CardOwner, CardOrder, CardID, CardZone).
set_card_used_skill(CardOwner, CardOrder, CardID, CardZone) ->
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, using_skill, CardZone),
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			case CardZone of
				arena_zone -> arena_zone:set_seal_inactive(CardOwner, CardOrder, CardID);
				_ -> do_nothing
			end;
		is_not_seal -> 
			{ok, Status} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, card_status, CardZone),
			AllCardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
			case attribute_check:use_skill_duration_contain(AllCardFx) of
				no_use_skill_duration -> 
					case used_skill(Status) of
						{ok, 0} -> card_utility:add_card_status(CardOwner, CardOrder, CardID, {used_skill, 1}, CardZone);
						{ok, UsedCount} ->
							card_utility:remove_card_status(CardOwner, CardOrder, CardID, {used_skill, UsedCount}, CardZone),
							card_utility:add_card_status(CardOwner, CardOrder, CardID, {used_skill, UsedCount+1}, CardZone)
					end;
				{_, Fx, {use_skill, CountRequire}} ->
					{ok, UsedCount} = used_skill(Status),
					if 
						UsedCount =:= CountRequire-1 -> 
							stack_pool:set_stack_option(self(), play, go_to_end_of_use_skill),
							case Fx of
								[{move, sacrifice}] -> 
									{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
									destroy:check_card_destroyed(PlayerPid, [{CardOwner, CardOrder, CardID}], use_at_skill_destroy);
								_ -> end_of_use_skill(CardOwner, CardOrder, CardID)
							end;
						UsedCount =:= 0 -> 
							card_utility:add_card_status(CardOwner, CardOrder, CardID, {used_skill, 1}, CardZone);
						true ->
							card_utility:remove_card_status(CardOwner, CardOrder, CardID, {used_skill, UsedCount}, CardZone),
							card_utility:add_card_status(CardOwner, CardOrder, CardID, {used_skill, UsedCount+1}, CardZone)
					end
			end
	end.
