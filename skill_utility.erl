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
-module(skill_utility).
-export([
						target_to_client/3,
						real_fx_to_target/2,
						depend_on_target/2,
						select_curse_to_fx/2,
						add_selected_curse_to_fx/1,
						select_element_to_fx/4,
						add_selected_elem_to_fx/1,
						operate_player_selected_decision/1,
						verify_skill_target/2,
						real_skill_target/2,
						check_target_cancel_skill/4,
						target_player/1,
						add_select_seal_power_to_fx/1,
						select_seal_swap_skill_target/1,
						aaa/0,
						player_select_deck/2
					]).
			
target_to_client([], _, TargetSize) -> [TargetSize];
target_to_client([{PlayerPid, CardOrder, CardID, CardZone, CardLine} | T], PlayerPid, TargetSize) ->
	target_to_client(T, PlayerPid, TargetSize + 1) ++ [1, CardOrder, <<CardID:16>>, CardZone, CardLine];
target_to_client([{_, CardOrder, CardID, CardZone, CardLine} | T], PlayerPid, TargetSize) ->
	target_to_client(T, PlayerPid, TargetSize + 1) ++ [0, CardOrder, <<CardID:16>>, CardZone, CardLine].
	
real_fx_to_target(FxToTarget, TargetList) ->
	[{_, Fx, _}] = FxToTarget,
	case Fx of
		[] -> 
			NewFxToTarget = FxToTarget,
			Target = TargetList;
		_ -> 
			AllInterest = [{heal_selected_curse, last_dance_curse}, {heal_selected_curse, charm_curse}],
			LeftOver = Fx--AllInterest,
			Interest = Fx--LeftOver,
			case Interest of
				[{heal_selected_curse, last_dance_curse}] ->
					Target = function_utility:card_match_condition(TargetList,  [{curse, last_dance_curse}]),
					NewFxToTarget = FxToTarget;
					%stack_pool:set_stack_option(self(), fx_to_target, NewFxToTarget);
				[{heal_selected_curse, charm_curse}] ->
					Target = function_utility:card_match_condition(TargetList,  [{curse, charm_curse}]),
					NewFxToTarget = FxToTarget;
					%stack_pool:set_stack_option(self(), fx_to_target, NewFxToTarget);
				_ ->
					NewFxToTarget = FxToTarget,
					Target = TargetList
			end
	end,
	{NewFxToTarget, Target}.

depend_on_target({WhomFx, OtherSkillId}, [{{WhomFx, SkillID}, Target}|Tail]) ->
	io:format("+++++++++--------------------"),
	case OtherSkillId of
		SkillID -> Target;
		_ -> depend_on_target(OtherSkillId, Tail)
	end;
depend_on_target(_, []) -> [].
					
select_curse_to_fx(AllCurse, CurseAmount) ->
	CurseCode = function_utility:curse_code(AllCurse),
	CurseSize = length(CurseCode),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	gen_server:cast(self(), {activate_select_curse, PlayerPid, 0, CurseAmount, CurseSize, function_utility:del(function_utility:qsort(CurseCode))}).
	
add_selected_curse_to_fx(Data) ->% ถ้าทำบรรทัดบนให้ลบอันนี้ออก
	%stack_pool:set_stack_option(self(), selected_curse, Data),
	%stack_pool:set_stack_option(self(), play, update_player_select_curse),
	%self() ! {update_player_select_curse, Data}.
	
%add_selected_curse_to_fx() ->
	%{ok, Data} = stack_pool:get_last_stack(self(), selected_curse),
	CuresSize = length(Data),
	SelectedCurse = function_utility:reverse_curse_code(Data),
	{ok, [{WhomEffect, TargetList, EffectTargetType, TargetNumber,  [{CardGive, Fx, Duration}], SkillID}]} = stack_pool:get_last_stack(self(), this_set_seal_use_skill),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	NewFx = [{heal_selected_curse, SelectedCurse}]++Fx--[{heal_curse, {select, CuresSize}}],
	Condition = function_utility:with_effect_type(curse, SelectedCurse),
	NewTargetList = function_utility:card_match_condition(TargetList, Condition),
	% ต้องเข้า pre_set_seal_use_skill_loop() หรือเปล่า เอาไว้ค่อยเช็ค
	new_seal_skill:set_seal_use_skill_loop(PlayerPid, {WhomEffect, NewTargetList, EffectTargetType, TargetNumber, [{CardGive, NewFx, Duration}], SkillID}).
					
select_element_to_fx(PlayerPid, SelectType, ElemToSelect, SelectAmount) ->
	ElemSize = length(ElemToSelect),
	gen_server:cast(self(), {activate_select_element, PlayerPid, SelectType, SelectAmount, ElemSize, ElemToSelect}).

add_selected_elem_to_fx(Data) ->
	ElemSize = length(Data),
	case stack_pool:get_last_stack(self(), select_element_case) of
		{ok, mystic_condition} ->
			{ok, {TargetType, MAbilityID, {SelectType, SelectAmount, Target, Fx, IdDuration}}} = stack_pool:get_last_stack(self(), this_mystic_ability_id),
			NewFx = [{elem, Data}]++Fx--[{elem, {select, ElemSize}}],
			stack_pool:set_stack_option(self(), this_mystic_ability_id, {TargetType, MAbilityID, {SelectType, SelectAmount, Target, NewFx, IdDuration}}),
			new_mystic_check:generalize_effect_to_each_target();
		_ ->
			{ok, [{WhomEffect, TargetList, EffectTargetType, TargetNumber, [{{GPid, GOrder, GID}, Fx, Duration}], SkillID}]} = stack_pool:get_last_stack(self(), this_set_seal_use_skill),
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
			NewFx = [{elem, Data}]++Fx--[{elem, {select, ElemSize}}],
			% ต้องเข้า pre_set_seal_use_skill_loop() หรือเปล่า เอาไว้ค่อยเช็ค
			new_seal_skill:set_seal_use_skill_loop(PlayerPid, {WhomEffect, TargetList, EffectTargetType, TargetNumber, [{{GPid, GOrder, GID}, NewFx, Duration}], SkillID})
	end.
	
add_select_seal_power_to_fx(TargetSelect) ->
	[{TPlayerPid, TCardOrder, TCardID}|_] = TargetSelect,
	{ok, [{Power, {{IntitialCard, ConditionNeed}, PowerType}}]} = stack_pool:get_last_stack(self(), interest_effect),
	{ok, [{WhomEffect, TargetList,  EffectTargetType,  TargetNumber, [{{ReceivePid, RCardOrder, RCardID}, Fx, Duration}], SkillID}]} = stack_pool:get_last_stack(self(), this_set_seal_use_skill),
	NewFx = [{Power, {'+',{PowerType, {TPlayerPid, TCardOrder, TCardID}}}}]++Fx--[{Power, {{IntitialCard, ConditionNeed}, PowerType}}],
	stack_pool:set_stack_option(self(), this_set_seal_use_skill, [{WhomEffect, TargetList, EffectTargetType, TargetNumber,  [{{ReceivePid, RCardOrder, RCardID}, NewFx, Duration}], SkillID}]),
	TarZoLi = skill_card_list:with_zone_line(TargetSelect),
	TextCode = dialog_text:text_on_panel([{{ReceivePid, RCardOrder, RCardID}, NewFx, Duration}]),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	stack_pool:set_stack_option(self(), play, go_to_aaa),
	gen_server:cast(self(), {update_player_select_target_skill, PlayerPid, ReceivePid, TarZoLi, TextCode, all_is_card}).
	
select_seal_swap_skill_target(TargetSelect) ->
	[{TPlayerPid, TCardOrder, TCardID}|_] = TargetSelect,
	{ok, [{move_skill_target, {{PotenCard, Condition, Amnt}, ToWhich}}]} = stack_pool:get_last_stack(self(), interest_effect),
	{ok, [{WhomEffect, TargetList,  EffectTargetType,  TargetNumber, [{{ReceivePid, RCardOrder, RCardID}, Fx, Duration}], SkillID}]} = stack_pool:get_last_stack(self(), this_set_seal_use_skill),
	NewFx = [{move_skill_target, {TargetSelect, ToWhich}}]++Fx--[{move_skill_target, {{PotenCard, Condition, Amnt}, ToWhich}}],
	stack_pool:set_stack_option(self(), this_set_seal_use_skill, [{WhomEffect, TargetList, EffectTargetType, TargetNumber,  [{{ReceivePid, RCardOrder, RCardID}, NewFx, Duration}], SkillID}]),
	TarZoLi = skill_card_list:with_zone_line(TargetSelect),
	TextCode = dialog_text:text_on_panel([{{ReceivePid, RCardOrder, RCardID}, NewFx, Duration}]),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	stack_pool:set_stack_option(self(), play, go_to_aaa),
	gen_server:cast(self(), {update_player_select_target_skill, PlayerPid, ReceivePid, TarZoLi, TextCode, all_is_card}).
	
% เรียกหลังจาก ส่งไปอัพเดทฝั่งตรงข้ามเสร็จแล้วว่า ผู้เล่นเลือก การ์ด XXX เพื่อจะ......(ที่ไม่ใช่การเลือกเพื่อเป็นเป้าของ Skill)
aaa() ->
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	{ok, [{WhomEffect, TargetList, EffectTargetType, TargetNumber, [{{ReceivePid, RCardOrder, RCardID}, NewFx, Duration}], SkillID}]} = stack_pool:get_last_stack(self(), this_set_seal_use_skill),
	new_seal_skill:set_seal_use_skill_loop(PlayerPid, {WhomEffect, TargetList, EffectTargetType, TargetNumber, [{{ReceivePid, RCardOrder, RCardID}, NewFx, Duration}], SkillID}).

operate_player_selected_decision(Data) ->
	{ok, [{WhomEffect, Target, EffectTargetType, TargetNumber, FxToTarget, SkillID}]} = stack_pool:get_last_stack(self(), this_set_seal_use_skill),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	case Data of
		[1] ->
			FxTargetType = 
			case EffectTargetType of
				player_can_select_own_exact_target -> player_select_exact_target;
				opponent_can_select_own_exact_target -> opponent_select_exact_target
			end,
			new_seal_skill:set_seal_use_skill_loop(PlayerPid, {WhomEffect, Target, FxTargetType, TargetNumber, FxToTarget, SkillID});
		[0] ->
			new_seal_skill:set_use_skill(PlayerPid)
	end.
	
target_player(PlayerSelected) ->
	{ok, [{WhomEffect, TargetList, EffectTargetType, TargetNumber, [{CardGive, Fx, Duration}], SkillID}]} = stack_pool:get_last_stack(self(), this_set_seal_use_skill),
	%{ok, [{EffectTargetType, WhomEffect, TargetNumber, TargetList,  [{CardGive, Fx, Duration}], SkillID}]} = stack_pool:get_last_stack(self(), this_set_seal_use_skill),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	PlayerOpPid = mnesia_play:get_opponent_pid(PlayerPid),
	case PlayerSelected of
		[PlayerPid] -> Target = TargetList -- function_utility:remove_card_zone(skill_card_list:player_zone_list({opponent, [arena_zone], seal}, {PlayerPid, PlayerOpPid}));
		_ -> Target = TargetList -- function_utility:remove_card_zone(skill_card_list:player_zone_list({owner, [arena_zone], seal}, {PlayerPid, PlayerOpPid}))
	end,
	{ok, [{FxType, {Effect, all}}]} = stack_pool:get_last_stack(self(), effect_interest),
	%NewFx = [{FxType, Effect}]++Fx--[{FxType, {Effect, all}}],
	NewFx = [{FxType, {Effect, all}}],
	new_seal_skill:set_seal_use_skill_loop(PlayerPid, {WhomEffect, Target, EffectTargetType, TargetNumber, [{CardGive, NewFx, Duration}], SkillID}).

% SkillTarget เป้าที่ check หลัง Interfere
% TargetCard เป้่าที่ระบุไว้ หรือ เลือกไว้ ก่อน Interfere
verify_skill_target(SkillTarget, TargetCard) ->
	case SkillTarget of
		[] -> {target_fail, []};
		_ ->
			Result = TargetCard--SkillTarget,
			if 
				Result =:= [] -> target_verify;
				true -> {target_fail, SkillTarget}
			end
	end.

%การตรวจสอบ Target ของ effect ประเภทที่ต้องเลือก condition ก่อนนั้น Target ทีได้จาก s_skill_target:list_target_check()
%กับ Target ที่ต้องตรวจสอบจริงจะไม่เท่ากันต้องเข้า Function นี้เพื่อหา Target ที่แท้จริง
real_skill_target(TargetList, FxToTarget) ->
	[{_, Fx, _}] = FxToTarget,
	%io:format('Fx ~p~n', [Fx]),
	case Fx of
		[{heal_selected_curse, Curse}] ->
			Condition = function_utility:with_effect_type(curse, Curse),
			function_utility:card_match_condition(TargetList,  Condition);
		_ -> TargetList
	end.
	
% check_target_cancel_skill(_PlayerPid, _CardOrder, _CardID, []) -> [];	
% check_target_cancel_skill(PlayerPid, CardOrder, CardID, [{TPid, TOrder, TID}|MysticTarget]) ->
	% Interest = [{cancel_skill, [all]}, {cancel_skill, [all_opponent]}, {cancel_skill, [opponent_relic]}],
	% TargetFx = card_utility:get_all_card_effect(TPid, TOrder, TID),
	% %smo_logger:fmsg("Target Effect are ~p~n", [TargetFx]),
	% FxContain = function_utility:is_contain(Interest, TargetFx),
	% %smo_logger:fmsg("Effect card contain are ~p~n", [FxContain]),
	% case FxContain of
		% [{cancel_skill, [all]}] -> check_target_cancel_skill(PlayerPid, CardOrder, CardID, MysticTarget);
		% [{cancel_skill, [all_opponent]}] ->
			% case TPid of
				% PlayerPid -> [{TPid, TOrder, TID}] ++ check_target_cancel_skill(PlayerPid, CardOrder, CardID, MysticTarget);
				% _ ->	check_target_cancel_skill(PlayerPid, CardOrder, CardID, MysticTarget)
			% end;			
		% _A -> %smo_logger:fmsg("Card can not cancel skill case ~p~n", [_A]),
			% [{ TPid, TOrder, TID}] ++ check_target_cancel_skill(PlayerPid, CardOrder, CardID, MysticTarget)
	% end;
% check_target_cancel_skill(PlayerPid, CardOrder, CardID, [Pid|MysticTarget]) -> [Pid] ++ check_target_cancel_skill(PlayerPid, CardOrder, CardID, MysticTarget).
% ------------------------------------------------------------
check_target_cancel_skill(GPid, GOrder, GID, [TargetSkill|Tail]) ->
	case cancel_any_skill(TargetSkill, {GPid, GOrder, GID}) of
		{ok, cancel} -> check_target_cancel_skill(GPid, GOrder, GID, Tail);
		_ -> 
			case mnesia_odbc:is_seal_card(GID) of
				is_not_seal ->
					MysticTarget= 	mystic_effect:check_target_cancel_mystic(GPid, GOrder, GID, [TargetSkill]),
					MysticTarget ++ check_target_cancel_skill(GPid, GOrder, GID, Tail);
				_ -> [TargetSkill] ++ check_target_cancel_skill(GPid, GOrder, GID, Tail)
			end
	end;
check_target_cancel_skill(_, _, _, []) -> [].

cancel_any_skill(TargetSkill, CardGive) ->
	case TargetSkill of
		{CardOwner, CardOrder, CardID} ->
			CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
			case function_utility:is_cancel_any_skill(CardFx) of
				{ok, [all]} -> {ok, cancel};
				{ok, Any} -> function_utility:will_cancel(Any, CardGive, CardOwner, CardOrder, CardID);
				{error, _} -> {error, []}
			end;
		_ -> {error, []}
	end.
	
player_select_deck(PlayerPid, Data) ->
	{ok, [{_WhomEffect, _TargetList, EffectTargetType, _TargetNumber,  _, _SkillID}]} = stack_pool:get_last_stack(self(), this_set_seal_use_skill),
			case EffectTargetType of
				player_select_1_deck_and_system_select_top_deck ->
					case Data of
						[0] -> 
							{ok, [{{CardOwner, CardOrder, CardID}, _}|_]} = mnesia_play:get_player_data(PlayerPid, seal_deck),
							system_select_target_skill([{CardOwner, CardOrder, CardID}]);
						[1] ->
							{ok, [{{CardOwner, CardOrder, CardID}, _}|_]} = mnesia_play:get_player_data(PlayerPid, mystic_deck),
							system_select_target_skill([{CardOwner, CardOrder, CardID}])
					end;
				_ ->
					case Data of
						[0] -> 
							{ok, [{{CardOwner, CardOrder, CardID}, _}|_]} = mnesia_play:get_player_data(PlayerPid, seal_deck),
							system_select_target_skill([{CardOwner, CardOrder, CardID}]);
						[1] ->
							{ok, [{{CardOwner, CardOrder, CardID}, _}|_]} = mnesia_play:get_player_data(PlayerPid, mystic_deck),
							system_select_target_skill([{CardOwner, CardOrder, CardID}])
					end
			end.
			
system_select_target_skill(TargetList) ->
	{ok, [{WhomEffect, _Target, EffectTargetType, TargetNumber, FxToTarget, SkillID}]} = stack_pool:get_last_stack(self(), this_set_seal_use_skill),
	%ส่งไปบอกฝั่งตรงข้่ามว่า เราเลือก seal ตาม TargetList นี้จากผลของการใช้ skill = TextCode
	stack_pool:add_stack_option_field(self(), skill_target, [{{SkillID, WhomEffect}, TargetList}]),
	stack_pool:add_stack_option_field(self(), skill_all_effect_result, [{WhomEffect, TargetList, EffectTargetType, TargetNumber, FxToTarget, SkillID}]),
	new_seal_skill:set_use_skill().
