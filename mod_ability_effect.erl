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
-module(mod_ability_effect).
-export([
						check_any_ability_activate/2,
						effect_activate_1/0,
						start_effect_activate/1,
						check_ability_effect_affect/0,
						affect_current_effect/1,
						check_effect_affect/1,
						check_effect_to_player/0,
						activate_effect_to_player/3,
						affect_current_player_effect/0,
						check_continuous_effect/0,
						check_next_effect/0,
						set_new_power/2
					]).

check_any_ability_activate(Status, PlayerPid) ->
	OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	% ตรวจสอบว่ามี Seal ใบไหน ที่จะเกิด Effect บ้างใน Status นี้ ถ้ามีจะคืนค่ามาเป็น 
		% [{{CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID}|Tail]
	UpdateAbility = update_ability:update(Status, PlayerPid, OpponentPid),
	%io:format("all potential ability ~p~n", [function_utility:qsort(UpdateAbility)]),
	case UpdateAbility of
		% ถ้าไม่มีก็ไม่ต้องทำการใดๆ ให้ไปทำงาน Activate Effect ของ เดิม
			%(ที่ได้จากการเลือกเป้า หรือ มีการเลือกว่าจะให้เกิดผลหรือไม่)ที่มีอยู่เลย
		[] -> start_effect_activate(Status);
		% ถ้ามีให้ส่งไปทำงานต่อไป
		_ -> 
			stack_pool:set_stack_option(self(), play_status, Status),
			stack_pool:set_stack_option(self(), player, PlayerPid),
			stack_pool:set_stack_option(self(), opponent, OpponentPid),
			stack_pool:set_stack_option(self(), {all_potential_ability, Status}, UpdateAbility),
			stack_pool:set_stack_option(self(), all_potential_ability, UpdateAbility),
			check_ability_effect()
	end.
	
% ตรวจสอบว่า Effect ทีีเกิดคืออะไร
check_ability_effect() ->
	case stack_pool:get_last_stack(self(), all_potential_ability) of
		% เมื่อ generalize_effect_to_each_target ให้กับทุก AbilityId แล้ว ให้ไปทำงาน Activate Effect ทั้งหมดของ Play Status นั้น
		{ok, []} -> 
			{ok, Status} = stack_pool:get_last_stack(self(), play_status),
			start_effect_activate(Status);
		{ok, AllAbility} ->
			{{CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID} = Last = lists:last(AllAbility),
			stack_pool:set_stack_option(self(), all_potential_ability, AllAbility--[Last]),
			Fx = s_ability_effect:s_ability_effect({CardZone, {PlayerOwnID, CardOrder, CardID}}, AbilityId, PlayerOppID),
			check_ability_type(Fx);
			_ ->
				interfere_step:return_play (check_play_step)
	end.
	
check_ability_type([{AbilityType, EnebleCondition, EffectToAll}]) ->
	stack_pool:set_stack_option(self(), this_ability_type_and_condition, {AbilityType, EnebleCondition}),
	stack_pool:set_stack_option(self(), this_ability_effect, EffectToAll),
	generalize_effect_to_each_target().
	
generalize_effect_to_each_target() ->
	case stack_pool:get_last_stack(self(), this_ability_effect) of
		{ok, []} -> check_ability_effect();
		{ok, [{WhoseFx, Effect}|T]} ->
			stack_pool:set_stack_option(self(), this_ability_effect, T),
			stack_pool:set_stack_option(self(), whose_effect, WhoseFx),
			check_select_case(Effect)
	end.

% ตรวจสอบว่า Effect ที่ีเกิดต้องเลือก ให้่มีการทำงาน หรือ มีการเลือกเป้าหมายหรือไม่ และส่งไปทำงาน ตามแต่ละกรณี
check_select_case({{CardOwner, CardOrder, CardID, AbilityId}, Target, {SelectType, SelectAmount, Effect}, Duration}) ->
	stack_pool:set_stack_option(self(), effect_before_select_activate, {{CardOwner, CardOrder, CardID, AbilityId}, Target, {SelectType, SelectAmount, Effect}, Duration}),
	case SelectType of
		do_not_need_select -> do_not_need_select();% ไม่ต้องเลือกเป้า
		{check, AbiIDCheck, Select} -> check_what_player_select({CardOwner, CardOrder, CardID}, AbiIDCheck, Select);
		_ -> generalize_effect_to_each_target() % กรณีอื่นที่ต้องมีการเลือกเป้าหรือ เลือกการทำงานของ Ability แล้ว ไม่ต้องทำอะไร
	end.
	
do_not_need_select() -> 
	{ok, Status} = stack_pool:get_last_stack(self(), play_status),
	{ok, WhoseFx} = stack_pool:get_last_stack(self(), whose_effect),
	{ok, {{CardOwner, CardOrder, CardID, AbilityId}, Target, {_, _, Effect}, Duration}} = stack_pool:get_last_stack(self(), effect_before_select_activate),
	{ok, A_Type_Cond} = stack_pool:get_last_stack(self(), this_ability_type_and_condition),
	stack_pool:add_stack_option_field(self(), {all_fx_to_target, Status}, [{WhoseFx, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  Effect, Duration}]),
	generalize_effect_to_each_target().	
% -----------------เป็น AbilityID ที่เป็น do_not_need_select แต่จะต้องขึ้นอยูกับ AbilityID ก่อนหน้่าว่าผู้เล่นเลือก จะใช้ Ability หรือไม่
check_what_player_select({CardOwner, CardOrder, CardID}, AbiIDCheck, Select) ->
	{ok, Status} = stack_pool:get_last_stack(self(), play_status),
	case stack_pool:get_last_stack(self(), {all_fx_to_target, Status}) of
		{ok, FxTarget} ->
			% Check ว่า Effect ที่ Set ไปแล้ว มี Effect ที่เป็น Ability Number เดียวกันของ Card ใบเดียวกันหรือไม่
			case had_active_ability_number({CardOwner, CardOrder, CardID}, AbiIDCheck, FxTarget) of
				% ถ้ามี
				have ->
					case Select of
						do_not_need_select ->
							{ok, {{CardOwner, CardOrder, CardID, AbilityId}, Target, {_, _, Effect}, Duration}} = stack_pool:get_last_stack(self(), effect_before_select_activate),
							{ok, WhoseFx} = stack_pool:get_last_stack(self(), whose_effect),
							{ok, A_Type_Cond} = stack_pool:get_last_stack(self(), this_ability_type_and_condition),
							stack_pool:add_stack_option_field(self(), {all_fx_to_target, Status}, [{WhoseFx, A_Type_Cond, {CardOwner, CardOrder, CardID, AbilityId}, Target,  Effect, Duration}]);
						_ -> ""
					end;
				% ถ้าไม่มี
				_ -> ""
			end;
		_ -> ""
	end,
	generalize_effect_to_each_target().

had_active_ability_number({_CardOwner, _CardOrder, _CardID}, _AbiIDCheck, []) -> does_not_have;
had_active_ability_number({CardOwner, CardOrder, CardID}, AbiIDCheck, [{_, _, {CardOwner, CardOrder, CardID, AbiIDCheck}, _Target,  _Effect, _Duration}|_Ability]) -> have;
had_active_ability_number({CardOwner, CardOrder, CardID}, AbiIDCheck, [_|Ability]) -> had_active_ability_number({CardOwner, CardOrder, CardID}, AbiIDCheck, Ability).
%------------------------------------------------------------------
start_effect_activate(Status) ->
	special_remove_card_status(Status),
	case stack_pool:get_last_stack(self(), {all_fx_to_target, Status}) of
		{error, _} -> interfere_step:return_play(check_play_step);
		{ok, AbilityAllFx} ->
			%smo_logger:fmsg("get last stack of {all_fx_to_target, ~p} ~p~n", [Status, AbilityAllFx]),
			stack_pool:set_stack_option (self(), ability_all_fx_result, AbilityAllFx),
			effect_activate_1()
	end.
	
special_remove_card_status(Status) ->
	case Status of
		into_shrine -> 
			case stack_pool:get_last_stack (self(), card_destroy) of
				{ok, DestroyedCards} ->
					lists:foreach(fun({PlayerPid, CardOrder, CardID}) -> remove_cards_status(PlayerPid, CardOrder, CardID) end, DestroyedCards);
				_ -> ""
			end;
		_ -> ""
	end.
	
remove_cards_status(CardOwner, CardOrder, CardID) ->
	Zone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, moving_to_shrine, Zone).
		
effect_activate_1() ->
	case stack_pool:get_last_stack (self(), ability_all_fx_result) of
		{ok, []} -> 
			interfere_step:return_play(check_play_step);
		{ok, [{_WhoseFx, {FxType, _EnableCondition}, {CardOwner, CardOrder, CardID, AbilityId}, Target,  Effect, Duration}|Tail]} ->
			%[{WhoseFx, {FxType, EnableCondition}, {CardOwner, CardOrder, CardID, AbilityId}, Target,  Effect, Duration}],
			Subturn = get(subturn),
			AbilityNo = query_ability:no_of_ability(AbilityId),
			case get({card_give_fx, Subturn}) of
				undefined -> 
					put({card_give_fx, Subturn}, [{CardOwner, CardOrder, CardID, AbilityNo}]);
				AllGFx-> put({card_give_fx, Subturn}, AllGFx ++ [{CardOwner, CardOrder, CardID, AbilityNo}])
			end,
			stack_pool:set_stack_option (self(), ability_all_fx_result, Tail),
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), player),
			OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
			case {Target, FxType} of
				{[PlayerPid], y} ->
					stack_pool:set_stack_option(self(), call_by, effect_activate_1),
					continuous_ability:set_ability_active({CardOwner, CardOrder, CardID, AbilityId}),
					effect_activate_1();
					%interfere_step:return_play (check_play_step);
					%ability_effect:set_ability(EnableCondition, WhoseFx, {{CardOwner, CardOrder, CardID, AbilityId}, Effect, Duration});
				{[PlayerPid], _} ->
					StackOption = [{play, player_effect_affect}, {card_give_effect, {CardOwner, CardOrder, CardID, AbilityId}}, {fx_affect, Effect}, {duration, Duration}, {target, Target}, {ability, AbilityId}, {ability_controller, PlayerPid}],
					stack_pool:push_stack(self(), CardOwner, CardOrder, CardID, StackOption),
					interfere_step:return_play (check_play_step);
				{[OpponentPid], y} ->
					stack_pool:set_stack_option(self(), call_by, effect_activate_1),
					%ability_effect:set_ability(EnableCondition, WhoseFx, {{CardOwner, CardOrder, CardID, AbilityId}, Effect, Duration});
					continuous_ability:set_ability_active({CardOwner, CardOrder, CardID, AbilityId}),
					effect_activate_1();
					%interfere_step:return_play (check_play_step);
				{[OpponentPid], _} ->
					StackOption = [{play, player_effect_affect}, {card_give_effect, {CardOwner, CardOrder, CardID, AbilityId}}, {fx_affect, Effect}, {duration, Duration}, {target, Target}, {ability, AbilityId}, {ability_controller, PlayerPid}],
					stack_pool:push_stack (self(), CardOwner, CardOrder, CardID, StackOption),
					interfere_step:return_play (check_play_step);
				_ ->
					case FxType of
						y ->
							%stack_pool:set_stack_option(self(), call_by, effect_activate_1),
							%set_ability(EnableCondition, WhoseFx, {{CardOwner, CardOrder, CardID, AbilityId}, Effect, Duration});
							continuous_ability:set_ability_active({CardOwner, CardOrder, CardID, AbilityId}),
							effect_activate_1();
						h ->
							%ability_effect:set_hand_ability(EnableCondition, WhoseFx, {{CardOwner, CardOrder, CardID, AbilityId}, [], Effect, Duration});
							continuous_ability:set_ability_active({CardOwner, CardOrder, CardID, AbilityId}),
							interfere_step:return_play(check_play_step);
						_ ->
							StackOption = [{play, ability_effect_affect}, {card_give_effect, {CardOwner, CardOrder, CardID, AbilityId}}, {fx_affect, Effect}, {duration, Duration}, {target, Target}, {ability, AbilityId}, {ability_controller, PlayerPid}],
							stack_pool:push_stack(self(), CardOwner, CardOrder, CardID, StackOption),
							interfere_step:return_play (check_play_step)
					end
			end;
		_ -> ""
	end.
%------------------------------------------------------------------
check_effect_to_player() ->
	AllFx = stack_pool:get_last_stack(self(), fx_affect),
	case AllFx of
		{ok, []} -> 
			stack_pool:pop_stack_out(self()),
			effect_activate_1();
		{ok, [Fx | FxAffect]} ->
			stack_pool:set_stack_option(self(), play, affect_player_ability_effect),
			stack_pool:set_stack_option(self(), fx_affect, FxAffect),
			stack_pool:set_stack_option(self(), current_effect, Fx),
			interfere_step:return_play(check_play_step)
	end.
	
affect_current_player_effect() ->
	{ok, Fx} = stack_pool:get_last_stack(self(), current_effect),
	stack_pool:set_stack_option(self(), play, return_to_check_effect_to_player),
	check_effect_affect(Fx).
%------------------------------------------------------------------	
check_ability_effect_affect() ->
	AllFx = stack_pool:get_last_stack(self(), fx_affect),
	case AllFx of
		{ok, []} -> 
			check_continuous_effect();
		{ok, Fx} ->
			affect_current_effect(Fx)
	end.
	
affect_current_effect([Fx|FxRemain]) ->
	stack_pool:set_stack_option(self(), fx_affect, FxRemain),
	stack_pool:set_stack_option(self(), play, return_to_check_ability_effect_affect),
	check_effect_affect(Fx).
	
check_continuous_effect() ->
	stack_pool:set_stack_option(self(), play, check_ability_continuous_effect),
	continuous_ability:check_continuous_target().
	
check_next_effect() ->
	stack_pool:pop_stack_out(self()),
	effect_activate_1().
%---------------------------------------------------------------------
check_effect_affect({EffectType, EffectToDo}) ->
	io:format("{~p, ~p} ~n", [EffectType, EffectToDo]),
	case EffectType of
		player_action ->
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			case EffectToDo of
				move_to_remove_zone ->
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), ability_controller),
					remove_zone:move_to_remove_zone(PlayerPid, Target);
				sacrifice ->
					{ok, PlayerPid} = stack_pool:get_last_stack (self(), ability_controller),
					AbilityTarget = skill_card_list:only_on_zone(arena_zone, Target),
					destroy:check_card_destroyed(PlayerPid, AbilityTarget, sacrifice);
				{sacrifice, put_value, {ID, GetValue}} ->	
					{ok, [{TPid, TOrder, TCardID}]} = stack_pool:get_last_stack(self(), target),
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), ability_controller),
					{_, Value} = arena_zone:get_card_power(TPid, TOrder, TCardID, GetValue),
					put(ID, Value),
					destroy:check_card_destroyed(PlayerPid, [{TPid, TOrder, TCardID}], sacrifice);
				{show_opponent_card, CardType} ->
					[AbilityTarget] = Target,
					OpponentPid = mnesia_play:get_opponent_pid(AbilityTarget),
					reveal_controller:activate_player_reveal_hand(AbilityTarget, OpponentPid, CardType);
				show ->
					reveal_controller:activate_consider_card(Target);
				move_to_hand ->
					{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
					move_to_hand:move_card_to_hand(PlayerPid, Target)
			end;
		{check_up_step, X} ->
			{ok, [{CardOwner, CardOrder, CardID}]} = stack_pool:get_last_stack(self(), target),
			case get(checkup_target) of
				[] -> put(checkup_target, [{CardOwner, CardOrder, CardID}]);
				undefined -> put(checkup_target, [{CardOwner, CardOrder, CardID}]);
				_ -> put(checkup_target,get(checkup_target)++[{CardOwner, CardOrder, CardID}])
			end,
		{ok,  Target} = stack_pool:get_last_stack(self(), target),
		{ok, {_CardOwner, _CardOrder, _CardID, AbilityId}} = stack_pool:get_last_stack(self(), card_give_effect),
		AbilityTarget= function_utility:check_card_cancel_ability(Target, {_CardOwner, _CardOrder, _CardID}),
		{ok, Duration} = stack_pool:get_last_stack(self(), duration),
		activate_effect_to_target(AbilityTarget, [{{_CardOwner, _CardOrder, _CardID, AbilityId}, {EffectType, EffectToDo}, Duration}]);
		draw_card ->
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			case EffectToDo of
				DrawAmount ->
					[AbilityTarget|_] = Target,
					draw_card:draw(AbilityTarget, whatever, DrawAmount, 0)
			end;
		draw_seal ->
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			case EffectToDo of
				DrawAmount ->
					[AbilityTarget|_] = Target,
					draw_card:draw(AbilityTarget, is_seal, DrawAmount, 0)
			end;
		draw_mystic ->
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			case EffectToDo of
				DrawAmount ->
					[AbilityTarget|_] = Target,
					draw_card:draw(AbilityTarget, is_not_seal, DrawAmount, 0)
			end;
		curse ->
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			{ok, {CardOwner, CardOrder, CardID, AbilityId}} = stack_pool:get_last_stack(self(), card_give_effect),
			AbilityTarget_ = skill_card_list:only_on_zone(arena_zone, Target),
			AbilityTarget = function_utility:check_card_cancel_ability(AbilityTarget_, {CardOwner, CardOrder, CardID}),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), ability_controller),
			case EffectToDo  of
				charm_curse -> curse_activation:curse_activation_assign(PlayerPid, AbilityTarget, [{{CardOwner, CardOrder, CardID, AbilityId}, [{curse, {charm_curse, CardOwner}}], Duration}], receive_effect);
				_ -> curse_activation:curse_activation_assign(PlayerPid, AbilityTarget, [{{CardOwner, CardOrder, CardID, AbilityId}, [{curse, EffectToDo}], Duration}], receive_effect)
			end;
		{curse_timer, Curse} ->
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			{ok, {CardOwner, CardOrder, CardID, _AbilityId}} = stack_pool:get_last_stack(self(), card_give_effect),
			AbilityT = skill_card_list:only_on_zone(arena_zone, Target),
			AbilityTarget= function_utility:check_card_cancel_ability(AbilityT, {CardOwner, CardOrder, CardID}),
			curse_activation:curse_duration_decrese(AbilityTarget, {curse, Curse}, EffectToDo);
		remove_counter ->
			{ok, [{CardOwner, CardOrder, CardID}]} = stack_pool:get_last_stack(self(), target),
			CardZone = card_utility:check_card_zone (CardOwner, CardOrder, CardID),
			{ok, ReceiveFx} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect, CardZone),
			case EffectToDo of
				all -> 
					CounterRemove = game_info:card_counter({CardZone, {CardOwner, CardOrder, CardID}});
				_ -> 
					CounterRemove = EffectToDo
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
			effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update),
			interfere_step:return_play (check_play_step);
		action ->
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			{ok, {GOwner, GOrder, GID, _AbilityId}} = {ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
			AbilityTarget= function_utility:check_card_cancel_ability(Target, {GOwner, GOrder, GID}),
			% AbilityTarget เป็น List ฉะนั้นต้องแก้ ทุก Function ที่ใช้ AbilityTarget ในรูปที่ไม่ใช่ List ใหม่หมด
			% หรือ 
			case AbilityTarget of
				[] -> interfere_step:return_play(check_play_step);
				_ ->
					case EffectToDo of
						attach_to_s ->
							[{TPid, TOrder, TCardID}] = AbilityTarget,
							%{ok, PlayerPid} = stack_pool:get_last_stack (self(), ability_controller),
							%OnAreList = skill_card_list:only_on_zone(arena_zone, SkillTarget),
							%{ok, {CardOwner, CardOrder, CardID, _AbilityId}} = stack_pool:get_last_stack(self(), card_give_effect),
							stack_pool:set_stack_option(self(), assigned_target, [{GOwner, GOrder, GID}]), 
							move_to_arena:move_mystic_card_to_arena(TPid, TOrder, TCardID);
						atk_to_hand ->
							stack_pool:set_stack_option(self(), play, new_activate_hand_target),
							interfere_step:return_play(check_play_step);
						move_to_arena -> 
							move_to_arena:move_to_arena(AbilityTarget);
						move_to_arena_inactive ->
							[{TOwner, TOrder, TID}] = Target,
							case get(same_card) of
								Target ->
									erase(same_card),
									interfere_step:return_play();
								_ ->
									put(same_card, Target),
									move_to_arena:move_to_arena(AbilityTarget),
									arena_zone:set_seal_inactive(TOwner, TOrder, TID)
							end;
						inactive_seal ->
							[{TOwner, TOrder, TID}] = Target,
							arena_zone:set_seal_inactive(TOwner, TOrder, TID);
							%interfere_step:return_play();
						% sacrifice ->
							% {ok, PlayerPid} = stack_pool:get_last_stack (self(), ability_controller),
							% %OnAreList = skill_card_list:only_on_zone(arena_zone, SkillTarget),
							% destroy:check_card_destroyed(PlayerPid, Target);
						break_combine ->
							{ok, [{TOwner, TOrder, TID}|_]} = stack_pool:get_last_stack(self(), target),
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
							%AbilityT = skill_card_list:only_on_zone(arena_zone, Target),
							card_utility:add_card_status(TOwner, TOrder, TID, break_combine_fx, arena_zone),
							force_break:force_break_combine(PlayerPid, TOwner, TOrder, TID);
						to_df_line ->
							line_change:move_to_line(AbilityTarget, 0);
						to_at_line ->
							line_change:move_to_line(AbilityTarget, 1);
						move_to_hand ->
							{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
							move_to_hand:move_card_to_hand(PlayerPid, AbilityTarget);
						move_to_remove_zone ->
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), ability_controller),
							remove_zone:move_to_remove_zone(PlayerPid, AbilityTarget);
						move_to_deck ->
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
							move_to_library:move_to_library (PlayerPid, AbilityTarget);
						destroy ->
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), ability_controller),
							destroy:check_card_destroyed(PlayerPid, AbilityTarget, destroy_effect);
						discard ->
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), ability_controller),
							shrine_zone:card_to_shrine(PlayerPid, AbilityTarget);
						change_line ->
							line_change:move_to_line(AbilityTarget);
						cast_without_paying_cost ->
							[{TCardOwner, TCardOrder, TCardID}] = AbilityTarget,
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), ability_controller),
							put(paying_cost, no),
							casting_card:check_card_cast (PlayerPid, TCardOwner, TCardOrder, TCardID);
						show ->
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), ability_controller),
							OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
							reveal_controller:activate_player_reveal(AbilityTarget, OpponentPid),
							interfere_step:return_play (check_play_step);
						{FstFx, HowMany1, CheckNext, SndFx, HowMany2} ->
							{ok, PlayerPid} = stack_pool:get_last_stack(self(), ability_controller),
							special_effect:check_effect(PlayerPid, AbilityTarget, {FstFx, HowMany1, CheckNext, SndFx, HowMany2});	
						_ ->
							{ok, Duration} = stack_pool:get_last_stack(self(), duration),
							add_effect_to_all_target(AbilityTarget, CardGive, [{EffectType, EffectToDo}], Duration)
					end
			end;
		move ->
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			{ok, {CardOwner, CardOrder, CardID, AbilityId}} = stack_pool:get_last_stack(self(), card_give_effect),
			AbilityTarget= function_utility:check_card_cancel_ability(Target, {CardOwner, CardOrder, CardID}),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			add_effect_to_all_target(AbilityTarget, {CardOwner, CardOrder, CardID, AbilityId}, [{EffectType, EffectToDo}], Duration);
		protect_attack ->
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			{ok, {CardOwner, CardOrder, CardID, AbilityId}} = stack_pool:get_last_stack(self(), card_give_effect),
			AbilityTarget= function_utility:check_card_cancel_ability(Target, {CardOwner, CardOrder, CardID}),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			add_effect_to_all_target(AbilityTarget, {CardOwner, CardOrder, CardID, AbilityId}, [{EffectType, EffectToDo}], Duration);
		protect_curse ->
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			{ok, {CardOwner, CardOrder, CardID, AbilityId}} = stack_pool:get_last_stack(self(), card_give_effect),
			AbilityTarget= function_utility:check_card_cancel_ability(Target, {CardOwner, CardOrder, CardID}),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			add_effect_to_all_target(AbilityTarget, {CardOwner, CardOrder, CardID, AbilityId}, [{EffectType, EffectToDo}], Duration);
		cancel_curse ->
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			{ok, CardGive} = stack_pool:get_last_stack(self(), card_give_effect),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			curse_activation:heal_target_curse(Target, EffectToDo, ability),
			add_effect_to_all_target(Target, CardGive, [{EffectType, EffectToDo}], Duration);
		loss_cancel_curse -> 
			{ok, Target} = stack_pool:get_last_stack(self(), target),
			{ok, {CardOwner, CardOrder, CardID, AbilityId}} = stack_pool:get_last_stack(self(), card_give_effect),
			AbilityTarget= function_utility:check_card_cancel_ability(Target, {CardOwner, CardOrder, CardID}),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			add_effect_to_all_target(AbilityTarget, {CardOwner, CardOrder, CardID, AbilityId}, [{EffectType, EffectToDo}], Duration);
		combat ->
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			{ok, {CardOwner, CardOrder, CardID, AbilityId}} = stack_pool:get_last_stack(self(), card_give_effect),
			AbilityTarget= function_utility:check_card_cancel_ability(Target, {CardOwner, CardOrder, CardID}),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			add_effect_to_all_target(AbilityTarget, {CardOwner, CardOrder, CardID, AbilityId}, [{EffectType, EffectToDo}], Duration);
		mp ->
			{ok,  [Target|_]} = stack_pool:get_last_stack(self(), target),
			OpponentPid = mnesia_play:get_opponent_pid(Target),
			activate_effect_to_player({EffectType, EffectToDo}, Target, OpponentPid);
		check_condition ->
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			{ok, {CardOwner, CardOrder, CardID, _AbilityId}} = stack_pool:get_last_stack(self(), card_give_effect),
			AbilityTarget= function_utility:check_card_cancel_ability(Target, {CardOwner, CardOrder, CardID}),
			case EffectToDo of
				{Condition, SubFx} ->
					CardMatch = function_utility:card_match_condition(AbilityTarget, Condition),
					case CardMatch of
						[] -> interfere_step:return_play(check_play_step);
						ConTarget -> 
							stack_pool:set_stack_option(self(), target, ConTarget),
							check_effect_affect(SubFx)
					end
			end;
		add_flag ->
			{ok, [{CardOwner, CardOrder, CardID}]} = stack_pool:get_last_stack(self(), target),
			card_utility:add_card_status(CardOwner, CardOrder, CardID, EffectToDo),
			interfere_step:return_play (check_play_step);
		clear_flag ->
			{ok, [{CardOwner, CardOrder, CardID}]} = stack_pool:get_last_stack(self(), target),
			card_utility:remove_card_status(CardOwner, CardOrder, CardID, EffectToDo),
			interfere_step:return_play (check_play_step);
		power_change ->
			case EffectToDo of
				{equal, {value, Type}} ->
					{ok, {CardOwner, _, _, _}} = stack_pool:get_last_stack(self(), card_give_effect),
					case Type of
						at -> Data = 1;
						df -> Data = 2;
						sp -> Data = 3;
						mp -> Data = 4;
						mc -> Data = 5;
						ma -> Data = 6;
						ms -> Data = 7
					end,
					gen_server:cast(self(), {act_seal_power, CardOwner, [16#88, 16#86], Data});
				_ ->
					{ok,  Target} = stack_pool:get_last_stack(self(), target),
					{ok, {CardOwner, CardOrder, CardID, AbilityId}} = stack_pool:get_last_stack(self(), card_give_effect),
					AbilityTarget= function_utility:check_card_cancel_ability(Target, {CardOwner, CardOrder, CardID}),
					{ok, Duration} = stack_pool:get_last_stack(self(), duration),
					activate_effect_to_target(AbilityTarget, [{{CardOwner, CardOrder, CardID, AbilityId}, {EffectType, EffectToDo}, Duration}])
			end;
		_Other ->	
			{ok,  Target} = stack_pool:get_last_stack(self(), target),
			{ok, {CardOwner, CardOrder, CardID, AbilityId}} = stack_pool:get_last_stack(self(), card_give_effect),
			AbilityTarget= function_utility:check_card_cancel_ability(Target, {CardOwner, CardOrder, CardID}),
			{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			activate_effect_to_target(AbilityTarget, [{{CardOwner, CardOrder, CardID, AbilityId}, {EffectType, EffectToDo}, Duration}])
	end.

set_new_power(Value, Data) ->
	case Data of
		1 -> Type = at;
		2 -> Type = df;
		3 -> Type = sp;
		4 -> Type = mp;
		5 -> Type = mc;
		6 -> Type = ma;
		7 -> Type = ms
	end,
	[New_Power] = Value,
	{ok,  Target} = stack_pool:get_last_stack(self(), target),
	{ok, {CardOwner, CardOrder, CardID, AbilityId}} = stack_pool:get_last_stack(self(), card_give_effect),
	AbilityTarget= function_utility:check_card_cancel_ability(Target, {CardOwner, CardOrder, CardID}),
	{ok, Duration} = stack_pool:get_last_stack(self(), duration),
	activate_effect_to_target(AbilityTarget, [{{CardOwner, CardOrder, CardID, AbilityId}, {Type, {equal, New_Power}}, Duration}]).
%-------------------------------------Card Effect Part--------------------------
add_effect_to_all_target(Target, CardGive, Effect, Duration) ->
	lists:foreach(fun({TPid, TCor, TCid}) ->
									add_to_target({TPid, TCor, TCid}, CardGive, Effect, Duration) 
								end, Target),
	interfere_step:return_play(check_play_step).

add_to_target({TPid, TCor, TCid}, CardGive, Effect, Duration) ->
 TargetCardZone = card_utility:check_card_zone(TPid, TCor, TCid),
 card_utility:set_card_option_field(TPid, TCor, TCid, receive_effect, [{CardGive, Effect, Duration}], TargetCardZone).
	
activate_effect_to_target([], _) -> interfere_step:return_play(check_play_step);%check_ability_effect_affect();
activate_effect_to_target([{TPid, TCor, TCid} | Target], [{CardGive, Fx, Duration}]) ->
	TargetCardZone = card_utility:check_card_zone(TPid, TCor, TCid),
	{GOwner, GOrder, GID, _} = CardGive,
	ResFX = effect_value:check_value(GOwner, GOrder, GID, Fx, {TPid, TCor, TCid}),
	card_utility:set_card_option_field(TPid, TCor, TCid, receive_effect, [{CardGive, ResFX, Duration}], TargetCardZone),
	%effect_activate:send_update_activate_effect(TPid, TCor, TCid, ResFX, add),
	effect_activate:send_update_activate_effect(TPid, TCor, TCid, [], update),
	activate_effect_to_target(Target, [{CardGive, Fx, Duration}]).
%------------------------------------- Player Effect Part--------------------------
activate_effect_to_player({EffectType, EffectToDo}, ReceivePid, OpposePid) ->
	case EffectType of
		mp ->
			Value = check_effect_todo(EffectToDo),
			case ReceivePid of
				{PlayerPid, _, _} -> 
					set_effect(PlayerPid, mp_rest, Value),
					{ok, ReceiverMp} = mnesia_play:get_player_data(PlayerPid, mp_rest),
					{ok, OpposeMp} = mnesia_play:get_player_data(OpposePid, mp_rest),
					gen_server:cast(PlayerPid, {send, [16#88, 16#76, 0, ReceiverMp, OpposeMp]}),
					gen_server:cast(OpposePid, {send, [16#88, 16#76, 0, OpposeMp, ReceiverMp]}),
					interfere_step:return_play(check_play_step);
				_ ->
					set_effect(ReceivePid, mp_rest, Value),
					{ok, ReceiverMp} = mnesia_play:get_player_data(ReceivePid, mp_rest),
					{ok, OpposeMp} = mnesia_play:get_player_data(OpposePid, mp_rest),
					gen_server:cast(ReceivePid, {send, [16#88, 16#76, 0, ReceiverMp, OpposeMp]}),
					gen_server:cast(OpposePid, {send, [16#88, 16#76, 0, OpposeMp, ReceiverMp]}),
					interfere_step:return_play(check_play_step)
			end
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
		false -> 
			case EffectToDo of
				{'+', s_current_mc} ->
					{ok,  [{CardOwner, CardOrder, CardID}|_]} = stack_pool:get_last_stack(self(), target),				
					Zone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
					Power = game_info:card_mpcast({Zone, {CardOwner, CardOrder, CardID}});
				_ -> 0
			end
	end.

