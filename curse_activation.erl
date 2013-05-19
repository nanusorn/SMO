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
-module (curse_activation).

-export ([curse_activation_assign/4]).
-export ([curse_activation_assign_1_1/0]).
-export ([curse_activation_assign_2/0]).
-export ([curse_activation_assign_2_1/0]).
-export ([curse_activation_assign_3/0]).
-export ([curse_activation_assign_4/0]).
-export ([curse_activation_assign_5/0]).
-export ([check_remain_curse_effect/2]).
-export ([curse_duration_decrese/3]).
-export ([heal_target_curse/2]).
-export ([heal_target_curse/3]).

% 709.14. ขั้นตอนการทำงานของ Curse
% CardFxCurse = [{CardGive, [{curse, EffectToDo}], Duration}]
curse_activation_assign(PlayerPid, TargetList, CardFxCurse, FxType) ->
	case get_card_not_duplicate_curse(TargetList, CardFxCurse) of
		[] -> interfere_step:return_play(check_play_step);
		_ ->
		% 709.14.1. เมื่อมี Effect ใด ๆ ทำให้เกิด Curse ที่ Seal ใด ให้ถือว่า Seal นั้นเป็น Seal ที่ติด Curse
			stack_pool:push_stack(self(), 0, 0, 0, [{play, play_curse_activation_assign}, {curse_controller, PlayerPid}, {curse_fx, CardFxCurse}, {curse_target, TargetList}, {effect_type, FxType}]),
			set_curse_to_target(TargetList, CardFxCurse),
			interfere_step:return_play(check_play_step)
	end.
	
get_card_not_duplicate_curse(TargetList, [{_CardGive, [{curse, EffectToDo}], _Duration}]) ->
	function_utility:card_match_condition(TargetList, [{curse, {n, EffectToDo}}]).
	
set_curse_to_target(TargetList, CardFxCurse) ->
	lists:foreach(
								fun({PlayerPid, CardOrder, CardID}) -> 
									case CardFxCurse of
										[{GFx,Curse,infinity}] -> CardFxCurse2 = [{GFx,Curse,198}];
										_ -> CardFxCurse2 = CardFxCurse
									end,
									card_utility:set_card_option_field(PlayerPid, CardOrder, CardID, receive_effect, CardFxCurse2),
									CurseFx = get_curse_fx(CardFxCurse2),
									effect_activate:send_update_activate_effect(PlayerPid, CardOrder, CardID, CurseFx, add)
								end, 
							TargetList).
	

get_curse_fx ([]) -> [];
get_curse_fx ([{_, Fx, _} | CardFxCurse]) ->
	get_curse_fx (Fx) ++ get_curse_fx (CardFxCurse);
get_curse_fx ([FxCheck | Fx]) ->
	case FxCheck of
		{curse, _} ->
			[FxCheck] ++ get_curse_fx (Fx);
		_ ->	get_curse_fx (Fx)
	end.
	
% 709.14.1.1 อบิลิตี้ที่เกิดผลเมื่อ Seal ติด Curse ทำงาน
curse_activation_assign_1_1 () ->
	stack_pool:set_stack_option (self(), play, play_curse_activation_assign_1_1),
	% case ability_effect:check_all_ability_affect () of
		% 0 -> interfere_step:return_play (check_play_step);
		% _ -> card_utility:check_card_affect()
	% end.
	continuous_ability:check_continuous_target().

% 709.14.2. Interfere Step
curse_activation_assign_2 () ->
	stack_pool:set_stack_option (self(), play, play_curse_activation_assign_2),
	interfere_step:into_sub_interfere ().

% 709.14.2.1 อบิลิตี้ ยกเลิก Curse ทำงาน
curse_activation_assign_2_1 () ->
	case stack_pool:get_last_stack(self(), curse_fx) of
		%{ok, [{GFx, Fx, Duration}]} -> 
		%{ok, [{{CardOwner, CardID, CardOrder}, _, _} | _]} ->
		{ok, [{{CardOwner, CardOrder, CardID, _}, _, _}| _]} ->
			case card_utility:check_card_zone(CardOwner, CardOrder, CardID) of
				arena_zone ->
					check_ability_cancel_curse();
				shrine_cards ->
					check_ability_cancel_curse();
				_ ->	interfere_step:return_play ()
			end;
		_ ->	interfere_step:return_play ()
	end.

check_ability_cancel_curse() ->
	stack_pool:set_stack_option (self(), play, play_curse_activation_assign_2_1),
	{ok, CFx} = stack_pool:get_last_stack (self(), curse_fx),
	case stack_pool:get_last_stack (self(), curse_target) of
		{ok, TargetList} ->
			check_target_cancel_curse(TargetList, CFx),
			interfere_step:return_play(check_play_step);
		_ ->	io:format ("Cannot find curse target~n")
	end.
	
check_target_cancel_curse(TargetList, CFx) ->
	lists:foreach(
								fun({TOwner, TOrder, TID}) -> 
									CardFx = card_utility:get_all_card_effect(TOwner, TOrder, TID),
									case get_cancel_curse_fx(CardFx) of
										[] ->	
											card_utility:add_card_status(TOwner, TOrder, TID, curse_affect);
										FxCancel ->
											case have_cancel_curse(CFx, FxCancel) of
												no -> card_utility:add_card_status(TOwner, TOrder, TID, curse_affect);					
												yes ->   
													put(card, [{TOwner, TOrder, TID}]),
													put(fx, FxCancel),
													case get(card_list) of
														undefined -> 
															put(card_list, get(card)),
															put(fx_all, get(fx));
														_ -> 
															put(card_list, get(card)++get(card_list)),
															put(fx_all, get(fx)++get(fx_all))
													end
													%curse_activation:heal_target_curse ([{PlayerPid, CardOrder, CardID}], FxCancel),
													%stack_pool:add_stack_option_field(self(), heal_target, [{PlayerPid, CardOrder, CardID}])
													%curse_cancel
											end
											%curse_activation:heal_target_curse ([{PlayerPid, CardOrder, CardID}], FxCancel),
									end
								end, TargetList),
								case get(card_list) of
									undefined -> "";
									CardListGet->
										heal_target_curse(CardListGet, get(fx_all)),
										stack_pool:add_stack_option_field(self(), heal_target, CardListGet),
										erase(card_list),
										erase(fx_all)
								end.
								
get_cancel_curse_fx ([]) -> [];
get_cancel_curse_fx ([{_, Fx, _} | CardFx]) ->
	get_cancel_curse_fx (Fx) ++ get_cancel_curse_fx (CardFx);
get_cancel_curse_fx ([{cancel_curse, CancelCurse} | Fx]) ->
	CancelCurse ++ get_cancel_curse_fx (Fx);
get_cancel_curse_fx ([{_, _} | Fx]) -> get_cancel_curse_fx (Fx).

%have_cancel_curse ([], _) -> no;
have_cancel_curse ([{_GFx, Fx, _Duration}], FxCancel) ->
	case check_cancel_effect(Fx, FxCancel) of
		cancel_curse -> yes;
		not_cancel -> no
	end.

%check_cancel_effect ([], _) -> not_cancel;
check_cancel_effect([{curse, Curse}], FxCancel) ->
	case check_cancel_curse(Curse, FxCancel) of
		cancel_curse -> cancel_curse;
		cancel_none -> not_cancel%check_cancel_effect (Fx, FxCancel)
	end.

check_cancel_curse (_, []) -> cancel_none;
check_cancel_curse (_, [all | _]) -> cancel_curse;
check_cancel_curse (Curse, [Curse | _]) -> cancel_curse;
check_cancel_curse (Curse, [_ | FxCancel]) -> check_cancel_curse (Curse, FxCancel).

% 709.14.3. เริ่มนับจำนวน Turn ใน Phase นี้ หาก Turn กลายเป็น 0 Turn Curse จะหายไปทันที โดยยังไม่ส่งผลใดๆ
curse_activation_assign_3 () ->
	stack_pool:set_stack_option (self(), play, play_curse_activation_assign_3),
	case stack_pool:get_last_stack (self(), curse_target) of
		{ok, Target} ->
			ResTarget =
			case stack_pool:get_last_stack (self(), heal_target) of
				{ok, HealTarget} -> Target -- HealTarget;
				_ -> Target
			end,
			remove_curse_zero_turn(ResTarget),
			stack_pool:set_stack_option(self(), curse_target, ResTarget),
			% case ability_effect:check_all_ability_affect () of
				% 0 -> interfere_step:return_play (check_play_step);
				% _ -> card_utility:check_card_affect()
			% end
			continuous_ability:check_continuous_target();
		_ ->	io:format ("Can not find curse target ~n")
	end.
	
remove_curse_zero_turn(TargetRemove) ->
	lists:foreach(
								fun({PlayerPid, CardOrder, CardID}) -> 
									{ok, RFx} = card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, receive_effect),
									RFxUpdate = remove_curse_zero_turn_from_target (RFx),
									card_utility:update_card_option_field(PlayerPid, CardOrder, CardID, receive_effect, RFxUpdate)
									% {ok, SFX} = card_utility:get_card_option_field (PlayerPid, CardOrder, CardID, skill_effect),	
									% SFxUpdate = remove_curse_zero_turn_from_target (SFX),
									% card_utility:update_card_option_field (PlayerPid, CardOrder, CardID, skill_effect, SFxUpdate)	
								end, TargetRemove).
	

remove_curse_zero_turn_from_target ([]) -> [];
remove_curse_zero_turn_from_target ([{_, _, 0} | CardFx]) ->
	remove_curse_zero_turn_from_target (CardFx);
remove_curse_zero_turn_from_target ([Fx | CardFx]) ->
	[Fx] ++ remove_curse_zero_turn_from_target (CardFx).

% 709.14.4. Effect ที่เกิดจาก curse จะส่งผลทันที -
curse_activation_assign_4() ->
	stack_pool:set_stack_option(self(), play, curse_activation_assign_4),
	case stack_pool:get_last_stack (self(), curse_target) of
		{ok, Target} ->
			case stack_pool:get_last_stack (self(), curse_fx) of
				{ok, CardFxCurse} ->
					CurseFx = get_curse_fx (CardFxCurse),
					check_target_activate_curse(Target, CardFxCurse, CurseFx);
				_ ->	io:format ("Activate curse error can not find curse~n"),
					%check_target_activate_curse(Target, [], [])
					interfere_step:return_play(check_play_step)
			end;
		_ ->	io:format ("Can not find curse target ~n"),
			interfere_step:return_play(check_play_step)
	end.
	
curse_activation_assign_5() ->
	stack_pool:pop_stack_out(self()),
	interfere_step:return_play(check_play_step).
	
	
check_target_activate_curse(CurseTarget, CardFxCurse, CurseFx) ->
	TarRemain = check_remain_target_to_curse(CurseTarget, CardFxCurse, CurseFx),
	case TarRemain of
		[] ->
			lists:foreach(fun({CardOwner, CardOrder, CardID}) -> 
				effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update) end, CurseTarget -- TarRemain),
				interfere_step:return_play(check_play_step);
		_ ->
			lists:foreach(fun({CardOwner, CardOrder, CardID}) -> 
				effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update) end, CurseTarget -- TarRemain),
				activate_curse_effect(TarRemain, CurseFx)
	end.
	
% lists:foreach(
							  % fun({CardOwner, CardOrder, CardID}) -> 
									% case check_remain_curse({CardOwner, CardOrder, CardID}, CardFxCurse, CurseFx) of
										 % {ok, curse_heal} ->
										 	% effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update),
											% interfere_step:return_play(check_play_step);
										% {ok, RemainCurse} ->	
											% stack_pool:get_last_stack(self(), curse_to_activate, RemainCurse),
											% activate_curse_effect(CardOwner, CardOrder, CardID);
											% %effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update);
										% _ ->
											% effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update),
											% interfere_step:return_play(check_play_step)
									% end
								% end, CurseTarget).
	
check_remain_target_to_curse([], _, _) -> [];
check_remain_target_to_curse([{CardOwner, CardOrder, CardID}|Target], CardFxCurse2, CurseFx) ->
	CardFxCurse = 
	case CardFxCurse2 of
		[{GFx, Curse, infinity}] -> [{GFx, Curse, 198}];
		_ -> CardFxCurse2
	end,
	AllFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	LeftOver = AllFx -- CardFxCurse,
	Interest = AllFx -- LeftOver,
	case Interest of
		[] -> check_remain_target_to_curse(Target, CardFxCurse, CurseFx);
		_ -> [{CardOwner, CardOrder, CardID}] ++ check_remain_target_to_curse(Target, CardFxCurse, CurseFx)
	end.
	
% check_remain_curse({CardOwner, CardOrder, CardID}, CardFxCurse2, CurseFx) ->
	% CardFxCurse = 
	% case CardFxCurse2 of
		% [{GFx, Curse, infinity}] -> [{GFx, Curse, 198}];
		% _ -> CardFxCurse2
	% end,
	% AllFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	% LeftOver = AllFx -- CardFxCurse,
	% Interest = AllFx -- LeftOver,
	% case length(Interest) of
		% 0 -> {ok, curse_heal};
		% _ -> 
			% case length(Interest) =:= length(CardFxCurse) of
				% true -> {ok, CurseFx};
				% _ -> 
					% {ok, FxType} = stack_pool:get_last_stack(self(), effect_type),
					% {ok, ReceiveFx} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, FxType),
					% NewReceiveFx = ReceiveFx -- CardFxCurse,
					% card_utility:set_card_option_field(CardOwner, CardOrder, CardID, FxType, NewReceiveFx),
					% RemainCurse = check_remain_curse_effect(card_effect(Interest), CurseFx),
					% {ok, RemainCurse}
			% end
	% end.
	
% card_effect([]) -> [];	
% card_effect([{_,CardFx,_}|Tail]) -> CardFx ++ card_effect(Tail).
	
check_remain_curse_effect(_, []) -> [];
check_remain_curse_effect([], _) -> [];
check_remain_curse_effect([{curse, CurseInflect}|CardFx], [{curse, CurseInflect}|CurseFx]) -> [{curse, CurseInflect}] ++ check_remain_curse_effect(CardFx, CurseFx);
check_remain_curse_effect([{curse, Any}|CardFx], [{curse, CurseInflect}|CurseFx]) -> check_remain_curse_effect([{curse, Any}] ++ CardFx, CurseFx ++[{curse, CurseInflect}]);
check_remain_curse_effect([{_, _}|CardFx], [{curse, CurseInflect}|CurseFx]) -> check_remain_curse_effect(CardFx, [{curse, CurseInflect}|CurseFx]).
	
activate_curse_effect(TargetList, [{curse, Curse}]) ->
	case Curse of
		stone_curse ->
			lists:foreach(fun({CardOwner, CardOrder, CardID}) -> 
				effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update) end, TargetList),
				interfere_step:return_play(check_play_step);
		freeze_curse ->
			lists:foreach(fun({CardOwner, CardOrder, CardID}) ->
				line_change:update_move_to_line(CardOwner, CardOrder, CardID, 0),
				% CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID, arena_zone),
				% case function_utility:is_contain([{change_line, disallow}], CardFx) of
					% [] -> line_change:update_move_to_line(CardOwner, CardOrder, CardID, 0);
					% _ -> do_nothing 
				% end,
				effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update) end, TargetList);
			%interfere_step:return_play(check_play_step);
		{charm_curse, _} ->
			lists:foreach(fun({CardOwner, CardOrder, CardID}) -> 
				effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update) end, TargetList),
				interfere_step:return_play(check_play_step);
		poison_curse ->
			lists:foreach(fun({CardOwner, CardOrder, CardID}) -> 
				effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update) end, TargetList),
				interfere_step:return_play(check_play_step);
		death_curse ->
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), curse_controller),
			
			destroy:check_card_destroyed(PlayerPid, TargetList,death_curse);
		{last_dance_curse, _} ->
			lists:foreach(fun({CardOwner, CardOrder, CardID}) -> 
				effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update) end, TargetList),
				interfere_step:return_play(check_play_step);
		dimension_curse ->
			lists:foreach(fun({CardOwner, CardOrder, CardID}) -> 
				effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update) end, TargetList),
				interfere_step:return_play(check_play_step)
	end.
		
	
% activate_curse_effect (_, _, _, []) -> [];
% activate_curse_effect (PlayerPid, CardOrder, CardID, [{_, Fx, _} | CurseFx]) ->
	% activate_curse_effect (PlayerPid, CardOrder, CardID, Fx),
	% activate_curse_effect (PlayerPid, CardOrder, CardID, CurseFx);
% activate_curse_effect (PlayerPid, CardOrder, CardID, [{curse, Curse} | Fx]) ->
	% case Curse of
		% stone_curse ->
			% activate_stone_curse (PlayerPid, CardOrder, CardID);
		% freeze_curse ->
			% activate_freeze_curse (PlayerPid, CardOrder, CardID);
		% {charm_curse, _} ->
			% activate_charm_curse (PlayerPid, CardOrder, CardID);
		% poison_curse ->
			% activate_poison_curse (PlayerPid, CardOrder, CardID);
		% death_curse ->
			% activate_death_curse (PlayerPid, CardOrder, CardID);
		% {last_dance_curse, _} ->
			% activate_last_dance_curse (PlayerPid, CardOrder, CardID);
		% dimension_curse ->
			% activate_dimension_curse (PlayerPid, CardOrder, CardID)
	% end,
	% activate_curse_effect (PlayerPid, CardOrder, CardID, Fx);
% activate_curse_effect (PlayerPid, CardOrder, CardID, [_ | Fx]) ->
	% activate_curse_effect (PlayerPid, CardOrder, CardID, Fx).

% 709. Curse
% 709.1. Curse นับเป็น Effect อีกประเภทหนึ่ง ที่ส่งผลให้เกิด Effect ตามที่ระบุในชนิดของ Curse
% 709.2. หาก Seal ที่ติด Curse ไม่ได้อยู่ใน Arena Zone Curse นั้นๆก็จะหายไปด้วยทันที -
% 709.3. เมื่อ Seal ติด Curse จะมี Interfere Step ก่อน ที่ Curse จะแสดงผล
% 709.4. หาก Effect จาก Curse นั้นถูกทำลาย หรือ ถูกยกเลิก Effect Curse นั้นจะหมดไปทันที -

% 709.5. Stone Curse
% activate_stone_curse (PlayerPid, CardOrder, CardID) ->
% % 709.5.1. Seal ที่ติด Stone Curse จะไม่สามารถถูกสั่งการได้
% % 709.5.2. Seal ที่ติด Stone Curse ไม่สามารถโจมตี, ใช้ Skill, Combination, Break Combination และ กำหนด Line โดยผู้เล่น
% % 709.5.3. Seal ที่ติด Stone Curse ยังคงตกเป็นเป้าหมายของการโจมตี, Mystic Card, Skill หรือ Ability ตามปกติ -
% % 709.5.4. หาก Seal ที่ติด Stone Curse ถูกโจมตีที่ At Line Seal นั้นยังคงทำการสวนกลับได้ -
% % 709.5.5. Ability ของ Seal ที่ติด Stone Curse ยังคงทำงานตามปกติ -
	% io:format ("Stone curse to ~p~n", [{PlayerPid, CardOrder, CardID}]).
% 
% % 709.6. Freeze Curse
% activate_freeze_curse (PlayerPid, CardOrder, CardID) ->
% % 709.6.1. Seal ที่ติด Freeze Curse จะต้องอยู่ที่ Df Line เท่านั้น เว้นแต่มี Effect ใดๆระบุให้ไม่สามารถ เปลี่ยน Line ได้ -
	% line_change:update_move_to_line(PlayerPid, CardOrder, CardID, 0),
% % 709.6.2. Seal ที่ติด Freeze Curse ไม่สามารถโจมตีได้ -
% % 709.6.3. Seal ที่ติด Freeze Curse ยังคงตกเป็นเป้าหมายของการโจมตี, Mystic Card, Skill หรือ Ability ตามปกติ -
% % 709.6.4. Seal ที่ติด Freeze Curse สามารถรวมร่าง หรือ แยกการรวมร่างได้ตามปกติ
% % 709.6.5. หาก Seal ที่ติด Freeze Curse ถูกโจมตีที่ At Line Seal นั้นยังคงทำการสวนกลับได้
% % 709.6.6. Ability ของ Seal ที่ติด Freeze Curse ยังคงทำงานตามปกติ- 
	% io:format ("Freeze curse to ~p~n", [{PlayerPid, CardOrder, CardID}]).
% 
% % 709.7. Charm Curse
% activate_charm_curse (PlayerPid, CardOrder, CardID) ->
% % 709.7.1. ผู้ควบคุม Seal ที่ใช้ Charm Curse ในขณะที่ใช้ Charm Curse จะเป็นผู้ควบคุม Seal ที่ติด Charm Curse
% % 709.7.2. ผู้ใช้ Curse ไม่สามารถสั่ง Seal ที่ติด Charm Curse รวมร่าง หรือแยกการรวมร่าง
% % 709.7.3. Seal ที่ติด Charm Curse ยังคงตกเป็นเป้าหมายของการโจมตี, Mystic Card, Skill หรือ Ability ตามปกติ-
% % 709.7.4. หาก Seal ที่ติด Charm Curse ถูกโจมตีที่ At Line Seal นั้นยังคงทำการสวนกลับได้ -
	% io:format ("Charm curse to ~p~n", [{PlayerPid, CardOrder, CardID}]).
% 
% % 709.8. Poison Curse
% activate_poison_curse (PlayerPid, CardOrder, CardID) ->
% % 709.8.1. Seal ที่ติด Poison Curse จะถูกทำลาย เมื่อครบจำนวน Turn ที่กำหนด
% % 709.8.2. Seal ที่ติด Poison Curse ยังคงตกเป็นเป้าหมายของการโจมตี, Mystic Card, Skill หรือ Ability ตามปกติ-
% % 709.8.3. หาก Seal ที่ติด Poison Curse ถูกโจมตีที่ At Line Seal นั้นยังคงทำการสวนกลับได้-
% % 709.8.4. Ability ของ Seal ที่ติด Poison Curse ยังคงทำงานตามปกติ-
	% io:format ("Poison curse to ~p~n", [{PlayerPid, CardOrder, CardID}]).
% 
% % 709.9. Death Curse
% activate_death_curse (PlayerPid, CardOrder, CardID) ->
% % 709.9.1. Seal ที่ติด Death Curse เมื่อ Death Curse แสดงผลสำเร็จ Seal นั้นจะถูกทำลายทันที -
	% destroy:check_card_destroyed(PlayerPid, [{PlayerPid, CardOrder, CardID}]).
% 
% % 709.10. Last Dance Curse
% activate_last_dance_curse (PlayerPid, CardOrder, CardID) ->
% % 709.10.1. Seal ที่ติด Last Dance Curse จะได้รับ Effect ตามที่กำหนด
% % 709.10.2. Seal ที่ติด Last Dance Curse ยังคงตกเป็นเป้าหมายของการโจมตี, Mystic Card, Skill หรือ Ability ตามปกติ-
% % 709.10.3. Seal ที่ติด Last Dance Curse จะถูกทำลาย เมื่อครบจำนวน Turn ที่กำหนด
% % 709.10.4. หาก Seal ที่ติด Last Dance Curse ถูกโจมตีที่ At Line Seal นั้นยังคงทำการสวนกลับได้-
% % 709.10.5. Ability ของ Seal ที่ติด Last Dance Curse ยังคงทำงานตามปกติ-
	% io:format ("Last dance curse to ~p~n", [{PlayerPid, CardOrder, CardID}]).
% 
% % 709.11. Dimension Curse
% activate_dimension_curse (PlayerPid, CardOrder, CardID) ->
% % 709.11.1. Seal ที่ติด Dimension Curse จะไม่อยู่ใน Line ใดๆ แต่ยังคงอยู่ในสนาม เว้นแต่มี Effect อื่น ใดกำหนดให้ผิดไปจากนี้-
% % 709.11.2. Seal ที่ติด Dimension Curse จะไม่สามารถถูกสั่งการได้-
% % 709.11.3. Seal ที่ติด Dimension Curse ไม่สามารถโจมตี, ใช้ Skill, Combination, Break Combination และ กำหนด Line โดยผู้เล่น
% % 709.11.4. Seal ที่ติด Dimension Curse จะไม่ตกเป็นเป้าหมายของการโจมตี และจะไม่รับความเสียหายจากการโจมตี-
% % 709.11.5. Seal ที่ติด Dimension Curse ยังคงตกเป็นเป้าหมายของ Mystic Card, Skill หรือ Ability ตามปกติ-
% % 709.11.6. Ability ของ Seal ที่ติด Dimension Curse ยังคงทำงานตามปกติ-
% % 709.11.7. เมื่อ Seal หายจาก Dimension Curse หาก Seal นั้นไม่อยู่ใน Line ใด ๆ ให้ผู้ควบคุม Seal นั้นกำหนด Line ให้กับ Seal ใบนั้น -
% % ถ้า Seal นั้นอยู่ใน Line ใด ๆ ก่อนแล้วไม่ต้องทำการกำหนด Line ใหม่-
	% io:format ("Dimension curse to ~p~n", [{PlayerPid, CardOrder, CardID}]).

% 709.12. Seal ที่ติด Curse ชนิดใดอยู่จะไม่สามารถตกเป็นเป้าหมายของ Curse ชนิดนั้น-
% 709.13. หากมีการรักษาหรือยกเลิก Curse ให้ Seal ที่ติด Curse หลายชนิดพร้อมกัน ให้ Curse ทั้งหมดหยุดทำงานพร้อมกัน

curse_duration_decrese(Target, FxType, DecreseAmount) ->
	lists:foreach(
								fun({CardOwner, CardOrder, CardID}) ->
									{ok, ReceiveFx} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect),
									RemainRecFx = duration_decrese({CardOwner, CardOrder, CardID}, FxType, ReceiveFx, DecreseAmount),
									IsSealDestroyed = check_seal_destroyed(RemainRecFx),
									case IsSealDestroyed of
										1 -> stack_pool:add_stack_option_field(self(), curse_duration_decrease_destroy, [{CardOwner, CardOrder, CardID}]);
										_ -> 
											card_utility:update_card_option_field(CardOwner, CardOrder, CardID, receive_effect, RemainRecFx),
											%{ok, SkillFx} = card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, skill_effect),
											%RemainSkillFx = duration_decrese({PlayerPid, CardOrder, CardID}, FxType, SkillFx, DecreseAmount),
											% case RemainSkillFx of
												% destroy -> stack_pool:add_stack_option_field(self(), curse_duration_decrease_destroy, [{PlayerPid, CardOrder, CardID}]);
												% _ -> 
													% card_utility:update_card_option_field(PlayerPid, CardOrder, CardID, skill_effect, RemainSkillFx),
													%effect_activate:send_update_activate_effect(PlayerPid, CardOrder, CardID, RemainRecFx, remove)
											case FxType of
												{curse,y} ->
													CurseContain = function_utility:all_curse_from_target([{CardOwner, CardOrder, CardID}]),
													update_contain_curse(CardOwner, CardOrder, CardID, CurseContain);
												_ ->	effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [FxType], add)
											end
									end
								end, Target),
	case stack_pool:get_last_stack(self(), curse_duration_decrease_destroy) of
		{ok, Destroy} -> 
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
			destroy:check_card_destroyed(PlayerPid, Destroy, curse_duration_decrease_destroy);
		_ -> interfere_step:return_play(check_play_step)
	end.
	
duration_decrese({_PlayerPid, _CardOrder, _CardID}, _FxType, [], _DecreseAmount) -> [];
duration_decrese({PlayerPid, CardOrder, CardID}, FxType, [{GFX, CardFx, Duration}|Fx], DecreseAmount) ->
case FxType of
   {curse, y} ->
   	Curse_All = [{curse, poison_curse}, {curse, last_dance_curse}, {curse, freeze_curse}, {curse, dimension_curse}, {curse, charm_curse}],
		check_all_curse	(Curse_All, {PlayerPid, CardOrder, CardID}, FxType, [{GFX, CardFx, Duration}|Fx], DecreseAmount);
   _ -> 
	% check ก่อนว่า Card นี้มี FxType ที่สนใจหรือไม่
	case function_utility:is_contain([FxType], [{GFX, CardFx, Duration}]) of
		[FxType] ->
			% ถ้ามี FxType นั้น เช็คก่อนว่ามี Duration เป็น 198 หรือไม่ (198 = infinity) ถ้าใช่ ไม่ต้องลดจำนวน turn
			case Duration =:= 198 of
				true ->	io:format ("~p has infinity turn~n", [FxType]),
					[{GFX, CardFx, Duration}] ++ duration_decrese({PlayerPid, CardOrder, CardID}, FxType, Fx, DecreseAmount);
				false ->
					% ถ้ามี Duration ของ FxType ที่มี เมื่อคำนวณแล้ว เหลือน้อยกว่า 0 หรือไม่
					DuraRemain = Duration + DecreseAmount,
					case DuraRemain =< 0 of
						% ถ้าน้อยกว่าหรือเท่ากับ 0
						true ->
							% เช็คว่ามี FxType เป็น Effect ที่ืทำให้ การ์ด ตก Shrine เมื่อหมด Duration หรือไม่  
							case FxType of
								{curse, poison_curse} -> [{destroy,0,0}];
								{curse, last_dance_curse} -> [{destroy,0,0}];
								_ -> 
									effect_activate:send_update_activate_effect(PlayerPid, CardOrder, CardID, [{GFX, CardFx}], remove),
									[] ++ duration_decrese({PlayerPid, CardOrder, CardID}, FxType, Fx, DecreseAmount)
							end;
						false ->
							[{GFX, CardFx, DuraRemain}] ++ duration_decrese({PlayerPid, CardOrder, CardID}, FxType, Fx, DecreseAmount)
							% {RemainFx, FxRemove} =
							% case FxType of
								% {curse, Curse} -> check_curse_effect({curse, Curse}, CardFx, {[], []})
								% % skill_effect ->
								% % mystic_effect ->
								% % ability_effect ->
							% end,
							% %smo_logger:fmsg("Remain effect ~p and Remove effect ~p~n", [RemainFx, FxRemove]),
							% % Remark ต้่องไม่ใช้ Remove
							% %effect_activate:send_update_activate_effect(PlayerPid, CardOrder, CardID, FxRemove, remove),
							% case RemainFx of
								% [] -> [] ++ duration_decrese({PlayerPid, CardOrder, CardID}, FxType, Fx, DecreseAmount);
								% RemainFx -> [{GFX, RemainFx, DuraRemain}] ++ duration_decrese({PlayerPid, CardOrder, CardID}, FxType, Fx, DecreseAmount)
							% end
					end
			end;
		_ -> [{GFX, CardFx, Duration}] ++ duration_decrese({PlayerPid, CardOrder, CardID}, FxType, Fx, DecreseAmount)
	end
end.

check_all_curse ([], {PlayerPid, CardOrder, CardID}, FxType, [{GFX, CardFx, Duration}|Fx], DecreseAmount) -> [{GFX, CardFx, Duration}] ++ duration_decrese({PlayerPid, CardOrder, CardID}, FxType, Fx, DecreseAmount);
check_all_curse ([Curse_Type|Tail], {PlayerPid, CardOrder, CardID}, FxType, [{GFX, CardFx, Duration}|Fx], DecreseAmount) ->
	case function_utility:is_contain([Curse_Type], [{GFX, CardFx, Duration}]) of
		[Curse_Type] ->
			case Duration =:= 198 of
				true ->	io:format ("~p has infinity turn~n", [Curse_Type]),
					[{GFX, CardFx, Duration}] ++ duration_decrese({PlayerPid, CardOrder, CardID}, FxType, Fx, DecreseAmount);
				false ->
					DuraRemain = Duration + DecreseAmount,
					case DuraRemain =< 0 of
						true ->
							case Curse_Type of
								{curse, poison_curse} -> [{destroy,0,0}];
								{curse, last_dance_curse} -> [{destroy,0,0}];
								_ -> 
									effect_activate:send_update_activate_effect(PlayerPid, CardOrder, CardID, [{GFX, CardFx}], remove),
									[] ++ duration_decrese({PlayerPid, CardOrder, CardID}, FxType, Fx, DecreseAmount)
							end;
						false ->
							[{GFX, CardFx, DuraRemain}] ++ duration_decrese({PlayerPid, CardOrder, CardID}, FxType, Fx, DecreseAmount)
					end
			end;
		_ ->
			check_all_curse (Tail, {PlayerPid, CardOrder, CardID}, FxType, [{GFX, CardFx, Duration}|Fx], DecreseAmount)
	end.
					
update_contain_curse(_, _, _,[])->[];
update_contain_curse(PlayerPid, CardOrder, CardID,[Curse|T])->
	effect_activate:send_update_activate_effect(PlayerPid, CardOrder, CardID, [{curse,Curse}], add),
	update_contain_curse(PlayerPid, CardOrder, CardID,T).
	
check_seal_destroyed([Fx|T]) ->
	case Fx of 
		{destroy,0,0} -> 1;
		_ -> check_seal_destroyed(T)
	end;
check_seal_destroyed([]) -> [].
% ------------------ Heal Curse -------------------
heal_target_curse([], _) ->
	case stack_pool:get_last_stack(self(), destroyed) of
		{ok, []} ->
			interfere_step:return_play(check_play_step);
		{error, _} ->
			interfere_step:return_play(check_play_step);
		{ok, [{CardOwner, CardOrder, CardID} | Cards]} ->
			destroy:check_card_destroyed(CardOwner, [{CardOwner, CardOrder, CardID}] ++ Cards, heal_destroy)
	end;
heal_target_curse([{PlayerPid, CardOrder, CardID} | Targets], Curse) ->
	case lists:flatlength(Curse) of
		1 ->
			Zone = card_utility:check_card_zone(PlayerPid, CardOrder, CardID),
			heal_curse(PlayerPid, CardOrder, CardID, Curse, Zone),
			heal_target_curse(Targets, Curse);
		_ ->
			[_Curse|Tail] = Curse,
			Zone = card_utility:check_card_zone(PlayerPid, CardOrder, CardID),
			heal_curse(PlayerPid, CardOrder, CardID, [_Curse], Zone),
			heal_target_curse(Targets, Tail)
	end.

heal_curse(CardOwner, CardOrder, CardID, [], Zone) -> 
	{ok, RFX} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect, Zone),
	%{ok, SFx} = card_utility:get_card_option_field (CardOwner, CardOrder, CardID, skill_effect, Zone),
	URFx = remove_heal_effect(RFX),
	%USFx = remove_heal_effect (SFx),
	card_utility:update_card_option_field(CardOwner, CardOrder, CardID, receive_effect, URFx, Zone);
	%card_utility:update_card_option_field (CardOwner, CardOrder, CardID, skill_effect, USFx, Zone);
heal_curse(CardOwner, CardOrder, CardID, [HCurse | Curse], Zone) ->
	%smo_logger:fmsg("heal curse ~p~n", [HCurse]),
	{ok, RFX} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, receive_effect, Zone),
	%{ok, SFx} = card_utility:get_card_option_field (CardOwner, CardOrder, CardID, skill_effect, Zone),
	% URFX คือ Effect ที่เหลือ นอกจาก curse ที่ ถูก Heal 
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
	% Remove Effect ที่ Heal ออก จาก Card ที่ Client
	case stack_pool:get_last_stack(self(), remove_effect_data) of
		{ok, []} -> stack_pool:remove_stack_option(self(), remove_effect_data);
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
	
remove_curse_effect ([], _) -> [];
remove_curse_effect ([{GFx, Fx, Duration} | CardFx], HCurse) ->
	case remove_effect (GFx, Fx, {curse, HCurse}) of
		[] -> remove_curse_effect (CardFx, HCurse);
		UFx -> [{GFx, UFx, Duration}] ++ remove_curse_effect (CardFx, HCurse)
	end.

remove_effect(_, [], _) -> [];
remove_effect({CardOwner, CardOrder, CardID, AbilityID}, [CurseFx | Fx], CurseFx) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			stack_pool:add_stack_option_field(self(), remove_effect_data, [{{CardOwner, CardOrder, CardID, AbilityID}, [CurseFx]}]);
		is_not_seal ->
			% {ok, [{SPid, SOrder, Sid}]} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, paste_to, arena_zone),
			% stack_pool:add_stack_option_field(self(), destroyed, [{CardOwner, CardOrder, CardID}]),
			% end_of_subturn:remove_mystic_and_effect(CardOwner, CardOrder, CardID, SPid, SOrder, Sid)
			stack_pool:add_stack_option_field(self(), remove_effect_data, [{{CardOwner, CardOrder, CardID, AbilityID}, [CurseFx]}]),
			case  card_utility:get_card_option_field(CardOwner, CardOrder, CardID, paste_to, arena_zone) of
				{ok, [{SPid, SOrder, Sid}]} ->
					stack_pool:add_stack_option_field(self(), destroyed, [{CardOwner, CardOrder, CardID}]);
				_ ->
					{ok, [{_, [{SPid, SOrder, Sid}], _}]} = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, give_effect, arena_zone)
			end,
					end_of_subturn:remove_mystic_and_effect(CardOwner, CardOrder, CardID, SPid, SOrder, Sid)
	end,
	remove_effect({CardOwner, CardOrder, CardID, AbilityID}, Fx, CurseFx);
remove_effect(GFx, [Any | Fx], CurseFx) ->
	[Any] ++ remove_effect(GFx, Fx, CurseFx).

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
	
remove_all_prefix_effect(_, [], _) -> [];
remove_all_prefix_effect({CardOwner, CardOrder, CardID, AbilitId}, [{Prefix, Value} | Fx], Prefix) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal ->
			stack_pool:add_stack_option_field(self(), remove_effect_data, [{{CardOwner, CardOrder, CardID, AbilitId}, [{Prefix, Value}]}]);
		is_not_seal ->
			stack_pool:add_stack_option_field(self(), remove_effect_data, [{{CardOwner, CardOrder, CardID, AbilitId}, [{Prefix, Value}]}]),
			ReceiveFx = mystic_effect:mystic_give_effect_to(CardOwner, CardOrder, CardID),
			lists:foreach(fun({SPid, SOrder, Sid}) -> end_of_subturn:remove_mystic_and_effect(CardOwner, CardOrder, CardID, SPid, SOrder, Sid) end, ReceiveFx),
			stack_pool:add_stack_option_field(self(), destroyed, [{CardOwner, CardOrder, CardID}])
	end,
	remove_all_prefix_effect({CardOwner, CardOrder, CardID, AbilitId}, Fx, Prefix);
remove_all_prefix_effect(GFx, [Any | Fx], Prefix) ->
	[Any] ++ remove_all_prefix_effect(GFx, Fx, Prefix).

% check_curse_effect(_, [], {Fx, CurseRemove}) -> {Fx, CurseRemove};
% check_curse_effect({curse, y}, [{curse, Curse}|CardFx], {Fx, CurseRemove}) -> 
	% check_curse_effect({curse, y}, CardFx, {Fx, CurseRemove ++ [{curse, Curse}]});
% check_curse_effect({curse, y}, [OtherFx|CardFx], {Fx, CurseRemove}) ->
	% check_curse_effect({curse, y}, CardFx, {Fx ++ [OtherFx], CurseRemove});
% check_curse_effect({curse, Curse}, [{curse, Curse}|CardFx], {Fx, CurseRemove}) ->
	% check_curse_effect({curse, Curse}, CardFx, {Fx, CurseRemove ++ [{curse, Curse}]});
% check_curse_effect({curse, Curse}, [OtherFx|CardFx], {Fx, CurseRemove}) -> 
	% check_curse_effect({curse, Curse}, CardFx, {Fx ++ [OtherFx], CurseRemove}).
	

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