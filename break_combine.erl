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
-module (break_combine).

-import (lists, [append/2, flatlength/1]).

-export ([assign_break_combine/4, break_combine_resume/3]).
-export ([separate_seal/3, set_break_combine_complete/0, break_combine_success/3]).
-export ([return_play_from_break_combine/0]).
-export([
						check_break_combine_ability/0,
						check_break_combine_success_ability/0,
						into_sub_interfere/0,
						break_combine_ability_verify/0,
						break_combine_success_ability_verify/0,
						activate_break_combine_effect/0,
						activate_break_combine_success_effect/0
					]).

reject_break_combination (PlayerPid, Reason) ->
	io:format("Reason break combine reject ~p~n", [Reason]),
	case Reason of
		no_combine_state -> gen_server:cast(self(), {reject_break_combine, PlayerPid, 0});
		seal_inactive -> gen_server:cast(self(), {reject_break_combine, PlayerPid, 1});
		curse_effect -> gen_server:cast(self(), {reject_break_combine, PlayerPid, 2});
		break_combined -> gen_server:cast(self(), {reject_break_combine, PlayerPid, 3});
		not_seal_card -> gen_server:cast(self(), {reject_break_combine, PlayerPid, 4});
		combined -> gen_server:cast(self(), {reject_break_combine, PlayerPid, 5});
		other_reason -> gen_server:cast(self(), {reject_break_combine, PlayerPid, 6});
		cannot_break_combine_interfere -> gen_server:cast(self(), {reject_break_combine, PlayerPid, 7});
		controller_not_allow -> gen_server:cast(self(), {reject_break_combine, PlayerPid, 8});
		card_not_on_arena -> gen_server:cast(self(), {reject_break_combine, PlayerPid, 9});
		_ -> io:format("Reject break combine - Reason ~p out of range~n", [Reason])
	end.

%% 513.4. เงื่อนไขในการ Break Combination
%% 513.4.1. Seal ที่สั่ง Break Combination ต้องเป็น Seal ที่อยู่ในสภาพ Combined และเป็น Active Seal และ ไม่มีการสั่งการอื่นค้างอยู่ นอกจากจะมี Effect ใดๆระบุให้สามารถทำได้ --
%% 513.4.2. ผู้เล่นสามารถสั่ง Break Combination ได้เฉพาะ Seal ที่เป็นเจ้าของเท่านั้นนอกจากมี Effect ใดๆระบุให้สามารถทำได้ --
%% 513.4.3. ผู้เล่นที่สั่ง Seal Combination แล้ว จะไม่สามารถสั่ง Break Combination ใน Subturn เดียวกันได้นอกจากมี Effect ใดๆระบุให้สามารถทำได้ --
%% 513.4.4. ผู้เล่นสามารถสั่ง Seal แต่ละใบ Break Combination ได้ 1 ครั้งใน 1 Subturn นอกจากมี Effectใดๆระบุให้สามารถทำได้ --
%% 513.4.5. ไม่มี Effect ใดๆที่ห้ามไม่ให้สั่ง Break Combination
check_condition (CardOwner, CardOrder, CardID) ->
	CardFx = card_utility:get_all_card_effect (CardOwner, CardOrder, CardID, arena_zone),
	case curse:check_curse_status (CardFx, break_combine) of
		{ok, allow} ->
			check_be_combine_condition (CardOwner, CardOrder, CardID);
		{ok, disallow} ->
			{disallow, curse_effect}
	end.

check_be_combine_condition (CardOwner, CardOrder, CardID) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			case card_utility:check_card_status (CardOwner, CardOrder, CardID, be_combine, arena_zone) of
				{ok, have_status} ->
					check_combined (CardOwner, CardOrder, CardID);
				{ok, have_no_status} ->
					{disallow, no_combine_state};
				_ ->	{disallow, other_reason}
			end;
		is_not_seal ->
			{disallow, not_seal_card}
	end.

check_combined (CardOwner, CardOrder, CardID) ->
	case card_utility:check_card_status (CardOwner, CardOrder, CardID, break_combine_fx, arena_zone) of
		{ok, have_status} ->
			card_utility:remove_card_status (CardOwner, CardOrder, CardID, break_combine_fx, arena_zone),
			{allow};
		{ok, have_no_status} ->
			io:format ("no break_combine_fx ~n"),
			case card_utility:check_card_status (CardOwner, CardOrder, CardID, combined, arena_zone) of
				{ok, have_status} ->
					{disallow, combined};
				{ok, have_no_status} ->
					{allow}
			end
	end.

%% 514. ขั้นตอนการสั่ง Break Combination
%% 514.1. ประกาศ Seal ที่จะ Break Combination Seal ตรวจสอบเงื่อนไขการ Break Combination ถ้าเงื่อนไขไม่ครบ
%% ให้ นำ Seal ออกจากการประกาศทำ Break Combination จะเรียกว่าเป็นการสั่ง Seal Break Combination ใน Phase นี้ --
assign_break_combine(PlayerPid, CardOwner, CardOrder, CardID) ->
	OppPid = mnesia_play:get_opponent_pid(CardOwner),
	{ControllerPid, _UncontPid, _} = attribute_check:check_controller({CardOwner, CardOrder, CardID}, OppPid, controller),
	% ตรวจสอบผู้ควบคุมการ์ดใบนั้นๆ ว่าผู้ควบคุมเป็นคนเดียวกับเจ้าของการ์ดหรือไม่
	case ControllerPid of
		PlayerPid ->
			check_break_combination_condition(PlayerPid, CardOwner, CardOrder, CardID);
		_ ->
			% case PlayerPid of
				% CardOwner ->
					reject_break_combination(PlayerPid, controller_not_allow)
				% _ ->	check_break_combination_condition(PlayerPid, CardOwner, CardOrder, CardID)
			% end
	end.

check_break_combination_condition(PlayerPid, CardOwner, CardOrder, CardID) ->
	case stack_pool:get_last_stack(self(), play) of
		{ok, skill_effect_affect} ->
			check_break_activate(PlayerPid, CardOwner, CardOrder, CardID);
		{ok, _} ->
			case card_utility:check_card_status(CardOwner, CardOrder, CardID, break_combine_fx, arena_zone) of
				{ok, have_status} ->
					check_break_activate(PlayerPid, CardOwner, CardOrder, CardID);
				{ok, have_no_status} ->
					reject_break_combination(PlayerPid, cannot_break_combine_interfere)
			end;
		_ ->	check_break_activate(PlayerPid, CardOwner, CardOrder, CardID)
	end.

check_break_activate(PlayerPid, CardOwner, CardOrder, CardID) ->
	case card_utility:check_card_active(CardOwner, CardOrder, CardID) of
		active_seal ->
			check_seal_condition(PlayerPid, CardOwner, CardOrder, CardID);
		inactive_seal ->
			reject_break_combination(PlayerPid, seal_inactive)
	end.

check_seal_condition (PlayerPid, CardOwner, CardOrder, CardID) ->
	case card_utility:check_card_zone (CardOwner, CardOrder, CardID) of
		arena_zone ->
			case check_condition (CardOwner, CardOrder, CardID) of
				{allow} ->
					check_break_combine_step (PlayerPid, CardOwner, CardOrder, CardID);
				{disallow, Reason} ->
					reject_break_combination (PlayerPid, Reason)
			end;
		_ ->	reject_break_combination (PlayerPid, card_not_on_arena)
	end.

check_break_combine_step(PlayerPid, CardOwner, CardOrder, CardID) ->
	%% 514.2. ถ้าต้องจ่าย Cost ในการ Break Combination ตรวจสอบ Cost ถ้าไม่เพียงพอให้นำ Seal ออกจากการประกาศ Break Combination
	%% 514.3. จ่าย Cost สำหรับการ Break Combination ทั้งหมด เรียก Seal ที่ถูกประกาศว่า Seal ที่กำลัง แยกการรวมร่าง	
	card_utility:add_card_status(CardOwner, CardOrder, CardID, breaking_combine, arena_zone),
	%% Ability ที่จะทำงาน เมื่อ Seal แยกการรวมร่างหรือแยกการรวมร่างสำเร็จ ถ้ามีการเลือกการทำงานของ Ability หรือเลือกเป้าหมายให้เลือกใน Phase นี้ --
	stack_pool:push_stack(self(), CardOwner, CardOrder, CardID, [{play, breaking_combine}, {card_player, PlayerPid}]),
	%BreakTarget = ability_utility:get_update_data(PlayerPid, [{CardOwner, CardOrder, CardID}]),
	%self() ! {update_break_combine, [16#88] ++ [16#60] ++ BreakTarget}.
	interfere_step:return_play(check_play_step).
	
check_break_combine_ability() ->
	stack_pool:set_stack_option(self(), play, check_break_combine_ability),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_activate:check_any_ability_activate(breaking_combine, PlayerPid).
	
check_break_combine_success_ability() ->
	stack_pool:set_stack_option(self(), play, check_break_combine_success_ability),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_activate:check_any_ability_activate(break_combine_success, PlayerPid).
	
% update_break_combine(CardOwner, CardOrder, CardID) ->
	% {ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	% stack_pool:set_stack_option(self(), play, update_break_combine),
	% BreakTarget = ability_utility:get_update_data(PlayerPid, [{CardOwner, CardOrder, CardID}]),
	% gen_server:cast(self(), {update_break_combine, [16#88] ++ [16#60] ++ BreakTarget}).
	
%% 514.4. Interfere Step
into_sub_interfere() ->
	stack_pool:set_stack_option(self(), play, get_into_break_combina_sub_interfere),
	interfere_step:into_sub_interfere().

%% 514.5. หาก Ability ไม่สามารถส่งผลกับเป้าหมายที่กำหนด หรือ เป้าหมายที่กำหนดไม่อยู่ในสภาพตกเป็นเป้าหมายได้ --
%% ให้กลับไปเลือกเป้าหมายของ Abilityใน phase 514.3 ใหม่จนกว่าจะไม่มีเป้าหมายที่ Ability สามารถส่งผล หรือ กำหนดได้ --
break_combine_ability_verify() ->
	stack_pool:set_stack_option(self(), play, break_combine_ability_verify),
	mod_ability_activate:verify_ability_condition(breaking_combine).
	
break_combine_success_ability_verify() ->
	stack_pool:set_stack_option(self(), play, break_combine_success_ability_verify),
	mod_ability_activate:verify_ability_condition(break_combine_success).

break_combine_resume(CardOwner, CardOrder, CardID) ->
% 514.6. ตรวจสอบเงื่อนไขการ Break Combination ถ้าเงื่อนไขไม่ครบให้ Seal ที่กำลังแยกการรวมร่างสูญเสียสภาพ
% Seal ที่กำลังแยกการรวมร่างและนำ Seal ออกจากการ Break Combination
	stack_pool:set_stack_option (self(), play, check_break_combine_seal),
	case card_utility:check_card_active (CardOwner, CardOrder, CardID) of
		active_seal ->
			check_curse_condition (CardOwner, CardOrder, CardID);
		inactive_seal ->
			activate_reject_break_combine (CardOwner, CardOrder, CardID, seal_inactive)
	end.

check_curse_condition (CardOwner, CardOrder, CardID) ->
	CardFx = card_utility:get_all_card_effect (CardOwner, CardOrder, CardID),
	case curse:check_curse_status (CardFx, break_combine) of
		{ok, allow} ->
			interfere_step:return_play (check_play_step);
		{ok, disallow} ->
			activate_reject_break_combine (CardOwner, CardOrder, CardID, curse_effect)
	end.

activate_reject_break_combine (CardOwner, CardOrder, CardID, Reason) ->
	card_utility:remove_card_status (CardOwner, CardOrder, CardID, breaking_combine, arena_zone),
	card_utility:add_card_status (CardOwner, CardOrder, CardID, break_combined, arena_zone),
	{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	stack_pool:pop_stack_out (self()),
	reject_break_combination (PlayerPid, Reason).

get_mystic_data([{_,_,356}|_]) -> {already};
get_mystic_data([_|T]) -> get_mystic_data(T);
get_mystic_data([]) -> {no_combine_mystic}.

%% 514.7. การ Break Combination เสร็จสมบูรณ์ นำ Support Seal มาไว้ข้าง Main Seal
separate_seal(CardOwner, CardOrder, CardID) -> 
	{ok, ArenaReply, MysticCheck} = arena_zone:break_support_seal(CardOwner, CardOrder, CardID),
	case MysticCheck of
		[] -> 
		%Zone = card_utility:check_card_zone (CardOwner, CardOrder, CardID),
		%case card_utility:check_card_status (CardOwner, CardOrder, CardID, combine_with_effect, Zone) of
		%	{ok, have_status} ->
			case get_mystic_data(arena_zone:get_mystic_pasted(CardOwner, CardOrder, CardID)) of
				{already} ->
					MysticSupport = get(support_target),
					Value = lists:keysearch({CardOwner, CardOrder, CardID}, 1, MysticSupport),
					smo_logger:fmsg("mystic support check to card  ~p ~n", [Value]),
						case Value of
							{value, {{CardOwner, CardOrder, CardID}, OptionNo}} ->
							{ok, PowerChange} =  mnesia_odbc:get_power_change_data(CardID, OptionNo),
							OptionChange = [{combine_support, {{CardOwner, CardOrder, CardID}, {}}}, {combine_power, PowerChange}],
							ArenaCardOption = arena_zone:get_card_option(CardOwner, CardOrder, CardID),
							CardOption =
								case ArenaCardOption of
									{ok, TCardOption} -> TCardOption;
									_ -> []
								end,
						mystic_effect:update_option_effect(CardOwner, CardOrder, CardID, CardOption, OptionChange);
					_ ->""
						end;
				_-> ""
			end;
		[{MOnwerPid, MCardOrder, MCardID}|_] -> 
			MysticSupport = get(support_target),
			Value = lists:keysearch({CardOwner, CardOrder, CardID}, 1, MysticSupport),
			smo_logger:fmsg("mystic support check to card  ~p ~n", [Value]),
			case Value of
				{value, {{CardOwner, CardOrder, CardID}, OptionNo}} ->
					{ok, PowerChange} =  mnesia_odbc:get_power_change_data(CardID, OptionNo),
					OptionChange = [{combine_support, {{MOnwerPid, MCardOrder, MCardID}, {}}}, {combine_option, [OptionNo]}, {combine_power, PowerChange}],
					ArenaCardOption = arena_zone:get_card_option(CardOwner, CardOrder, CardID),
					CardOption =
					case ArenaCardOption of
						{ok, TCardOption} -> TCardOption;
						_ -> []
					end,
					mystic_effect:update_option_effect(CardOwner, CardOrder, CardID, CardOption, OptionChange);
				_ ->""
			end
	end,

	%% 514.8. ตรวจสอบว่า Ability นั้นถูกต้องตามเงื่อนไขในการทำงานหรือไม่ ถ้าเงื่อนไขไม่ครบ Ability นั้นจะไม่ทำงาน
	%% หรือ หากไม่มีเป้าหมายที่ Ability สามารถส่งผล หรือ กำหนดได้ Ability นั้นจะไม่ทำงาน ถ้าเงื่อนไขในการเกิด Ability
	%% ยังคงถูกต้องและมีเป้าหมายที่ถูกต้อง Ability เมื่อ Seal แยกการรวมร่างหรือ เมื่อ Seal แยกการรวมร่างสำเร็จ จะทำงาน ใน Phase นี้ --
	ArenaSize = flatlength(ArenaReply),
	ArenaDataReply = get_data_reply (ArenaReply),
	%{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	stack_pool:set_stack_option(self(), play, separate_seal),
	gen_server:cast(self(), {update_arena_cards, CardOwner, [ArenaSize] ++ ArenaDataReply}).

get_data_reply ([]) -> [];
get_data_reply ([{{CardOwner, CardOrder, CardID}, CardOption} | Cards]) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			{ok, Line} = seal_card:get_seal_option (CardOption, line),
			stack_pool:add_stack_option_field (self(), break_combine_to_arena_cards, [{CardOwner, CardOrder, CardID}]),
			[CardOrder, <<CardID:16>>, Line] ++ get_data_reply (Cards);
		is_not_seal ->
			get_data_reply (Cards)
	end.
	
activate_break_combine_effect() ->
	stack_pool:set_stack_option(self(), play, activate_break_combine_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(breaking_combine, PlayerPid).
	
activate_break_combine_success_effect() ->
	stack_pool:set_stack_option(self(), play, activate_break_combine_success_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(break_combine_success, PlayerPid).

set_break_combine_complete() ->
	stack_pool:set_stack_option(self(), play, play_set_break_combine_complete),
	% case ability_effect:check_all_ability_affect () of
		% 0 -> interfere_step:return_play (check_play_step);
		% _ -> card_utility:check_card_affect()
	% end.
	continuous_ability:check_continuous_target().

break_combine_success (CardOwner, CardOrder, CardID) ->
	%% 514.9. จบขั้นตอนการ Break Combination Seal ที่กำลังแยกการรวมร่าง สูญเสียสภาพ Seal ที่กำลังแยกการรวมร่าง
	%% และนำ Seal ที่ประกาศ Break Combination ออกจากการประกาศ Break Combination
	stack_pool:set_stack_option (self(), play, play_break_combine_success),
	card_utility:add_card_status (CardOwner, CardOrder, CardID, break_combined, arena_zone),
	card_utility:remove_card_status (CardOwner, CardOrder, CardID, breaking_combine, arena_zone),
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, be_combine, arena_zone),
	%continuous_ability:check_continuous_target().
	interfere_step:return_play(check_play_step).

return_play_from_break_combine () ->
	case stack_pool:get_last_stack (self(), break_combine_to_arena_cards) of
		{ok, []} -> no_card_update;
		{ok, Cards} ->
			update_card_effect_to_client (Cards)
	end,
	stack_pool:pop_stack_out (self()),
	case stack_pool:get_last_stack (self(), play) of
		{ok, StackPlay} -> interfere_step:return_play(StackPlay);
		{error, _} -> gen_server:cast(self(), {act_next_command})
	end.

update_card_effect_to_client ([]) -> [];
update_card_effect_to_client ([{CardOwner, CardOrder, CardID} | Cards]) ->
%	io:format ("Card update effect after break combine ~p~n", [{CardOwner, CardOrder, CardID}]),
	effect_activate:send_update_activate_effect (CardOwner, CardOrder, CardID, [], update),
	update_card_effect_to_client (Cards).