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
-module(force_break).
-export([
						force_break_combine/4,
						check_break_combine_ability/0,
						check_break_combine_success_ability/0,
						separate_seal/3,
						activate_break_combine_effect/0,
						activate_break_combine_success_effect/0,
						set_break_combine_complete/0,
						break_combine_success/3,
						return_play_from_break_combine/0,
						get_mystic_data/1
					]).

force_break_combine(PlayerPid, CardOwner, CardOrder, CardID) ->
	card_utility:add_card_status(CardOwner, CardOrder, CardID, breaking_combine, arena_zone),
	%% Ability ที่จะทำงาน เมื่อ Seal แยกการรวมร่างหรือแยกการรวมร่างสำเร็จ ถ้ามีการเลือกการทำงานของ Ability หรือเลือกเป้าหมายให้เลือกใน Phase นี้ --
	stack_pool:push_stack (self(), CardOwner, CardOrder, CardID, [{play, force_breaking_combine}, {card_player, PlayerPid}]),
	interfere_step:return_play(check_play_step).
	
check_break_combine_ability() ->
	stack_pool:set_stack_option(self(), play, check_break_force_combine_ability),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_activate:check_any_ability_activate(breaking_combine, PlayerPid).
	
check_break_combine_success_ability() ->
	stack_pool:set_stack_option(self(), play, check_force_break_combine_success_ability),
{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_activate:check_any_ability_activate(break_combine_success, PlayerPid).
	
% update_break_combine(CardOwner, CardOrder, CardID) ->
	% {ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	% stack_pool:set_stack_option(self(), play, update_force_break_combine),
	% BreakTarget = ability_utility:get_update_data(PlayerPid, [{CardOwner, CardOrder, CardID}]),
	% gen_server:cast(self(), {update_break_combine, [16#88] ++ [16#60] ++ BreakTarget}).
	
%separate_seal(CardOwner, CardOrder, CardID) -> 
%	{ok, ArenaReply, _MysticSupport} = arena_zone:break_support_seal(CardOwner, CardOrder, CardID),
	%card_utility:add_card_status(CardOwner, CardOrder, CardID, be_combine, arena_zone),
	%% 514.8. ตรวจสอบว่า Ability นั้นถูกต้องตามเงื่อนไขในการทำงานหรือไม่ ถ้าเงื่อนไขไม่ครบ Ability นั้นจะไม่ทำงาน
	%% หรือ หากไม่มีเป้าหมายที่ Ability สามารถส่งผล หรือ กำหนดได้ Ability นั้นจะไม่ทำงาน ถ้าเงื่อนไขในการเกิด Ability
	%% ยังคงถูกต้องและมีเป้าหมายที่ถูกต้อง Ability เมื่อ Seal แยกการรวมร่างหรือ เมื่อ Seal แยกการรวมร่างสำเร็จ จะทำงาน ใน Phase นี้ --
%	case _MysticSupport of
%		[] -> "";
%		[{MOnwerPid, MCardOrder, MCardID}|_] -> 
%			MysticSupport = get(support_target),
%			case lists:keysearch({CardOwner, CardOrder, CardID}, 1, MysticSupport) of
%				{value, {{CardOwner, CardOrder, CardID}, OptionNo}} ->
%					{ok, PowerChange} =  mnesia_odbc:get_power_change_data(CardID, OptionNo),
%					OptionChange = [{combine_support, {{MOnwerPid, MCardOrder, MCardID}, {}}}, {combine_power, PowerChange}],
%					ArenaCardOption = arena_zone:get_card_option(CardOwner, CardOrder, CardID),
%					CardOption =
%					case ArenaCardOption of
%						{ok, TCardOption} -> TCardOption;
%						_ -> []
%					end,
%					mystic_effect:update_option_effect(CardOwner, CardOrder, CardID, CardOption, OptionChange);
%				_ ->""
%			end
%	end,

get_mystic_data([{_,_,356}|_]) -> {already};
get_mystic_data([_|T]) -> get_mystic_data(T);
get_mystic_data([]) -> {no_combine_mystic}.

separate_seal(CardOwner, CardOrder, CardID) -> 
	{ok, ArenaReply, _MysticSupport} = arena_zone:break_support_seal(CardOwner, CardOrder, CardID),
	case _MysticSupport of
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
	ArenaSize = length(ArenaReply),
	ArenaDataReply = get_data_reply(ArenaReply),
	%{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	stack_pool:set_stack_option(self(), play, force_separate_seal),
	gen_server:cast(self(), {update_arena_cards, CardOwner, [ArenaSize] ++ ArenaDataReply}).
	
get_data_reply([]) -> [];
get_data_reply([{{CardOwner, CardOrder, CardID}, CardOption} | Cards]) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			{ok, Line} = seal_card:get_seal_option (CardOption, line),
			stack_pool:add_stack_option_field (self(), break_combine_to_arena_cards, [{CardOwner, CardOrder, CardID}]),
			[CardOrder, <<CardID:16>>, Line] ++ get_data_reply (Cards);
		is_not_seal ->
			get_data_reply (Cards)
	end.
	
activate_break_combine_effect() ->
	stack_pool:set_stack_option(self(), play, activate_force_break_combine_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(breaking_combine, PlayerPid).
	
activate_break_combine_success_effect() ->
	stack_pool:set_stack_option(self(), play, activate_force_break_combine_success_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(break_combine_success, PlayerPid).

set_break_combine_complete() ->
	stack_pool:set_stack_option (self(), play, play_set_force_break_combine_complete),
	% case ability_effect:check_all_ability_affect () of
		% 0 -> interfere_step:return_play (check_play_step);
		% _ -> card_utility:check_card_affect()
	% end.
	continuous_ability:check_continuous_target().

break_combine_success(CardOwner, CardOrder, CardID) ->
	%% 514.9. จบขั้นตอนการ Break Combination Seal ที่กำลังแยกการรวมร่าง สูญเสียสภาพ Seal ที่กำลังแยกการรวมร่าง
	%% และนำ Seal ที่ประกาศ Break Combination ออกจากการประกาศ Break Combination
	stack_pool:set_stack_option (self(), play, play_force_break_combine_success),
	card_utility:add_card_status (CardOwner, CardOrder, CardID, break_combined, arena_zone),
	card_utility:remove_card_status (CardOwner, CardOrder, CardID, breaking_combine, arena_zone),
	card_utility:remove_card_status (CardOwner, CardOrder, CardID, be_combine, arena_zone),
	interfere_step:return_play (success_cast).
	
return_play_from_break_combine() ->
	case stack_pool:get_last_stack (self(), force_breaking_combine) of
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
