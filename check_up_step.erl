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
-module (check_up_step).

-compile (export_all).

% 401.2. Check up Step
% 401.2.1. ใน Check Up Step ผู้เล่นเจ้าของ Subturn โจมตีทำการตรวจสอบว่ามี Seal ในสนามของตนหรือไม่ -
% 401.2.2. ใน Check up Step ผู้เล่นทั้งสองฝ่ายไม่สามารถร่ายหรือสั่งการ Seal Card หรือ Mystic Card ใดๆ ทั้งที่เป็นและไม่เป็น Interfere
% แม้ว่าจะมี Interfere Step แทรกขึ้นมาก็ตาม ยกเว้นจะมี Effect ระบุให้ร่ายหรือสั่งการได้ใน Step นี้ -
checkup_step() ->
	{ok, PlayerPid} = mnesia_play:get_game_data(self(), player_turn),
	OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
	stack_pool:push_stack(self(), PlayerPid, 0, 0, [{play, play_checkup_step}, {card_player, PlayerPid}]),
	case get(subturn) of
		undefined -> 
			continuous_ability:check_continuous(),
			DeckAbi = update_ability:retrieve_deck_ability(PlayerPid, OpponentPid),
			smo_logger:fmsg("Card in deck which have in deck ability are ~p~n", [DeckAbi]),
			mnesia_play:set_game_data(self(), deck_ability_card, DeckAbi),
			put(subturn, 1);
		SubturnCount -> put(subturn, SubturnCount+1)
	end,
	seal_check_up(PlayerPid),
	mnesia_play:remove_player_status(OpponentPid, disallow_first_attack),
	interfere_step:return_play(check_play_step).
	
% ability ของ การ์ดบนมือใน Subturn แรกควรเริ่มทำงานในช่วงนี้
% แต่ต้องเช็คก่อนว่าเป็น Subturn แรกหรือไม่
check_1st_subturn_onhand_ability(PlayerPid) ->
	case mnesia_play:check_player_status (PlayerPid, disallow_first_attack) of
		{ok, have_no_status} -> checkup_step_3(PlayerPid);
		{ok, have_status} ->
			stack_pool:set_stack_option(self(), play, check_1st_subturn_onhand_ability),
			mod_ability_activate:check_any_ability_activate(on_hand, PlayerPid)
	end.

verify_1st_subturn_ability_condition() ->
	stack_pool:set_stack_option(self(), play, verify_1st_subturn_ability_condition),
	mod_ability_activate:verify_ability_condition(on_hand).

activate_1st_subturn_onhand_effect() ->
	stack_pool:set_stack_option(self(), play, activate_1st_subturn_onhand_effect),
	{ok, {PlayerPid, _, _, _}} = stack_pool:get_last_stack(self()),
	mod_ability_effect:check_any_ability_activate(on_hand, PlayerPid).
	
activate_1st_onhand_as_long_as_effect() ->
	stack_pool:set_stack_option(self(), play, activate_1st_onhand_as_long_as_effect),
	% case ability_effect:check_all_ability_affect () of
		% 0 -> interfere_step:return_play(check_play_step);
		% _ -> card_utility:check_card_affect()
	% end.
	continuous_ability:check_continuous_target().

% 401.2.3. Ability เมื่อเข้าสู่ Subturn โจมตีทำงาน
% Ability ที่จะทำงาน เมื่อเข้าสู่ Subturn โจมตี ถ้ามีการเลือกการทำงานของ Ability หรือเลือกเป้าหมายให้เลือกใน Phase นี้ --
checkup_step_3(PlayerPid) ->
	%stack_pool:set_stack_option (self(), play, play_at_subturn_activate_ability),
	%stack_pool:set_stack_option (self(), stack_option_field_check, checkup_attack),
	stack_pool:set_stack_option (self(), play, into_at_subturn_ability_activate),
	mod_ability_activate:check_any_ability_activate(into_at_subturn, PlayerPid).
	
checkup_step_3_1() ->
	stack_pool:set_stack_option (self(), play, verify_into_at_subturn_ability),
	mod_ability_activate:verify_ability_condition(into_at_subturn).

activate_into_at_subturn_effect() ->
	stack_pool:set_stack_option (self(), play, activate_into_at_subturn_effect),
	{ok, {PlayerPid, _, _, _}} = stack_pool:get_last_stack(self()),
	mod_ability_effect:check_any_ability_activate(into_at_subturn, PlayerPid).
%เช็คการ์ดที่ได้รับ effect ให้เกิดตอนต้น turn at ใน check up step
activate_check_up_suspend_effect() ->
	stack_pool:set_stack_option(self(), play, activate_suspend_checkup_effect),
	case get(checkup_target) of
		[{CardOwner, CardOrder, CardID}|T] -> check_card_recieve_effect([{CardOwner, CardOrder, CardID}|T]);
		_ -> interfere_step:return_play(check_play_step)
	end.

check_card_recieve_effect([]) -> interfere_step:return_play(check_play_step);
check_card_recieve_effect([{CardOwner, CardOrder, CardID}|Tail]) ->
	CheckUpFx = check_up_step_effect(CardOwner, CardOrder, CardID),
		case CheckUpFx of
			[{_,[SubFx],_,_}|T] ->
				{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
				activate_check_up_step_effect(PlayerPid, CheckUpFx);
			[{_,[{check_up_step, X}, _],_,_}|T] ->
				{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
				set_new_effect(PlayerPid, CheckUpFx);
			_ ->
				check_card_recieve_effect(Tail)
		end,
	check_card_recieve_effect(Tail).
	
check_up_step_effect(CardOwner, CardOrder, CardID) ->
	CardFx = card_utility:get_all_card_effect(CardOwner, CardOrder, CardID),
	Require = {check_up_step, 0},
	is_contain({CardOwner, CardOrder, CardID}, Require, CardFx).	
	
is_contain(_, _, []) -> [];
is_contain({CardOwner, CardOrder, CardID}, FxRequire, [{Gfx, CardFx, Duration}|Tail]) ->
	[{Gfx, check_card_effect(FxRequire, CardFx), Duration, [{CardOwner, CardOrder, CardID}]}] ++ is_contain({CardOwner, CardOrder, CardID}, FxRequire, Tail).

check_card_effect(_, []) -> [];
check_card_effect(FxType, [{{check_up_step, 0}, SubFx}|CardFx]) -> [SubFx] ++ check_card_effect(FxType, CardFx);
check_card_effect(FxType, [{{check_up_step, X}, SubFx}|CardFx]) -> [{check_up_step, X-1}, SubFx] ++ check_card_effect(FxType, CardFx);
check_card_effect(FxType, [_|CardFx]) -> check_card_effect(FxType, CardFx).	
% 401.2.4. Ability เมื่อเข้าสู่ Subturn ป้องกันของผู้เล่น Subturn ป้องกันทำงาน
% Ability ที่จะทำงาน เมื่อเข้าสู่ Subturn ป้องกัน ถ้ามีการเลือกการทำงานของ Ability หรือเลือกเป้าหมายให้เลือกใน Phase นี้ --
checkup_step_4(PlayerPid) ->
	%stack_pool:set_stack_option (self(), play, play_df_subturn_activate_ability),
	stack_pool:set_stack_option (self(), play, into_df_subturn_ability_activate),
	mod_ability_activate:check_any_ability_activate(into_df_subturn, PlayerPid).
	
checkup_step_4_1() ->
	stack_pool:set_stack_option (self(), play, verify_into_df_subturn_ability),
	mod_ability_activate:verify_ability_condition(into_df_subturn).
	
activate_into_df_subturn_effect() ->
	stack_pool:set_stack_option (self(), play, activate_into_df_subturn_effect),
	{ok, {PlayerPid, _, _, _}} = stack_pool:get_last_stack(self()),
	mod_ability_effect:check_any_ability_activate(into_df_subturn, PlayerPid).
	
% 401.2.5. Ability เมื่อเข้าสู่ Check up Step ทำงาน
checkup_step_5(PlayerPid) ->
	stack_pool:set_stack_option (self(), play, into_any_subturn_ability_activate),
	mod_ability_activate:check_any_ability_activate(into_any_subturn, PlayerPid).
	
checkup_step_5_1() ->
	stack_pool:set_stack_option (self(), play, verify_into_any_subturn_ability),
	mod_ability_activate:verify_ability_condition(into_any_subturn).
	
activate_into_any_subturn_effect() ->
	stack_pool:set_stack_option (self(), play, activate_into_any_subturn_effect),
	{ok, {PlayerPid, _, _, _}} = stack_pool:get_last_stack(self()),
	mod_ability_effect:check_any_ability_activate(into_any_subturn, PlayerPid).

% 401.2.6. ทำการเลือกว่าจะทำการ Progress หรือไม่ ถ้าเลือกให้เริ่มทำการ Progress ใน Phase นี้ -
checkup_step_6() ->
	stack_pool:pop_stack_out(self()),
	play_utility:into_next_step().

% seal_check_up (player_turn_pid, player_data)
% ArenaZone = {CardData, Active, Line, Growth, Ability, Combine, Mystic, Effect}
seal_check_up (PlayerPid) ->
	{ok, ArenaZone} = mnesia_play:get_player_data (PlayerPid, arena_zone),
	CheckUpSeal = check_seal (ArenaZone),
	mnesia_play:set_game_data(self(), seal_checkup, CheckUpSeal),
	arena_zone:check_up_arena (PlayerPid).

check_seal ([{{_, _, CardID}, _}|T]) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal -> have_seal;
		is_not_seal -> check_seal (T)
	end;
check_seal ([]) -> have_not_seal.

% ------------------- External Call Function -------------------------------------------
activate_checkup_effect () ->
	{ok, SOFCheck} = stack_pool:get_last_stack (self(), stack_option_field_check),
	EOSCheck = stack_pool:get_last_stack (self(), SOFCheck),
	case EOSCheck of
		{ok, []} ->
			check_stack_return_play (SOFCheck);
		{error, _} ->
			check_stack_return_play (SOFCheck);
		{ok, [{CardOwner, CardOrder, CardID, Fx} | Cards]} ->
			stack_pool:set_stack_option (self(), SOFCheck, Cards),
			activate_ability (CardOwner, CardOrder, CardID, Fx)
	end.

check_stack_return_play (SOFCheck) ->
	stack_pool:remove_stack_option (self(), SOFCheck),
	case SOFCheck of
		checkup_attack -> interfere_step:return_play (play_checkup_step_3);
		checkup_defend -> interfere_step:return_play (play_checkup_step_4);
		checkup -> interfere_step:return_play (play_checkup_step_5);
		_ ->	io:format ("Check stack checkup return play ~p out of range~n", [SOFCheck])
	end.

activate_ability (CardOwner, CardOrder, CardID, Fx) ->
	case Fx of
		{move, to_df} ->
			line_change:move_to_line ([{CardOwner, CardOrder, CardID}], 0);
		{move, to_at} ->
			line_change:move_to_line ([{CardOwner, CardOrder, CardID}], 1);
		{move, to_deck} ->
			move_to_library:move_to_library (CardOwner, [{CardOwner, CardOrder, CardID}]);
		_ ->	io:format ("Check up ~p MUST process~n", [Fx])
	end.
%====================================================================
%เช็ค ability ที่ได้รับมาให้ส่งผลเกิดตอน check up step
set_new_effect(PlayerPid, CheckUpFx) ->
	case CheckUpFx of
		[{GFx, Fx, Duration, RFx}|Remain] ->		
			[{TPid, TCor, TCid}] = RFx,
			{GOwner, GOrder, GID, _} = GFx,
			TZone = card_utility:check_card_zone(TPid, TCor, TCid),
			ResFX = effect_value:check_value(GOwner, GOrder, GID, list_to_tuple(Fx), {TPid, TCor, TCid}),
			card_utility:update_card_option_field(TPid, TCor, TCid, receive_effect, [{GFx, ResFX, Duration}], TZone);
		_ ->
			interfere_step:return_play(check_play_step)
	end.				
%====================================================================
%เช็ค ability ที่ได้รับมาให้ส่งผลเกิดตอน check up step
activate_check_up_step_effect(PlayerPid, CheckUpFx) ->
	StackOption = [{activate_effect, CheckUpFx}],
	stack_pool:push_stack(self(), PlayerPid, 0, 0, StackOption),
	activate_effect().

activate_effect() -> 
	case stack_pool:get_last_stack(self(), activate_effect) of
		{ok, []} -> 
			stack_pool:pop_stack_out(self());
			%interfere_step:return_play(check_play_step);
		{ok, [{GFx, Fx, Duration, RFx}|Remain]} ->
			stack_pool:set_stack_option(self(), activate_effect, Remain),
			stack_pool:set_stack_option(self(), card_give_effect, GFx),
			stack_pool:set_stack_option(self(), effect_activate, Fx),
			stack_pool:set_stack_option(self(), target, RFx),
			stack_pool:set_stack_option(self(), duration, Duration),
			effect_activate()
		end.	

effect_activate() ->
	case stack_pool:get_last_stack(self(), effect_activate) of
		{ok, []} -> activate_effect();
		{ok, [{EffectType, Effect}|FxRemain]} ->
			stack_pool:set_stack_option(self(), effect_activate, FxRemain),
			stack_pool:set_stack_option(self(), play, checkup_activate_no_trigger_effect),
			activate({EffectType, Effect})
	end.
	
activate({EffectType, Effect}) ->
	case EffectType of
		_ ->
			%{ok, GFx} = stack_pool:get_last_stack(self(), card_give_effect),
			{ok, RFx} = stack_pool:get_last_stack(self(), target),
			%{ok, Duration} = stack_pool:get_last_stack(self(), duration),
			%activate_effect_to_target(RFx, [{GFx, {EffectType, Effect}, Duration}]),
			mod_ability_effect:check_effect_affect({EffectType, Effect}),
			put(checkup_target, get(checkup_target) -- RFx),
			interfere_step:return_play(check_play_step)
	end.	