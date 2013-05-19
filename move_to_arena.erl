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
-module (move_to_arena).
-export ([
						move_to_arena/1, 
						move_each_card_to_arena/0
					]).
-export ([
						move_mystic_card_to_arena/3,
						check_mystic_move_condition/3,
						check_move_mystic_to_arena_ability/0,
						check_move_mystic_on_arena_ability/0,
						select_mystic_target/3,
						verify_move_mystic_into_arena_ability/0,
						verify_move_mystic_to_arena_ability/0,
						activate_move_mystic_into_arena_effect/0,
						activate_move_mystic_on_arena_effect/0,
						verify_move_mystic_to_arena/3,
						%reselect_mystic_target/3,
						update_move_mystic_arena/3,
						mystic_change_zone/0,
						activate_mystic_to_target/3,
						next_command_to_client/0,
						selected_mystic_ability/1
					]).
-export([
						check_move_seal_to_arena_ability/0,
						check_move_seal_on_arena_ability/0,
						check_seal_select_line/0,
						select_line/1,
						verify_move_seal_into_arena_ability/0,
						verify_move_seal_on_arena_ability/0,
						activate_move_into_arena_effect/0,
						activate_move_seal_on_arena_effect/0,
						move_seal_to_arena_success/3,
						move_seal_to_arena_complete/0
					]).

% 707. การนำการ์ดเข้ามาในสนาม
% 707.1. การนำเข้ามาในสนามคือการนำ Seal หรือ Mystic Card จาก Zone ใดๆก็ตามเข้ามาใน Arena Zone โดยไม่ต้องจ่าย Cost ค่าร่าย
% 707.2. การ์ดที่กำลังเข้ามาในสนามถือว่าอยู่ในสนามแล้ว
% 707.3. Seal ที่กำลังเข้ามาในสนามจะไม่เป็นเป้าหมายของ Mystic Card ชนิด PS และ Mystic Card ที่กำลังเข้ามาในสนามจะไม่เป็นเป้าหมายของ Mystic Card ชนิด PM
% 707.4. หาก Seal ที่กำลังเข้ามาในสนามติด Curse Curse จะยังไม่แสดงผลจนกว่า Seal ที่กำลังเข้ามาในสนามจะเข้ามาในสนามสำเร็จ
reject_move_to_arena (PlayerPid, Reason) ->
	case Reason of
		no_mystic_target -> 
			gen_server:cast(self(), {rejected_move_to_arena, PlayerPid, 0});
		_ ->	io:format("Other reason rejected move to arena ~p~n", [Reason]),
			gen_server:cast(self(), {rejected_move_to_arena, PlayerPid, 99})
	end.
	
move_to_arena(CardMove) ->
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	MoveFxStack = stack_pool:get_last_stack(self(), move_effect),
	TarAssigned = stack_pool:get_last_stack(self(), assigned_target),
	TarExcept = stack_pool:get_last_stack(self(), except_target),
	stack_pool:push_stack(self(), PlayerPid, 0, 0, [{card_to_move, CardMove}]),
	case MoveFxStack of
		{ok, MoveFx} ->
			stack_pool:set_stack_option(self(), move_effect, MoveFx);
		_ ->	""%io:format ("Normal move to arena~n")
	end,
	case TarAssigned of
		{ok, AssignedTarget} ->
			lists:foreach(fun({AssiPid, AssiOrder, AssiID}) -> card_utility:add_card_status(AssiPid, AssiOrder, AssiID, must_paste_to_s)end, AssignedTarget),
			stack_pool:set_stack_option(self(), assigned_target, AssignedTarget);
		_ -> ""%io:format ("Normal move to arena~n")
	end,
	case TarExcept of
		{ok, ExceptTarget} -> stack_pool:set_stack_option(self(), except_target, ExceptTarget);
		_ -> stack_pool:set_stack_option(self(), except_target, [])
	end,
	move_each_card_to_arena().
	
move_each_card_to_arena() ->
	case stack_pool:get_last_stack(self(), card_to_move) of
		{ok, []} ->
			stack_pool:pop_stack_out(self()),
			interfere_step:return_play(check_play_step);
		{ok, [{CardOwner, CardOrder, CardID}|CardMove]} ->
			stack_pool:set_stack_option(self(), play, move_each_card_to_arena),
			stack_pool:set_stack_option(self(), card_to_move, CardMove),
			case card_utility:check_card_zone(CardOwner, CardOrder, CardID) of
				arena_zone -> move_each_card_to_arena();
				_ -> move_card_to_arena(CardOwner, CardOrder, CardID)
			end			
	end.
	
move_card_to_arena(CardOwner, CardOrder, CardID) ->
	case mnesia_odbc:is_seal_card(CardID) of
		is_seal -> move_seal_to_arena(CardOwner, CardOrder, CardID);
		is_not_seal -> move_mystic_card_to_arena(CardOwner, CardOrder, CardID)
	end.
	
move_seal_to_arena(CardOwner, CardOrder, CardID) ->
	MoveFxStack = stack_pool:get_last_stack(self(), move_effect),
	{ok, {PlayerPid, _, _, _}} = stack_pool:get_last_stack(self()),
	stack_pool:push_stack(self(), CardOwner, CardOrder, CardID, [{play, play_move_seal_to_arena}, {card_player, PlayerPid}]),
	case MoveFxStack of
		{ok, MoveFx} ->
			stack_pool:set_stack_option(self(), move_effect, MoveFx);
		_ ->	""%io:format ("Normal move to arena~n")
	end,
	update_seal_card_move(CardOwner, CardOrder, CardID).
		
update_seal_card_move(CardOwner, CardOrder, CardID) ->
	%Zone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	PreviousZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	stack_pool:set_stack_option(self(), previous_zone, PreviousZone),
	%arena_zone:seal_to_arena(CardOwner, CardOrder, CardID, [{card_status, moving_to_arena}], Zone),
	arena_zone:seal_to_arena(CardOwner, CardOrder, CardID, [{card_status, moving_to_arena}], PreviousZone),
	card_utility:add_card_status(CardOwner, CardOrder, CardID, card_on_arena_success, arena_zone),
	case PreviousZone of
		seal_deck ->
			card_utility:add_card_status(CardOwner, CardOrder, CardID, deck_to_arena, arena_zone);
		mystic_deck ->
			card_utility:add_card_status(CardOwner, CardOrder, CardID, deck_to_arena, arena_zone);
		hand_cards ->
			card_utility:add_card_status(CardOwner, CardOrder, CardID, hand_to_arena, arena_zone);
		remove_cards ->
			card_utility:add_card_status(CardOwner, CardOrder, CardID, remove_to_arena, arena_zone);
		shrine_cards ->
			card_utility:add_card_status(CardOwner, CardOrder, CardID, shrine_to_arena, arena_zone);
		Z ->	io:format ("Cards out of Zone ~p~n", [Z])
	end,
	{ok, MpRest} = mnesia_play:get_player_data(PlayerPid, mp_rest),
	mnesia_play:set_game_data(self(), seal_checkup, have_seal),
	gen_server:cast(self(), {update_card_casting, CardOwner, CardOrder, CardID, MpRest}).
	
check_move_seal_to_arena_ability() ->
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
 	stack_pool:set_stack_option(self(), play, check_move_seal_into_arena_ability),
	mod_ability_activate:check_any_ability_activate(into_arena, PlayerPid).
	
check_move_seal_on_arena_ability() ->
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	stack_pool:set_stack_option(self(), play, check_move_seal_on_arena_ability),
	mod_ability_activate:check_any_ability_activate(on_arena, PlayerPid).
	
% 708.6. ถ้าเป็น Seal ให้ทำการกำหนด Line โดยเจ้าของการ์ดเสมอ
check_seal_select_line() ->
	{ok, {CardOwner, _CardOrder, _CardID, _}} = stack_pool:get_last_stack (self()),
	MoveFx = stack_pool:get_last_stack(self(), move_effect),
	case MoveFx of
		{ok, {move_to_line, Line}} ->
			select_line([Line]);
		_ ->
			gen_server:cast(self(), {act_select_line, CardOwner, move_to_arena})
	end.
	
select_line(Data) ->
	Line =
	case Data of
		[LineSelect] -> LineSelect;
		_Error -> smo_logger:fmsg("Line Selected Error ~p~n", [_Error]), 1
	end,
	{ok, {CardOwner, CardOrder, CardID, _}} = stack_pool:get_last_stack (self()),
	stack_pool:set_stack_option(self(), assign_line, Line),
	card_utility:set_card_option_field(CardOwner, CardOrder, CardID, line, Line, arena_zone),	
	%card_utility:add_card_status(PlayerPid, CardOrder, CardID, already_assign_line_to_arena, arena_zone),
	stack_pool:set_stack_option(self(), play, play_move_seal_select_line),
	%{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	gen_server:cast(self(), {update_select_line, CardOwner, Line}).
	
verify_move_seal_into_arena_ability() ->
	stack_pool:set_stack_option(self(), play, verify_move_seal_into_arena_ability),
	mod_ability_activate:verify_ability_condition(into_arena).
	
verify_move_seal_on_arena_ability() ->
	stack_pool:set_stack_option(self(), play, verify_move_seal_on_arena_ability),
	mod_ability_activate:verify_ability_condition(on_arena).

activate_move_into_arena_effect() ->
	stack_pool:set_stack_option(self(), play, activate_move_seal_into_arena_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(into_arena, PlayerPid).
	
activate_move_seal_on_arena_effect() ->
	stack_pool:set_stack_option(self(), play, activate_move_seal_on_arena_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(on_arena, PlayerPid).

% 708.8. การ์ดที่กำลังเข้ามาในสนามจะสูญเสียสภาพการ์ดที่กำลังเข้ามาในสนามใน Phase นี้ -
% 708.9. การนำเข้ามาในสนามสำเร็จ -
move_seal_to_arena_success(CardOwner, CardOrder, CardID) ->
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, moving_to_arena, arena_zone),
	case stack_pool:get_last_stack(self(), previous_zone) of
		{ok, hand_cards} ->
			card_utility:remove_card_status(CardOwner, CardOrder, CardID, hand_to_arena, arena_zone),
			card_utility:add_card_status(CardOwner, CardOrder, CardID, on_arena_success, arena_zone);
		{ok, seal_deck} ->
			card_utility:remove_card_status(CardOwner, CardOrder, CardID, deck_to_arena, arena_zone);
		{ok, remove_cards} ->
			card_utility:remove_card_status(CardOwner, CardOrder, CardID, remove_to_arena, arena_zone);
		{ok, shrine_cards} ->
			card_utility:remove_card_status(CardOwner, CardOrder, CardID, shrine_to_arena, arena_zone)
		% {ok, support_cards} ->
			% card_utility:remove_card_status(CardOwner, CardOrder, CardID, supported, arena_zone)
	end,
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, card_on_arena_success, arena_zone),
% 708.9.1. นำ Seal ไปไว้ยัง Line ที่กำหนด
	activate_continuous_ability().
	
activate_continuous_ability() ->
	stack_pool:set_stack_option(self(), play, activate_move_seal_to_arena_continuous),
	continuous_ability:check_continuous_target().
	
move_seal_to_arena_complete() ->
	case stack_pool:get_last_stack(self(), previous_zone) of
		{ok, hand_cards} -> hand_zone:update_hand_data();
		_ -> ""
	end,
	stack_pool:pop_stack_out(self()),
	case stack_pool:get_last_stack (self(), play) of
		{ok, StackPlay} -> interfere_step:return_play(StackPlay);
		{error, _} -> gen_server:cast(self(), {act_next_command})
	end.
	
% 708. ขั้นตอนการนำการ์ดเข้ามาในสนาม
% 708.1. ประกาศการนำ Seal หรือ Mystic Card เข้ามาในสนาม
move_mystic_card_to_arena(CardOwner, CardOrder, CardID) ->
	TarAssigned = stack_pool:get_last_stack(self(), assigned_target),
	TarExcept = stack_pool:get_last_stack(self(), except_target),
	{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
	stack_pool:push_stack(self(), CardOwner, CardOrder, CardID, [{play, move_mystic_card_to_arena}, {card_player, PlayerPid}, {call_function, move_to_arena}]),
	CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	case CardZone of
		seal_deck ->
			card_utility:add_card_status(CardOwner, CardOrder, CardID, deck_to_arena);
		mystic_deck ->
			card_utility:add_card_status(CardOwner, CardOrder, CardID, deck_to_arena);
		hand_cards ->
			card_utility:add_card_status(CardOwner, CardOrder, CardID, hand_to_arena);
		remove_cards ->
			card_utility:add_card_status(CardOwner, CardOrder, CardID, remove_to_arena);
		shrine_cards ->
			card_utility:add_card_status(CardOwner, CardOrder, CardID, shrine_to_arena);
		Z ->	io:format ("Cards out of Zone ~p~n", [Z])
	end,
	stack_pool:set_stack_option(self(), previous_zone, CardZone),
% 708.2. Ability ของ Seal หรือ Mystic Card ที่จะทำงาน ถ้ามีการเลือกการทำงานของ Ability ให้เลือกในPhase นี้ และจะไม่มีการเปลี่ยนแปลง นอกจากจะมี Effect ให้สามารถทำได้ -
%	io:format ("Move to arena with ~p~n", [MoveFxStack]),
	case TarAssigned of
		{ok, AssignedTarget} ->
			lists:foreach(fun({AssiPid, AssiOrder, AssiID}) -> card_utility:add_card_status(AssiPid, AssiOrder, AssiID, must_paste_to_s)end, AssignedTarget),
			stack_pool:set_stack_option(self(), assigned_target, AssignedTarget);
		_ -> ""%io:format ("Normal move to arena~n")
	end,
	case TarExcept of
		{ok, ExceptTarget} -> stack_pool:set_stack_option(self(), except_target, ExceptTarget);
		_ -> stack_pool:set_stack_option(self(), except_target, [])
	end,
	move_mystic_to_arena(PlayerPid, CardOwner, CardOrder, CardID).

move_mystic_to_arena(_PlayerPid, CardOwner, _CardOrder, CardID) ->
	case new_mystic_check:check_mystic_ability(CardID) of
		{do_not_need_to_select, [AbilityNumber]} ->
			selected_mystic_ability(AbilityNumber);
		{need_to_select_one, _} ->
			%stack_pool:set_stack_option(self(), play, move_mystic_select_number),
			gen_server:cast(self(), {act_select_mystic_ability, {move_to_arena, CardOwner}, [16#88, 16#50], [<<CardID:16>>]});
		R -> io:format("mystic_ability return ~p~n", [R])
	end.
	
selected_mystic_ability(AbilityNumber) ->
	stack_pool:set_stack_option(self(), mystic_ability_number, AbilityNumber),
	interfere_step:return_play(check_play_step).

check_mystic_move_condition(CardOwner, CardOrder, CardID) ->
	{ok, AbilityNumber} = stack_pool:get_last_stack (self(), mystic_ability_number),
	OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
	MstZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	case new_mystic_check:check_casting_condition({MstZone, {CardOwner, CardOrder, CardID}}, AbilityNumber, OpponentPid) of
		{ok, can_cast} -> update_mystic_card_move(CardOwner, CardOrder, CardID);
		_ -> 
			{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
			reject_move_to_arena(PlayerPid, no_mystic_target)
	end.

% 708.4. การ์ดจะอยู่ในสภาพการ์ดที่กำลังเข้ามาในสนาม จะถือว่าการ์ดนั้นอยู่ในสนาม และหากเป็น Seal จะเป็น Active Seal
update_mystic_card_move(CardOwner, CardOrder, CardID) ->
	case card_utility:check_card_zone(CardOwner, CardOrder, CardID) of
		hand_cards -> FZ = 0;
		_ -> FZ = 1
	end,
	Zone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	arena_zone:mystic_to_arena(CardOwner, CardOrder, CardID, [{card_status, moving_to_arena}], Zone),
	{ok, MpRest} = mnesia_play:get_player_data(CardOwner, mp_rest),
	{ok, AbilityNumber} = stack_pool:get_last_stack(self(), mystic_ability_number),
	HandSize = hand_zone:check_card_size(CardOwner),
	stack_pool:set_stack_option(self(), play, move_mystic_to_arena_update),
	%{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	gen_server:cast(self(), {update_casting_mystic, CardOwner, [16#88, 16#51], [CardOrder, <<CardID:16>>, HandSize, MpRest, AbilityNumber, FZ]}).	
	
check_move_mystic_to_arena_ability() ->
 	stack_pool:set_stack_option(self(), play, check_move_mystic_into_arena_ability),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_activate:check_any_ability_activate(into_arena, PlayerPid).
	
check_move_mystic_on_arena_ability() ->
	stack_pool:set_stack_option(self(), play, check_move_mystic_on_arena_ability),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_activate:check_any_ability_activate(on_arena, PlayerPid).

select_mystic_target(CardOwner, CardOrder, CardID) ->
	{ok, AbilityNumber} = stack_pool:get_last_stack(self(), mystic_ability_number),
	OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
	% [{TargetType, MAbilityID, {SelectType, SelectAmount, Target}}|_]  = IDHaveFx
	stack_pool:set_stack_option(self(), play, move_to_arena_select_mystic_target),
	MstZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	IDHaveFx = new_mystic_check:check_mystic_ability_target({MstZone, {CardOwner, CardOrder, CardID}}, AbilityNumber, OpponentPid),
	stack_pool:set_stack_option(self(), all_potential_mystic_ability, IDHaveFx),
	new_mystic_check:select_mystic_target().
	
verify_move_mystic_into_arena_ability() ->
	stack_pool:set_stack_option(self(), play, verify_move_mystic_into_arena_ability),
	mod_ability_activate:verify_ability_condition(into_arena).
	
verify_move_mystic_to_arena_ability() ->
	stack_pool:set_stack_option(self(), play, verify_move_mystic_on_arena_ability),
	mod_ability_activate:verify_ability_condition(on_arena).
	
activate_move_mystic_into_arena_effect() ->
	stack_pool:set_stack_option(self(), play, activate_move_mystic_into_arena_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(into_arena, PlayerPid).
	
activate_move_mystic_on_arena_effect() ->
	stack_pool:set_stack_option(self(), play, activate_move_mystic_on_arena_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(on_arena, PlayerPid).
	
remove_moving_mystic_status(CardOwner, CardOrder, CardID) ->
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, moving_to_arena, arena_zone),
	case stack_pool:get_last_stack(self(), previous_zone) of
		{ok, hand_cards} ->
			card_utility:remove_card_status(CardOwner, CardOrder, CardID, hand_to_arena, arena_zone);
		{ok, seal_deck} ->
			card_utility:remove_card_status(CardOwner, CardOrder, CardID, deck_to_arena, arena_zone);
		{ok, remove_cards} ->
			card_utility:remove_card_status(CardOwner, CardOrder, CardID, remove_to_arena, arena_zone);
		{ok, shrine_cards} ->
			card_utility:remove_card_status(CardOwner, CardOrder, CardID, shrine_to_arena, arena_zone)
		% {ok, support_cards} ->
			% card_utility:remove_card_status(CardOwner, CardOrder, CardID, supported, arena_zone)
	end,
	card_utility:add_card_status(CardOwner, CardOrder, CardID, mystic_on_arena_success, arena_zone).
	
verify_move_mystic_to_arena(CardOwner, CardOrder, CardID) ->
	remove_moving_mystic_status(CardOwner, CardOrder, CardID),
	case card_utility:check_card_zone(CardOwner, CardOrder, CardID) of
		arena_zone ->
			{ok, AbilityNumber} = stack_pool:get_last_stack(self(), mystic_ability_number),
			OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
			case new_mystic_check:check_casting_condition({arena_zone, {CardOwner, CardOrder, CardID}}, AbilityNumber, OpponentPid) of
				{ok, can_cast} -> verify_mystic_target(CardOwner, CardOrder, CardID);
				_ -> 
					{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
					reject_move_to_arena(PlayerPid, casting_condition_mismatch)
			end;
		_ -> next_command_to_client()
	end.

verify_mystic_target(CardOwner, CardOrder, CardID) ->
	%stack_pool:set_stack_option(self(), play, verify_mystic_target),
	%{ok, AbilityNumber} = stack_pool:get_last_stack(self(), mystic_ability_number),
	stack_pool:set_stack_option(self(), play, verify_move_mytic_arena_target),
	new_mystic_check:verify_mystic_target(CardOwner, CardOrder, CardID).
	
update_move_mystic_arena(CardOwner, CardOrder, CardID) ->
	stack_pool:set_stack_option (self(), play, update_move_mystic_to_arena),
% 610.12. การร่ายสำเร็จ
	%{ok, PlayerPid} = stack_pool:get_last_stack (self(), card_player),
% 610.12.4. หากเป็น Mystic Card PS, PA, PM ให้นำไปติดที่เป้าหมายที่กำหนดไว้ และจะมีสภาพทุกอย่างที่กำหนดไว้ ใน Phase นี้ --
	Paste = new_mystic_check:check_paste_mystic_to_target(CardOwner, CardOrder, CardID),
	%smo_logger:fmsg("move to arena Paste case ~p~n", [Paste]),
	case Paste of
		{ok, mystic_pasted, TPid, TOrder, Tid} ->
			DurationMystic = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, duration, arena_zone),
			case DurationMystic of
				{ok, 0} ->	no_paste;
				{ok, _} ->	
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, paste_to, [{TPid, TOrder, Tid}]),
					card_utility:set_card_option_field(TPid, TOrder, Tid, mystic, [{CardOwner, CardOrder, CardID}]);
				_Other -> 
					card_utility:set_card_option_field(CardOwner, CardOrder, CardID, paste_to, [{TPid, TOrder, Tid}]),
					card_utility:set_card_option_field(TPid, TOrder, Tid, mystic, [{CardOwner, CardOrder, CardID}])
			end,
		gen_server:cast(self(), {update_cast_mystic_success, [16#88, 16#54, 0], CardOwner, CardOrder, CardID, TPid, TOrder, Tid});
		% กรณีที่ Ability ไม่เกิดผลกับเป้าหมายให้นำมิสติกลง shrine
		{ok, cancel_mystic, TPid, TOrder, Tid} ->
			stack_pool:set_stack_option(self(), card_cancel_mystic, move_to_shrine),
			gen_server:cast(self(), {update_cast_mystic_success, [16#88, 16#54, 0], CardOwner, CardOrder, CardID, TPid, TOrder, Tid});
		{ok, paste_on_arena} ->
			gen_server:cast(self(), {update_cast_mystic_success, [16#88, 16#54, 1], CardOwner, CardOrder, CardID, 0, 0, 0});
		{ok, activate_effect_and_move_to_shrine} ->
			gen_server:cast(self(), {update_cast_mystic_success, [16#88, 16#54, 3], CardOwner, CardOrder, CardID, 0, 0, 0});
		{ok,no_target_then_move_to_shrine} ->
			gen_server:cast(self(), {update_cast_mystic_success, [16#88, 16#54, 3], CardOwner, CardOrder, CardID, 0, 0, 0})
	end.
	
mystic_change_zone() ->
	stack_pool:set_stack_option(self(), play, play_activate_mystic_change_zone),
	continuous_ability:check_continuous_target().
	
activate_mystic_to_target(PlayerPid, CardOrder, CardID) ->
	%mystic_effect:update_ability_continues (),
	MAbility = stack_pool:get_last_stack (self(), result_target),
%	io:format ("Mystic ability effect ~p~n", [MAbility]),
	case MAbility of
		{ok, []} ->
			%stack_pool:remove_stack_option (self(), target_selected),
			check_cancel_mystic_effect(PlayerPid, CardOrder, CardID);
		{ok, [MAbilityTuple| MAbilityRest]} ->
			stack_pool:set_stack_option(self(), result_target, MAbilityRest),
			%stack_pool:add_stack_option_field(self(), mystic_ability_activated, [MAbilityTuple]),
			stack_pool:set_stack_option(self(), play, move_mystic_arena_activate_mystic_effect),
			mystic_effect:set_activate_mystic_to_target(PlayerPid, CardOrder, CardID, MAbilityTuple);
		_ -> 
			stack_pool:set_stack_option(self(), card_cancel_mystic, move_to_shrine),
			check_cancel_mystic_effect(PlayerPid, CardOrder, CardID)
	end.
	
check_cancel_mystic_effect(CardOwner, CardOrder, CardID) ->
	case stack_pool:get_last_stack(self(), card_cancel_mystic) of
		{ok, move_to_shrine} ->
			stack_pool:set_stack_option(self(), play, move_cancel_move_mystic_to_shrine),
			shrine_zone:card_to_shrine(CardOwner, [{CardOwner, CardOrder, CardID}]);
		_ -> check_send_fx_and_duration_mystic(CardOwner, CardOrder, CardID)
	end.	
	
check_send_fx_and_duration_mystic(CardOwner, CardOrder, CardID) ->
	%smo_logger:fmsg("check_send_fx_and_duration_mystic Mystic {~p, ~p, ~p} gave effect ~p~n", [PlayerPid, CardOrder, CardID, card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, give_effect)]),
	DurationMystic = card_utility:get_card_option_field(CardOwner, CardOrder, CardID, duration, arena_zone),
	case DurationMystic of
		{ok, 0} ->	
			stack_pool:set_stack_option(self(), play, move_mystic_arena_zero_turn),
			shrine_zone:card_to_shrine(CardOwner, [{CardOwner, CardOrder, CardID}]);
		{ok, _} -> next_command_to_client();
		_ -> next_command_to_client()
	end.

next_command_to_client() ->
	hand_zone:update_hand_data(),
	stack_pool:pop_stack_out (self()),
	case stack_pool:get_last_stack (self(), play) of
		{ok, StackPlay} -> interfere_step:return_play(StackPlay);
		{error, _} -> gen_server:cast(self(), {act_next_command})
	end.
%------------------------------------------------------------------	
																													% 708.5. หากต้องทำการเลือกเป้าหมายให้เลือกใน Phase นี้ Ability เมื่อตกเป็นเป้าหมาย Ability หรือ Mystic Card จะทำงานใน Phase นี้ -
																														%move_card_to_arena_5 (PlayerPid, CardOrder, CardID) ->
																															%stack_pool:set_stack_option (self(), play, play_move_card_to_arena_5),
																															%case card_utility:check_card_status (PlayerPid, CardOrder, CardID, ability_activation) of
																																%{ok, have_status} ->
																																	%activate_select_target (PlayerPid, CardOrder, CardID);
																																%{ok, have_no_status} ->
																																	%interfere_step:return_play (check_play_step)
																															%end.

																														%activate_select_target (PlayerPid, CardOrder, CardID) ->
																															%case mnesia_odbc:is_seal_card (CardID) of
																																%is_seal -> check_seal_ability_target (PlayerPid, CardOrder, CardID);
																																%is_not_seal ->
																																	%MoveFx = stack_pool:get_last_stack (self(), move_effect),
																																	%case MoveFx of
																																		%{ok, {attach_to, [{TargetPid, TargetOrder, TargetId}]}} ->
																																			%{ok, [MAbilityTuple]} = stack_pool:get_last_stack (self(), mystic_ability),
																																			%stack_pool:set_stack_option (self(), wait_player_select_target, MAbilityTuple),
																																			%stack_pool:remove_stack_option (self(), mystic_ability),
																																			%mystic_effect:update_mystic_target ([{TargetPid, TargetOrder, TargetId}]),
																																			%interfere_step:return_play (check_play_step);
																																		%_ ->	mystic_effect:check_ability_select_target (PlayerPid)
																																	%end
																															%end.

																														%check_seal_ability_target (PlayerPid, CardOrder, CardID) ->
																															%Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
																															%case card_utility:check_card_status (PlayerPid, CardOrder, CardID, ability_select_target, Zone) of
																																%{ok, have_status} ->
																																	%ability_utility:check_ability_select_target (PlayerPid, CardOrder, CardID);
																																%{ok, have_no_status} ->
																																	%interfere_step:return_play (check_play_step)
																															%end.


																													%check_ability_target (CardOwner, CardOrder, CardID) ->
																														%case card_utility:check_card_status (CardOwner, CardOrder, CardID, ability_activation) of
																															%{ok, have_status} ->
																																%case stack_pool:get_last_stack (self(), ability_target_selected_cards) of
																																	%{ok, Cards} ->
																																		%case ability_affect:ability_target_verify (Cards) of
																																			%[] ->	interfere_step:return_play(check_play_step);
																																			%_ ->	move_card_to_arena_5 (CardOwner, CardOrder, CardID)
																																		%end;
																																	%_ -> interfere_step:return_play(check_play_step)
																																%end;
																															%{ok, have_no_status} ->
																																%interfere_step:return_play (check_play_step)
																														%end.
	
												% 708.9.2. ถ้าเงื่อนไขในการเกิด Ability ยังคงถูกต้องและมีเป้าหมายที่ถูกต้อง Ability เมื่อเข้ามาในสนาม หรือ Ability อื่นๆจะทำงาน
																														%case card_utility:check_card_status (CardOwner, CardOrder, CardID, ability_activation) of
																															%{ok, have_status} ->
																																%stack_pool:add_stack_option_field (self(), ability_activate, [{CardOwner, CardOrder, CardID}]),
																																%ard_utility:remove_card_status (CardOwner, CardOrder, CardID, ability_activation);
																															%{ok, have_no_status} ->
																																%interfere_step:return_play (check_play_step)
																														%end,
																														%activate_ability_card (PlayerPid).

																													%activate_ability_card (PlayerPid) ->
																														%case stack_pool:get_last_stack (self(), ability_activate) of
																															%{ok, []} -> interfere_step:return_play (check_play_step);
																															%{ok, Cards} ->
																													%			io:format("Card activate ability ~p~n", [Cards]),
																																%ability_affect:check_have_to_arrange_ability_affect (PlayerPid, Cards, [], []);
																															%{error, _} -> interfere_step:return_play (check_play_step)
																														%end.
																														
																													% move_card_to_arena_9_2_1 () ->
																														% stack_pool:set_stack_option (self(), play, play_move_card_to_arena_9_2_1),
																														% mod_ability_effect:start_effect_activate(into_arena).

% 708.11. หากการ์ดที่กำลังเข้ามาในสนามเปลี่ยนไปยัง Zone อื่นได้สำเร็จ ให้การ์ดที่กำลังร่ายสูญเสียสภาพการ์ดที่กำลังเข้ามาในสนามทันที -
