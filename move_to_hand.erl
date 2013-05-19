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
-module (move_to_hand).

-export ([
						move_card_to_hand/2, 
						check_moving_to_hand_ability/0,
						moving_to_hand_interfere/0,
						verify_moving_to_hand_ability/0,
						activate_moving_to_hand_ability/0,
						moving_to_hand_interfere2/0,
						do_card_to_hand/2,
						card_to_hand_continuous_activate/0,
						card_to_hand_complete/0,
						check_on_hand_ability/0,
						verify_on_hand_ability/0,
						activate_on_hand_ability/0
						]).%, hand_card_added/0]).

move_card_to_hand(PlayerPid, CardsToHand) ->
	SupportSeal = get_support_seal(CardsToHand),
	case change_zone:check_card_to_other_zone(CardsToHand ++ SupportSeal, hand_cards) of
			[] -> do_card_to_hand(PlayerPid, CardsToHand ++ SupportSeal);
			ToOtherZone -> 
				{ok, LastPlay} = stack_pool:get_last_stack(self(), play),
				put(last_play, LastPlay),
				stack_pool:set_stack_option(self(), play, from_hand_to_oher_zone),
				stack_pool:set_stack_option(self(), card_to_other_zone, ToOtherZone),
				interfere_step:return_play(check_play_step)
	end.

% move_card_to_hand(PlayerPid, CardOrder, CardID) ->
	% case change_zone:check_card_to_other_zone([{PlayerPid, CardOrder, CardID}], hand_cards) of
			% [] -> do_card_to_hand(PlayerPid, CardOrder, CardID);
			% ToOtherZone -> 
				% {ok, LastPlay} = stack_pool:get_last_stack(self(), play),
				% put(last_play, LastPlay),
				% stack_pool:set_stack_option(self(), play, from_hand_to_oher_zone),
				% stack_pool:set_stack_option(self(), card_to_other_zone, ToOtherZone),
				% interfere_step:return_play(check_play_step)
	% end.

% 705.	ขั้นตอนการนำการ์ดขึ้นมือ
do_card_to_hand(PlayerPid, CardsToHand) ->
% 705.1. เมื่อ Seal หรือ Mysic Card ที่ถูกนำขึ้นมือ ด้วย Effect ต่างๆ จะเรียก Seal หรือ Mystic Card ที่ถูกนำขึ้นมือว่า การ์ดที่กำลังขึ้นมือ
	%MoveCards = [{PlayerPid, CardOrder, CardID}],
	%CardZone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	% case CardZone of
		% hand_cards -> interfere_step:return_play();
		% _ ->
	stack_pool:push_stack(self(), PlayerPid, 0, 0, [{card_player, PlayerPid}, {play, card_to_hand}]), 

% 705.2. นำการ์ดที่กำลังขึ้นมือ  ขึ้นมือ โดยถือว่าการ์ดอยู่ใน Hand Zone ใน Phase นี้โดยเจ้าของการ์ดที่กำลังขึ้นมือ  เป็นผู้นำขึ้น หากการ์ดที่กำลังขึ้นมือ
% มี Mystic Card ติดอยู่ให้ทำการนำ Mystic Card ที่ติดอยู่ลง Shrine  ใน Phase นี้ การ์ดที่กำลังขึ้นมือทุกใบจะลืม  Effect ทั้งหมดที่กระทำกับตัวมัน
% นอกจาก Effect นั้นจะระบุไว้ว่ายังคงส่งผลแม้การ์ดนั้นอยู่ในมือ
	MysticPasted = shrine_zone:pasted_mystic_to_shrine(CardsToHand),
	stack_pool:set_stack_option(self(), cards_to_hand, CardsToHand),
	change_card_zone(CardsToHand),
	case MysticPasted of
		[] -> check_moving_to_hand_ability();
		_ ->	%stack_pool:push_stack(self(), PlayerPid, CardOrder, CardID, [{play, destroyed}, {card_zone, arena_zone}, {card_destroyed, MysticPasted}]),
			shrine_zone:card_to_shrine(PlayerPid, MysticPasted)
	end.

change_card_zone(Cards) ->
	lists:foreach(fun({CardOwner, CardOrder, CardID} ) -> 
		CardZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
		%stack_pool:add_stack_option_field(self(), card_previous_zone, [{{CardOwner, CardOrder, CardID}, CardZone}]),
		MoveFrom = from_zone_status(CardZone),
		stack_pool:add_stack_option_field(self(), card_previous_zone, [{{CardOwner, CardOrder, CardID}, MoveFrom}]),
		card_utility:change_card_zone(CardOwner, CardOrder, CardID, CardZone, hand_cards),
		card_utility:add_card_status(CardOwner, CardOrder, CardID, moving_to_hand, hand_cards),
		card_utility:add_card_status(CardOwner, CardOrder, CardID, MoveFrom, hand_cards) end, Cards).

from_zone_status(FromZone) ->
	case FromZone of
		hand_cards -> hand_to_hand;
		arena_zone -> arena_to_hand;
		support_cards -> arena_to_hand;
		seal_deck -> deck_to_hand;
		mystic_deck -> deck_to_hand;
		_ -> io:format ("From zone out of range ~p~n", [FromZone])
	end.
	
	
% % 705.3. Ability ของ Seal หรือ Mystic Card ที่จะทำงานเมื่อขึ้นมือ หรือ เมื่อออกจากสนาม  ถ้ามีการเลือกการทำงานให้เลือก ใน Phase นี้ --
% % และจะไม่มีการเปลี่ยนแปลง นอกจากจะมี Effect ให้สามารถทำได้ --
% % 705.4. หากต้องทำการเลือกเป้าหมาย ให้เลือกเป้าหมายใน Phase นี้ --	
check_moving_to_hand_ability() ->
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	stack_pool:set_stack_option (self(), play, check_moving_to_hand_ability),
	mod_ability_activate:check_any_ability_activate(moving_to_hand, PlayerPid).

% % 705.5. Interfere Step
moving_to_hand_interfere() ->
	stack_pool:set_stack_option(self(), play, card_to_hand_interfere),
	interfere_step:into_sub_interfere().
	 
% 705.6. ตรวจสอบว่า Ability สามารถส่งผลกับเป้าหมายที่กำหนดได้หรือไม่ หาก Ability ไม่สามารถส่งผลกับเป้าหมายที่กำหนด หรือ
			% เป้าหมายที่กำหนดไม่อยู่ในสภาพตกเป็นเป้าหมายได้ ให้กลับไปเลือกเป้าหมายใน Phase 705.4 ใหม่ หากไม่มีเป้าหมายใหม่ที่ Ability นั้นๆ สามารถส่งผล
			% หรือ กำหนดได้ Ability นั้นจะไม่ทำงาน
verify_moving_to_hand_ability() ->
	stack_pool:set_stack_option(self(), play, verify_moving_to_hand_ability),
	mod_ability_activate:verify_ability_condition(moving_to_hand).

% 705.7. Ability เมื่อขึ้นมือ, เมื่อออกจากสนาม, Ability ที่ทำงานในมือ หรือ Ability ต่างๆที่ทำงานเมื่อมีการ์ดในมือตรงตามที่กำหนดจะทำงานทันที --
activate_moving_to_hand_ability() ->
	stack_pool:set_stack_option(self(), play, activate_moving_to_hand_ability),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(moving_to_hand, PlayerPid).
			
% 705.8. Interfere Step
moving_to_hand_interfere2() ->
	stack_pool:set_stack_option(self(), play, card_to_hand_interfere2),
	interfere_step:into_sub_interfere().
	
check_on_hand_ability() ->
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	stack_pool:set_stack_option (self(), play, check_on_hand_ability),
	mod_ability_activate:check_any_ability_activate(on_hand, PlayerPid).
	
verify_on_hand_ability() ->
	stack_pool:set_stack_option(self(), play, verify_on_hand_ability),
	mod_ability_activate:verify_ability_condition(on_hand).
	
activate_on_hand_ability() ->
	stack_pool:set_stack_option(self(), play, activate_on_hand_ability),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	mod_ability_effect:check_any_ability_activate(on_hand, PlayerPid).

% 705.9. การ์ดที่กำลังขึ้นมือจะสูญเสียสภาพการ์ดที่กำลังขึ้นมือใน Phase นี้ --
card_to_hand_continuous_activate() ->
	%{ok, PlayerPid} = stack_pool:get_last_stack(self(), card_player),
	%{ok, Hands} = mnesia_play:get_player_data(PlayerPid, hand_cards),
	%OppPid = mnesia_play:get_opponent_pid(PlayerPid),
	%{ok, OppHand} = mnesia_play:get_player_data(OppPid, hand_cards),
	io:format("Hand card add~n"),
	{ok, Cards} = stack_pool:get_last_stack(self(), cards_to_hand),
	lists:foreach(fun({CardOwner, CardOrder, CardID}) -> 
	MoveFrom = find_previous_zone(CardOwner, CardOrder, CardID),
		case MoveFrom of
			arena_to_hand -> 
				card_utility:remove_card_status(CardOwner, CardOrder, CardID, arena_to_hand, hand_cards),
				card_utility:add_card_status(CardOwner, CardOrder, CardID, arena_to_hand_success, hand_cards);
			deck_to_hand ->
				card_utility:remove_card_status(CardOwner, CardOrder, CardID, deck_to_hand, hand_cards);
			_ ->""
		end,
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, moving_to_hand, hand_cards) end, Cards),
	gen_server:cast(self(), {hand_card_add, Cards}),
	stack_pool:set_stack_option(self(), play, activate_on_hand_effect),
	continuous_ability:check_continuous_target().
	
find_previous_zone(CardOwner, CardOrder, CardID) ->
	{ok, Cards} = stack_pool:get_last_stack(self(), card_previous_zone),
	Zone = lists:keysearch({CardOwner, CardOrder, CardID}, 1, Cards),
	case Zone of
		{value, {{_, _, _ }, MoveFrom}} -> MoveFrom;
		_ -> not_zone
	end.
	

card_to_hand_complete() ->
	stack_pool:pop_stack_out(self()),
	case stack_pool:get_last_stack(self(), play) of
		{ok, StackPlay} -> interfere_step:return_play(StackPlay);
		{error, _} -> gen_server:cast(self(), {act_next_command})
	end.
	%gen_server:cast(self(), {hand_card_add, lists:flatlength(OppHand), Cards}).

%hand_card_added () ->
	%interfere_step:return_play().
% 705.10. หากมี Effect ใดๆ ทำให้การ์ดที่กำลังขึ้นมือเปลี่ยนไปยัง Zone อื่นได้สำเร็จ ให้การ์ดที่กำลังขึ้นมือสูญเสียสภาพการ์ดที่กำลังขึ้นมือทันที --

% -------------------- Other Function ------------------------------
get_support_seal ([]) -> [];
get_support_seal ([{PlayerPid, CardOrder, CardID} | T]) ->
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			case card_utility:check_card_zone (PlayerPid, CardOrder, CardID) of
				arena_zone ->
					{ok, Support, _MysticSupport} = arena_zone:break_support_seal (PlayerPid, CardOrder, CardID),
					card_utility:remove_card_status(PlayerPid, CardOrder, CardID, be_combine),
					card_utility:remove_card_status(PlayerPid, CardOrder, CardID, combined),
					effect_activate:send_update_activate_effect (PlayerPid, CardOrder, CardID, [], update),
					SupportData = get_support_data (Support),
					io:format ("Support data ~p~n", [SupportData]),
					SupportData -- [{PlayerPid, CardOrder, CardID}] ++ get_support_seal (T);
				_ ->	get_support_seal (T)
			end;
		is_not_seal ->	get_support_seal (T)
	end.

get_support_data ([]) -> [];
get_support_data ([{{PlayerPid, CardOrder, CardID}, _} | T]) ->
	[{PlayerPid, CardOrder, CardID}] ++ get_support_data (T);
get_support_data ([{PlayerPid, CardOrder, CardID} | T]) ->
	[{PlayerPid, CardOrder, CardID}] ++ get_support_data (T).
