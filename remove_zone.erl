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
-module (remove_zone).

-export ([req_remove_info/2]).
-export ([remove_card_to_zone/1]). %activate_remove_ability/0, check_card_ability_target/0, 
-export ([move_to_remove_zone/2, card_to_remove_zone/1]). %, change_zone_to_remove/1
%-export ([activate_select_target_ability/0]). %check_ability_select_target/0, 
-export ([interfere_remove_2/0, completed_remove_cards/0]). %activate_all_ability_affact/1, 
-export([
						check_into_remove_ability/1,
						remove_card_sub_interfere/0,
						verify_into_remove_ability/0,
						activate_into_remove_effect/0
					]).


req_remove_info (RequestPid, PlayerPid) ->
	case 1 of
		1 ->	{ok, RemoveCards} = mnesia_play:get_player_data(PlayerPid, remove_cards),
			response_remove_info (RequestPid, RemoveCards);
		0 ->	RemoveCards = get_test_data_reply (seal) ++ get_test_data_reply (mystic),
			response_remove_info (RequestPid, RemoveCards)
	end.

get_test_data_reply (Type) ->
	case Type of
		seal ->
			[{{0, 1, 257}, 0}, {{0, 2, 258}, 0}, {{0, 3, 259}, 0}, {{0, 4, 260}, 0}, {{0, 5, 261}, 0}, 
			{{0, 6, 262}, 0}, {{0, 7, 263}, 0}, {{0, 8, 264}, 0}, {{0, 9, 265}, 0}, {{0, 10, 266}, 0}];
		mystic ->
			[{{0, 1, 356}, 0}, {{0, 2, 356}, 0}, {{0, 3, 357}, 0}, {{0, 4, 357}, 0}, {{0, 5, 358}, 0}, 
			{{0, 6, 358}, 0}, {{0, 7, 359}, 0}, {{0, 8, 359}, 0}, {{0, 9, 360}, 0}, {{0, 10, 360}, 0},
			{{0, 11, 361}, 0}, {{0, 12, 361}, 0}, {{0, 13, 362}, 0}]
	end.

response_remove_info (RequestPid, RemoveCards) ->
	{SealDR, SSize} = get_data_reply (RemoveCards, [], is_seal),
	{MysticDR, MSize} = get_data_reply (RemoveCards, [], is_not_seal),
	SealReply = [SSize] ++ SealDR,
	MysticReply = [MSize] ++ MysticDR,
	ReplyData = [16#8b, 16#02] ++ SealReply ++ MysticReply,
	io:format ("Reply data ~p~n", [ReplyData]),
	gen_server:cast(self(), {res_remove_info, RequestPid, ReplyData}).

get_data_reply (List, DataReply, CheckType) ->
	get_data_reply (List, DataReply, CheckType, 0).

get_data_reply ([], DataReply, _, NumCard) -> {DataReply, NumCard};
get_data_reply ([{{_, CardOrder, CardID}, _} | T], DataReply, CheckType, NumCard) ->
	case mnesia_odbc:is_seal_card (CardID) of
		CheckType ->
			get_data_reply (T, DataReply ++ [CardOrder, <<CardID:16>>], CheckType, NumCard + 1);
		_ ->
			get_data_reply (T, DataReply, CheckType, NumCard)
	end.

% 706. ขั้นตอนการถูก Remove
move_to_remove_zone (PlayerPid, ToRemove) ->
	case change_zone:check_card_to_other_zone(ToRemove, remove_zone) of
		[] -> smo_logger:fmsg("card to oher zone is ~p~n", [none]),
			do_move_to_remove_zone(PlayerPid, ToRemove);
		ToOtherZone -> 
			smo_logger:fmsg("card to oher zone is ~p~n", [ToOtherZone]),
			RemoveCards = ToRemove -- ToOtherZone,
			{ok, LastPlay} = stack_pool:get_last_stack(self(), play),
			put(last_play, LastPlay),
			stack_pool:set_stack_option(self(), play, from_remove_to_oher_zone),
			stack_pool:set_stack_option(self(), card_to_other_zone, ToOtherZone),
			case RemoveCards of
				[] -> interfere_step:return_play(check_play_step);
				_ -> 	do_move_to_remove_zone(PlayerPid, RemoveCards)
			end
	end.
	
do_move_to_remove_zone(PlayerPid, RemoveCards) ->
% 706.1. เมื่อ Seal หรือ Mysic Card ที่ถูก Remove ด้วย Effect ต่างๆ จะเรียก Seal หรือ Mystic Card ที่ถูก Remove ว่า การ์ด ที่กำลังถูก Remove
	stack_pool:push_stack (self(), PlayerPid, 0, 0, [{play, card_remove}, {card_removing, RemoveCards}]),
	{ok, PlayerList} = mnesia_play:get_game_data (self(), player_list),
	send_remove_msg(PlayerList, RemoveCards).
	%interfere_step:return_play(check_play_step).

send_remove_msg(PlayerList, RemoveList) ->
	lists:foreach(fun({PlayerPid, _PlayerName}) ->
									ReplyMsg = function_utility:get_card_to_client(PlayerPid, RemoveList, 0),
									smo_logger:fmsg("send msg remove ~p~n", [ReplyMsg]),
									gen_server:cast(self(), {update_change_zone, remove_zone, PlayerPid, [16#88, 16#3d], ReplyMsg})
									% case mnesia_play:get_game_data(self(), game_step) of
										% {ok, eos} ->
											% gen_server:cast(self(), {update_change_zone, PlayerPid, [16#88, 16#3d], ReplyMsg});
										% _ ->
											% gen_server:cast(self(), {update_change_zone, PlayerPid, [16#88, 16#3d], ReplyMsg})
									% end
								end, PlayerList).
								
% 706.2. นำการ์ดที่กำลังถูก Remove เข้าสู่ Remove Zone โดยถือว่าการ์ดอยู่ใน Remove Zone ใน Phase นี้ โดยเจ้าของการ์ดที่กำลังถูก Remove
% เป็นผู้นำเข้า Remove Zone หากการ์ดที่กำลังถูก Remove มี Mystic Card ติดอยู่ให้ทำการนำ Mystic Card ที่ติดอยู่ลง Shrine ใน Phase นี้ -
% การ์ดที่กำลังถูก Remove ทุกใบจะลืม Effect ทั้งหมดที่กระทำกับตัวมันนอกจาก Effect นั้นจะระบุไว้ว่ายังคงส่งผลแม้การ์ดนั้นอยู่ใน Remove Zone
card_to_remove_zone(PlayerPid) ->
	{ok, RemoveCards} = stack_pool:get_last_stack(self(), card_removing),
	stack_pool:set_stack_option(self(), play, mystic_to_shrine_from_remove_card),
	case pasted_mystic_to_shrine(RemoveCards) of
		[] -> interfere_step:return_play(check_play_step);
		MysticDestroyed -> shrine_zone:card_to_shrine(PlayerPid, MysticDestroyed)
	end.

remove_card_to_zone(PlayerPid) ->
	{ok, RemoveCards} = stack_pool:get_last_stack(self(), card_removing),
	cards_remove(RemoveCards),
	% activate_remove_ability(PlayerPid).
	check_into_remove_ability(PlayerPid).
% 
% activate_remove_ability(PlayerPid) ->
	% stack_pool:set_stack_option (self(), play, check_ability_into_remove),
	% %mod_ability_activate:check_any_ability_activate(into_remove, PlayerPid).
	% interfere_step:return_play(check_play_step).

cards_remove([]) -> [];
cards_remove([{PlayerPid, CardOrder, CardID} | RemoveCards]) ->
	remove_to_zone(PlayerPid, CardOrder, CardID),
	cards_remove(RemoveCards).

remove_to_zone(PlayerPid, CardOrder, CardID) ->
	OpponentPid = mnesia_play:get_opponent_pid(PlayerPid),
	CardZone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	stack_pool:set_stack_option(self(), {previous_zone, PlayerPid, CardOrder, CardID}, CardZone),
	{ok, CardStatus} = card_utility:get_card_option_field(PlayerPid, CardOrder, CardID, card_status, CardZone),
	card_utility:remove_card(PlayerPid, CardOrder, CardID, CardZone, PlayerPid),
	card_utility:remove_card(PlayerPid, CardOrder, CardID, CardZone, OpponentPid),
	add_to_remove(PlayerPid, CardOrder, CardID, CardStatus),
	case CardZone of
		arena_zone -> card_utility:add_card_status(PlayerPid, CardOrder, CardID, remove_from_arena, remove_cards);
		shrine_cards -> card_utility:add_card_status(PlayerPid, CardOrder, CardID, remove_from_shrine, remove_cards);
		seal_deck -> card_utility:add_card_status(PlayerPid, CardOrder, CardID, remove_from_deck, remove_cards);
		support_cards -> card_utility:add_card_status(PlayerPid, CardOrder, CardID, remove_from_arena, remove_cards);
		mystic_deck -> card_utility:add_card_status(PlayerPid, CardOrder, CardID, remove_from_deck, remove_cards);
		hand_cards -> card_utility:add_card_status(PlayerPid, CardOrder, CardID, remove_from_hand, remove_cards)
	end.

add_to_remove (PlayerPid, CardOrder, CardID, CardStatus) ->
	{ok, Remove} = mnesia_play:get_player_data (PlayerPid, remove_cards),
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			CardOption = seal_card:set_create_option(CardID, transform(CardStatus));
		is_not_seal ->
			CardOption = mystic_card:set_create_option(CardID, transform(CardStatus))
	end,
	mnesia_play:set_player_data (PlayerPid, remove_cards, Remove ++ [{{PlayerPid, CardOrder, CardID}, CardOption}]).

transform([]) -> [];
transform([Status | T]) -> [{card_status, Status}] ++ transform(T).

% change_zone_to_remove (PlayerPid) ->
	% stack_pool:set_stack_option (self(), play, removing_cards),
	% OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	% {ok, RemoveCards} = stack_pool:get_last_stack (self(), card_removing),
	% {CardSize, PlayerRemoveData, OpponentPlayerData} = get_remove_cards_data (RemoveCards, PlayerPid, OpponentPid),
	% PlayerData = [CardSize] ++ PlayerRemoveData,
	% OpponentData = [CardSize] ++ OpponentPlayerData,
	% io:format ("Activate remove cards ~p ~p~n", [PlayerData, OpponentData]),
	% gen_server:cast(self(), {activate_remove_cards, PlayerPid, PlayerData, OpponentPid, OpponentData}).

% get_remove_cards_data ([], _, _) -> {0, [], []};
% get_remove_cards_data ([{PlayerPid, CardOrder, CardID} | RemoveCards], PlayerPid, OpponentPid) ->
	% {CardSize, PlayerCard, OpponentCard} = get_remove_cards_data (RemoveCards, PlayerPid, OpponentPid),
	% {CardSize + 1, PlayerCard ++ [1, CardOrder, <<CardID:16>>], OpponentCard ++ [0, CardOrder, <<CardID:16>>]};
% get_remove_cards_data ([{OpponentPid, CardOrder, CardID} | RemoveCards], PlayerPid, OpponentPid) ->
	% {CardSize, PlayerCard, OpponentCard} = get_remove_cards_data (RemoveCards, PlayerPid, OpponentPid),
	% {CardSize + 1, PlayerCard ++ [0, CardOrder, <<CardID:16>>], OpponentCard ++ [1, CardOrder, <<CardID:16>>]}.

% check_ability_select_target () ->
	% %{ok, RemoveCards} = stack_pool:get_last_stack (self(), card_removing),
	% %remove_status_check (RemoveCards),
	% stack_pool:set_stack_option(self(), play, activate_ability_remove_select_target),
	% {ok, AllArena} = card_utility:get_all_card (arena_zone),
	% {ok, AllShrine} = card_utility:get_all_card (shrine_cards),
	% {ok, AllRemove} = card_utility:get_all_card (remove_cards),
	% Cards = card_utility:get_cards_have_status (AllArena ++ AllShrine ++ AllRemove, ability_select_target),
	% stack_pool:set_stack_option (self(), select_ability_target, Cards),
	% interfere_step:return_play (check_play_step).

% 706.3. Ability ของ Seal หรือ Mystic Card ที่จะทำงานเมื่อถูก Remove หรือ เมื่อออกจากสนาม ถ้ามีการเลือกการทำงาน
% ให้เลือก ใน Phase นี้ และจะไม่มีการเปลี่ยนแปลง นอกจากจะมี Effect ให้สามารถทำได้ -
% 706.4. หากต้องทำการเลือกเป้าหมาย ให้เลือกเป้าหมายใน Phase นี้ -
check_into_remove_ability(PlayerPid) ->
	stack_pool:set_stack_option(self(), play, check_into_remove_ability),
	mod_ability_activate:check_any_ability_activate(into_remove, PlayerPid).

% 706.5. Interfere Step
remove_card_sub_interfere() ->
	stack_pool:set_stack_option(self(), play, remove_card_sub_interfere),
	interfere_step:into_sub_interfere().

% 706.6. ตรวจสอบว่า Ability สามารถส่งผลกับเป้าหมายที่กำหนดได้หรือไม่ หาก Ability ไม่สามารถส่งผลกับเป้าหมายที่กำหนด
% หรือ เป้าหมายที่กำหนดไม่อยู่ในสภาพตกเป็นเป้าหมายได้ ให้กลับไปเลือกเป้าหมายใน Phase 706.4. ใหม่ -
% หากไม่มีเป้าหมายใหม่ที่ Ability นั้นๆ สามารถส่งผล หรือ กำหนดได้ Ability นั้นจะไม่ทำงาน
verify_into_remove_ability() ->
	stack_pool:set_stack_option(self(), play, verify_into_remove_ability),
	mod_ability_activate:verify_ability_condition(into_remove).

% 706.7. Ability เมื่อถูก Remove, เมื่อออกจากสนาม, Ability ที่ทำงานใน Remove Zone หรือ Ability ต่างๆที่ทำงานเมื่อมีการ์ดใน Remove Zone ตรงตามที่กำหนดจะทำงานทันที -
activate_into_remove_effect() ->
	stack_pool:set_stack_option(self(), play, activate_into_remove_effect),
	{ok, {PlayerPid, _, _, _}} = stack_pool:get_last_stack(self()),
	mod_ability_effect:check_any_ability_activate(into_remove, PlayerPid).
	
remove_status_check(PlayerPid, CardOrder, CardID) ->
	CardZone = card_utility:check_card_zone(PlayerPid, CardOrder, CardID),
	case stack_pool:get_last_stack(self(), {previous_zone, PlayerPid, CardOrder, CardID}) of
		{ok, hand_cards} -> card_utility:remove_card_status(PlayerPid, CardOrder, CardID, remove_from_hand, CardZone);
		{ok, shrine_cards} ->	card_utility:remove_card_status(PlayerPid, CardOrder, CardID, remove_from_shrine, CardZone);
		{ok, arena_zone} -> card_utility:remove_card_status(PlayerPid, CardOrder, CardID, remove_from_arena, CardZone);
		{ok, seal_deck} -> card_utility:remove_card_status(PlayerPid, CardOrder, CardID, remove_from_deck, CardZone);
		{ok, mystic_deck} -> card_utility:remove_card_status(PlayerPid, CardOrder, CardID, remove_from_deck, CardZone);
		_ ->
			card_utility:remove_card_status(PlayerPid, CardOrder, CardID, remove_from_hand, CardZone),
			card_utility:remove_card_status(PlayerPid, CardOrder, CardID, remove_from_shrine, CardZone),
			card_utility:remove_card_status(PlayerPid, CardOrder, CardID, remove_from_arena, CardZone),
			card_utility:remove_card_status(PlayerPid, CardOrder, CardID, remove_from_deck, CardZone),
			card_utility:remove_card_status(PlayerPid, CardOrder, CardID, remove_from_deck, CardZone)
	end.
																													% activate_select_target_ability () ->
																														% case stack_pool:get_last_stack (self(), select_ability_target) of
																															% {ok, []} ->
																													% % 706.5. Interfere Step
																																% stack_pool:set_stack_option(self(), play, move_to_remove_interfere),
																																% interfere_step:into_sub_interfere();
																															% {ok, [{PlayerPid, CardOrder, CardID} | Cards]} ->
																																% stack_pool:set_stack_option (self(), select_ability_target, Cards),
																																% Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
																																% card_utility:remove_card_status (PlayerPid, CardOrder, CardID, ability_select_target, Zone),
																																% % ใส่ลงไปในการ์ดที่เลือกเป้าหมายเรียบร้อยแล้วเพื่อรอตรวจสอบเป้าหมายและแสดงผล
																																% stack_pool:add_stack_option_field (self(), ability_target_selected_cards, [{PlayerPid, CardOrder, CardID}]),
																																% ability_utility:check_ability_select_target (PlayerPid, CardOrder, CardID)
																														% end.
% 
																														% % 706.6. ตรวจสอบว่า Ability สามารถส่งผลกับเป้าหมายที่กำหนดได้หรือไม่ หาก Ability ไม่สามารถส่งผลกับเป้าหมายที่กำหนด
																														% % หรือ เป้าหมายที่กำหนดไม่อยู่ในสภาพตกเป็นเป้าหมายได้ ให้กลับไปเลือกเป้าหมายใน Phase 706.4. ใหม่ -
																														% % หากไม่มีเป้าหมายใหม่ที่ Ability นั้นๆ สามารถส่งผล หรือ กำหนดได้ Ability นั้นจะไม่ทำงาน
																														% check_card_ability_target () ->
																															% stack_pool:set_stack_option(self(), play, activate_ability_remove_target_verify),
																															% case stack_pool:get_last_stack (self(), ability_target_selected_cards) of
																																% {ok, Cards} ->
																																	% case ability_affect:ability_target_verify (Cards) of
																																		% [] ->	interfere_step:return_play(check_play_step);
																																		% _ ->	check_ability_select_target ()
																																	% end;
																																% _ -> interfere_step:return_play(check_play_step)
																															% end.
% 
																													% % 706.7. Ability เมื่อถูก Remove, เมื่อออกจากสนาม, Ability ที่ทำงานใน Remove Zone หรือ Ability ต่างๆที่ทำงานเมื่อมีการ์ดใน Remove Zone ตรงตามที่กำหนดจะทำงานทันที -
																													% activate_all_ability_affact (PlayerPid) ->
																														% stack_pool:set_stack_option (self(), play, activate_all_remove_ability_affect),
																														% case stack_pool:get_last_stack (self(), ability_activate) of
																															% {ok, []} -> interfere_step:return_play (check_play_step);
																															% {ok, Cards} ->
																													% %			io:format("Card activate ability ~p~n", [Cards]),
																																% ability_affect:check_have_to_arrange_ability_affect (PlayerPid, Cards, [], []);
																															% {error, _} -> interfere_step:return_play (check_play_step)
																														% end.

% 706.8. Interfere Step
interfere_remove_2 () ->
	stack_pool:set_stack_option(self(), play, move_to_remove_interfere_2),
	interfere_step:into_sub_interfere().
	
% 706.9. การ์ดที่กำลังถูก Remove จะสูญเสียสภาพการ์ดที่กำลังถูก Remove ใน Phase นี้ -
completed_remove_cards () ->
	{ok, RemoveCards} = stack_pool:get_last_stack (self(), card_removing),
	lists:foreach(fun({PlayerPid, CardOrder, CardID}) -> remove_status_check(PlayerPid, CardOrder, CardID) end, RemoveCards),
	lists:foreach(fun({PlayerPid, CardOrder, CardID}) -> remove_cards_status(PlayerPid, CardOrder, CardID) end, RemoveCards),
	io:format("-- Move to Remove zone completed-~n"),
	interfere_step:return_play().

remove_cards_status (PlayerPid, CardOrder, CardID) ->
	Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	card_utility:remove_card_status (PlayerPid, CardOrder, CardID, into_remove, Zone).
	
% 706.10. หากมี Effect ใดๆ ทำให้การ์ดที่กำลังถูก Remove เปลี่ยนไปยัง Zone อื่นได้สำเร็จ ให้การ์ดที่กำลังถูก Remove สูญเสียสภาพการ์ดที่กำลังถูก Remove ทันที -




%----------------------Function-------------------------
pasted_mystic_to_shrine([]) -> [];
pasted_mystic_to_shrine([{PlayerPid, CardOrder, CardID} | T]) ->
	MysticCards = 
	case arena_zone:get_mystic_pasted(PlayerPid, CardOrder, CardID) of
		[] -> [];
		Pasted -> shrine_zone:add_status_to_cards(Pasted), Pasted
	end,
	MysticCards ++ pasted_mystic_to_shrine(T).
