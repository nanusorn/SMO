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
-module (move_to_library).

-export ([update_card_to_library/0, move_to_library/2, move_to_library_resume/0, continue_move_to_library/0, card_to_library/1]).
%-export ([get_card_reply_data/2]).
-export([
						check_into_library_ability/1,
						check_remove_from_arena_ability/1,
						verify_into_library_ability/0,
						verify_remove_from_arena_ability/0,
						activate_into_library_effect/0,
						activate_remove_from_arena_effect/0,
						remove_from_zone_card_status/0,
						move_card_to_deck/0
					]).

% get_card_reply_data (_, []) -> [];
% get_card_reply_data (PlayerPid, [{PlayerPid, CardOrder, CardID} | T]) ->
	% [1, CardOrder, <<CardID:16>>] ++ get_card_reply_data (PlayerPid, T);
% get_card_reply_data (PlayerPid, [{_, CardOrder, CardID} | T]) ->
	% [0, CardOrder, <<CardID:16>>] ++ get_card_reply_data (PlayerPid, T).

% 704. ขั้นตอนการกลับเข้า Library
get_pasted_mystic ([]) -> [];
get_pasted_mystic ([{PlayerPid, CardOrder, CardID} | T]) ->
	Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	%ability_affect:remove_all_give_effect_target (PlayerPid, CardOrder, CardID),
	case Zone of
		arena_zone ->
			Pasted = arena_zone:get_mystic_pasted (PlayerPid, CardOrder, CardID);
		_ ->	Pasted = []
	end,
	Pasted ++ get_pasted_mystic (T).

card_to_library([]) -> [];
card_to_library([{CardOwner, CardOrder, CardID} | T]) ->
	OpponentPid = mnesia_play:get_opponent_pid(CardOwner),
	FromZone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	card_utility:remove_card(CardOwner, CardOrder, CardID, FromZone, CardOwner),
	card_utility:remove_card(CardOwner, CardOrder, CardID, FromZone, OpponentPid),
	MoveFrom = from_zone_status(FromZone),
	add_to_library(CardOwner, CardOrder, CardID, MoveFrom),
	effect_activate:send_update_activate_effect(CardOwner, CardOrder, CardID, [], update),
	card_to_library (T).

add_to_library(CardOwner, CardOrder, CardID, MoveFrom) ->
	io:format("Add card to library ~p~n", [{CardOwner, CardOrder, CardID}]),
	case mnesia_odbc:is_seal_card (CardID) of
		is_seal ->
			{ok, SealDeck} = mnesia_play:get_player_data(CardOwner, seal_deck),
			DeckUpdate = SealDeck ++ [{{CardOwner, CardOrder, CardID}, seal_card:set_create_option(CardID, [{card_status, moving_to_seal_deck}, {card_status, MoveFrom}])}],
			% ทำการสลับกอง
			DeckFinal = lib_arena_play:shuffer (DeckUpdate, 30),
			mnesia_play:set_player_data (CardOwner, seal_deck, DeckFinal);
%			CardOption = seal_card:create_option ([{card_status, moving_to_shrine}]);
		is_not_seal ->
			{ok, MysticDeck} = mnesia_play:get_player_data(CardOwner, mystic_deck),
			DeckUpdate = MysticDeck ++ [{{CardOwner, CardOrder, CardID}, mystic_card:set_create_option(CardID, [{card_status, moving_to_mystic_deck}, {card_status, MoveFrom}])}],
			% ทำการสลับกอง
			DeckFinal = lib_arena_play:shuffer(DeckUpdate, 30),
			mnesia_play:set_player_data(CardOwner, mystic_deck, DeckFinal)
%			CardOption = mystic_card:create_option ([{card_status, moving_to_shrine}])
	end.

move_to_library(PlayerPid, MoveCards) ->
	SupportSeal = card_utility:get_support_seal(MoveCards),
	AllMove = MoveCards ++ SupportSeal,
	case change_zone:check_card_to_other_zone(AllMove, library) of
		[] -> do_move_to_library(PlayerPid, AllMove);
		ToOtherZone -> 
			MoveList = AllMove -- ToOtherZone,
			{ok, LastPlay} = stack_pool:get_last_stack(self(), play),
			put(last_play, LastPlay),
			stack_pool:set_stack_option(self(), play, from_library_to_oher_zone),
			stack_pool:set_stack_option(self(), card_to_other_zone, ToOtherZone),
			case MoveList of
				[] -> interfere_step:return_play(check_play_step);
				_ -> 
					do_move_to_library(PlayerPid, MoveList)
			end
	end.
	
do_move_to_library(PlayerPid, MoveCards) ->
% 704.1. เมื่อ Seal หรือ Mystic Card ที่ถูกทำนำกลับเข้า Library ด้วย Effect ต่างๆ จะเรียก Seal หรือ Mystic Card ที่ถูกนำกลับเข้า Library ว่า การ์ด ที่กำลังกลับ Library
	stack_pool:push_stack(self(), PlayerPid, 0, 0, [{player, PlayerPid}, {play, cards_to_library}, {move_cards, MoveCards}]),
	MysticPasted = get_pasted_mystic(MoveCards),
% 704.2. นำการ์ดที่กำลังกลับ Library กลับเข้าใน Library โดยถือว่าการ์ดอยู่ใน Library ใน Phase นี้โดยเจ้าของการ์ดที่กำลังกลับ Library เป็นผู้จัดเรียงกลับ
% และทำการสลับ Library ให้อยู่ในสภาพสุ่มทุกครั้ง นอกจากจะมี Effect ให้ไม่ต้องทำให้ Library อยู่ในสภาพสุ่ม
	card_to_library(MoveCards),
% หากการ์ดที่กำลังกลับ Library มี Mystic Card ติดอยู่ให้ทำการนำ Mystic Card ที่ติดอยู่ลง Shrine ใน Phase นี้ การ์ดที่กำลังกลับ Library ทุกใบ
% จะลืม Effect ทั้งหมดที่กระทำกับตัวมันนอกจาก Effect นั้นจะระบุไว้ว่ายังคงส่งผลแม้การ์ดนั้นอยู่ใน Library
	case MysticPasted of
		[] ->	interfere_step:return_play(check_play_step);
		_ ->	shrine_zone:card_to_shrine(PlayerPid, MysticPasted)
	end.

from_zone_status(FromZone) ->
	case FromZone of
		hand_cards -> hand_to_library;
		arena_zone -> arena_to_library;
		support_cards -> arena_to_library;
		shrine_zone -> shrine_to_library;
		_ -> io:format ("From zone out of range ~p~n", [FromZone])
	end.
	
update_card_to_library() ->
	stack_pool:set_stack_option (self(), play, update_cards_move_to_library),
	{ok, MoveCards} = stack_pool:get_last_stack (self(), move_cards),
	{ok, PlayerList} = mnesia_play:get_game_data (self(), player_list),
	send_move_cards_msg(PlayerList, MoveCards).

%send_move_cards_msg ([], _) -> all_send;
send_move_cards_msg (PlayerList, MoveCards) ->
	lists:foreach(fun({PlayerPid, _}) -> 
		ReplyMsg = get_move_cards_msg (PlayerPid, MoveCards, 0),
%	ReplyMsgOpp = get_move_card_msg_opp (PlayerPid, MoveCards, 0),
		io:format("Reply msg ~p~n", [ReplyMsg]),
		gen_server:cast(self(), {update_cards_to_library, PlayerPid, [16#88, 16#74], ReplyMsg})
	end, PlayerList).

get_move_cards_msg (_, [], MoveSize) -> [MoveSize];
get_move_cards_msg (PlayerPid, [{PlayerPid, CardOrder, CardID} | MoveCards], MoveSize) ->
	get_move_cards_msg (PlayerPid, MoveCards, MoveSize + 1) ++ [1, CardOrder, <<CardID:16>>];
get_move_cards_msg (PlayerPid, [{_, CardOrder, CardID} | MoveCards], MoveSize) ->
	get_move_cards_msg (PlayerPid, MoveCards, MoveSize + 1) ++ [0, CardOrder, <<CardID:16>>].
	
% get_move_card_msg_opp (_, [], MoveSize) -> [MoveSize];
% get_move_card_msg_opp (PlayerPid, [{PlayerPid, CardOrder, CardID} | MoveCards], MoveSize) ->
	% get_move_card_msg_opp (PlayerPid, MoveCards, MoveSize + 1) ++ [0, CardOrder, <<CardID:16>>];
% get_move_card_msg_opp (PlayerPid, [{_, CardOrder, CardID} | MoveCards], MoveSize) ->
	% get_move_card_msg_opp (PlayerPid, MoveCards, MoveSize + 1) ++ [1, CardOrder, <<CardID:16>>].

% 704.3. Ability ของ Seal หรือ Mystic Card ที่จะทำงานเมื่อกลับเข้า Library  หรือ เมื่อออกจากสนามถ้ามีการเลือกการทำงานให้เลือก ใน Phase นี้ --
% และจะไม่มีการเปลี่ยนแปลง นอกจากจะมี Effect ให้สามารถทำได้ --
% 704.4. หากต้องทำการเลือกเป้าหมาย ให้เลือกเป้าหมายใน Phase นี้ --
check_into_library_ability(PlayerPid) ->
	stack_pool:set_stack_option (self(), play, check_into_library_ability),
	mod_ability_activate:check_any_ability_activate(into_library, PlayerPid).
	
check_remove_from_arena_ability(PlayerPid) ->
	stack_pool:set_stack_option(self(), play, check_remove_from_arena_to_lib_ability),
	mod_ability_activate:check_any_ability_activate(remove_from_arena, PlayerPid).
	
continue_move_to_library() ->
% 704.5. Interfere Step
	stack_pool:set_stack_option (self(), play, move_library_interfere),
	interfere_step:into_sub_interfere().
	
% 704.6. ตรวจสอบว่า Ability สามารถส่งผลกับเป้าหมายที่กำหนดได้หรือไม่ หาก Ability ไม่สามารถส่งผลกับเป้าหมายที่กำหนด หรือ
% เป้าหมายที่กำหนดไม่อยู่ในสภาพตกเป็นเป้าหมายได้ ให้กลับไปเลือกเป้าหมายใน Phase 704.4 ใหม่ หากไม่มีเป้าหมายใหม่ที่ Ability นั้นๆ
% สามารถส่งผล หรือ กำหนดได้ Ability นั้นจะไม่ทำงาน
verify_into_library_ability() ->
	stack_pool:set_stack_option(self(), play, verify_into_library_ability),
	mod_ability_activate:verify_ability_condition(into_library).
	
verify_remove_from_arena_ability() ->
	stack_pool:set_stack_option(self(), play, verify_remove_from_arena_to_lib_ability),
	mod_ability_activate:verify_ability_condition(remove_from_arena).

% 704.7. Ability เมื่อกลับเข้า Library, เมื่อออกจากสนาม, Ability ที่ทำงานใน Library หรือ Ability ต่างๆที่ทำงานเมื่อมีการ์ดใน Library ตรงตามที่กำหนดจะทำงานทันที --
activate_into_library_effect() ->
	stack_pool:set_stack_option(self(), play, activate_into_library_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), player),
	mod_ability_effect:check_any_ability_activate(into_library, PlayerPid).

activate_remove_from_arena_effect() ->
	stack_pool:set_stack_option(self(), play, activate_remove_from_arena_to_lib_effect),
	{ok, PlayerPid} = stack_pool:get_last_stack(self(), player),
	mod_ability_effect:check_any_ability_activate(remove_from_arena, PlayerPid).
	
remove_from_zone_card_status() ->
	stack_pool:set_stack_option(self(), play, remove_from_zone_to_lib_card_status),
	{ok, DestroyedCards} = stack_pool:get_last_stack(self(), move_cards),
	lists:foreach(fun({CardOwner, CardOrder, CardID}) -> remove_from_zone_status(CardOwner, CardOrder, CardID) end, DestroyedCards),
	interfere_step:return_play(check_play_step).
	
remove_from_zone_status(CardOwner, CardOrder, CardID) ->
	Zone = card_utility:check_card_zone(CardOwner, CardOrder, CardID),
	io:format("get card zone ~p~n", [Zone]),
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, moving_to_seal_deck, Zone),
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, moving_to_mystic_deck, Zone),
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, hand_to_library, Zone),
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, arena_to_library, Zone),
	card_utility:remove_card_status(CardOwner, CardOrder, CardID, shrine_to_library, Zone).

move_to_library_resume() ->
% 704.8. Interfere Step
	stack_pool:set_stack_option (self(), play, move_library_interfere2),
	interfere_step:into_sub_interfere().

move_card_to_deck() ->
% 704.9. การ์ดที่กำลังกลับ Library จะสูญเสียสภาพการ์ดที่กำลังกลับ Library ใน Phase นี้ --
	{ok, {PlayerPid, _, _, _}} = stack_pool:get_last_stack (self()),
	{ok, PS} = mnesia_play:get_player_data (PlayerPid, seal_deck),
	{ok, PM} = mnesia_play:get_player_data (PlayerPid, mystic_deck),
	OpponentPid = mnesia_play:get_opponent_pid (PlayerPid),
	{ok, OS} = mnesia_play:get_player_data (OpponentPid, seal_deck),
	{ok, OM} = mnesia_play:get_player_data (OpponentPid, mystic_deck),
	PSS = lists:flatlength(PS),
	PMS = lists:flatlength(PM),
	OSS = lists:flatlength(OS),
	OMS = lists:flatlength(OM),
	stack_pool:pop_stack_out(self()),
	gen_server:cast(self(), {update_deck_size, PlayerPid, PSS, PMS, OSS, OMS}).

% 704.10. หากมี Effect ใดๆ ทำให้การ์ดที่กำลังกลับ Library เปลี่ยนไปยัง Zone อื่นได้สำเร็จ ให้การ์ดที่กำลังกลับ Library สูญเสียสภาพการ์ดที่กำลังกลับ Library ทันที --