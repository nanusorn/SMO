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
-module (discard).

-compile (export_all).

player_discard (PlayerPid, Cards) ->
% 702. ขั้นตอนการถูก Discard
	io:format ("Discards ~p~n", [Cards]),
	stack_pool:push_stack (self(), PlayerPid, 0, 0, [{play, player_discard_0}, {cards_discard, Cards}]),
	interfere_step:return_play (check_play_step).

% 702.1. เมื่อ Seal หรือ Mystic Card ถูก Discard ด้วย Effect ต่างๆ จะเรียก Seal หรือ Mystic Card ที่ถูก Discard ว่า การ์ดที่ถูก Discard
player_discard_1 () ->
	stack_pool:set_stack_option (self(), play, play_player_discard_1),
	{ok, Cards} = stack_pool:get_last_stack (self(), cards_discard),
	set_card_status (Cards),
	interfere_step:return_play (check_play_step).

set_card_status ([]) -> ok;
set_card_status ([{PlayerPid, CardOrder, CardID} | Cards]) ->
%	Zone = card_utility:check_card_zone (PlayerPid, CardOrder, CardID),
	card_utility:add_card_status (PlayerPid, CardOrder, CardID, discard, hand_cards),
	set_card_status (Cards).

% 702.2. Ablity เมื่อ Discard, เมื่อ Discard สำเร็จ, เมื่อถูก Discard และ/หรือ Ability เมื่อถูก Discard สำเร็จ ถ้ามีการเลือกการทำงานให้เลือกใน Phase นี้ --
% และจะไม่มีการเปลี่ยนแปลง นอกจากจะมี Effect ให้สามารถทำได้ --
player_discard_2 () ->
	stack_pool:set_stack_option (self(), play, play_player_discard_2),
	interfere_step:return_play (check_play_step).

% 702.3. หากต้องทำการเลือกเป้าหมาย ให้เลือกเป้าหมายใน Phase นี้ --
player_discard_3 () ->
	stack_pool:set_stack_option (self(), play, play_player_discard_3),
	interfere_step:return_play (check_play_step).

% 702.4. ตรวจสอบว่า Ability สามารถส่งผลกับเป้าหมายที่กำหนดได้หรือไม่ หาก Ability ไม่สามารถส่งผลกับเป้าหมายที่กำหนด หรือ 
% เป้าหมายที่กำหนดไม่อยู่ในสภาพตกเป็นเป้าหมายได้ ให้กลับไปเลือกเป้าหมายใน Phase 702.3 ใหม่ หากไม่มีเป้าหมายใหม่ที่ Ability นั้นๆ
% สามารถส่งผล หรือ กำหนดได้ Ability นั้นจะไม่ทำงาน
player_discard_4 () ->
	stack_pool:set_stack_option (self(), play, play_player_discard_4),
	interfere_step:return_play (check_play_step).

% 702.5. Ability เมื่อ Discard, เมื่อ Discard สำเร็จ, เมื่อถูก Discard และ/หรือ Ability เมื่อถูก Discard สำเร็จ จะทำงานใน Phase นี้ ถ้าเงื่อนไขในการเกิด ยังถูกต้อง
player_discard_5 () ->
	stack_pool:set_stack_option (self(), play, play_player_discard_5),
	interfere_step:return_play (check_play_step).

% 702.6. การ์ดที่ถูก Discard จะสูญเสียสภาพการ์ดที่ถูก Discard ใน Phase นี้ --
player_discard_6 (PlayerPid) ->
	stack_pool:set_stack_option(self(), play, play_player_discard_6),
	{ok, Cards} = stack_pool:get_last_stack(self(), cards_discard),
	DiscardSize = lists:flatlength(Cards),
	DiscardData = remove_discard_status (Cards),
	{ok, Hand} = mnesia_play:get_player_data(PlayerPid, hand_cards),
	%%%self() ! {response_discard, PlayerPid, lists:flatlength(Hand) - 1, DiscardSize, DiscardData}.
	%gen_server:cast(self(), {response_discard, PlayerPid, lists:flatlength(Hand) - 1, DiscardSize, DiscardData}).
	gen_server:cast(self(), {response_discard, PlayerPid, lists:flatlength(Hand), DiscardSize, DiscardData}).

remove_discard_status ([]) -> [];
remove_discard_status ([{PlayerPid, CardOrder, CardID} | Cards]) ->
	card_utility:remove_card_status (PlayerPid, CardOrder, CardID, discard, hand_cards),
	[CardOrder, <<CardID:16>>] ++ remove_discard_status (Cards).

% 702.7. นำการ์ดนั้นไปยัง Shrine ยกเว้นมี Effect อื่นระบุไว้ให้ไม่เป็นเช่นนั้น
discard_to_shrine (PlayerPid) ->
	{ok, Cards} = stack_pool:get_last_stack (self(), cards_discard),
	stack_pool:pop_stack_out(self()),
	shrine_zone:card_to_shrine(PlayerPid, Cards).

% ฟั่งชั่นอื่นๆที่จะถูกเรียกใช้ซึ่งไม่เกี่ยวกับตัว Main Flow ของการทิ้งการ์ด
check_cards_discard (PlayerPid) ->
	case stack_pool:get_last_stack (self(), play) of
		{ok, attack_subturn_discard} ->
			stack_pool:set_stack_option (self(), play, attack_subturn_discarded);
		{ok, defend_subturn_discard} ->
			stack_pool:set_stack_option (self(), play, defend_subturn_discarded)
	end,
	{ok, Hands} = mnesia_play:get_player_data (PlayerPid, hand_cards),
	{ok, DiscardType} = stack_pool:get_last_stack (self(), discard_type),
	case DiscardType of
		seal_card ->
			Discards = get_cards_discard (Hands, is_seal),
			player_discard (PlayerPid, Discards);
		mystic_card ->
			Discards = get_cards_discard (Hands, is_not_seal),
			player_discard (PlayerPid, Discards);
		_ ->	io:format ("Other discard type ~p~n", [DiscardType]),
			player_discard (PlayerPid, Hands)
	end.

get_cards_discard ([], _) -> [];
get_cards_discard ([{{PlayerPid, CardOrder, CardID}, _} | Hands], CardType) ->
	case mnesia_odbc:is_seal_card (CardID) of
		CardType ->
			[{PlayerPid, CardOrder, CardID}] ++ get_cards_discard (Hands, CardType);
		_ ->	get_cards_discard (Hands, CardType)
	end.