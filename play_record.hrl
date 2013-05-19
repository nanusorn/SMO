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

% GameData = {game_state, player_turn, seal_check_up, max_mp, max_shrine}
% game_state = {checkup, draw, discard, inter1, main, inter2, refillmp, eos}
-record (game_data, {	room_pid,
					game_state,
					player_turn_pid,
					seal_check_up,
					max_mp,
					max_shrine,
					player_list,
					log_process_id,
					continue_ability,
					deck_ability_card} ).

% CardData = [{player_pid, card_order, card_id}]
% PlayerData = {player_pid, first_turn, sealdeck, mysticdeck, hand_cards, arena_cards, shrine_cards, shrine_lv, mp_rest}
% ArenaCard = [{CardData, CardDetail}]
% seal_card_detail = {is_active, line, is_growth, ability_status, combine_status, mystic_paste, effect_status}
% AbilityStauts = [{ability_type, ability_option1, ability_option2, ability_turn, is_activated}]
% EffectStatus = [{CardData, effect_type, effect_value, effect_turn}]
-record (player_data, {	player_pid,
					player_status,
					first_turn,
					seal_deck,
					mystic_deck,
					hand_cards,
					arena_cards,
					support_cards,
					shrine_cards,
					remove_cards,
					shrine_level,
					mp_rest,
					hand_max,
					player_effect} ).

% PlayStackList = [PlayerPid, CardOrder, CardID, Option]
% Option = [{play_status, ...}]
-record (play_stack, {		room_pid,
					play_stack_list} ).

% Growth = {CardID, CardSet, CardNo, GrowthAlternate}
% GrowthAlternate = {[{aa, bb, cc}, {aa}]} {...} เป็นข้อมูลของแต่ละ option ในการเลือกที่จะ growth
% ดังตัวอย่างคือ ต้องใช้ การ์ด ประเภท {aa, bb, cc} และ {aa} เป็นทางเลือกที่ 1
%-record (growth_data, {	card_id,
%					option1,
%					option2,
%					option3,
%					option4,
%					option5} ).
	
-record (growth_data, {	card_id,
					option_list} ).

-record (card_infomation, {	card_data, card_power} ).