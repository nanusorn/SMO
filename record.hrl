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
-record (room, {	room_id,
			room_pid,
			rank,
			mode,
			room_name,
			password,
			max_player_in_room,
			player_in_room,
			play_time,
			room_status,
			allow_observer,
			sub_time,
			is_ready}).

-record (new_player_data, {	user_id,
				player_name,
				player_name_utf,
				playing_status,
				point,
				rank,
				rating,
				avatar,
				deck_used,
				socket,
				name,
				arena_pid}).

-record (user_data, {	user_id,
				player_name,
				player_name_utf,
				playing_status,
				point,
				rank,
				rating,
				avatar,
				deck_used,
				process_id}).

-record (seal_card, {	card_id,
				card_set,
				card_no,
				card_name,
				card_type,
				card_element,
				card_naming,
				mp_cast,
				mp_atk,
				level,
				attack,
				defend,
				speed,
				rarity} ).

-record (mystic_card, {card_id,
					card_set,
					card_no,
					card_name,
					card_type, % 1:Tarot, 2:Relic, 3:Devas เปลี่ยนแปลงได้
					card_subtype, % เปลี่ยนได้
					paste_type, % 1:PS, 2:PA, 3:PM, 4:NP
					card_naming, % เปลี่ยนแปลงได้
					is_interfere, % เปลี่ยนแปลงได้
					mp_cast, % เปลี่ยนแปลงได้
					duration} ). % เปลี่ยนแปลงได้

-record (combination, {	card_id,
					option_list} ).

%% ability_list = [{ability_id, ability_option, line, combine, target_id, effect_id, effect_option, effect_subturn, effect_sub_option}, {...}]
