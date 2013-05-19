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
-module(set3_mystic_ability).
-export([mystic/0]).

mystic() -> 
	[
		% 100   Pege Lagoon
		%เมื่อ [Unicorn] และ/หรือ [Pegasus] ฝ่ายเราตก Shrine จากสนาม เราสามารถแสดง [Unicorn] และ/หรือ [Pegasus] ที่มีชื่อเดียวกับ [Unicorn] และ/หรือ [Pegasus] นั้น 1 ใบจากกองการ์ด Seal เราขึ้นมือ จากนั้นสลับกองการ์ดนั้น
		{card_ability, 
			s3_no100_a1, 868, 1,
				n,[],
					n, null, [],									
						%y, [{zone, [arena_zone]}, {{owner, [seal_deck], null, null}, {[{naming_or, ["Unicorn", "Pegasus"]}, {name, {as, card_destroy}}], 1}}],
						%y, [{zone, [arena_zone]}, {activate_ability, {2, 1}}, {{owner, [{seal_deck, top, 10}], null, null}, {[{naming_or, ["Unicorn", "Pegasus"]}], 1}}],
						y, [{zone, [arena_zone]}, {activate_ability, {2, 1}}, {{owner, [{seal_deck, top, 3}], null, null}, {[], 3}}],
							y, owner, [shrine_cards], n, [{naming_or, ["Unicorn", "Pegasus"]}, {action, into_shrine}, {action, arena_to_shrine}, {stack_check, card_destroy}], 1,
								n, [],
									n, null, {}, null,
										n, null, null, [],	null,												
						               n, n, {}, null,											
												y, owner, [{seal_deck, top, 3}], null, [],
													y, {do_not_need_select, 0,[{action, {show, 3, [{naming_or, ["Unicorn", "Pegasus"]}], move_to_hand, 1}}]}, null
		}
	].
