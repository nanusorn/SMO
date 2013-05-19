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
-module(set1_dark_continue).
-compile(export_all).

continue_condition() ->
	[
		%[S] สามารถโจมตีข้ามไปยัง Df Line ได้ ถ้า Seal ที่ถูก [S] โจมตีมี Sp มากกว่า [S]
		{continue_condition, 
		s1_dark_no76_a1, 332, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}],													
						n, null, [null], null, [], null,
							n,
							[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่ [S] อยู่ใน Shrine ไม่สามารถนำ [S] เข้ามาในสนามจาก Shrine ได้ 
		{continue_condition, 
		s1_dark_no80_a2, 336, 2,
			n,[],
				n, null, [],									
					y, [{zone, [shrine_cards]}],													
						n, null, [null], null, [], null,
							n,
							[{active_zone, [shrine_cards]}]
		},
		%[S] สามารถโจมตีข้ามไปยัง Df Line ได้ 
		{continue_condition, 
		s1_dark_no82_a1, 338, 1,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}],													
						n, null, [null], null, [], null,
							n,
							[{active_zone, [arena_zone]}]
		},
		%ผู้ควบคุม [S] ไม่สามารถกำหนด Line ให้ [S] อยู่ที่ Df Line ได้ 
		{continue_condition, 
		s1_dark_no82_a2, 338, 2,
			n,[],
				n, null, [],									
					y, [{zone, [arena_zone]}],													
						n, null, [null], null, [], null,
							n,
							[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่ [S] ติด Mystic Card ประเภท [Tarot]: The Moon [S] At +2
		{continue_condition, 
		s1_dark_no87_a1, 343, 1,
			n, [],
				n, null, [],									
					y, [{zone, [arena_zone]}, {paste, {[{mystic_type, 1}, {mystic_subtype, "The Moon"}], 1}}],													
						n, null, [null], null, [], null,
							n,
							[{active_zone, [arena_zone]}]
		}
	].
