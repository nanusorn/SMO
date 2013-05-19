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
-module(set1_wind_continue).
-compile(export_all).

continue_condition() ->
	[
		%[S] [S] สามารถโจมตีข้ามไปยัง Df Line ได้ ถ้า Seal ที่ถูก [S] โจมตีมี Sp น้อยกว่า [S]
		{
			continue_condition, 
			s1_wind_no61_a2, 317, 2,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],											
							n, null, [null], null, [], null,
								n,
								[{active_zone, [arena_zone]}]
		},
		%ผู้เล่นทุกคนไม่สามารถกำหนด Line Seal ที่มี Df มากกว่า [S] ให้เปลี่ยน Line ได้ (At Line)
		{
			continue_condition, 
			s1_wind_no65_a1, 321, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {line, 1}],													
							n, null, [null], n, [], null,
								n,
								[{active_zone, [arena_zone]}]
		},
		%[S] +Sp ตามจำนวน Mystic Card ในมือผู้ควบคุม
		{
			continue_condition, 
			s1_wind_no73_a1, 329, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							n, null, [null], null, [], null,
								e,
								[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่ [S] รวมร่าง Seal ทุกใบในสนามฝ่ายเรา Sp = 4
		{
			continue_condition, 
			s1_wind_no74_a1, 330, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {combine, y}],													
							n, null, [null], null, [], null,
								n,
								[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่เรามี [S] อยู่ในสนามตั้งแต่ 2 ใบขึ้นไป ผู้เล่นฝ่ายตรงข้ามทุกคนการ์ดในมือสูงสุด -1 (ถ้ามี [S] ตั้งแต่ 2 ใบขึ้นไปในสนามฝ่ายเรา เลือก Ability นี้ทำงานได้เพียง 1 ใบเท่านั้น)
		{
			continue_condition, 
			s1_wind_no75_a1, 331, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}],													
							y, owner, [arena_zone], y, [{card_type, seal}, {name, "Night Flower Fairy"}], 2,
								n,
								[{active_zone, [arena_zone]}]
		},
		%ตราบเท่าที่ [S] มี [Dragon] เป็น Seal ใบรองรวมร่าง [S] สามารถโจมตีข้ามไปยัง Df Line ได้
		{
			continue_condition, 
			s1_wind_no111_a1, 367, 1,
				n, [],
					n, null, [],									
						y, [{zone, [arena_zone]}, {combine, {type, "Dragon"}}],											
							n, null, [null], null, [], null,
								n,
								[{active_zone, [arena_zone]}]
		}	,
		%N[Charles] ทุกใบในสนาม Sp +1
		{
			continue_condition,
			p1_wind_no118_a1, 374, 1,
				n, [],
					n, null, [],									
						y, [{zone, [arena_zone]}],
							n, null, [null], null, [], null,
								n,
									[{active_zone, [arena_zone]}]
		}
	].
