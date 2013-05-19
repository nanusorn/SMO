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
-module(set3_earth_continue).
-compile(export_all).

continue_condition() ->
	[
		%*23 [S] +At ตามจำนวน [Be] ใน Shrine ฝ่ายตรงข้าม
		{
			continue_condition, 
				s3_earth_no023_a2, 791, 2,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [null], null, [], null,
									e, 
										[{active_zone, [arena_zone]}]
		},
		%*25 ตราบเท่าที่มี Grace, the Valkyrie อยู่ในสนาม [S] At +1 Df +1 
		{
			continue_condition, 
				s3_earth_no025_a1, 793, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								y, null, [arena_zone], y, [{card_type, seal}, {name, "Grace, the Valkyrie"}], 1,
									n, 
										[{active_zone, [arena_zone]}]
		},
		%*27 ผู้เล่นทุกคนไม่สามารถสั่ง [Be] โจมตี [S] ได้
		{
			continue_condition, 
				s3_earth_no027_a2, 795, 2,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [], null, [], null,
									n, 
										[{active_zone, [arena_zone]}]
		},
		%*29 [S] ยกเลิก Curse
		{
			continue_condition, 
				s3_earth_no029_a1, 797, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [], null, [], null,
									n, 
										[{active_zone, [arena_zone]}]
		},
		%*31 [S] + At ตามจำนวน [S] ใบอื่นในสนาม
		{
			continue_condition, 
				s3_earth_no031_a1, 799, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [], null, [], null,
									e, 
										[{active_zone, [arena_zone]}]
		},
		%*31 [S]  สามารถโจมตีจาก Df Line ได้
		{
			continue_condition, 
				s3_earth_no031_a2, 799, 2,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [], null, [], null,
									n, 
										[{active_zone, [arena_zone]}]
		},
		%*32 [Be] ใบอื่นในสนามฝ่ายเรา Df +1
		{
			continue_condition, 
				s3_earth_no032_a1, 800, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [], null, [], null,
									n, 
										[{active_zone, [arena_zone]}]
		},
		%*34 ตราบเท่าที่ฝ่ายตรงข้ามมี Seal ในสนามมากกว่าเรา [S] Df +3
		{continue_condition, 
			s3_earth_no034_a1, 802, 1,
				n,[],
					n, null, [],									
						y, [{zone, [arena_zone]}, {arena_count, {owner, seal, less}}],											
							n, null, [null], null, [], null,
								n,
								[{active_zone, [arena_zone]}]
		}
	].