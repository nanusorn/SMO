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
-module(set3_light_continue).
-compile(export_all).

continue_condition() ->
	[
		%*1 Growth: Pony Unicorn > [S] ยกเลิก Curse
		{
			continue_condition, 
				s3_light_no001_a1, 769, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [], null, [], null,
									n, 
										[{active_zone, [arena_zone]}]
		},		
		%*3 [S] ยกเลิก Curse
		{
			continue_condition, 
				s3_light_no003_a2, 771, 2,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [], null, [], null,
									n, 
										[{active_zone, [arena_zone]}]
		},
		%*4 [S] ยกเลิก Curse
		{
			continue_condition, 
				s3_light_no004_a1, 772, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [], null, [], null,
									n, 
										[{active_zone, [arena_zone]}]
		},
		%*5 [S] +At ตามจำนวน Charge Counter ของ [S]
		{
			continue_condition, 
				s3_light_no005_a1, 773, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [null], null, [], null,
									e, 
										[{active_zone, [arena_zone]}]
		},
		%*7 [S] -Mp ตามจำนวน Damica, the Black Wood Tamer ในสนาม
		{
			continue_condition, 
				s3_light_no007_a1, 775, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [], null, [], null,
									e, 
										[{active_zone, [arena_zone]}]
		},
		%*11 ตราบเท่าที่มี [Gri] ใบอื่นในสนาม [S] At +2
		{
			continue_condition, 
				s3_light_no011_a1, 779, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								y, null, [arena_zone], n, [{naming, "Griffin"}], 1,
									n, 
										[{active_zone, [arena_zone]}]
		},
		%11 ผู้เล่นไม่สามารถสั่ง [Gri] โจมตี [S] ได้
		{
			continue_condition, 
				s3_light_no011_a2, 779, 2,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [], null, [], null,
									n, 
										[{active_zone, [arena_zone]}]
		},
		%*1
		%*12 Growth : Pony Unicorn > [S] ยกเลิก Curse
		{
			continue_condition, 
				s3_light_no012_a1, 780, 1,
					n,[],
						n, null, [],									
							y, [{zone, [arena_zone]}],													
								n, null, [], null, [], null,
									n, 
										[{active_zone, [arena_zone]}]
		}
	].